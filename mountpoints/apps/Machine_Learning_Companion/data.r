pdf(file = NULL)
library(DBI)
library(RPostgreSQL)
library(shiny)
library(shinydashboard)
library(shinyBS)
#library(plotly)
library(treemap)
#library(RColorBrewer)
library(shinyTree)
library(ggplot2)
library(shinyWidgets)
library(dplyr)
library(scales)
library(cluster)
library(data.table)
library(DT)
library(epiDisplay)
library(bnlearn)
library(bnviewer)
library(fastcluster)
library(gridExtra)
library(kmed)
library(Rtsne)
library(umap)
library(rpart)
library(rpart.plot)
library(config)

set.seed(123)
select <- dplyr::select

dat2 <- list()
glm_plots <-list()

## For moving to postrgres

config <- config::get(file = "config.yml")

print("Testing connection to database...")
con <- dbConnect(
  PostgreSQL(),
  dbname = config$sql_credentials$database,
    user = config$sql_credentials$uid,
    host = config$sql_credentials$server,
    password = config$sql_credentials$pwd,
    port = config$sql_credentials$port
)


Query <- dbGetQuery(con, "SELECT 
--nhs_number, 
--id, 
-- execution_date, 
 --cpm_date, 
 --gpp_id, 
 --gpp_code, 
 --gpp_name, 
 gpp_short_name, 
 --ccg_code, 
 ccg_name, 
 --nhs_number_report, 
 age as \"Age\", 
 sex as \"Sex\", 
 risk_score as \"Risk score\", 
 risk_score_rank as \"Risk Score Rank\", 
 risk_score_trend as \"Risk Score Trend\", 
 risk_score_group as \"Risk Score Group\", 
 risk_score_int as \"Risk score int\", 
 risk_segment as \"Risk Segment\", 
 ip_admissions_in_last_12_months as \"IP admissions in last 12 months\", 
 --\"ip_admissions_in_last_12_months_(cpm)\", 
 ip_elective_admissions_in_last_12_months as \"IP elective admissions in last 12 months\", 
 op_appointments_in_last_12_months as \"OP appointments in last 12 months\", 
 ae_attendances_in_last_12_months as \"AE attendances in last 12 months\", 
 asthma 					as \"Ltc_asth\",
 chd                     as \"Ltc_cad\",
 heart_failure            as \"Ltc_chf\",
 cancer                   as \"Ltc_cncr\",
 copd                    as \"Ltc_copd\",
 depression               as \"Ltc_depr\",
 diabetes                 as \"Ltc_diab\",
 hypertension            as \"Ltc_hten\",
 atrial_fibrillation     as \"ltc_af\",
 ckd                      as \"ltc_ckd\",
 dementia               as \"ltc_dementia\",
 epilepsy                 as \"ltc_epilepsy\",
 hypothyroid               as \"ltc_hypothyroid\",
 mental_health             as \"ltc_mentalhealth\",
 learning_disabilities     as \"ltc_learning_dis\",
 osteoporosis             as \"ltc_osteoporosis\",
 \"pad\"                      as \"ltc_pad\",
 rheumatoid_arthritis     as \"ltc_ra\",
 palliative_care_flag,
 stroke_tia as \"ltc_stroketia\",
 --risk_of_nel_register, 
 smoker, 
 substance_misuse, 
 psychotic_disorder_flag, 
 cdiff_flag, 
 oxygen_flag, 
 mosaic_label, 
 --housebound_text, 
 --winter_planning_text, 
 --palliative_care_text, 
 --care_plan_start, 
 --care_plan_end, 
 average_ip_admission_in_following_year, 
 average_nel_costs_in_following_year, 
 --total_cost, 
 --total_nel_cost, 
 --\"total_nel_cost_(cpm)\", 
 community_matron_status, 
 --community_matron_type, 
 \"community_matron_status-type\", 
 --community_matron, 
 --dys_since_last_nel, 
 --days_since_last_ae, 
 wardcode, 
 wardname, 
 --top_2_percent_unplanned, 
 case when Age >= 55 and Age <65 then '55 and over' 
      when Age >= 65 and Age <75then '65 and over'
      when Age >= 75 then '75 and over'
      when Age <= 16 then 'Children' 
      when Age > 16 and Age < 55 then '17-54'
      end as \"age markers\"

      , case when Age >= 55 and Age <65 then 1 else 0 end as \"age 55 and over\" 
      ,case  when Age >= 65 and Age <75then 1 else 0 end as \"age 65 and over\"
      ,case  when Age >= 75 then 1 else 0 end as \"age 75 and over\"
      , case when Age <= 16 then 1 else 0 end as \"age Children\"
      ,case when Age > 16 and Age < 55 then 1 else 0 end as \"age 17-54\"

      ,case when Age >= 0 and Age < 5 then '0-4'
      when Age >= 5 and Age < 10 then '5-9'
      when Age >= 10 and Age < 15 then '10-14'
      when Age >= 15 and Age < 20 then '15-19'
      when Age >= 20 and Age < 25 then '20-24'
      when Age >= 25 and Age < 30 then '25-29'
      when Age >= 30 and Age < 35 then '30-34'
      when Age >= 35 and Age < 40 then '35-39'
      when Age >= 40 and Age < 45 then '40-44'
      when Age >= 45 and Age < 50 then '45-49'
      when Age >= 50 and Age < 55 then '50-54'
      when Age >= 55 and Age < 60 then '55-59'
      when Age >= 60 and Age < 65 then '60-64'
      when Age >= 65 and Age < 70 then '65-69'
      when Age >= 70 and Age < 75 then '70-74'
      when Age >= 75 and Age < 80 then '75-79'
      when Age >= 80 and Age < 85 then '80-84'
      when Age >= 85 and Age < 90 then '85-89'
      when Age >= 90 then 'over 90'
      end as \"age band narrow\",
 --age_band_narrow, 
 age_band_broad, 
 chronic_condition_count, 
 taxonomy, 
 area, 
 case when \"deprivation_decile\" in (1,2) then 1 else 0 end as \"top 20 percent deprived\",
 deprivation_decile, 
 gp_data_feed, 
 fcvanguard, 
 data_date, 
 cpm_risk_score, 
 --welsh_risk_score, 
 --frailty_text, 
 --rs_frailty, 
 lsoa, 
 msoa, 
 household_category, 
 household_group, 
 household_type, 
 household_description, 
 wellbeing_acorn_group, 
 wellbeing_acorn_type, 
 wellbeing_acorn_description, 
 --ethnicity_from_sus, 
 --rockwood_frailty_score, 
 ethniccategory, 
 mh_inremission
 -- ndh_reg, 
 -- nh_rch_resident, 
 -- iscarer_flag, 
 -- hascarer_flag, 
 -- mh_physicalexaminationdone_flag, 
 -- matron_status_flag, 
 -- bmi, 
 -- bloodpressuresystolic, 
 -- bloodpressurediastolic, 
 -- bloodpressurereadingtype, 
 -- patientactivationmeasure, 
 -- datecoviddiagnosis, 
 -- longcovid, 
 -- covidvaccinationcount, 
 -- datecovidlastvaccination, 
 -- nhs_hc_eligible, 
 -- nhs_hc_completed, 
 -- nhs_hc_date, 
 -- spl, 
 -- covid_risk, 
 -- covid_vuln, 
 -- pcn, 
 -- local_authority, 
 -- cnt, 

 --mosaic, 
 du, 
 electoral_ward_or_division 
 --lcnt, 
 --fcnt
	FROM public.population_master
  WHERE \"deprivation_decile\" <> 0")
dbDisconnect(con)

## Welcome page stuff

dates <- as.Date(Query$`data_date`)
mindate <-as.Date(min(dates, na.rm = TRUE))
maxdate <- as.Date(max(dates, na.rm = TRUE))


## Renaming columns
df_names <- c(
Asthma	=	"Ltc_asth",
`Coronary artery Disease`	=	"Ltc_cad",
`Congestive heart Failure`	=	"Ltc_chf",
Cancer	=	"Ltc_cncr",
`Chronic obstructive pulmonary disease`	=	"Ltc_copd",
`Persistent depressive disorder`	=	"Ltc_depr",
Diabetes	=	"Ltc_diab",
Hypertension	=	"Ltc_hten",
`Atrial fibrillation`	=	"ltc_af",
`Chronic kidney disease`	=	"ltc_ckd",
Dementia	=	"ltc_dementia",
Epilepsy	=	"ltc_epilepsy",
Hypothyroid	=	"ltc_hypothyroid",
`Mental health`	=	"ltc_mentalhealth",
`Learning disability`	=	"ltc_learning_dis",
Osteoporosis	=	"ltc_osteoporosis",
`Peripheral artery disease`	=	"ltc_pad",
`Rheumatoid arthritis`	=	"ltc_ra",
Stroke	=	"ltc_stroketia")

ltc <- rownames(as.data.frame(df_names))

library(dplyr)
Query <- Query %>% rename(!!!df_names)

colnames(Query) <-gsub("_", " ", colnames(Query))

## Convert data types
Query$Sex <- as.factor(Query$Sex)
Query$`age markers` <- as.factor(Query$`age markers`) 
Query$`age 55 and over` <- as.factor(Query$`age 55 and over`)
Query$`age 65 and over` <- as.factor(Query$`age 65 and over`)
Query$`age 75 and over` <- as.factor(Query$`age 75 and over`) 
Query$`age Children` <- as.factor(Query$`age Children`) 
Query$`age 17-54` <- as.factor(Query$`age 17-54`) 

convert <- colnames(Query %>% select (c( ltc, `top 20 percent deprived`,`deprivation decile`, `palliative care flag`,`smoker`,`substance misuse`,`psychotic disorder flag`,
                            `cdiff flag`, `oxygen flag`)))

for (variable in convert) {
  
  Query[[variable]] <- as.integer(Query[[variable]])
  Query[[variable]] <- as.factor(Query[[variable]])
  
}
    
## Re level factors for referncing
Query$`age band narrow` <- factor(Query$`age band narrow`, levels = c("0-4","5-9","10-14","15-19","20-24","25-29","30-34","35-39","40-44","45-49","50-54",
                                  "55-59","60-64","65-69","70-74","75-79","80-84","85-89","Over 90"))
Query$`deprivation decile` <- relevel(factor(Query$`deprivation decile`), ref = "5")
Query$`age band broad` <- relevel(factor(Query$`age band broad`), ref = "18-64")
Query$`age band narrow` <- relevel(factor(Query$`age band narrow`), ref = "40-44")
Query$`Risk Score Group` <- relevel(factor(Query$`Risk Score Group`), ref = "02_04")   
Query$`mosaic label` <- relevel(factor(Query$`mosaic label`), ref = "J43") 
Query$taxonomy <- relevel(factor(Query$taxonomy), ref = "Oval")
Query$`household category` <- relevel(factor(Query$`household category`), ref = "3")
Query$`household group` <- relevel(factor(Query$`household group`), ref = "O")
Query$`household type` <- relevel(factor(Query$`household type`), ref = "27")
Query$`household description` <- relevel(factor(Query$`household description`), ref = "Suburban semis conventional attitudes")
Query$`wellbeing acorn group` <- relevel(factor(Query$`wellbeing acorn group`), ref = "2")
Query$`wellbeing acorn type` <- relevel(factor(Query$`wellbeing acorn type`), ref = "9")
Query$`wellbeing acorn description` <- relevel(factor(Query$`wellbeing acorn description`), ref = "Everyday excesses")

binomials <- ( c("Sex",ltc))

poissons <- (c("IP admissions in last 12 months",
              "IP elective admissions in last 12 months",
              "OP appointments in last 12 months",
              "AE attendances in last 12 months" ))

gaussians <- (c("Age",
              "Risk score",
              "Risk score int"))

modeltype <- as.data.frame(if_else(colnames(Query) %in% binomials, "binomial",
                                   if_else(colnames(Query) %in% poissons, "poisson",
                                           if_else(colnames(Query) %in% gaussians, "gaussian","NO"))))

names(modeltype)[1] <- "type"

modeltype <- modeltype %>% mutate(lookup = colnames(Query)) %>%
  filter(type != "NO")


Query[sapply(Query, is.character)] <- lapply(Query[sapply(Query, is.character)], 
                                             as.factor)

DATA2 <- Query[ , names(Query) %in% modeltype$lookup]

DATA2 <- DATA2 %>% mutate(Sex = if_else(Sex == 'M',1,0))

Query  <- Query %>% mutate(Sex = if_else(Sex == 'M',"Male",if_else(Sex == 'F',"Female",'I')))
##Columns for UI
decision_tree_vars = colnames(Query %>% select(3:length(Query)))
decision_tree_targets = colnames(Query %>% select (c(3:39,`deprivation decile`,`age band broad`,`age band narrow`)))

glm_vars = colnames(Query %>% select(c(3:length(Query),-Age,-`Risk score`, -`Risk Score Rank`, -`Risk Score Trend`, -`Risk score int`,
  -`Risk Segment`, -`IP admissions in last 12 months`, -`IP elective admissions in last 12 months`, -`OP appointments in last 12 months`,
  -`AE attendances in last 12 months`, -`average ip admission in following year`, -`average nel costs in following year`,-`chronic condition count`,
  -`cpm risk score` )))

glm_targets = colnames(DATA2)

Bayesian_cols = colnames(Query %>% select (c(ltc,Sex,`top 20 percent deprived`,`age 55 and over`, `age 65 and over`, `age 75 and over`, `age Children`, `age 17-54`)))

clustering_cols = colnames(Query %>% select(5:length(Query)))

names(Query) <- str_to_sentence(names(Query))
names(DATA2) <- str_to_sentence(names(DATA2))
Query <- rename(Query, "IP admissions in last 12 months" = "Ip admissions in last 12 months", "IP elective admissions in last 12 months" = "Ip elective admissions in last 12 months",
              "OP appointments in last 12 months"  = "Op appointments in last 12 months", "AE attendances in last 12 months" = "Ae attendances in last 12 months")

DATA2 <- rename(DATA2, "IP admissions in last 12 months" = "Ip admissions in last 12 months", "IP elective admissions in last 12 months" = "Ip elective admissions in last 12 months",
              "OP appointments in last 12 months"  = "Op appointments in last 12 months", "AE attendances in last 12 months" = "Ae attendances in last 12 months")