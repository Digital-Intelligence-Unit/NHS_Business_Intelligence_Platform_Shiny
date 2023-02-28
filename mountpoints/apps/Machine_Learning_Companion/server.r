pdf(file = NULL)
#source("./data.r")
#source("./app/ui.r")

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
 risk_score as \"Risk Score\", 
 risk_score_rank as \"Risk Score Rank\", 
 risk_score_trend as \"Risk Score Trend\", 
 risk_score_group as \"Risk Score Group\", 
 risk_score_int as \"Risk Score Int\", 
 risk_segment as \"Risk Segment\", 
 ip_admissions_in_last_12_months as \"IP Admissions in Last 12 Months\", 
 --\"ip_admissions_in_last_12_months_(cpm)\", 
 ip_elective_admissions_in_last_12_months as \"IP Elective Admissions in Last 12 Months\", 
 op_appointments_in_last_12_months as \"OP Appointments in Last 12 Months\", 
 ae_attendances_in_last_12_months as \"AE Attendances in Last 12 Months\", 
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
    where \"deprivation_decile\" <> 0
    and \"sex\" <> 'I'
    ")
dbDisconnect(con)

## Welcome page stuff

dates <- as.Date(Query$`data_date`)
mindate <-as.Date(min(dates, na.rm = TRUE))
maxdate <- as.Date(max(dates, na.rm = TRUE))


## Renaming columns
df_names <- c(
Asthma	=	"Ltc_asth",
`Coronary Artery Disease`	=	"Ltc_cad",
`Congestive Heart Failure`	=	"Ltc_chf",
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

poissons <- (c("IP Admissions in Last 12 Months",
              "IP Elective Admissions in Last 12 Months",
              "OP Appointments in Last 12 Months",
              "AE Attendances in Last 12 Months" ))

gaussians <- (c("Age",
              "Risk Score",
              "Risk Score Int"))

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

##Columns for UI
decision_tree_vars = colnames(Query %>% select(3:length(Query)))
decision_tree_targets = colnames(Query %>% select (c(3:39,`deprivation decile`,`age band broad`,`age band narrow`)))

glm_vars = colnames(Query %>% select(c(3:length(Query),-Age,-`Risk Score`, -`Risk Score Rank`, -`Risk Score Trend`, -`Risk Score Int`,
  -`Risk Segment`, -`IP Admissions in Last 12 Months`, -`IP Elective Admissions in Last 12 Months`, -`OP Appointments in Last 12 Months`,
  -`AE Attendances in Last 12 Months`, -`average ip admission in following year`, -`average nel costs in following year`,-`chronic condition count`,
  -`cpm risk score` )))

glm_targets = colnames(DATA2)

Bayesian_cols = colnames(Query %>% select (c(ltc,Sex,`top 20 percent deprived`,`age 55 and over`, `age 65 and over`, `age 75 and over`, `age Children`, `age 17-54`)))

clustering_cols = colnames(Query %>% select(5:length(Query)))


# code functions
source("./app/code/mod_summary.r")
#source("./app/code/show_results.R")

server <- function(input,output,session){

 #Cart
  observeEvent(input$twoCARTgo, {
    if(!is.null(input$twoCARTVar1)) {
      showModal(
        modalDialog(
          HTML("<h2><strong>Please wait while the data is segmented.</strong></h2>"),
          p("This should finish in no more than a minute or two when using the default settings. Growing larger trees may take more time."),
          footer = tagList()
        )
      )
      try({
        add_sco <- function(x){
          gsub(" ", "_", x, fixed = T)
        }
        
        data <- as.data.frame(Query %>% select_at(c(unique(input$twoCARTVar1), input$twoCARTVar2)))

        contols <- rpart.control(minbucket=input$twoCARTminbucket,
                                  maxdepth=input$twoCARTmaxdepth,
                                  cp=input$twoCARTcp)
        
        print("CART model")
        model<-rpart(as.formula(paste("`",input$twoCARTVar2,"`", " ~ .", sep = "")),data=data ,
                     control=contols)
        print("CART model end")
        
        output$twoCARTTree <- renderPlot({

          node.fun1 <- function(x, labs, digits, varlen)

          {
            paste(labs, "\nn =", x$frame$n2)
          }
          
          milfun <- function(x) {ifelse(x>1e5,paste0(round(x/1e6,3),"M"),x)}
          
          model$frame$n2 <- sapply(model$frame$n,milfun)
          
        if ((sapply(rev(data)[1], class))[1] == "integer" | (sapply(rev(data)[1], class))[1] == "numeric") {  
          
         extras <- 100 
        }
          
        else {
          extras <- 109
        }
          
        prp(model,
            type = 1, # left and right split labels (see Figure 2)
            clip.right.labs = FALSE, # full right split labels
            extra = extras , # show nbr of obs and percentages (see Figure 3)
            node.fun = node.fun1,
            under = FALSE, # position extra info _under_ the boxes
            under.cex = .8, # size of text under the boxes (default is .8)
            fallen.leaves = TRUE, # put leaves at the bottom of plot
            box.palette = "GnYlRd", # color of the boxes
            branch = .3, # branch lines with narrow shoulders and down slopes
            branch.type = 5,
            branch.tweak =0.25 ,
            faclen = 0,
            varlen = 0,
            #nn.font = 5,
            round = 0, # no rounding of node corners i.e. use rectangles
            leaf.round = 2, # round leaf nodes (for leaves, this supersedes the round arg)
            #prefix = "ozone\n", # prepend this string to the node labels
            tweak = 1.2,
            xcompact = TRUE,
            xcompact.ratio = 1,
            cex.main = 1.5, # use big text for main title
            branch.col = "gray", # color of branch lines
            branch.lwd = 2)
        })
         
         dat2$cartModel <<- model
        ids <- sort(unique(dat2$cartModel$where))
        rules <- rpart.rules(dat2$cartModel)#cover = TRUE,
        newIDs <- 1:length(ids)
        names(newIDs) <- ids
        newSeg <- newIDs[as.character(dat2$cartModel$where)]

        dat2$segMem[[3]] <<- newSeg

         updateCheckboxInput(session, "twoCARTTreeRules", value = FALSE)
         updateCheckboxInput(session, "twoCARTTreeVARIMP", value = FALSE)
      })
      removeModal()
    }
  })
#show_results
  output$twoCARTTreeRulesTableUI <- renderUI({
    if(input$twoCARTTreeRules) {
      column(width = 12,
             tableOutput("twoCARTTreeRulesTable"),style = "overflow-y: scroll;overflow-x: scroll;",
      )
    } else {
      uiOutput("twoCARTTreeRulesTableUIBlank")
    }
  })
  observeEvent(input$twoCARTTreeRules, ignoreInit = T, {
    if(input$twoCARTTreeRules & !is.na(dat2$cartModel)) {
      output$twoCARTTreeRulesTable <- renderTable({
        rules <- rpart.rules(dat2$cartModel, nn = TRUE)
        newIDs <- 1:nrow(rules)
        names(newIDs) <- sort(as.integer(rules$nn))
        newSeg <- newIDs[as.character(rules$nn)]
        rules <- rules[,-1]
        colnames(rules)[1:2] <- c(colnames(rules)[1], "Rule")
        colnames(rules)[3:ncol(rules)] <- " "
        cbind("Segment" = paste0("Segment ", newSeg), rules)
      })
    }
  })

  output$twoCARTTreeRulesTableUIVARIMP <- renderUI({
    if(input$twoCARTTreeVARIMP) {
      column(width = 12,
             plotOutput("twoCARTTreeRulesTableVARIMP2"),style = "overflow-y: scroll;overflow-x: scroll;",
      )
    } else {
      plotOutput("twoCARTTreeRulesTableUIBlankVAR")
    }
  })

  observeEvent(input$twoCARTTreeVARIMP, ignoreInit = T, {
    if(input$twoCARTTreeVARIMP) {
      output$twoCARTTreeRulesTableVARIMP2 <- renderPlot({

        df <- data.frame(imp = dat2$cartModel$variable.importance)
        df2 <- df %>% 
          tibble::rownames_to_column() %>% 
          dplyr::rename("variable" = rowname) %>% 
          dplyr::arrange(imp) %>%
          dplyr::mutate(variable = forcats::fct_inorder(variable))
        
        ggplot2::ggplot(df2) +
          geom_segment(aes(x = variable, y = 0, xend = variable, yend = imp), 
                       size = 1.5, alpha = 0.7) +
          geom_point(aes(x = variable, y = imp, col = variable), 
                     size = 4, show.legend = F) +
          coord_flip() +
          theme_bw()        
        
      })
    }
  })

    #GLMS

    observeEvent(input$glmgo, {
    if(!is.null(input$glmVar1)) {
      showModal(
        modalDialog(
          HTML("<h2><strong>Please wait while the data is segmented.</strong></h2>"),
          p("This should finish in no more than a minute or two."),
          footer = tagList()
        )
      )
      try({
        add_sco <- function(x){
          gsub(" ", "_", x, fixed = T)
        }
        updateCheckboxInput(session, "glmoddsratios", value = FALSE)

        data <- as.data.frame(Query %>% select_at(c(unique(input$glmVar1), input$glm2Var2)))

        dichotomous <- colnames(data %>%
                  as_data_frame() %>%
                  select(-c(input$glm2Var2)) %>%
                  select_if(~ is.factor(.) & nlevels(.) > 2))

        onehot <- colnames(data %>%
                  as_data_frame() %>%
                  select(-c(dichotomous),-c(input$glm2Var2)))
                
        mtype <- modeltype %>% filter(lookup == input$glm2Var2) %>% select (type )%>% pull

        if(mtype == "binomial"){
          
          model <- glm(as.formula(paste("`",input$glm2Var2,"`", " ~ .", sep = "")), family=(modeltype %>% filter(lookup == input$glm2Var2) %>% select (type )%>% pull), data=data)             
                    
          output[["glmFamily"]] <- renderText({
            
            paste0('Model Family ', (modeltype %>% filter(lookup == input$glm2Var2) %>% select (type )%>% pull))
          })
          
          `mod summary stars` <- mod_summary(model)
          
          output$glmTable <- renderTable({
            table <- logistic.display(model, simplified=TRUE,decimal  =2)
            table <- as.data.frame(table$table)
            ordered<- rownames(table)
            table <- merge(table, as.data.frame(`mod summary stars`), by=0, all=TRUE) 
            table <-table[match(ordered, table$Row.names),]

            for(i2 in unique(onehot)){
              table <- table %>%
              mutate(across(Row.names, ~ ifelse(startsWith(.x, i2),substring(.x, 1, nchar(.x)-1),
               ifelse(startsWith(.x, paste0("`",i2)),substring(.x, 1, nchar(.x)-1), .x))))
            }

            table <- na.omit(table,) %>% as.data.frame(row.names = 1:nrow(.))
            table <-table %>%  
                mutate(across(OR:upper95ci, ~ifelse(.x > 1000,formatC(.x, format="e", digits=2),round(.x,2))))
            
            table$OR <- as.character(table$OR)
            table$Row.names <- gsub("`", "", table$Row.names, fixed = TRUE)

            table$lower95ci <- as.character(table$lower95ci)
            table$`Pr(>|Z|)` <- as.character(table$`Pr(>|Z|)`)
            table$upper95ci <- as.character(table$upper95ci)
            table$`mod summary stars` <- as.character(table$`mod summary stars`)

            for(i in unique(dichotomous)){
            

            lv <-  as.integer(round(length(unique(levels(data[[i]])))/2) +1) 
          
            rt <- which(table$Row.names == paste0(i,levels(data[[i]])[lv]))[1]
            table <-table %>% add_row(Row.names = paste(c(": \t", levels(data[[i]])[1]),collapse = '\t'), OR = "Reference", lower95ci = "", `Pr(>|Z|)` = "", upper95ci="", `mod summary stars` ="", .before = rt)

            rt2 <- which(table$Row.names == paste0(i,levels(data[[i]])[2]))[1]
            

            table <- table %>% rowwise() %>% #paste(c("blah","blah"), collapse = "    ")
              mutate(across(Row.names, ~ ifelse(startsWith(.x, i),paste(c(": \t", substring(.x, nchar(i)+1)),collapse = '\t'),.x)))

            table <-table %>% add_row(Row.names = paste0(i), OR = "Categorical" , lower95ci = "", `Pr(>|Z|)` = "", upper95ci="", `mod summary stars` ="",.before = rt2)

            }

            table <- table %>% 
              rename(
                `variables` = `Row.names`
                )

            table
          }, 	
          include.rownames=TRUE) 
            
          glm_plots$table <<- logistic.display(model, simplified=TRUE)
          glm_plots$table <<- as.data.frame(glm_plots$table$table)
            
        }
        
        else if (mtype == "poisson") {
          
          names(data)<-make.names(names(data))

           model.poisson <- glm(as.formula(paste("`",make.names(input$glm2Var2),"`", " ~ .", sep = "")),
           family=(modeltype %>% filter(lookup == input$glm2Var2) %>% select (type )%>% pull),
           data=data)          
          
          output[["glmFamily"]] <- renderText({
            
            paste0('Model Family ', (modeltype %>% filter(lookup == input$glm2Var2) %>% select (type )%>% pull))
          })
          
          #mod_summary_stars <- mod_summary(model.poisson)
          
          output$glmTable <- renderTable({

           table <- idr.display(model.poisson)
           table <- as.data.frame(table$table)

           
           #table <- table[table$`crude.IDR.95.CI.`!='' &  table$`P.LR.test.` !="", ]
           table <- cbind(variables = rownames(table), data.frame(table, row.names=NULL))
           
           table$variables <- as.character(table$variables)
           table$variables <- gsub(".", " ", table$variables, fixed = TRUE)

          #  table <- table %>% 
          #   filter(`P.LR.test.` !="" & `crude.IDR.95.CI.` !="")
           table <- table %>% 
              rename(
                `crude IDR 95 CI` = `crude.IDR.95.CI.`,
                `adj IDR 95 CI` = `adj..IDR.95.CI.`,
                `P Wald s test` = `P.Wald.s.test.`,
                `P LR test` =  `P.LR.test.`
                ) %>%
                #mutate(flag = ifelse(`crude IDR 95 CI` !="" | `P LR test` !="",1,0)) 
            filter(`crude IDR 95 CI` !="" | `P LR test` !="")
            #table$`crude IDR 95 CI` <- as.character(table$`crude IDR 95 CI`)
            #table$`P LR test` <- as.character(table$`P LR test`)
            #table <- table %>% 
            # filter((`P LR test` !="" &`crude IDR 95 CI` !=""))
           			           
            #table <- table %>% rowwise() %>% 
             #  mutate(across(variables, ~ ifelse(startsWith(.x,"X "),paste(c(": \t", substring(.x, nchar(.x)-2)),collapse = '\t'),.x)))

            

          #print(table)
          table
          },
          include.rownames=FALSE)
          
          #output$glmTableSig <- renderTable({
            
          #table <- as.data.frame(mod_summary_stars)
          #}, 
          #include.rownames=TRUE) 
          
        }
        
        else if (mtype == "gaussian") {
          
          model <- glm(as.formula(paste("`",input$glm2Var2,"`", " ~ .", sep = "")), family=(modeltype %>% filter(lookup == input$glm2Var2) %>% select (type )%>% pull), data=data)             
          
          output[["glmFamily"]] <- renderText({
            
            paste0('Model Family ', (modeltype %>% filter(lookup == input$glm2Var2) %>% select (type )%>% pull))
          })
          `mod summary stars` <- mod_summary(model)
          
          output$glmTable <- renderTable({

            table <- regress.display(model, simplified=TRUE,decimal  =2)
            table <- as.data.frame(table$table)
            ordered<- rownames(table)
            table <- merge(table, as.data.frame(`mod summary stars`), by=0, all=TRUE) 
            table <-table[match(ordered, table$Row.names),]

            for(i2 in unique(onehot)){
              table <- table %>%
              mutate(across(Row.names, ~ ifelse(startsWith(.x, i2),substring(.x, 1, nchar(.x)-1),
               ifelse(startsWith(.x, paste0("`",i2)),substring(.x, 1, nchar(.x)-1), .x))))
            }

            table <- na.omit(table,) %>% as.data.frame(row.names = 1:nrow(.))
            table <-table %>%  
                mutate(across(Coeff:upper095ci, ~ifelse(.x > 1000,formatC(.x, format="e", digits=2),round(.x,2))))
            
            table$Coeff <- as.character(table$Coeff)
            table$Row.names <- gsub("`", "", table$Row.names, fixed = TRUE)

            #table$Coeff <- as.character(table$Coeff)
            table$lower095ci <- as.character(table$lower095ci)
            table$upper095ci <- as.character(table$upper095ci)
            table$`Pr>|t|` <- as.character(table$`Pr>|t|`)
            table$`mod summary stars` <- as.character(table$`mod summary stars`)
            
            for(i in unique(dichotomous)){         

            lv <-  as.integer(round(length(unique(levels(data[[i]])))/2) +1) 
          
            rt <- which(table$Row.names == paste0(i,levels(data[[i]])[lv]))[1]
            table <-table %>% add_row(Row.names = paste(c(": \t", levels(data[[i]])[1]),collapse = '\t'), Coeff = "Reference" ,lower095ci="",upper095ci="",`Pr>|t|`="",`mod summary stars`="", .before = rt)

            rt2 <- which(table$Row.names == paste0(i,levels(data[[i]])[2]))[1]
            
            table <- table %>% rowwise() %>%
              mutate(across(Row.names, ~ ifelse(startsWith(.x, i),paste(c(": \t", substring(.x, nchar(i)+1)),collapse = '\t'),.x)))

            table <-table %>% add_row(Row.names = paste0(i), Coeff = "Categorical" ,lower095ci="",upper095ci="",`Pr>|t|`="",`mod summary stars`="", .before = rt2)

            }

            table <- table %>% 
              rename(
                `variables` = `Row.names`
                )

            table 
          }, 
          include.rownames=TRUE) 
          
        }
        
      })
      # eCART add in error message?
      removeModal()
    }
  })

  output$glmoddsratiosUI <- renderUI({
    if(input$glmoddsratios) {
      column(width = 12,
             plotOutput("glmoddsratios2"),#,style = "overflow-y: scroll;overflow-x: scroll;",
      )
    } else {
      plotOutput("glmoddsratiosBlank")
    }
  })

  observeEvent(input$glmoddsratios, ignoreInit = T, {
    if((modeltype %>% filter(lookup == input$glm2Var2) %>% select (type )%>% pull) == "binomial") {
      output$glmoddsratios2 <- renderPlot({
        
       # plot1 <- 
       ggplot(glm_plots$table, aes(y = 1:nrow(glm_plots$table), x = OR)) +
          geom_point(shape = 18, size = 5) +  
          geom_errorbarh(aes(xmin = lower95ci, xmax = upper95ci), height = 0.25) +
          geom_vline(xintercept = 1, color = "red", linetype = "dashed", cex = 1, alpha = 0.5) +
          scale_y_continuous(name = "", breaks=1:nrow(glm_plots$table), labels = rownames(glm_plots$table), trans = "reverse") +
          #scale_x_continuous(trans = 'log10') +
          xlab("Odds Ratio (95% CI)") + 
          ylab(" ") + 
          theme_bw() +
          theme(panel.border = element_blank(),
                panel.background = element_blank(),
                panel.grid.major = element_blank(), 
                panel.grid.minor = element_blank(), 
                axis.line = element_line(colour = "black"),
                axis.text.y = element_text(size = 12, colour = "black"),
                axis.text.x.bottom = element_text(size = 12, colour = "black"),
                axis.title.x = element_text(size = 12, colour = "black"))
        
        #plot1
        
      })
    }
    
    else {
      
      output$glmoddsratios2 <- renderPlot({})

    }
  })
    
  ### BN
  
  observeEvent(input$bngo, {
    if(!is.null(input$bnVar1)) {
      showModal(
        modalDialog(
          HTML("<h2><strong>Please wait while the data is segmented.</strong></h2>"),
          p("This should finish in no more than a minute or two."),
          footer = tagList()
        )
      )
      try({
        add_sco <- function(x){
          gsub(" ", "_", x, fixed = T)
        }
        
        counter2 <- reactiveValues(n = 0)
        prevcount2 <-reactiveValues(n = 0)
        counter <- reactiveValues(n = 0)
        prevcount <-reactiveValues(n = 0)
        output$textbox_ui <- renderUI(NULL)
        output$textbox_ui2 <- renderUI(NULL)
        output$BNCOMFIRM <- renderUI(NULL)

        data <- as.data.frame(Query %>% select_at(c(unique(input$bnVar1))))
        

        dummys <- colnames(data %>%
                  as_data_frame() %>%
                  select_if(~ is.factor(.) & nlevels(.) > 2))

        if (length(dummys) > 0) {
        data <- fastDummies::dummy_cols(data,select_columns = dummys ,remove_selected_columns = T)               
        data<-data.frame(lapply(data,factor))
        colnames(data) <-gsub("\\_", " ", colnames(data))
        names(data) <- gsub("\\.", " ", names(data))
        }
        
        res <- hc(data)
        
        # Track the number of input boxes to render
        counter <- reactiveValues(n = 0)
        
        #Track the number of input boxes previously
        prevcount <-reactiveValues(n = 0)
        
        observeEvent(input$add_btn, {
          counter$n <- counter$n + 1
          prevcount$n <- counter$n - 1})
        
        observeEvent(input$rm_btn, {
          if (counter$n > 0) {
            counter$n <- counter$n - 1 
            prevcount$n <- counter$n + 1
          }
          
        })
        
        output$counter <- renderPrint(print(counter$n))
        
        textboxes <- reactive({
          
          n <- counter$n
          
          if (n > 0) {
            # If the no. of textboxes previously where more than zero, then 
            #save the text inputs in those text boxes 
            if(prevcount$n > 0){
              
              vals = c()
              if(prevcount$n > n){
                lesscnt <- n
                isInc <- FALSE
              }else{
                lesscnt <- prevcount$n
                isInc <- TRUE
              }
              for(i in 1:lesscnt){
                inpid = paste0("textin",i)
                vals[i] = input[[inpid]] 
              }
              if(isInc){
                vals <- c(vals, input$textin[[i]])
              }
              
              lapply(seq_len(n), function(i) {
                div(class = 'bnpickers',
                pickerInput(
                  inputId = paste0("textin", i),
                  label = "From",
                  choices =unique(colnames(data)),
                  selected = vals[i],
                  multiple = FALSE,
                  #value = vals[i],
                  options = list(`actions-box` = TRUE, size = 12, noneSelectedText = "Please select at least 1 option")
                )
                )
              })
              
            }else{
              lapply(seq_len(n), function(i) {
                div(class = 'bnpickers',
                pickerInput(
                  inputId = paste0("textin", i),
                  label = "From",
                  choices =unique(colnames(data)),
                  selected = 1,
                  multiple = FALSE,
                  options = list(`actions-box` = TRUE, size = 12, noneSelectedText = "Please select at least 1 option")
                )
                )
              }) 
            }
            
          }
          
        })
        
        output$textbox_ui <- renderUI({ textboxes() })
        
        ## Text box 2
        # Track the number of input boxes to render
        counter2 <- reactiveValues(n = 0)
        
        #Track the number of input boxes previously
        prevcount2 <-reactiveValues(n = 0)
        
        observeEvent(input$add_btn, {
          counter2$n <- counter2$n + 1
          prevcount2$n <- counter2$n - 1})
        
        observeEvent(input$rm_btn, {
          if (counter2$n > 0) {
            counter2$n <- counter2$n - 1 
            prevcount2$n <- counter2$n + 1
          }
          
        })
        
        output$counter2 <- renderPrint(print(counter2$n))
        
        textboxes2 <- reactive({
          
          n <- counter2$n
          
          if (n > 0) {
            # If the no. of textboxes previously where more than zero, then 
            #save the text inputs in those text boxes 
            if(prevcount2$n > 0){
              
              vals = c()
              if(prevcount2$n > n){
                lesscnt <- n
                isInc <- FALSE
              }else{
                lesscnt <- prevcount2$n
                isInc <- TRUE
              }
              for(i in 1:lesscnt){
                inpid = paste0("textinN",i)
                vals[i] = input[[inpid]] 
              }
              if(isInc){
                vals <- c(vals, input$textinN[[i]])
              }
              
              lapply(seq_len(n), function(i) {
                div(class = 'bnpickers',
                pickerInput(
                  inputId = paste0("textinN", i),
                  label = "To",
                  choices =unique(colnames(data)),
                  selected = vals[i],
                  multiple = FALSE,
                  #value = vals[i],
                  options = list(`actions-box` = TRUE, size = 12, noneSelectedText = "Please select 1 option")
                )
                )
              })
              
            }else{
              lapply(seq_len(n), function(i) {
                div(class = 'bnpickers',
                pickerInput(
                  inputId = paste0("textinN", i),
                  label = "To",
                  choices =unique(colnames(data)),
                  selected = 1,
                  multiple = FALSE,
                  options = list(`actions-box` = TRUE, size = 12, noneSelectedText = "Please select 1 option")
                )
                )
              }) 
            }
            
          }
          
        })
        
        output$textbox_ui2 <- renderUI({ textboxes2() })

        output$BNCOMFIRM <- renderUI({
          actionButton("BNCOMFIRM", label = "Press To Fit Model")
        })
        
        if(length(colnames(data)) > 5) {
          
          output$TABLE <- renderUI({
            
            viewer(res,
                   bayesianNetwork.width = "100%",
                   bayesianNetwork.height = "500px",
                   bayesianNetwork.layout = "layout_in_circle",
                   
                   node.colors = list(background = "#f4bafd",
                                      border = "#2b7ce9",
                                      highlight = list(background = "#97c2fc",
                                                       border = "#2b7ce9"))
            )
          })
        }
        
        else {
          output$TABLE <- renderUI({

            viewer(res,
                   bayesianNetwork.width = "100%",
                   bayesianNetwork.height = "500px",
                   bayesianNetwork.layout = "layout_on_grid",
                   node.colors = list(background = "#f4bafd",
                                      border = "#2b7ce9",
                                      highlight = list(background = "#97c2fc",
                                                       border = "#2b7ce9"))
                   
            )
            
          })
        }
        
        observeEvent(input$BNCOMFIRM, {
          if(!is.null(counter$n >0)) {

            try({
              add_sco <- function(x){
                gsub(" ", "_", x, fixed = T)
              }

          if (counter$n >0){
          
          for (i in seq.int(1,counter$n , by=1)) {
          
            print(i)
            
            res <- drop.arc(res,input[[paste0("textin", i)]],input[[paste0("textinN", i)]])
            
          }
          
          }  

        if(length(colnames(data)) > 5) {
          
          output$TABLE <- renderUI({
            
            viewer(res,
                   bayesianNetwork.width = "100%",
                   bayesianNetwork.height = "500px",
                   bayesianNetwork.layout = "layout_in_circle",
                   
                   node.colors = list(background = "#f4bafd",
                                      border = "#2b7ce9",
                                      highlight = list(background = "#97c2fc",
                                                       border = "#2b7ce9"))
                   
            )
          })
        }
        
        else {
        output$TABLE <- renderUI({

          viewer(res,
                 bayesianNetwork.width = "100%",
                 bayesianNetwork.height = "500px",
                 bayesianNetwork.layout = "layout_on_grid",
                 
                 node.colors = list(background = "#f4bafd",
                                    border = "#2b7ce9",
                                    highlight = list(background = "#97c2fc",
                                                     border = "#2b7ce9"))
                 
          )
          
      })
    }

        fittedbn <- bn.fit(res, data = data)
       
        output$bntext <- renderPrint({ fittedbn })
        
        counter2 <- reactiveValues(n = 0)
        prevcount2 <-reactiveValues(n = 0)
        counter <- reactiveValues(n = 0)
        prevcount <-reactiveValues(n = 0)

        })
   
      }
      else {
        fittedbn <- bn.fit(res, data = data)
        
        output$bntext <- renderPrint({ fittedbn })

        }
    })
      
      # eCART add in error message?
      removeModal()
      
      counter2 <- reactiveValues(n = 0)
      prevcount2 <-reactiveValues(n = 0)
      counter <- reactiveValues(n = 0)
      prevcount <-reactiveValues(n = 0)
      
      })
      
    }
    
  })
  
  ## Clustering

  observeEvent(input$kmgo, {
      if(!is.null(input$kmVar1)) {
      showModal(
        modalDialog(
          HTML("<h2><strong>Please wait while the data is segmented.</strong></h2>"),
          p("This should finish in no more than a minute or two."),
          footer = tagList()
        )
      )
      try({
        add_sco <- function(x){
          gsub(" ", "_", x, fixed = T)
        }
        
        data <- as.data.frame(Query %>% select_at(c(unique(input$kmVar1),unique(input$kmVar2))))
        data <-data[sample(nrow(data), 3000), ]
        
        Numericals <- colnames(dplyr::select_if(data, is.numeric))
        
        if (!is.null(input$kmVar1)){
      
          gower_dist <- daisy(data %>% select(input$kmVar1), metric = "gower")
          gower_mat <- as.matrix(gower_dist)
          
          k <- as.numeric(input$clusters)
          pam_fit <- pam(gower_dist, diss = TRUE, k)
          
          results <- data %>%
            mutate(cluster = pam_fit$clustering)
          
          results$cluster <- as.factor(results$cluster)
         
          plot_list = list()
          for(i in Numericals) {
            
           p<- ggplot(results, aes(x=cluster, y=!!as.symbol(i))) +
            geom_violin(trim=FALSE, fill='#A4A4A4', color="darkred")+
             geom_boxplot(width=0.1) + theme_minimal()
           plot_list[[i]] = p
          }
          
          plot_list2 = list()
          Cats <- colnames(results %>% select(-Numericals,-cluster))

          for(i in Cats) {
            
            counts <- table(results %>% select(!!as.symbol(i), cluster)) 

           p <- ggplot(as.data.frame(counts)  %>%
                         rename_all(~str_replace_all(.,"\\."," ")), aes(cluster, Freq, fill = !!as.symbol(i))) +
              geom_col() +
              ylab("Population") +
              coord_flip() +
              theme_bw()

            plot_list2[[i]] = p
          }

          output$plot2 <- renderUI({
            if(input$tsnegr) {
              
                     plotOutput("plot21")
              
            } else {
              uiOutput("plot21UIBlank")
            }
          })
            if(input$tsnegr) {
              output$plot21 <- renderPlot({
                tsne_obj <- Rtsne(gower_dist, is_distance = TRUE)
                tsne_data <- tsne_obj$Y %>%
                  data.frame() %>%
                  setNames(c("tsne dimension 1", "tsne dimension 2")) %>%
                  mutate(cluster = factor(pam_fit$clustering))
                ggplot(aes(x = `tsne dimension 1`, y = `tsne dimension 2`), data = tsne_data) +
                  geom_point(aes(color = cluster))
              })
            }
          
          if (length(Numericals) >0) {

            output$plot3 <- renderUI({

              plotOutput("plot31")

              })
             
          output$plot31 <- renderPlot({grid.arrange(grobs = plot_list)})

          }
          
          if (length(Cats) >0) {

            output$plot4 <- renderUI({

              plotOutput("plot41")

              })

          output$plot41 <- renderPlot({grid.arrange(grobs = plot_list2)})

          }
          
        }
        else {
          
        }
      
      })
      
      removeModal()
    }

  })
  
  ##sh
  
  observeEvent(input$kmgosh, {
    if(!is.null(input$kmVar1)) {
      showModal(
        modalDialog(
          HTML("<h2><strong>Please wait while the data is segmented.</strong></h2>"),
          p("This should finish in no more than a minute or two."),
          footer = tagList()
        )
      )
      try({
        add_sco <- function(x){
          gsub(" ", "_", x, fixed = T)
        }

        data <- as.data.frame(Query %>% select_at(c(unique(input$kmVar1))))
        data <-data[sample(nrow(data), 3000), ]
        
        gower_dist <- daisy(data, metric = "gower")
        gower_mat <- as.matrix(gower_dist)
          
          sil_width <- c(NA)
          for(i in 2:8){  
            pam_fit <- pam(gower_dist, diss = TRUE, k = i)  
            sil_width[i] <- pam_fit$silinfo$avg.width  
          }
          
          output$plot1 <- renderUI({
            fluidPage(plotOutput("plot11"))
            
          })
          
          output$plot11 <- renderPlot({
            
            plot(1:8, sil_width,
                 xlab = "Number of clusters",
                 ylab = "Silhouette Width");lines(1:8, sil_width)
          })

      })
      # eCART add in error message?

      removeModal()
    }
    
  })
  
}
