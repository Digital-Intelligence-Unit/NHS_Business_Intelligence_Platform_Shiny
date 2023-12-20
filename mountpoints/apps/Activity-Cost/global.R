## load data
timing <- system.time({
library(DBI)
library(RPostgreSQL)
library(PHEindicatormethods)
library(xgboost)
library(dplyr)
library(caret)
library(ggplot2)
library(tidyverse)
library(readxl)
library(plotly)
library(config)
library(zoo)
library(readr)


'%!in%' <- function(x,y)!('%in%'(x,y))

config <- config::get(file = "config.yml")

print("Testing connection to database...")
con <- dbConnect(
  PostgreSQL(),
  dbname = config$database,
  user = config$uid,
  host = config$server,
  password = config$pwd,
  port = config$port
)


## mean cost per pbc
pop_mean_cost <- dbGetQuery(con, "
SELECT CASE 
      WHEN age < 90 THEN CONCAT(FLOOR(age/5)*5, '-', FLOOR(age/5)*5 + 4)
      ELSE '90+'
    END as age_group
      ,Sex
      ,\"ProgrammeBudgetCode\"
      ,COUNT(DISTINCT Det.NHS_Number) as pop
      ,COUNT(*) as activity
      ,SUM(Act.\"Total Cost\") total_cost
      ,CAST(SUM(Act.\"Total Cost\") AS FLOAT) / COUNT(DISTINCT Det.NHS_Number) mean_avg
      ,CAST(COUNT(*) AS FLOAT) / COUNT(DISTINCT Det.NHS_Number) mean_activity_avg
  FROM public.population_master as Det
  LEFT JOIN public.population_activity as Act
  ON Det.NHS_Number = Act.\"NHS Number\"
  --where sex <> 'I'
GROUP BY CASE 
        WHEN age < 90 THEN CONCAT(FLOOR(age/5)*5, '-', FLOOR(age/5)*5 + 4)
        ELSE '90+'
    END
      ,Sex
      ,\"ProgrammeBudgetCode\"

")

## pcn lookup

## mean cost per pbc
pcn_lookup <- dbGetQuery(con, "

SELECT distinct gpp_code, pcn FROM public.population_master;

")
###

tester <- dbGetQuery(con, "

SELECT gpp_code, gpp_short_name, a.\"ProgrammeBudgetCode\",
CASE 
        WHEN age < 90 THEN FLOOR(age/5)*5 || '-' || FLOOR(age/5)*5 + 4
        ELSE '90+'
    END AS age_group,
sex, count(DISTINCT a.\"NHS Number\"),  sum(a.\"Total Cost\")
FROM public.population_master m

left join public.population_activity a
    ON a.\"NHS Number\" = m.\"nhs_number\"
--where sex <> 'I'

group by gpp_code, gpp_short_name,\"ProgrammeBudgetCode\", CASE 
        WHEN age < 90 THEN FLOOR(age/5)*5 || '-' || FLOOR(age/5)*5 + 4
        ELSE '90+'
    END, sex
order by gpp_short_name,CASE 
        WHEN age < 90 THEN FLOOR(age/5)*5 || '-' || FLOOR(age/5)*5 + 4
        ELSE '90+'
    END    
        ")



monthcost <- dbGetQuery(con, "
SELECT to_char(DATE (a.\"Event Date\"), 'YYYY-MM') , gpp_code, gpp_short_name,
a.\"ProgrammeBudgetCode\",sum(a.\"Total Cost\")
FROM public.population_master m

left join public.population_activity a
    ON a.\"NHS Number\" = m.\"nhs_number\"


group by  to_char(DATE (a.\"Event Date\"), 'YYYY-MM')  , 
gpp_code, gpp_short_name,\"ProgrammeBudgetCode\"
        ")

# ## Query
pop_total_cost <- dbGetQuery(con, "
WITH total AS (
  SELECT gpp_code, CASE 
        WHEN age < 90 THEN CONCAT(FLOOR(age/5)*5, '-', FLOOR(age/5)*5 + 4)
        ELSE '90+'
    END as age_groups
  ,COUNT(DISTINCT Det.NHS_Number) as pop
  FROM public.population_master as Det
  GROUP BY gpp_code, CASE 
        WHEN age < 90 THEN CONCAT(FLOOR(age/5)*5, '-', FLOOR(age/5)*5 + 4)
        ELSE '90+'
    END
)

SELECT Det.gpp_code, \"ProgrammeBudgetCode\", CASE 
        WHEN age < 90 THEN CONCAT(FLOOR(age/5)*5, '-', FLOOR(age/5)*5 + 4)
        ELSE '90+'
    END as age_group
      
      ,total.pop
      ,COUNT(*) as activity
      ,SUM(Act.\"Total Cost\") total_cost
      ,CAST(SUM(Act.\"Total Cost\") AS FLOAT) / COUNT(DISTINCT Det.NHS_Number) mean_avg
      ,CAST(COUNT(*) AS FLOAT) / COUNT(DISTINCT Det.NHS_Number) mean_activity_avg
  FROM public.population_master as Det 
  LEFT JOIN public.population_activity as Act
  ON Det.NHS_Number = Act.\"NHS Number\"
  left join total on
  total.gpp_code = Det.gpp_code and
  total.age_groups = CASE 
        WHEN age < 90 THEN CONCAT(FLOOR(age/5)*5, '-', FLOOR(age/5)*5 + 4)
        ELSE '90+'
    END
  --where sex <> 'I'
GROUP BY Det.gpp_code, \"ProgrammeBudgetCode\", CASE 
        WHEN age < 90 THEN CONCAT(FLOOR(age/5)*5, '-', FLOOR(age/5)*5 + 4)
        ELSE '90+'
    END, total.pop

")

## Query
pop_total_cost_pcn <- dbGetQuery(con, "
WITH total AS (
  SELECT pcn, CASE 
        WHEN age < 90 THEN CONCAT(FLOOR(age/5)*5, '-', FLOOR(age/5)*5 + 4)
        ELSE '90+'
    END as age_groups
  ,COUNT(DISTINCT Det.NHS_Number) as pop
  FROM public.population_master as Det
  GROUP BY pcn, CASE 
        WHEN age < 90 THEN CONCAT(FLOOR(age/5)*5, '-', FLOOR(age/5)*5 + 4)
        ELSE '90+'
    END
)

SELECT Det.pcn, \"ProgrammeBudgetCode\", CASE 
        WHEN age < 90 THEN CONCAT(FLOOR(age/5)*5, '-', FLOOR(age/5)*5 + 4)
        ELSE '90+'
    END as age_group
      
      ,total.pop
      ,COUNT(*) as activity
      ,SUM(Act.\"Total Cost\") total_cost
      ,CAST(SUM(Act.\"Total Cost\") AS FLOAT) / COUNT(DISTINCT Det.NHS_Number) mean_avg
      ,CAST(COUNT(*) AS FLOAT) / COUNT(DISTINCT Det.NHS_Number) mean_activity_avg
  FROM public.population_master as Det 
  LEFT JOIN public.population_activity as Act
  ON Det.NHS_Number = Act.\"NHS Number\"
  left join total on
  total.pcn = Det.pcn and
  total.age_groups = CASE 
        WHEN age < 90 THEN CONCAT(FLOOR(age/5)*5, '-', FLOOR(age/5)*5 + 4)
        ELSE '90+'
    END
  --where sex <> 'I'
GROUP BY Det.pcn, \"ProgrammeBudgetCode\", CASE 
        WHEN age < 90 THEN CONCAT(FLOOR(age/5)*5, '-', FLOOR(age/5)*5 + 4)
        ELSE '90+'
    END, total.pop

")

# monthcost <- dbGetQuery(con, "
# 
# SELECT to_char(DATE (a.\"Event Date\"), 'YYYY-MM') , gpp_code, gpp_short_name, a.\"ProgrammeBudgetCode\",
# CASE 
#         WHEN age < 90 THEN FLOOR(age/5)*5 || '-' || FLOOR(age/5)*5 + 4
#         ELSE '90+'
#     END AS age_group,
# sex, count(DISTINCT a.\"NHS Number\"),  sum(a.\"Total Cost\")
# FROM public.population_master m
# 
# left join public.population_activity a
#     ON a.\"NHS Number\" = m.\"nhs_number\"
# where sex <> 'I'
# 
# group by  to_char(DATE (a.\"Event Date\"), 'YYYY-MM')  , gpp_code, gpp_short_name,\"ProgrammeBudgetCode\", CASE 
#         WHEN age < 90 THEN FLOOR(age/5)*5 || '-' || FLOOR(age/5)*5 + 4
#         ELSE '90+'
#     END, sex
# order by gpp_short_name,CASE 
#         WHEN age < 90 THEN FLOOR(age/5)*5 || '-' || FLOOR(age/5)*5 + 4
#         ELSE '90+'
#     END 
#     
#    -- LIMIT 100 
#         ")

## month cost

dbDisconnect(con)

monthcost$date <- as.Date(as.yearmon(monthcost$to_char, format = "%Y-%m"))

monthcostdf <- monthcost %>% select(gpp_short_name,ProgrammeBudgetCode, date, sum) %>%
  arrange(gpp_short_name,ProgrammeBudgetCode, date) %>% # Sort the dataframe by ID and date
  group_by(gpp_short_name,ProgrammeBudgetCode) %>%
  mutate(prev_month_item = lag(sum, n = 1)) %>%
  ungroup()# Here, n = 1 retrieves the value from the previous row, which is the previous month's value after sorting.



## join both queries together to work out predicted cost
df_pbc <- tester %>% left_join(pop_mean_cost, by=c("age_group" = "age_group","sex" = "sex", "ProgrammeBudgetCode" = "ProgrammeBudgetCode")) %>% 
  mutate(predicted = mean_avg * count ) %>% 
  group_by(gpp_code,ProgrammeBudgetCode) %>%
  summarise(predicted = sum(predicted), `Total Cost` = sum(`sum`, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(diff = predicted - `Total Cost`) %>%
  arrange(diff)


## place lookup table

GP_PCN_Place <- read_csv("GP_PCN_Place.csv")

pcn_lookup <- pcn_lookup %>% 
  left_join(GP_PCN_Place, by=c('gpp_code' = 'P Code'))

# PBC look up table
Programme_Budgeting_Mappings_Definitions <- read_excel("Programme-Budgeting-Mappings-Definitions.xls", 
                                                       sheet = "Inpatient Activity", skip = 4)

Programme_Budgeting_MAIN <- read_excel("Programme-Budgeting-Mappings-Definitions.xls", 
                                                       sheet = "Programme Budgeting Categories", skip = 3) %>%
  # Grouping by 'Main Programme'
  group_by(Main_Programme = `Main Programme`) %>%
  # Creating a new column with the number of observations per group
  mutate(count = n()) %>%
  # Ungrouping to remove the grouping structure
  ungroup() %>%
  # Creating a new column 'Programme_Display' with 'Other' for groups with less than 2 observations
  mutate(Programme_Display = ifelse(count >= 2, as.character(Main_Programme), "Other"))                                                       

Programme_Budgeting_Mappings_Definitions <- Programme_Budgeting_Mappings_Definitions %>%
  select(PBC02, `Programme Category Name`) %>%
  mutate(pbccode = str_sub(PBC02, -3, -1)) %>%
  distinct %>% 
  left_join(Programme_Budgeting_MAIN, by = c("pbccode" = "Programme Budgeting Code"))

## gp short name lookups
gp_lookup <- tester %>% 
  select(gpp_code, gpp_short_name) %>%
  distinct()

## add lookup tables to dataframe
df_pbc <- df_pbc %>%
  left_join(Programme_Budgeting_Mappings_Definitions , by = c("ProgrammeBudgetCode" = "PBC02")) %>%
  left_join(gp_lookup) %>%
  mutate(`Programme Category Name` = ifelse(is.na(`Programme Category Name`), "Not coded", `Programme Category Name`)) %>%
  left_join(pcn_lookup)


#######################
###  FUNNEL PLOTS  ####
#######################

## Modiefied functions from PHE
pw_poisson_funnel <-  function (obs, p, side) {
  alp <- (p * 2) 
  interval <- c(0, obs * 5 + 5)
  
  if(side == "high"){
    fn <- function(obs, ans, alpha = alp) ppois(obs, ans) - alpha/2
  } else if (side == "low"){
    fn <- function(obs, ans, alpha = alp) 1 - ppois(obs, ans) + dpois(obs, ans) - alpha/2
  } 
  
  if (obs == 0 & side == "low") {
    result <- 0
  } else {
    if(exists("fn")){
      result <- uniroot(fn, interval = interval, obs = obs)$root
    } else {
      result <- NA
    }
  } 
  return(result)
}

calculate_funnel_limits_2 <- function (data, numerator, denominator, rate, type = "full", 
                                       multiplier = NULL, statistic = NULL, ratio_type = NULL, rate_type = NULL, 
                                       years_of_data = NULL) 
{
  if (missing(statistic)) {
    stop("statistic must be provided as proportion, rate or ratio")
  }
  if (statistic == "rate") {
    if (missing(data) | missing(numerator) | missing(rate) | 
        is.null(rate_type) | is.null(years_of_data) | is.null(multiplier)) {
      stop(paste0("the following arguments are required for rates: ", 
                  "data, numerator, rate, rate_type, multiplier, years_of_data"))
    }
    else if (any(is.na(pull(data, {
      {
        numerator
      }
    })))) {
      stop(paste0("for rates, numerators must be provided for all records, ", 
                  "even when their values are zero"))
    }
    else if (!missing(denominator)) {
      if (any(is.na(pull(data, {
        {
          denominator
        }
      })) & is.na(pull(data, {
        {
          rate
        }
      })))) {
        stop(paste0("for rates, rates must be provided for all records, ", 
                    "or a denominator must be provided if the rate is zero"))
      }
    }
    else {
      if (any(is.na(pull(data, {
        {
          rate
        }
      })))) {
        stop(paste0("for rates, rates must be provided for all records, ", 
                    "or a denominator must be provided if the rate is zero"))
      }
    }
    if (any(pull(data, {
      {
        numerator
      }
    }) == 0)) {
      if (missing(denominator)) {
        stop(paste0("for rates, where there are 0 events for a record, the ", 
                    "denominator field needs to be provided using the denominator argument"))
      }
      else if (any(pull(data, {
        {
          numerator
        }
      }) == 0 & (pull(data, {
        {
          denominator
        }
      }) <= 0 | is.na(pull(data, {
        {
          denominator
        }
      }))))) {
        stop(paste0("for rates, where there are 0 events for a record, the ", 
                    "denominator must be provided"))
      }
    }
    rate_type <- match.arg(rate_type, c("dsr", "crude"))
  }
  else if (statistic == "ratio") {
    if (missing(data) | missing(numerator) | missing(denominator) | 
        is.null(ratio_type) | is.null(multiplier)) {
      stop(paste0("the following arguments are required for ratios: ", 
                  "data, numerator, denominator, ratio_type, multiplier"))
    }
    ratio_type <- match.arg(ratio_type, c("count", 
                                          "isr"))
  }
  else if (statistic == "proportion") {
    if (missing(data) | missing(numerator) | missing(denominator) | 
        is.null(multiplier)) {
      stop(paste0("the following arguments are required for proportions: ", 
                  "data, numerator, denominator, multiplier"))
    }
    else if (any(is.na(pull(data, {
      {
        numerator
      }
    })) | is.na(pull(data, {
      {
        denominator
      }
    })))) {
      stop(paste0("for proportions, numerators and denominators must be provided ", 
                  "for all records, even when their values are zero"))
    }
  }
  type <- match.arg(type, c("full", "standard"))
  statistic <- match.arg(statistic, c("proportion", "ratio", 
                                      "rate"))
  if (statistic == "rate") {
    data <- data %>% mutate(`:=`({
      {
        rate
      }
    }, as.numeric({
      {
        rate
      }
    })))
    if (rate_type == "dsr") {
      data <- data %>% mutate(denominator_derived = case_when({
        {
          numerator
        }
      } == 0 ~ NA_real_, TRUE ~ multiplier * {
        {
          numerator
        }
      }/{
        {
          rate
        }
      }))
    }
    else if (rate_type == "crude") {
      if (missing(denominator)) {
        data <- data %>% mutate(denominator_derived = multiplier * 
                                  {
                                    {
                                      numerator
                                    }
                                  }/{
                                    {
                                      rate
                                    }
                                  })
      }
      else {
        data <- data %>% mutate(denominator_derived = case_when({
          {
            numerator
          }
        } == 0 ~ as.numeric({
          {
            denominator
          }
        }), TRUE ~ multiplier * {
          {
            numerator
          }
        }/{
          {
            rate
          }
        }))
      }
    }
  }
  else if (statistic %in% c("proportion", "ratio")) {
    data <- data %>% mutate(denominator_derived = as.numeric({
      {
        denominator
      }
    }))
  }
  summaries <- data %>% summarise(av = sum({
    {
      numerator
    }
  })/sum(.data$denominator_derived, na.rm = TRUE), min_denominator = min(.data$denominator_derived, 
                                                                         na.rm = TRUE), max_denominator = max(.data$denominator_derived, 
                                                                                                              na.rm = TRUE))
  av <- summaries$av
  min_denominator <- summaries$min_denominator
  max_denominator <- summaries$max_denominator
  signif.floor <- function(x, percentage_down = 0.95) {
    n <- nchar(floor(x * percentage_down)) - 1
    y <- floor(x * percentage_down/10^n) * 10^n
    y[x == 0] <- 0
    y
  }
  signif.ceiling <- function(x, percentage_up = 1.05) {
    n <- nchar(ceiling(x * percentage_up)) - 2
    y <- ceiling(x * percentage_up/10^n) * 10^n
    y[x == 0] <- 0
    y
  }
  if (max_denominator > 2 * min_denominator) {
    axis_minimum <- 0
  }
  else {
    if (statistic == "rate") {
      axis_minimum <- signif.floor(min_denominator/years_of_data) * 
        years_of_data
    }
    else if (statistic %in% c("proportion", "ratio")) {
      axis_minimum <- signif.floor(min_denominator)
    }
  }
  if (statistic == "rate") {
    axis_maximum <- signif.ceiling(max_denominator/years_of_data) * 
      years_of_data
  }
  else if (statistic %in% c("proportion", "ratio")) {
    axis_maximum <- signif.ceiling(max_denominator)
  }
  if (statistic == "proportion") {
    col_header <- "Population"
  }
  else if (statistic == "ratio") {
    col_header <- "Observed_events"
  }
  else if (statistic == "rate") {
    col_header <- "Events"
    axis_minimum <- floor(axis_minimum * av)
    axis_maximum <- ceiling(axis_maximum * av)
  }
  first_col <- max(1, axis_minimum)
  for (j in 2:100) {
    if (statistic %in% c("proportion", "rate")) {
      offset <- j
    }
    else if (statistic == "ratio") {
      offset <- j - 1
    }
    first_col[j] <- max(round((axis_maximum/first_col[j - 
                                                        1])^(1/(101 - offset)) * first_col[j - 1]), first_col[j - 
                                                                                                                1] + 1)
  }
  t <- tibble(`:=`(!!rlang::sym(col_header), first_col))
  if (statistic == "proportion") {
    t <- t %>% group_by(.data$Population) %>% mutate(lower_2s_limit = max(0, 
                                                                          sigma_adjustment(0.975, .data$Population, av, "low", 
                                                                                           multiplier)), upper_2s_limit = min(100, sigma_adjustment(0.975, 
                                                                                                                                                    .data$Population, av, "high", multiplier)), 
                                                     lower_3s_limit = max(0, sigma_adjustment(0.999, .data$Population, 
                                                                                              av, "low", multiplier)), upper_3s_limit = min(100, 
                                                                                                                                            sigma_adjustment(0.999, .data$Population, av, 
                                                                                                                                                             "high", multiplier)), baseline = av * 
                                                       multiplier) %>% ungroup()
  }
  else if (statistic == "ratio") {
    t <- t %>% group_by(.data$Observed_events) %>% mutate(lower_2s_exp_events = pw_poisson_funnel(.data$Observed_events, 
                                                                                                  0.025, "high"), lower_2s_limit = .data$Observed_events/.data$lower_2s_exp_events, 
                                                          upper_2s_exp_events = pw_poisson_funnel(.data$Observed_events, 
                                                                                                  0.025, "low"), upper_2s_limit = .data$Observed_events/.data$upper_2s_exp_events, 
                                                          lower_3s_exp_events = pw_poisson_funnel(.data$Observed_events, 
                                                                                                  0.001, "high"), lower_3s_limit = .data$Observed_events/.data$lower_3s_exp_events, 
                                                          upper_3s_exp_events = pw_poisson_funnel(.data$Observed_events, 
                                                                                                  0.001, "low"), upper_3s_limit = .data$Observed_events/.data$upper_3s_exp_events, 
    ) %>% ungroup()
    if (ratio_type == "count") {
      t <- t %>% mutate(across(ends_with("limit"), 
                               function(x) x - 1))
    }
    else if (ratio_type == "isr") {
      t <- t %>% mutate(across(ends_with("limit"), 
                               function(x) x * 100))
    }
  }
  else if (statistic == "rate") {
    t <- t %>% group_by(.data$Events) %>% mutate(lower_2s_population_1_year = pw_poisson_funnel(.data$Events, 
                                                                                                0.025, "high")/av, lower_2s_limit = .data$Events/.data$lower_2s_population_1_year, 
                                                 upper_2s_population_1_year = pw_poisson_funnel(.data$Events, 
                                                                                                0.025, "low")/av, upper_2s_limit = .data$Events/.data$upper_2s_population_1_year, 
                                                 lower_3s_population_1_year = pw_poisson_funnel(.data$Events, 
                                                                                                0.001, "high")/av, lower_3s_limit = .data$Events/.data$lower_3s_population_1_year, 
                                                 upper_3s_population_1_year = pw_poisson_funnel(.data$Events, 
                                                                                                0.001, "low")/av, upper_3s_limit = .data$Events/.data$upper_3s_population_1_year, 
                                                 baseline = av * multiplier, across(ends_with("limit"), 
                                                                                    function(x) x * multiplier), across(ends_with("1_year"), 
                                                                                                                        function(x) x/years_of_data)) %>% ungroup()
  }
  if (type == "full") {
    t$statistic <- statistic
    t <- t %>% mutate(statistic = case_when(.env$statistic == 
                                              "ratio" ~ paste0(.env$statistic, " (", 
                                                               ratio_type, ")"), .env$statistic == "rate" ~ 
                                              paste0(.env$statistic, " (", rate_type, " per ", 
                                                     format(multiplier, big.mark = ",", scientific = FALSE), 
                                                     ")"), TRUE ~ .env$statistic), method = case_when(.env$statistic == 
                                                                                                        "proportion" ~ "Wilson", .env$statistic %in% 
                                                                                                        c("ratio", "rate") ~ "Poisson"))
  }
  return(t)
}

## ordering age band levels
pop_total_cost$age_bands <- factor(pop_total_cost$age_group,
                                   levels = c("0-4" , "5-9", "10-14", "15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49" ,"50-54", "55-59",   
                                              "60-64", "65-69" ,"70-74" ,"75-79" ,"80-84" ,"85-89","90+"))

## add lookups tables
pop_total_cost <- pop_total_cost %>%
  left_join(Programme_Budgeting_Mappings_Definitions , by = c("ProgrammeBudgetCode" = "PBC02")) %>%
  left_join(gp_lookup) %>%
  mutate(`Programme Category Name` = ifelse(is.na(`Programme Category Name`), "Not coded", `Programme Category Name`))


## ordering age band levels
pop_total_cost_pcn$age_bands <- factor(pop_total_cost_pcn$age_group,
                                   levels = c("0-4" , "5-9", "10-14", "15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49" ,"50-54", "55-59",   
                                              "60-64", "65-69" ,"70-74" ,"75-79" ,"80-84" ,"85-89","90+"))

## add lookups tables
pop_total_cost_pcn <- pop_total_cost_pcn %>%
  left_join(Programme_Budgeting_Mappings_Definitions , by = c("ProgrammeBudgetCode" = "PBC02")) %>%
  #left_join(gp_lookup) %>%
  mutate(`Programme Category Name` = ifelse(is.na(`Programme Category Name`), "Not coded", `Programme Category Name`))

 dollar_formatter <- scales::dollar_format(prefix = "£", big.mark = ",", accuracy = 1)
})
elapsed_time <- timing["elapsed"]
print(elapsed_time)