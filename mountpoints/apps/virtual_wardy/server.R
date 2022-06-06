library(shiny)
library(dplyr)
library(stringr)
library(ggplot2)
library(plotly)
library(RPostgreSQL)
library(lubridate)
library(config)

config <- get()

get_query <- function(query) {
  print("Testing connection to database...")
  print(config$sql_credentials$database)
  postgres <- dbConnect(
    PostgreSQL(),
    dbname = config$sql_credentials$database,
    user = config$sql_credentials$uid,
    host = config$sql_credentials$server,
    password = config$sql_credentials$pwd,
    port = config$sql_credentials$port
  )

  response <- dbGetQuery(postgres, query)

  dbDisconnect(postgres)

  response
}

# Check DB Connection
get_query('SELECT * FROM covid19_cases_p1p2 LIMIT 1;') %>% print()

# Define server logic required to draw a histogram
server <- function(input, output) {
    output$distPlot <- renderPlotly({

        if (!exists('virtual_ward')) {
            postgres <- dbConnect(
                PostgreSQL(),
                dbname = config$sql_credentials$database,
                user = config$sql_credentials$uid,
                host = config$sql_credentials$server,
                password = config$sql_credentials$pwd,
                port = config$sql_credentials$port
            )

            virtual_ward_full <- dbGetQuery(
                postgres,
                "WITH ics AS (
                    SELECT
                        nhs_number,
                        title,
                        gpp_code,
                        ccg_code,
                        asthma,
                        copd,
                        respiratory AS late_copd,
                        chd,
                        heart_failure,
                        atrial_fibrillation,
                        pad,
                        stroke_tia,
                        cancer,
                        chemo_radiotherapy,
                        haematological_cancers,
                        depression,
                        dementia,
                        mental_health,
                        psychotic_disorder_flag,
                        learning_disabilities,
                        diabetes,
                        hypothyroid,
                        ckd,
                        epilepsy,
                        osteoporosis,
                        rheumatoid_arthritis,
                        smoker,
                        substance_misuse,
                        dispensing_flag,
                        pregnant_with_congenital_heart_defect,
                        rare_diseases,
                        transplant,
                        palliative_care_flag,
                        spl,
                        covid_vuln,
                        rs_frailty_index,
                        risk_score
                    FROM
                        population_master),
                c AS (
                    SELECT nhs_number, age_in_years, ethnicity FROM covid19_cases_p1p2
                )
                SELECT *
                FROM c
                INNER JOIN ics USING (nhs_number);"
            ) %>%
                mutate(
                    status = 'Pending',
                    loaded_date = as.character(today()),
                    updated_date = as_date(NA),
                    recommendation = case_when(
                        is.na(nhs_number) ~ 'No NHS Number',
                        spl ~ 'National SOP',
                        age_in_years > 65 ~ 'National SOP',
                        risk_score > 12 ~ 'RS Greater Risk',
                        TRUE ~ 'RS Lower Risk'
                    ),
                    gpp_code = coalesce(gpp_code, 'Unknown'),
                    ccg_code = coalesce(ccg_code, 'Unknown'),
                    docobo_ref = FALSE,
                    notes = NA,
                    newcontact = NA
                )

            virtual_ward_referred <- dbGetQuery(
                postgres,
                "SELECT age_in_years,
                    ethnicity,
                    specimen_date,
                    age_band,
                    patient_sex,
                    gpp_code,
                    ccg_code,
                    asthma,
                    copd,
                    late_copd,
                    chd,
                    heart_failure,
                    atrial_fibrillation,
                    pad,
                    stroke_tia,
                    cancer,
                    chemo_radiotherapy,
                    haematological_cancers,
                    depression,
                    dementia,
                    mental_health,
                    psychotic_disorder_flag,
                    learning_disabilities,
                    diabetes,
                    hypothyroid,
                    ckd,
                    epilepsy,
                    osteoporosis,
                    rheumatoid_arthritis,
                    smoker,
                    substance_misuse,
                    dispensing_flag,
                    pregnant_with_congenital_heart_defect,
                    rare_diseases,
                    transplant,
                    palliative_care_flag,
                    spl,
                    covid_vuln,
                    rs_frailty_index,
                    risk_score,
                    status,
                    loaded_date,
                    updated_date,
                    recommendation,
                    docobo_ref,
                    home_type
                FROM
                    virtual_ward_decision
                WHERE
                    status != 'Pending'"
            )

            dbDisconnect(postgres)

            virtual_ward <- bind_rows(virtual_ward_referred, virtual_ward_full)

            virtual_ward <<- virtual_ward %>%
                mutate(
                    specimen_week = floor_date(ymd(specimen_date), 'week'),
                    age_band = case_when(
                        age_in_years > 64 ~ '65+',
                        age_in_years > 49 ~ '50 - 64',
                        age_in_years > 39 ~ '40 - 49',
                        age_in_years > 29 ~ '30 - 39',
                        age_in_years > 17 ~ '18 - 29',
                        TRUE ~ '0 - 17'
                    ),
                    age_band = paste('Age', str_remove_all(age_band, 'Age '))
                )
        }

        group_1_variables <- input$group_1_variables
        group_2_variables <- input$group_2_variables
        group_3_variables <- input$group_3_variables
        group_4_variables <- input$group_4_variables
        group_5_variables <- input$group_5_variables
        ccg <- input$ccg
        date_range <- input$date_range
        plot_vw <- data.frame()

        if (length(ccg) > 0) {
            virtual_ward <- virtual_ward %>% filter(ccg_code %in% ccg)
        }

        if (length(date_range) > 0) {
            virtual_ward <- virtual_ward %>%
                filter(
                    between(ymd(specimen_date), date_range[1], date_range[2])
                )
        }

        if (length(group_1_variables) > 0) {
            plot_1 <- TRUE
            plot_vw_1 <- virtual_ward

            if (any(str_detect(group_1_variables, 'Age'))) {
                age_variables <- group_1_variables[str_detect(group_1_variables, 'Age')]

                plot_vw_1 <- plot_vw_1 %>% filter(age_band %in% age_variables)
            }

            if (any(!str_detect(group_1_variables, 'Age'))) {
                flag_variables <- group_1_variables[!str_detect(group_1_variables, 'Age')]

                flag_variables <- paste(flag_variables, collapse = ' | ')

                plot_vw_1 <- plot_vw_1 %>% filter(eval(rlang::parse_expr(flag_variables)))
            }

            plot_vw_1 <- plot_vw_1 %>%
                count(specimen_week) %>%
                mutate(group = 'Group 1')

            plot_vw <- bind_rows(plot_vw, plot_vw_1)
        }
        if (length(group_2_variables) > 0) {
            plot_2 <- TRUE
            plot_vw_2 <- virtual_ward


            if (any(str_detect(group_2_variables, 'Age'))) {
                age_variables <- group_2_variables[str_detect(group_2_variables, 'Age')]

                plot_vw_2 <- plot_vw_2 %>% filter(age_band %in% age_variables)
            }

            if (any(!str_detect(group_2_variables, 'Age'))) {
                flag_variables <- group_2_variables[!str_detect(group_2_variables, 'Age')]

                flag_variables <- paste(flag_variables, collapse = ' | ')

                plot_vw_2 <- plot_vw_2 %>% filter(eval(rlang::parse_expr(flag_variables)))
            }

            plot_vw_2 <- plot_vw_2 %>%
                count(specimen_week) %>%
                mutate(group = 'Group 2')

            plot_vw <- bind_rows(plot_vw, plot_vw_2)
        }
        if (length(group_3_variables) > 0) {
            plot_3 <- TRUE
            plot_vw_3 <- virtual_ward


            if (any(str_detect(group_3_variables, 'Age'))) {
                age_variables <- group_3_variables[str_detect(group_3_variables, 'Age')]

                plot_vw_3 <- plot_vw_3 %>% filter(age_band %in% age_variables)
            }

            if (any(!str_detect(group_3_variables, 'Age'))) {
                flag_variables <- group_3_variables[!str_detect(group_3_variables, 'Age')]

                flag_variables <- paste(flag_variables, collapse = ' | ')

                plot_vw_3 <- plot_vw_3 %>% filter(eval(rlang::parse_expr(flag_variables)))
            }

            plot_vw_3 <- plot_vw_3 %>%
                count(specimen_week) %>%
                mutate(group = 'Group 3')

            plot_vw <- bind_rows(plot_vw, plot_vw_3)
        }
        if (length(group_4_variables) > 0) {
            plot_4 <- TRUE
            plot_vw_4 <- virtual_ward


            if (any(str_detect(group_4_variables, 'Age'))) {
                age_variables <- group_4_variables[str_detect(group_4_variables, 'Age')]

                plot_vw_4 <- plot_vw_4 %>% filter(age_band %in% age_variables)
            }

            if (any(!str_detect(group_4_variables, 'Age'))) {
                flag_variables <- group_4_variables[!str_detect(group_4_variables, 'Age')]

                flag_variables <- paste(flag_variables, collapse = ' | ')

                plot_vw_4 <- plot_vw_4 %>% filter(eval(rlang::parse_expr(flag_variables)))
            }

            plot_vw_4 <- plot_vw_4 %>%
                count(specimen_week) %>%
                mutate(group = 'Group 4')

            plot_vw <- bind_rows(plot_vw, plot_vw_4)
        }
        if (length(group_5_variables) > 0) {
            plot_5 <- TRUE
            plot_vw_5 <- virtual_ward


            if (any(str_detect(group_5_variables, 'Age'))) {
                age_variables <- group_5_variables[str_detect(group_5_variables, 'Age')]

                plot_vw_5 <- plot_vw_5 %>% filter(age_band %in% age_variables)
            }

            if (any(!str_detect(group_5_variables, 'Age'))) {
                flag_variables <- group_5_variables[!str_detect(group_5_variables, 'Age')]

                flag_variables <- paste(flag_variables, collapse = ' | ')

                plot_vw_5 <- plot_vw_5 %>% filter(eval(rlang::parse_expr(flag_variables)))
            }

            plot_vw_5 <- plot_vw_5 %>%
                count(specimen_week) %>%
                mutate(group = 'Group 5')

            plot_vw <- bind_rows(plot_vw, plot_vw_5)
        }

        # output_dist <<- plot_vw

        if (nrow(plot_vw) > 0) {
            total_plot <- plot_vw %>%
                ggplot(
                  aes(
                    specimen_week,
                    n,
                    fill = group,
                    text = paste(
                      '<b>Week Start [yyyy-mm-dd]:</b>',
                      specimen_week,
                      '<br><b>Group:</b>',
                      group,
                      '<br><b>Number of Cases:</b>',
                      n
                    )
                  )
                ) +
                geom_bar(stat = 'identity') +
                xlab('Specimen Date [yyyy-mm-dd]') +
                ylab('Number of Positive Cases') +
                ggtitle('Weekly COVID Cases')

            ggplotly(total_plot, tooltip = 'text')
        }
    })

    output$relativePlot <- renderPlotly({
        group_1_variables <- input$group_1_variables
        group_2_variables <- input$group_2_variables
        group_3_variables <- input$group_3_variables
        group_4_variables <- input$group_4_variables
        group_5_variables <- input$group_5_variables
        ccg <- input$ccg
        date_range <- input$date_range
        plot_vw <- data.frame()


        if (!exists('virtual_ward')) {
            postgres <- dbConnect(
                PostgreSQL(),
                dbname = config$sql_credentials$database,
                user = config$sql_credentials$uid,
                host = config$sql_credentials$server,
                password = config$sql_credentials$pwd,
                port = config$sql_credentials$port
            )

            virtual_ward_full <- dbGetQuery(
                postgres,
                "WITH ics AS (
                    SELECT
                        nhs_number,
                        title,
                        gpp_code,
                        ccg_code,
                        asthma,
                        copd,
                        respiratory AS late_copd,
                        chd,
                        heart_failure,
                        atrial_fibrillation,
                        pad,
                        stroke_tia,
                        cancer,
                        chemo_radiotherapy,
                        haematological_cancers,
                        depression,
                        dementia,
                        mental_health,
                        psychotic_disorder_flag,
                        learning_disabilities,
                        diabetes,
                        hypothyroid,
                        ckd,
                        epilepsy,
                        osteoporosis,
                        rheumatoid_arthritis,
                        smoker,
                        substance_misuse,
                        dispensing_flag,
                        pregnant_with_congenital_heart_defect,
                        rare_diseases,
                        transplant,
                        palliative_care_flag,
                        spl,
                        covid_vuln,
                        rs_frailty_index,
                        risk_score
                    FROM
                        population_master),
                c AS (
                    SELECT nhs_number, age_in_years, ethnicity FROM covid19_cases_p1p2
                )
                SELECT *
                FROM c
                INNER JOIN ics USING (nhs_number);"
            ) %>%
                mutate(
                    status = 'Pending',
                    loaded_date = as.character(today()),
                    updated_date = as_date(NA),
                    recommendation = case_when(
                        is.na(nhs_number) ~ 'No NHS Number',
                        spl ~ 'National SOP',
                        age_in_years > 65 ~ 'National SOP',
                        risk_score > 12 ~ 'RS Greater Risk',
                        TRUE ~ 'RS Lower Risk'
                    ),
                    gpp_code = coalesce(gpp_code, 'Unknown'),
                    ccg_code = coalesce(ccg_code, 'Unknown'),
                    docobo_ref = FALSE,
                    notes = NA,
                    newcontact = NA
                )

            virtual_ward_referred <- dbGetQuery(postgres, "SELECT * FROM virtual_ward_decision WHERE status != 'Pending'")

            dbDisconnect(postgres)

            virtual_ward <- bind_rows(virtual_ward_referred, virtual_ward_full)

            virtual_ward <- virtual_ward %>%
                mutate(
                    specimen_week = floor_date(ymd(specimen_date), 'week'),
                    age_band = case_when(
                        age_in_years > 64 ~ '65+',
                        age_in_years > 49 ~ '50 - 64',
                        age_in_years > 39 ~ '40 - 49',
                        age_in_years > 29 ~ '30 - 39',
                        age_in_years > 17 ~ '18 - 29',
                        TRUE ~ '0 - 17'
                    ),
                    age_band = paste('Age', str_remove_all(age_band, 'Age '))
                )
        }

        if (length(ccg) > 0) {
            virtual_ward <- virtual_ward %>% filter(ccg_code %in% ccg)
        }

        if (length(date_range) > 0) {
            virtual_ward <- virtual_ward %>%
                filter(
                    between(ymd(specimen_date), date_range[1], date_range[2])
                )
        }

        if (length(group_1_variables) > 0) {
            plot_1 <- TRUE
            plot_vw_1 <- virtual_ward

            if (any(str_detect(group_1_variables, 'Age'))) {
                age_variables <- group_1_variables[str_detect(group_1_variables, 'Age')]

                plot_vw_1 <- plot_vw_1 %>% filter(age_band %in% age_variables)
            }

            if (any(!str_detect(group_1_variables, 'Age'))) {
                flag_variables <- group_1_variables[!str_detect(group_1_variables, 'Age')]

                flag_variables <- paste(flag_variables, collapse = ' | ')

                plot_vw_1 <- plot_vw_1 %>% filter(eval(rlang::parse_expr(flag_variables)))
            }

            plot_vw_1 <- plot_vw_1 %>%
                count(specimen_week) %>%
                mutate(group = 'Group 1')

            plot_vw <- bind_rows(plot_vw, plot_vw_1)
        }
        if (length(group_2_variables) > 0) {
            plot_2 <- TRUE
            plot_vw_2 <- virtual_ward


            if (any(str_detect(group_2_variables, 'Age'))) {
                age_variables <- group_2_variables[str_detect(group_2_variables, 'Age')]

                plot_vw_2 <- plot_vw_2 %>% filter(age_band %in% age_variables)
            }

            if (any(!str_detect(group_2_variables, 'Age'))) {
                flag_variables <- group_2_variables[!str_detect(group_2_variables, 'Age')]

                flag_variables <- paste(flag_variables, collapse = ' | ')

                plot_vw_2 <- plot_vw_2 %>% filter(eval(rlang::parse_expr(flag_variables)))
            }

            plot_vw_2 <- plot_vw_2 %>%
                count(specimen_week) %>%
                mutate(group = 'Group 2')

            plot_vw <- bind_rows(plot_vw, plot_vw_2)
        }
        if (length(group_3_variables) > 0) {
            plot_3 <- TRUE
            plot_vw_3 <- virtual_ward


            if (any(str_detect(group_3_variables, 'Age'))) {
                age_variables <- group_3_variables[str_detect(group_3_variables, 'Age')]

                plot_vw_3 <- plot_vw_3 %>% filter(age_band %in% age_variables)
            }

            if (any(!str_detect(group_3_variables, 'Age'))) {
                flag_variables <- group_3_variables[!str_detect(group_3_variables, 'Age')]

                flag_variables <- paste(flag_variables, collapse = ' | ')

                plot_vw_3 <- plot_vw_3 %>% filter(eval(rlang::parse_expr(flag_variables)))
            }

            plot_vw_3 <- plot_vw_3 %>%
                count(specimen_week) %>%
                mutate(group = 'Group 3')

            plot_vw <- bind_rows(plot_vw, plot_vw_3)
        }
        if (length(group_4_variables) > 0) {
            plot_4 <- TRUE
            plot_vw_4 <- virtual_ward


            if (any(str_detect(group_4_variables, 'Age'))) {
                age_variables <- group_4_variables[str_detect(group_4_variables, 'Age')]

                plot_vw_4 <- plot_vw_4 %>% filter(age_band %in% age_variables)
            }

            if (any(!str_detect(group_4_variables, 'Age'))) {
                flag_variables <- group_4_variables[!str_detect(group_4_variables, 'Age')]

                flag_variables <- paste(flag_variables, collapse = ' | ')

                plot_vw_4 <- plot_vw_4 %>% filter(eval(rlang::parse_expr(flag_variables)))
            }

            plot_vw_4 <- plot_vw_4 %>%
                count(specimen_week) %>%
                mutate(group = 'Group 4')

            plot_vw <- bind_rows(plot_vw, plot_vw_4)
        }
        if (length(group_5_variables) > 0) {
            plot_5 <- TRUE
            plot_vw_5 <- virtual_ward


            if (any(str_detect(group_5_variables, 'Age'))) {
                age_variables <- group_5_variables[str_detect(group_5_variables, 'Age')]

                plot_vw_5 <- plot_vw_5 %>% filter(age_band %in% age_variables)
            }

            if (any(!str_detect(group_5_variables, 'Age'))) {
                flag_variables <- group_5_variables[!str_detect(group_5_variables, 'Age')]

                flag_variables <- paste(flag_variables, collapse = ' | ')

                plot_vw_5 <- plot_vw_5 %>% filter(eval(rlang::parse_expr(flag_variables)))
            }

            plot_vw_5 <- plot_vw_5 %>%
                count(specimen_week) %>%
                mutate(group = 'Group 5')

            plot_vw <- bind_rows(plot_vw, plot_vw_5)
        }

        # output_relative <<- plot_vw

        if (nrow(plot_vw) > 0) {
            relative_plot <- plot_vw %>%
                ggplot(
                  aes(
                    specimen_week,
                    n,
                    fill = group,
                    text = paste(
                      '<b>Week Start [yyyy-mm-dd]:</b>',
                      specimen_week,
                      '<br><b>Group:</b>',
                      group,
                      '<br><b>Proportion of Cases:</b>',
                      n
                    )
                  )
                ) +
                geom_bar(stat = 'identity', position = 'fill') +
                xlab('Specimen Date [yyyy-mm-dd]') +
                ylab('Proportion of Positive Cases Within Group') +
                ggtitle('Weekly COVID Cases - Proportion')

            ggplotly(relative_plot, tooltip = 'text')
        }
    })

    output$estimationPlot <- renderPlotly({
        group_1_variables <- input$group_1_variables
        group_2_variables <- input$group_2_variables
        group_3_variables <- input$group_3_variables
        group_4_variables <- input$group_4_variables
        group_5_variables <- input$group_5_variables
        ccg <- input$ccg
        date_range <- input$date_range
        plot_vw <- data.frame()

        if (!exists('virtual_ward')) {
            postgres <- dbConnect(
                PostgreSQL(),
                dbname = config$sql_credentials$database,
                user = config$sql_credentials$uid,
                host = config$sql_credentials$server,
                password = config$sql_credentials$pwd,
                port = config$sql_credentials$port
            )

            virtual_ward_full <- dbGetQuery(
                postgres,
                "WITH ics AS (
                    SELECT
                        nhs_number,
                        title,
                        gpp_code,
                        ccg_code,
                        asthma,
                        copd,
                        respiratory AS late_copd,
                        chd,
                        heart_failure,
                        atrial_fibrillation,
                        pad,
                        stroke_tia,
                        cancer,
                        chemo_radiotherapy,
                        haematological_cancers,
                        depression,
                        dementia,
                        mental_health,
                        psychotic_disorder_flag,
                        learning_disabilities,
                        diabetes,
                        hypothyroid,
                        ckd,
                        epilepsy,
                        osteoporosis,
                        rheumatoid_arthritis,
                        smoker,
                        substance_misuse,
                        dispensing_flag,
                        pregnant_with_congenital_heart_defect,
                        rare_diseases,
                        transplant,
                        palliative_care_flag,
                        spl,
                        covid_vuln,
                        rs_frailty_index,
                        risk_score
                    FROM
                        population_master),
                c AS (
                    SELECT nhs_number, age_in_years, ethnicity FROM covid19_cases_p1p2
                )
                SELECT *
                FROM c
                INNER JOIN ics USING (nhs_number);"
            ) %>%
                mutate(
                    status = 'Pending',
                    loaded_date = as.character(today()),
                    updated_date = as_date(NA),
                    recommendation = case_when(
                        is.na(nhs_number) ~ 'No NHS Number',
                        spl ~ 'National SOP',
                        age_in_years > 65 ~ 'National SOP',
                        risk_score > 12 ~ 'RS Greater Risk',
                        TRUE ~ 'RS Lower Risk'
                    ),
                    gpp_code = coalesce(gpp_code, 'Unknown'),
                    ccg_code = coalesce(ccg_code, 'Unknown'),
                    docobo_ref = FALSE,
                    notes = NA,
                    newcontact = NA
                )

            virtual_ward_referred <- dbGetQuery(postgres, "SELECT * FROM virtual_ward_decision WHERE status != 'Pending'")

            dbDisconnect(postgres)

            virtual_ward <- bind_rows(virtual_ward_referred, virtual_ward_full)

            virtual_ward <- virtual_ward %>%
                mutate(
                    specimen_week = floor_date(ymd(specimen_date), 'week'),
                    age_band = case_when(
                        age_in_years > 64 ~ '65+',
                        age_in_years > 49 ~ '50 - 64',
                        age_in_years > 39 ~ '40 - 49',
                        age_in_years > 29 ~ '30 - 39',
                        age_in_years > 17 ~ '18 - 29',
                        TRUE ~ '0 - 17'
                    ),
                    age_band = paste('Age', str_remove_all(age_band, 'Age '))
                )
        }

        if (length(ccg) > 0) {
            virtual_ward <- virtual_ward %>% filter(ccg_code %in% ccg)
        }

        if (length(date_range) > 0) {
            virtual_ward <- virtual_ward %>%
                filter(
                    between(ymd(specimen_date), date_range[1], date_range[2])
                )
        }

        if (length(group_1_variables) > 0) {
            plot_1 <- TRUE
            plot_vw_1 <- virtual_ward

            if (any(str_detect(group_1_variables, 'Age'))) {
                age_variables <- group_1_variables[str_detect(group_1_variables, 'Age')]

                plot_vw_1 <- plot_vw_1 %>% filter(age_band %in% age_variables)
            }

            if (any(!str_detect(group_1_variables, 'Age'))) {
                flag_variables <- group_1_variables[!str_detect(group_1_variables, 'Age')]

                flag_variables <- paste(flag_variables, collapse = ' | ')

                plot_vw_1 <- plot_vw_1 %>% filter(eval(rlang::parse_expr(flag_variables)))
            }

            plot_vw_1 <- plot_vw_1 %>%
                count(specimen_week) %>%
                mutate(group = 'Group 1')

            plot_vw <- bind_rows(plot_vw, plot_vw_1)
        }
        if (length(group_2_variables) > 0) {
            plot_2 <- TRUE
            plot_vw_2 <- virtual_ward


            if (any(str_detect(group_2_variables, 'Age'))) {
                age_variables <- group_2_variables[str_detect(group_2_variables, 'Age')]

                plot_vw_2 <- plot_vw_2 %>% filter(age_band %in% age_variables)
            }

            if (any(!str_detect(group_2_variables, 'Age'))) {
                flag_variables <- group_2_variables[!str_detect(group_2_variables, 'Age')]

                flag_variables <- paste(flag_variables, collapse = ' | ')

                plot_vw_2 <- plot_vw_2 %>% filter(eval(rlang::parse_expr(flag_variables)))
            }

            plot_vw_2 <- plot_vw_2 %>%
                count(specimen_week) %>%
                mutate(group = 'Group 2')

            plot_vw <- bind_rows(plot_vw, plot_vw_2)
        }
        if (length(group_3_variables) > 0) {
            plot_3 <- TRUE
            plot_vw_3 <- virtual_ward


            if (any(str_detect(group_3_variables, 'Age'))) {
                age_variables <- group_3_variables[str_detect(group_3_variables, 'Age')]

                plot_vw_3 <- plot_vw_3 %>% filter(age_band %in% age_variables)
            }

            if (any(!str_detect(group_3_variables, 'Age'))) {
                flag_variables <- group_3_variables[!str_detect(group_3_variables, 'Age')]

                flag_variables <- paste(flag_variables, collapse = ' | ')

                plot_vw_3 <- plot_vw_3 %>% filter(eval(rlang::parse_expr(flag_variables)))
            }

            plot_vw_3 <- plot_vw_3 %>%
                count(specimen_week) %>%
                mutate(group = 'Group 3')

            plot_vw <- bind_rows(plot_vw, plot_vw_3)
        }
        if (length(group_4_variables) > 0) {
            plot_4 <- TRUE
            plot_vw_4 <- virtual_ward


            if (any(str_detect(group_4_variables, 'Age'))) {
                age_variables <- group_4_variables[str_detect(group_4_variables, 'Age')]

                plot_vw_4 <- plot_vw_4 %>% filter(age_band %in% age_variables)
            }

            if (any(!str_detect(group_4_variables, 'Age'))) {
                flag_variables <- group_4_variables[!str_detect(group_4_variables, 'Age')]

                flag_variables <- paste(flag_variables, collapse = ' | ')

                plot_vw_4 <- plot_vw_4 %>% filter(eval(rlang::parse_expr(flag_variables)))
            }

            plot_vw_4 <- plot_vw_4 %>%
                count(specimen_week) %>%
                mutate(group = 'Group 4')

            plot_vw <- bind_rows(plot_vw, plot_vw_4)
        }
        if (length(group_5_variables) > 0) {
            plot_5 <- TRUE
            plot_vw_5 <- virtual_ward


            if (any(str_detect(group_5_variables, 'Age'))) {
                age_variables <- group_5_variables[str_detect(group_5_variables, 'Age')]

                plot_vw_5 <- plot_vw_5 %>% filter(age_band %in% age_variables)
            }

            if (any(!str_detect(group_5_variables, 'Age'))) {
                flag_variables <- group_5_variables[!str_detect(group_5_variables, 'Age')]

                flag_variables <- paste(flag_variables, collapse = ' | ')

                plot_vw_5 <- plot_vw_5 %>% filter(eval(rlang::parse_expr(flag_variables)))
            }

            plot_vw_5 <- plot_vw_5 %>%
                count(specimen_week) %>%
                mutate(group = 'Group 5')

            plot_vw <- bind_rows(plot_vw, plot_vw_5)
        }

        if (nrow(plot_vw) > 0) {
            plot_vw <- plot_vw %>%
                mutate(
                    n_asymptomatic = n * 0.9,
                    n_low = n_asymptomatic * 0.16,
                    n_mid = n_asymptomatic * 0.23,
                    n_high = n_asymptomatic * 0.31,
                    caseload_low = n_low * 8.26,
                    caseload_mid = n_mid * 8.26,
                    caseload_high = n_high * 8.26
                )

            # output_caseload <<- plot_vw

            caseload_plot <- plot_vw %>%
                ggplot(
                    aes(
                        specimen_week,
                        caseload_mid,
                        ymin = caseload_low,
                        ymax = caseload_high,
                        fill = group,
                        colour = group,
                        text = paste(
                          '<b>Week Start [yyyy-mm-dd]:</b>',
                          specimen_week,
                          '<br><b>Group:</b>',
                          group,
                          '<br><b>Caseload:</b>',
                          caseload_mid,
                          paste0(
                            '<br><b>Caseload Range (Low, High):</b> (',
                            caseload_low,
                            ', ',
                            caseload_high,
                            ')'
                          )
                        )
                      )
                ) +
                geom_ribbon(alpha = 0.4, colour = NA, group = 1) +
                geom_line(size = 1, group = 1) +
                xlab('Specimen Date [yyyy-mm-dd]') +
                ylab('Caseload') +
                ggtitle('Weekly CO@H Caseload')

            n_groups <- n_distinct(plot_vw$group)

            caseload_plotly <- ggplotly(caseload_plot, tooltip = 'text')

            for (i in 1 : n_groups) {
              caseload_plotly <- caseload_plotly %>%
                style(hoverinfo = 'skip', traces = i)
            }

            caseload_plotly
        }
    })

    # output$downloadData <- downloadHandler(
    #     filename = function() {
    #         paste("covid_cases", now(), ".csv", sep="")
    #     },
    #     content = function(file) {
    #         write.csv(output_data, file)
    #         rm(output_data, output_caseload, output_dist, output_relative)
    #     }
    # )
}
