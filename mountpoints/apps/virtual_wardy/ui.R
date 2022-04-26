library(shiny)
library(dplyr)
library(stringr)
library(ggplot2)
library(plotly)
library(RPostgreSQL)
library(lubridate)

user_chooser <- c(
    'Age 0 - 17',
    'Age 18 - 29',
    'Age 30 - 39',
    'Age 40 - 49',
    'Age 50 - 64',
    'Age 65+',
    'Shielding' = 'spl',
    'Asthma' = 'asthma',
    'COPD' = 'copd',
    'CHD' = 'chd',
    'Heart Failure' = 'heart_failure',
    'Atrial Fibrillation' = 'atrial_fibrillation',
    'PAD' = 'pad',
    'Stroke/TIA' = 'stroke_tia',
    'Cancer' = 'cancer',
    'Chemo-Radiotherapy' = 'chemo_radiotherapy',
    'Haematological Cancers' = 'haematological_cancers',
    'Depression' = 'depression',
    'Dementia' = 'dementia',
    'Severe Mental Illness' = 'mental_health',
    'Learning Disabilities' = 'learning_disabilities',
    'Type-II Diabetes' = 'diabetes',
    'Hypothyroidism' = 'hypothyroid',
    'CKD' = 'ckd',
    'Epilepsy' = 'epilepsy',
    'Osteoporosis' = 'osteoporosis',
    'Rheumatoid Arthritis' = 'rheumatoid_arthritis'
)

ccg_chooser <- c(
    'Chorley & South Ribble CCG' = '00X',
    'Blackburn With Darwen CCG' = '00Q',
    'West Lancashire CCG' = '02G',
    'Blackpool CCG' = '00R',
    'East Lancashire CCG' = '01A',
    'Morecambe Bay CCG' = '01K',
    'Greater Preston CCG' = '01E',
    'Fylde & Wyre CCG' = '02M'
)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("COVID Oximetry @ Home Estimations"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            selectInput(
                "ccg",
                "CCG (s):",
                ccg_chooser,
                multiple = TRUE
            ),
            dateRangeInput(
                "date_range",
                "Date Range [yyyy-mm-dd]",
                start = today() - (6 * 7),
                end = today(),
                format = "yyyy-mm-dd",
                startview = "month"
            ),
            selectInput(
                "group_1_variables",
                "Group 1 Variable (s):",
                user_chooser,
                multiple = TRUE
            ),
            selectInput(
                "group_2_variables",
                "Group 2 Variable (s):",
                user_chooser,
                multiple = TRUE
            ),
            selectInput(
                "group_3_variables",
                "Group 3 Variable (s):",
                user_chooser,
                multiple = TRUE
            ),
            selectInput(
                "group_4_variables",
                "Group 4 Variable (s):",
                user_chooser,
                multiple = TRUE
            ),
            selectInput(
                "group_5_variables",
                "Group 5 Variable (s):",
                user_chooser,
                multiple = TRUE
            ),
        ),

        # Show a plot of the generated distribution
        mainPanel(
            plotlyOutput("distPlot"),
            plotlyOutput("relativePlot"),
            plotlyOutput("estimationPlot")#,
            #downloadLink("downloadData", "Download")
            # renderUI({
            #     req(input$group_1_variables)
            #     downloadLink("downloadData", "Download")
            # })
        )
    )
)
