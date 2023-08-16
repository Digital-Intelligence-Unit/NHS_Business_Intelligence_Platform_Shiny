library(shiny)
library(shinythemes)
library(dplyr)
library(ggplot2)
library(plotly)
library(tidyverse)
library(lubridate)
library(scales)
library(qicharts2)



str_to_title_nhs_vectorized <- function(x) {
  words <- str_to_lower(x)
  words <- str_to_title(words)
  words <- str_replace_all(words, regex("nhs", ignore_case = TRUE), "NHS")
  words <- str_replace_all(words, regex("\\bAnd\\b", ignore_case = TRUE), "and")
  words <- str_replace_all(words, regex("\\bmri\\b", ignore_case = TRUE), "MRI")
  words <- str_replace_all(words, regex("\\bct\\b", ignore_case = TRUE), "CT")
  
  words
}

data <- readRDS("data.rds")

lookup <- data.frame(
  original = c("AUDIOLOGY_ASSESSMENTS", "BARIUM_ENEMA", "COLONOSCOPY", "CT", "CYSTOSCOPY",
               "DEXA_SCAN", "ECHOCARDIOGRAPHY", "ELECTROPHYSIOLOGY", "FLEXI_SIGMOIDOSCOPY",
               "GASTROSCOPY", "MRI", "NON_OBSTETRIC_ULTRASOUND", "PERIPHERAL_NEUROPHYS",
               "SLEEP_STUDIES", "TOTAL", "URODYNAMICS", "AUDIOLOGY - AUDIOLOGY ASSESSMENTS",
               "BARIUM ENEMA", "CARDIOLOGY - ECHOCARDIOGRAPHY", "CARDIOLOGY - ELECTROPHYSIOLOGY",
               "COMPUTED TOMOGRAPHY", "DEXA SCAN", "FLEXI SIGMOIDOSCOPY",
               "MAGNETIC RESONANCE IMAGING", "NEUROPHYSIOLOGY - PERIPHERAL NEUROPHYSIOLOGY",
               "NON-OBSTETRIC ULTRASOUND", "RESPIRATORY PHYSIOLOGY - SLEEP STUDIES",
               "URODYNAMICS - PRESSURES & FLOWS"),
  cleaned = c("AUDIOLOGY ASSESSMENTS", "BARIUM ENEMA", "COLONOSCOPY", "CT", "CYSTOSCOPY",
              "DEXA SCAN", "ECHOCARDIOGRAPHY", "ELECTROPHYSIOLOGY", "FLEXI SIGMOIDOSCOPY",
              "GASTROSCOPY", "MRI", "NON OBSTETRIC ULTRASOUND", "PERIPHERAL NEUROPHYS",
              "SLEEP STUDIES", "TOTAL", "URODYNAMICS", "AUDIOLOGY ASSESSMENTS",
              "BARIUM ENEMA", "ECHOCARDIOGRAPHY", "ELECTROPHYSIOLOGY", "CT", "DEXA SCAN",
              "FLEXI SIGMOIDOSCOPY", "MRI", "PERIPHERAL NEUROPHYS",
              "NON OBSTETRIC ULTRASOUND", "SLEEP STUDIES", "URODYNAMICS")
)

# Use left_join to join df with lookup
data <- data %>%
  mutate(`Diagnostic Tests` = toupper(`Diagnostic Tests`)) %>%
  left_join(lookup, by = c("Diagnostic Tests" = "original")) %>%
  mutate(`Diagnostic Tests` = ifelse(is.na(cleaned), `Diagnostic Tests`, cleaned)) %>%
  #rename_with(.fn = ~ gsub("\\.", " ", .)) %>%
  select(-cleaned) %>%
  filter(`Diagnostic Tests` != "TOTAL") %>%
  mutate(`Diagnostic Tests` = str_to_title_nhs_vectorized(`Diagnostic Tests`)) %>%
  mutate(`Provider Org Name` = str_to_title_nhs_vectorized(`Provider Org Name`)) %>%
  mutate(`Provider Parent Name` = str_to_title_nhs_vectorized(`Provider Parent Name`))


## clean duplicated test names


library(shinyWidgets)

ui <- fluidPage(
  theme = shinytheme("cosmo"),
  navbarPage(
    "Waiting List Explorer",
    tabPanel("SPC",
             titlePanel("SPC Chart Generator"),
             sidebarLayout(
               sidebarPanel(
                 materialSwitch(inputId = "facet_toggle", label = "Enable Facet", value = FALSE),
                 hr(),
                 selectInput(
                   "commissioner",
                   "Commissioner",
                   choices = unique(data$`Provider Parent Name`),
                   multiple = TRUE,
                   selected = c("NHS Lancashire and South Cumbria Integrated Care Board")
                 ),
                 uiOutput("provider_ui"),
                 uiOutput("test_ui"),
                 dateRangeInput("date_range", "Date Range",
                                min = min(data$Date), max = max(data$Date),
                                start = min(data$Date), end = max(data$Date)),
                 selectInput("value_field", "Value Field",
                             choices = c("Total Activity", "Total WL")),
                 radioButtons("rebase_mode", "Rebase Mode", c("Manual", "Auto"),selected = "Auto"),
                 conditionalPanel(condition = "input.rebase_mode == 'Manual'",
                                  textInput("rebase_dates", "Rebase Dates (comma-separated)", "")),
                 conditionalPanel(
                   condition = "input.rebase_mode == 'Auto'",
                   sliderInput("run_length", "Run Length", min = 3, max = 20, value = 6)
                 ),
                 hr(),
                 helpText("Note: With facet enabled, providers and tests data will be combined and displayed as aggregated results."),
                 conditionalPanel(condition = "input.facet_toggle",
                                  tags$div(
                                    materialSwitch(inputId = "facet_field", label = "Provider Org Name", inline = TRUE, value = FALSE),
                                    tags$span("Diagnostic Tests")
                                  )),
                                  #uiOutput("group_by_value_input")),
                 actionButton("submit", "Generate SPC Chart")
               ),
               mainPanel(
                 conditionalPanel(condition = "!input.facet_toggle",
                                  plotlyOutput("spc_chart", width = "100%", height = "1000px")),
                 conditionalPanel(condition = "input.facet_toggle",
                                  plotlyOutput("facet_spc_chart", width = "100%", height = "1000px"))
               )
             )
    ),
    tabPanel("Time Series",
             titlePanel("Time Series"),
             sidebarLayout(
             sidebarPanel(
               selectInput("model_type", "Model Type", choices = c("Prophet", "Auto ARIMA"), selected = "Prophet"),
               selectInput("commissioner_ts", "Commissioner", choices = unique(data$`Provider Parent Name`),
                           multiple = TRUE,
                           selected = c("NHS Lancashire and South Cumbria Integrated Care Board")),
               uiOutput("provider_ui_ts"),
               uiOutput("test_ui_ts"),
               #selectInput("provider_ts", "Provider", choices = unique(data$Provider.Org.Name), multiple = FALSE),
               #selectInput("test_ts", "Diagnostic Test", choices = unique(data$Diagnostic.Tests), multiple = FALSE),
               dateRangeInput("date_range_ts", "Date Range",
                              min = min(data$Date), max = max(data$Date),
                              start = "2021-01-01", end = max(data$Date)),
               selectInput("value_field_ts", "Value Field",
                           choices = c("Total Activity", "Total WL"), multiple = FALSE),
               #materialSwitch(inputId = "decomposition_toggle", label = "Display Decomposition", value = TRUE),
               numericInput("predict_months", "Predict X Months", value = 6, min = 1),
               actionButton("generate_model", "Generate Model"),
               
             ),
             mainPanel(
               textOutput("model_summary"),
               
               uiOutput("time_series_plot_ui"),
               uiOutput("decomposition_plot_ui")
             )
    )
    )
    
    
  )
)
