# Load required libraries

library(shiny)
library(shinyjs)
library(plotly)
library(dplyr)
library(ggplot2)
library(shinydashboard)
library(shinydashboardPlus)
library(shinycssloaders)
library(scales)
library(funnelR)
library(DT)
library(shinythemes)
library(shinyBS)
library(shinyWidgets)
library(PHEindicatormethods)
library(RColorBrewer)
library(DBI)
library(RPostgreSQL)
library(NHSRplotthedots)
library(runcharter)
library(readxl)
library(tidyverse)
library(plotly)
library(zoo)


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

  pcn_lookup <- dbGetQuery(con, "
    SELECT distinct gpp_short_name, pcn FROM public.population_master;
  ")

  dbDisconnect(con)

 Programme_Budgeting_Mappings_Definitions <- read_excel("Programme-Budgeting-Mappings-Definitions.xls", sheet = "Inpatient Activity", skip = 4)

  Programme_Budgeting_MAIN <- read_excel("Programme-Budgeting-Mappings-Definitions.xls", sheet = "Programme Budgeting Categories", skip = 3) %>%
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

# Shiny UI
ui <- dashboardPage(
  dashboardHeader(disable = T),
  dashboardSidebar(disable = T,
                   sidebarMenu(
                     menuItem("GPP Costs", tabName = "gppcosts", icon = icon("stethoscope")),
                     menuItem("Program Costs", tabName = "programcosts", icon = icon("dashboard"))#,
                     #menuItem("PCN Costs", tabName = "pcncosts", icon = icon("dashboard"))
                   )
  ),
  dashboardBody(
    useShinyjs(),
    tags$script(HTML(
      '
      function _clickOnFront(el) {
          $(el)
            .find(".card-front")
            .css({
              "-webkit-transform": "perspective(1600px) rotateY(-180deg)",
              transform: "perspective(1600px) rotateY(-180deg)"
            })
            .toggleClass("active");
          $(el)
            .find(".card-back")
            .css({
              "-webkit-transform": "perspective(1600px) rotateY(0deg)",
              transform: "perspective(1600px) rotateY(0deg)"
            })
            .toggleClass("active");
       }
      function _clickOnBack(el) {
          $(el)
            .find(".card-front")
            .css({ "-webkit-transform": "", transform: "" })
            .toggleClass("active");
          $(el)
            .find(".card-back")
            .css({ "-webkit-transform": "", transform: "" })
            .toggleClass("active");
      }
      '
    )),
    tags$style(HTML("
    
      .navbar-custom-menu {
      height: 0;
      width: 0;
      }
    
      #sidebarCollapsed {
      position: fixed;
      top: 50px; /* adjust this to match the height of your dashboard header */
      bottom: 0;
      overflow: auto;
      height: auto;
    }
    
      .uiheading {
        position: relative;
        text-align: center;
        font-size:30px; 
        font-weight:300; 
        color:#222; 
        letter-spacing:1px;
        text-transform: uppercase;
        display: grid;
        grid-template-columns: 1fr max-content 1fr;
        grid-template-rows: 27px 0;
        grid-gap: 20px;
        align-items: center;
      }
      .uiheading:before, .uiheading:after {
        content: ''; 
        display: block; 
        border-bottom: 1px solid #3c8dbc; 
        border-top: 1px solid #3c8dbc; 
        height: 5px; 
        background-color:#3c8dbc;
      }
      
      .small-box bg-green {
      word-wrap: break-word;
      white-space: pre-line;
      }
  
      ")),
    tabItems(
      # GPP Costs Tab
      tabItem(
        tabName = "gppcosts",
        tags$head(
          tags$style(
            HTML(
              "
              #mydropdown2, #mydropdown3 {
                list-style-type: none;
              }
              .dropdown-menu {
                background-color: #f8f9fa;
              }
              a.dropdown-toggle {
                display: inline-block;
                padding: 10px 20px;
                background-color: #3c8dbc;
                color: white;
                text-decoration: none;
                border-radius: 5px; 
                transition: background-color 0.3s ease; 
              }

              a.dropdown-toggle:hover {
                background-color: #367fa9;
              }
              
               .btn-flip {
                display: inline-block;
                padding: 10px 20px;
                background-color: #3c8dbc;
                color: white;
                text-decoration: none;
                border-radius: 5px; 
                transition: background-color 0.3s ease; 
              }

              .btn-flip:hover {
                background-color: #367fa9;
                color: white;
              }


              /* Styling the switch container */
              

              /* Styling the label text */
              div.switch-container label {
                  font-family: 'Arial', sans-serif;
                  font-weight: bold;
                  color: #333;
                  font-size: 1.5em;
              }

              /* Styling the adjacent text */
              div.switch-container span {
                  font-family: 'Arial', sans-serif;
                  font-weight: bold;
                  color: #333;
                  margin-left: 10px;
                  font-size: 1.5em;
              }

      
              .carousel-tablist-highlight.focus {
                outline: none;
                background-color: transparent;
              }


              .well {
        background-color: transparent;
        border: none;
        box-shadow: none;
      }
        
              
              "
            ),
            
            tags$script("
  $(document).on('shiny:connected', function(event) {
    $('#id2 .carousel-inner .item').first().addClass('active');
  });
")
            
            
          )
        ),
        
        fluidPage(
          
          
          tags$div(class = "switch-container",
                   materialSwitch(inputId = "switchView", label = "GP", inline = TRUE, value = TRUE),
                   tags$span("PCN")
          ),
          
          ## gpp 
          conditionalPanel(
            condition = "input.switchView == false", # Note the lowercase 'true' for JavaScript condition
            
            wellPanel(class = "fixed-height-well",
              column(width = 3,
                     selectizeInput(
                       inputId = "pbc",
                       label = "Select PBCs to highlight:",
                       choices = setdiff(unique(Programme_Budgeting_Mappings_Definitions$`Programme Category Name`), "Not coded"),
                       multiple = TRUE
                     )),
              
              
              column(width = 6, 
                     selectizeInput(
                       inputId = "gpp",
                       label = "Select GP:",
                       choices = unique(pcn_lookup$gpp_short_name),
                       width = "100%"
                     ))
              ,
              column(width = 3, 
                     selectizeInput(
                       inputId = "excludePBC",
                       label = "PBCs to exclude:",
                       choices = append(c('Not coded'),unique(Programme_Budgeting_Mappings_Definitions$`Programme Category Name`)),
                       multiple = TRUE,
                       selected = 'Not coded'
                     ))
            )
            
            
          ),
          #PCN
          conditionalPanel(
            condition = "input.switchView == true", # Note the lowercase 'true' for JavaScript condition
            
            wellPanel(class = "fixed-height-well",
              column(width = 3,
                     
                     selectizeInput(
                       inputId = "pbc_pcn",
                       label = "Select PBCs to highlight:",
                       choices = setdiff(unique(Programme_Budgeting_Mappings_Definitions$`Programme Category Name`), "Not coded"),
                       multiple = TRUE
                     )),
              column(width = 6,selectizeInput(
                inputId = "pcn",
                label = "Select PCN:",
                choices = unique(pcn_lookup$pcn)
              )),
              column(width = 3,selectizeInput(
                inputId = "excludePBC_pcn",
                label = "PBCs to exclude:",
                choices = append(c('Not coded'),unique(Programme_Budgeting_Mappings_Definitions$`Programme Category Name`)),
                multiple = TRUE,
                selected = 'Not coded'
              ))
              
              
            ))
          
          
          ,
          # fluidRow(
          #   # column(
          #   #   4,
          #   #   selectInput("gpp", "Select GP:", choices = unique(df_pbc$gpp_short_name))
          #   # ),
          #   # column(
          #   #   4,
          #   #   selectInput("pbc", "Select PBCs to highlight:", choices = unique(df_pbc$`Programme Category Name`), multiple = TRUE)
          #   # ),
          #   # column(
          #   #   4,
          #   #   selectInput("excludePBC", "PBCs to exclude:", choices = unique(df_pbc$`Programme Category Name`), multiple = TRUE, selected = 'Not coded')
          #   # ),  
          #   column(12, div(class = "uiheading", uiOutput("dynamicTitleGPP") %>% withSpinner(type = 3, color = '#3c8dbc', size = 1.5,color.background ='#ecf0f5' )))
          # ),
          fluidRow(column(12,
            h3("Metrics"),
            valueBoxOutput("mostUnderspendingGroup", width = 6) %>% 
              withSpinner(type = 3, color = '#3c8dbc', size = 1.5
                          ,color.background ='#ecf0f5' ),
            
            valueBoxOutput("mostOverspendingGroup", width = 6) %>% 
              withSpinner(type = 3, color = '#3c8dbc', size = 1.5
                          ,color.background ='#ecf0f5' ),
          )),
          fluidRow(#valueBoxOutput("RsquaredBox"),
            #valueBoxOutput("MAEBox"),
            #valueBoxOutput("RMSEBox"),
            h3("Costs"),
            valueBoxOutput("PredictedCostBox", width = 6) %>% withSpinner(type = 3, color = '#3c8dbc', size = 1.5,color.background ='#ecf0f5' ),
            valueBoxOutput("ActualCostBox", width = 6) %>% withSpinner(type = 3, color = '#3c8dbc', size = 1.5,color.background ='#ecf0f5' ),
            
          ),
          tags$hr(style = "border-top: 2px solid #ccc; margin-top: 5px; margin-bottom: 5px;"),
          fluidRow(
            # column( width = 12,
            box(
              title = "Statistical Process Control", collapsible = TRUE, solidHeader = TRUE, status = "primary", width = 12,
              plotlyOutput("gpSPC") %>% withSpinner(type = 3, color = '#3c8dbc', size = 1.5,color.background ='#ecf0f5' )
            ),
          ),
          tags$hr(style = "border-top: 2px solid #ccc; margin-top: 5px; margin-bottom: 5px;"),
          
          
          fluidRow(
            box(
              tags$div(
                materialSwitch(inputId = "absrel", label = "Absolute", inline = TRUE, value = FALSE),
                tags$span("Relative")
              ),
              
              title = "Variations Across Programs", 
              collapsible = TRUE, 
              solidHeader = TRUE, 
              status = "primary", 
              width = 12,
              style = "min-height: 500px;", 
              conditionalPanel(
                condition = "input.absrel == false", # Note the lowercase 'false' for JavaScript condition
                
                plotlyOutput("gppResidualPlot1", height = "100%", width= "100%") %>% withSpinner(proxy.height ="250px",type = 3, color = '#3c8dbc', size = 1.5, color.background ='#ecf0f5' ),
                tags$br(), # This will add a single line space
                #tags$hr(style = "border-top: 2px solid #ccc; margin-top: 20px; margin-bottom: 20px;"),
                #plotlyOutput("gppHist1") %>% withSpinner(type = 3, color = '#3c8dbc', size = 1.5, color.background ='#ecf0f5' )
              ),
              
              conditionalPanel(
                condition = "input.absrel == true", # Note the lowercase 'true' for JavaScript condition
                
                plotlyOutput("gppResidualPlot", height = "100%", width= "100%") %>% withSpinner(proxy.height ="250px",type = 3, color = '#3c8dbc', size = 1.5, color.background ='#ecf0f5' ),
                tags$br(), # This will add a single line space
                #tags$hr(style = "border-top: 2px solid #ccc; margin-top: 20px; margin-bottom: 20px;"),
                
              )#,
              #plotlyOutput("gppHist") %>% withSpinner(type = 3, color = '#3c8dbc', size = 1.5, color.background ='#ecf0f5' )
              
            )
          )
          ,
          #           tags$hr(style = "border-top: 2px solid #ccc; margin-top: 5px; margin-bottom: 5px;"),
          #           fluidRow(
          #             box(
          #     id = "tabset1",
          #     title = "Standardized Activity Rate Analysis",
          #     status = "primary",
          #     width = 12,
          #     collapsible = TRUE,
          #     collapsed = TRUE,
          #     solidHeader = TRUE,
          #     tabBox(
          #    width = 12,
          #     tabPanel(
          
          
          #         title = "Plot",  # You can name this as appropriate
          #         plotlyOutput("gppFunnelPlot") %>% withSpinner(
          #             type = 3, 
          #             color = '#3c8dbc', 
          #             size = 1.5, 
          #             color.background ='#ecf0f5'
          #         )
          
          #     ),
          #   tabPanel(
          #     title = "Description",
          #     fluidRow(
          #         column(
          #             width = 6,
          #             tags$h4("Navigating the Funnel Plot"),
          #             tags$p("This chart presents a comprehensive view of each program budgeting code's performance, emphasizing both event frequency and the ratio of real to predicted costs."),
          #             tags$ul(
          #                 tags$li("Horizontal Line (X-axis): Reflects the number of events (e.g., patient visits) associated with each program budgeting code. More events equate to a higher count."),
          #                 tags$li("Vertical Line (Y-axis): Denotes the frequency of an event for every 1,000 patients. A position higher on the axis indicates increased event occurrence."),
          #                 tags$li("Points: 
          #                         - Their position and color provide insights into a program budgeting code's activity rate. 
          #                         - Any location highlighted will display with an asterisk (*) rather than a circle, aiding in easy identification of specific program budgeting codes. 
          #                         - The color gradient of the points represents the ratio of the actual (real) cost to the predicted cost multiplied by 100. A value of 100 indicates that the real cost aligns precisely with the predicted cost. Values greater than 100 denote higher real costs than predicted, while those below 100 indicate lower real costs compared to predictions."),
          #                 tags$li("Guiding Lines: Derived from the Poisson distribution, these lines encapsulate the anticipated performance range. Points outside these lines are outliers, implying either significantly high or low performance.")
          #             ),
          #             tags$p("Tip: For detailed insights on a particular program budgeting code, hover over its corresponding point.")
          #         ),
          #         column(
          #             width = 6,
          #             tags$h4("Deep Dive into DSR"),
          #             tags$p("Direct Standardized Rates (DSR) elucidate the average activity rate for a specific program budgeting code patient group. Adjustments made for differences, like age distribution, ensure that DSR offers a dependable method for cross-comparison among program budgeting codes."),
          
          #         )
          #     )
          # )
          
          
          
          
          
          # )
          
          #           )
          
          #           )
          #,
          #tags$hr(style = "border-top: 2px solid #ccc; margin-top: 5px; margin-bottom: 5px;"),
          fluidRow(
            box(
              
              
              title = "Distribution Analysis",
              collapsible = TRUE,
              solidHeader = TRUE,
              status = "primary",
              collapsed = T,
              width = 12,
              plotlyOutput("gppDensity") %>% withSpinner(type = 3, color = '#3c8dbc', size = 1.5,color.background ='#ecf0f5' ),
              tags$hr(style = "border-top: 2px solid #ccc; margin-top: 20px; margin-bottom: 20px;"),
              plotlyOutput("gppHist") %>% withSpinner(type = 3, color = '#3c8dbc', size = 1.5, color.background ='#ecf0f5' )
            )
          ),
          tags$hr(style = "border-top: 2px solid #ccc; margin-top: 5px; margin-bottom: 5px;"),
          fluidRow(
            box(
              
              tags$div(
                materialSwitch(inputId = "gplogs", label = "Real scale", inline = TRUE, value = FALSE),
                tags$span("Log scale (base 10)")
              ),
              
              title = "Actual vs Predicted", collapsible = TRUE, solidHeader = TRUE, status = "primary", width = 12, collapsed = T,
              conditionalPanel(
                condition = "input.gplogs == false",#plotlyOutput("gppDensity") %>% withSpinner(type = 3, color = '#3c8dbc', size = 1.5,color.background ='#ecf0f5' ),
                tags$br(), # This will add a single line space
                #tags$hr(style = "border-top: 2px solid #ccc; margin-top: 20px; margin-bottom: 20px;"),
                plotlyOutput("gppPlot1") %>% withSpinner(type = 3, color = '#3c8dbc', size = 1.5,color.background ='#ecf0f5' )
              ),
              conditionalPanel(
                condition = "input.gplogs == true",
                tags$br(), # This will add a single line space
                #tags$hr(style = "border-top: 2px solid #ccc; margin-top: 20px; margin-bottom: 20px;"),
                plotlyOutput("gppPlot") %>% withSpinner(type = 3, color = '#3c8dbc', size = 1.5,color.background ='#ecf0f5' )
              )
            ),
          ),
        )
      ),
      
      # Program Costs Tab
      tabItem(
        tabName = "programcosts",
        tags$head(
          tags$style(
            HTML(
              "
              #mydropdown, #mydropdown4 {
                list-style-type: none;
              }
              .dropdown-menu {
                background-color: #f8f9fa;
              }
              a.dropdown-toggle {
                display: inline-block;
                padding: 10px 20px;
                background-color: #3c8dbc;
                color: white;
                text-decoration: none;
                border-radius: 5px; 
                transition: background-color 0.3s ease; 
              }

              a.dropdown-toggle:hover {
                background-color: #367fa9;
              }
              "
            )
            
          )
        ),
        fluidPage(
          
          tags$div(class = "switch-container",
                   materialSwitch(inputId = "switchViewPBC", label = "GP", inline = TRUE, value = TRUE),
                   tags$span("PCN")
          ),
          ## gpp 
          conditionalPanel(
            condition = "input.switchViewPBC == false",
            wellPanel(
              column(width = 3,
                     
                     selectizeInput(
                       inputId = "GP_H",
                       label = "Select GPs to highlight:",
                       choices = unique(pcn_lookup$gpp_short_name),
                       multiple = TRUE
                     )),
              column(width = 6,selectizeInput(
                inputId = "program",
                label = "Select Program Code:",
                choices = setdiff(unique(Programme_Budgeting_Mappings_Definitions %>% arrange(pbccode) %>% select(`Programme Category Name`) %>% distinct(.) %>% pull()), "Not coded")
              )),
              column(width = 3,selectizeInput(
                inputId = "excludeGP",
                label = "GPs to exclude:",
                choices = unique(pcn_lookup$gpp_short_name),
                multiple = TRUE
              ))
            )),
          
          ## pcn 
          conditionalPanel(
            condition = "input.switchViewPBC == true",
            wellPanel(
              column(width = 3,
                     
                     selectizeInput(
                       inputId = "PCN_H",
                       label = "Select PCNs to highlight:",
                       choices = unique(pcn_lookup$pcn),
                       multiple = TRUE
                     )),
              column(width = 6,selectizeInput(
                inputId = "programpcn",
                label = "Select Program Code:",
                choices = setdiff(unique(Programme_Budgeting_Mappings_Definitions %>% arrange(pbccode) %>% select(`Programme Category Name`) %>% distinct(.) %>% pull()), "Not coded")
              )),
              column(width = 3,selectizeInput(
                inputId = "excludePCN",
                label = "PCNs to exclude:",
                choices = unique(pcn_lookup$pcn),
                multiple = TRUE
              ))
            )),
          
          # fluidRow(
          #   # column(
          #   #   4,
          #   #   selectInput("program", "Select Program Code:", choices = setdiff(unique(df_pbc$`Programme Category Name`), "Not coded"))
          #   # ),
          #   # column(
          #   #   4,
          #   #   selectInput("GP_H", "Select GPs to highlight:", choices = unique(df_pbc$gpp_short_name), multiple = TRUE)
          #   # ),
          #   # column(
          #   #   4,
          #   #   selectInput("excludeGP", "GPs to exclude:", choices = unique(df_pbc$gpp_short_name), multiple = TRUE)
          #   # ), 
          #   column(12, div(class = "uiheading", uiOutput("dynamicTitleProgram")%>% withSpinner(type = 3, color = '#3c8dbc', size = 1.5,color.background ='#ecf0f5' )))
          # ),
        ),
        # fluidRow(
        #   h3("Metrics"),
        #   valueBoxOutput("RsquaredBox_pbc") %>% withSpinner(type = 3, color = '#3c8dbc', size = 1.5,color.background ='#ecf0f5' ),
        #   valueBoxOutput("MAEBox_pbc") %>% withSpinner(type = 3, color = '#3c8dbc', size = 1.5,color.background ='#ecf0f5' ),
        #   valueBoxOutput("RMSEBox_pbc") %>% withSpinner(type = 3, color = '#3c8dbc', size = 1.5,color.background ='#ecf0f5' ),
        # ),
        tags$hr(style = "border-top: 2px solid #ccc; margin-top: 5px; margin-bottom: 5px;"),
        fluidRow(
          # column( width = 12,
          box(
            title = "Statistical Process Control", collapsible = TRUE, solidHeader = TRUE, status = "primary", width = 12,
            plotlyOutput("programSPC") %>% withSpinner(type = 3, color = '#3c8dbc', size = 1.5,color.background ='#ecf0f5' )
          ),
        ),
        
        tags$hr(style = "border-top: 2px solid #ccc; margin-top: 5px; margin-bottom: 5px;"),
        
        fluidRow(
          shinydashboardPlus::box(
            tags$div(
              materialSwitch(inputId = "absrelpbc", label = "Absolute", inline = TRUE, value = FALSE),
              tags$span("Relative")
            ),
            
            title = "Variations Across Locations", 
            collapsible = TRUE, 
            solidHeader = TRUE, 
            status = "primary", 
            width = 12,
            style = "min-height: 500px;", # This sets a minimum height for the box
            
            conditionalPanel(
              condition = "input.absrelpbc == false", # Note the lowercase 'false' for JavaScript condition
              
              plotlyOutput("programResidualPlot1", height = "100%", width= "100%") %>% withSpinner(proxy.height ="250px",type = 3, color = '#3c8dbc', size = 1.5, color.background ='#ecf0f5' ),
              tags$br(), # This will add a single line space
              
              #plotlyOutput("programHist1") %>% withSpinner(type = 3, color = '#3c8dbc', size = 1.5, color.background ='#ecf0f5' )
            ),
            
            conditionalPanel(
              condition = "input.absrelpbc == true", # Note the lowercase 'true' for JavaScript condition
              
              plotlyOutput("programResidualPlot", height = "100%", width= "100%") %>% withSpinner(proxy.height ="250px", type = 3, color = '#3c8dbc', size = 1.5, color.background ='#ecf0f5' ),
              tags$br(), # This will add a single line space
              
              
            )
          )
        ),
        
        # fluidRow(
        #   box(
        #     title = "Residual Analysis", collapsible = TRUE, solidHeader = TRUE, status = "primary", width = 12,
        #     plotlyOutput("programResidualPlot") %>% withSpinner(type = 3, color = '#3c8dbc', size = 1.5,color.background ='#ecf0f5' ),
        #     tags$br(), # This will add a single line space
        #     tags$hr(style = "border-top: 2px solid #ccc; margin-top: 20px; margin-bottom: 20px;"),
        #     plotlyOutput("programHist") %>% withSpinner(type = 3, color = '#3c8dbc', size = 1.5,color.background ='#ecf0f5' )
        #   ),
        # ),
        tags$hr(style = "border-top: 2px solid #ccc; margin-top: 5px; margin-bottom: 5px;"),
        fluidRow(
          box(
            
            
            title = "Standardized Activity Rate Analysis",
            collapsible = TRUE,
            solidHeader = TRUE,
            status = "primary",
            collapsed = T,
            width = 12,
            tabBox(
              width = 12,
              id = "id1",
              # indicators = F,
              tabPanel( 
                title = "Plot",
                active = T,
                width =12,
                
                #actionButton("flipButton", "Flip the box!"),
                plotlyOutput("programFunnelPlot") %>% withSpinner(type = 3, color = '#3c8dbc', size = 1.5,color.background ='#ecf0f5' )
              ),
              tabPanel(
                title = "Description",
                fluidRow(
                  column(
                    width = 6,
                    tags$h4("Navigating the Funnel Plot"),
                    # When input$switchViewPBC is TRUE
                    conditionalPanel(
                      condition = "input.switchViewPBC == true",
                      tags$p("This chart presents a comprehensive view of each PCN performance, emphasizing both event frequency and the ratio of real to predicted costs."),
                      tags$ul(
                        tags$li("Horizontal Line (X-axis): Reflects the Population associated with each PCN."),
                        tags$li("Vertical Line (Y-axis): Denotes the frequency of an event for every 1,000 patients. A position higher on the axis indicates increased event occurrence."),
                        tags$li("Points: 
                        - Their position and color provide insights into a PCNs activity rate. 
                        - Any location highlighted will display with an asterisk (*) rather than a circle, aiding in easy identification of specific PCN. 
                        - The color gradient of the points represents the ratio of the actual (real) cost to the predicted cost multiplied by 100. A value of 100 indicates that the real cost aligns precisely with the predicted cost. Values greater than 100 denote higher real costs than predicted, while those below 100 indicate lower real costs compared to predictions."),
                        tags$li("Guiding Lines: Derived from the Poisson distribution, these lines encapsulate the anticipated performance range. Points outside these lines are outliers, implying either significantly high or low performance.")
                      ),
                      tags$p("Tip: For detailed insights on a particular PCN, hover over its corresponding point.")
                    ),
                    # When input$switchViewPBC is FALSE
                    conditionalPanel(
                      condition = "input.switchViewPBC == false",
                      tags$p("This chart presents a comprehensive view of each GPs performance, emphasizing both event frequency and the ratio of real to predicted costs."),
                      tags$ul(
                        tags$li("Horizontal Line (X-axis): Reflects the population associated with each GP."),
                        tags$li("Vertical Line (Y-axis): Denotes the frequency of an event for every 1,000 patients. A position higher on the axis indicates increased event occurrence."),
                        tags$li("Points: 
                        - Their position and color provide insights into a GPs activity rate. 
                        - Any location highlighted will display with an asterisk (*) rather than a circle, aiding in easy identification of specific GPs. 
                        - The color gradient of the points represents the ratio of the actual (real) cost to the predicted cost multiplied by 100. A value of 100 indicates that the real cost aligns precisely with the predicted cost. Values greater than 100 denote higher real costs than predicted, while those below 100 indicate lower real costs compared to predictions."),
                        tags$li("Guiding Lines: Derived from the Poisson distribution, these lines encapsulate the anticipated performance range. Points outside these lines are outliers, implying either significantly high or low performance.")
                      ),
                      tags$p("Tip: For detailed insights on a particular GP practice, hover over its corresponding point.")
                    )
                  ),
                  column(
                    width = 6,
                    tags$h4("Deep Dive into DSR"),
                    tags$p("Direct Standardized Rates (DSR) elucidate the average activity rate for a specific GP practice patient group. Adjustments made for differences, like age distribution, ensure that DSR offers a dependable method for cross-comparison among program budgeting codes."),
                    
                  )
                )
              )
            )
          )
        ),
        tags$hr(style = "border-top: 2px solid #ccc; margin-top: 5px; margin-bottom: 5px;"),
        fluidRow(
          box(
            
            
            title = "Distribution Analysis",
            collapsible = TRUE,
            solidHeader = TRUE,
            status = "primary",
            collapsed = T,
            width = 12,
            plotlyOutput("programDensity") %>% withSpinner(type = 3, color = '#3c8dbc', size = 1.5,color.background ='#ecf0f5' ),
            tags$hr(style = "border-top: 2px solid #ccc; margin-top: 20px; margin-bottom: 20px;"),
            plotlyOutput("programHist") %>% withSpinner(type = 3, color = '#3c8dbc', size = 1.5, color.background ='#ecf0f5' )
          )
        ),
        tags$hr(style = "border-top: 2px solid #ccc; margin-top: 5px; margin-bottom: 5px;"),
        fluidRow(
          box(
            
            tags$div(
              materialSwitch(inputId = "pbclogs", label = "Real scale", inline = TRUE, value = FALSE),
              tags$span("Log scale (base 10)")
            ),
            
            title = "Actual vs Predicted", collapsible = TRUE, solidHeader = TRUE, status = "primary", width = 12, collapsed = T ,
            
            conditionalPanel(
              condition = "input.pbclogs == false", # Note the lowercase 'false' for JavaScript condition
              tags$br(), # This will add a single line space
              #tags$hr(style = "border-top: 2px solid #ccc; margin-top: 20px; margin-bottom: 20px;"),
              plotlyOutput("programPlot1") %>% withSpinner(type = 3, color = '#3c8dbc', size = 1.5,color.background ='#ecf0f5' )
            ),
            conditionalPanel(
              condition = "input.pbclogs == true", # Note the lowercase 'false' for JavaScript condition
              tags$br(), # This will add a single line space
              #tags$hr(style = "border-top: 2px solid #ccc; margin-top: 20px; margin-bottom: 20px;"),
              plotlyOutput("programPlot") %>% withSpinner(type = 3, color = '#3c8dbc', size = 1.5,color.background ='#ecf0f5' )
            )
            
          ),
          # ),
        )
        
      ),
      # PCN Costs Tab
      tabItem(
        tabName = "pcncosts",
        tags$head(
          tags$style(
            HTML(
              "
              #mydropdowncosts {
                list-style-type: none;
              }
              .dropdown-menu {
                background-color: #f8f9fa;
              }
              a.dropdown-toggle {
                display: inline-block;
                padding: 10px 20px;
                background-color: #3c8dbc;
                color: white;
                text-decoration: none;
                border-radius: 5px; 
                transition: background-color 0.3s ease; 
              }

              a.dropdown-toggle:hover {
                background-color: #367fa9;
              }
              "
            )
          )
        ),
        fluidPage(
          dropdownBlock(
            id = "mydropdowncosts",
            title = "Filters",
            icon = icon("sliders-h"),
            selectizeInput(
              inputId = "program_MC",
              label = "Select Program Code:",
              choices = setdiff(unique(Programme_Budgeting_Mappings_Definitions$`Programme Category Name`), "Not coded")
            ),
            selectizeInput(
              inputId = "GP_MC",
              label = "Select GPs to highlight:",
              choices = unique(pcn_lookup$gpp_short_name),
              multiple = TRUE
            ),
            selectizeInput(
              inputId = "excludeGP_MC",
              label = "GPs to exclude:",
              choices = unique(pcn_lookup$gpp_short_name),
              multiple = TRUE
            )
          ),
          
          fluidRow(
            
            column(12, div(class = "uiheading", uiOutput("dynamicTitleProgram_MC")%>% withSpinner(type = 3, color = '#3c8dbc', size = 1.5,color.background ='#ecf0f5' )))
          ),
        ),
      )
    )
  )
)