library(shiny)
library(shinyjs)
library(dplyr)
library(ggplot2)
library(shinydashboard)
library(shinydashboardPlus)
library(shinycssloaders)
library(scales)
library(DT)
library(shinythemes)
library(shinyWidgets)
library(d3r)

source("helpMenu.R", local = T)

ui <- dashboardPage(
  dashboardHeader(disable = T),
  dashboardSidebar(disable = T,
                   
                   sidebarMenu(
                     menuItem("menu", tabName = "MainMenu", icon = icon("bars")),
                     menuItem("Index2", tabName = "Index2", icon = icon("dashboard")),
                     menuItem("help", tabName = "help", icon = icon("circle-info")),
                     menuItem("dynamicTab", tabName = "dynamicTab", icon = icon("dashboard")),
                     menuItem("dynamicTabMaster", tabName = "dynamicTabMaster", icon = icon("dashboard")),
                     id = "sidebarID"
                   )
  ),
  dashboardBody(
    
    useShinyjs(),
    tags$style(HTML("
    
    
    
    
    .irs-with-grid .irs-grid-pol.small {height: 0px;}
    
    #group-by-select, #group-by-select-mort_rii, #group-by-select_rii_1cd10 , #group-by-select-rii, #group-by-select-riiLH, #group-by-select-mort {
    
    background-color: #ffffff; 
    color: #333333;
    border: 1px solid #cccccc;
    border-radius: 4px;
    height: 25px;
    font-size: 12px;
    
    }
    
    #group-by-select-add-mort, #group-by-select-add-mort_nonelective, #group-by-select-add-mort_nonelectiverii {
    background-color: #ffffff; 
    color: #333333;
    border: 1px solid #cccccc;
    border-radius: 4px;
    height: 25px;
    font-size: 12px;
    
    
    
    }
    
    .CrudeRates {
      background-color: #BFEFFF; 
    }
    
    .multi-line-button{
    white-space: normal;
    width:100%;
    border: 2px solid black;
    padding: 5px;
    
    
    }
    
    .domain-header {
    border-bottom: 1px solid black;
    display: block;
    width: 100%;
    margin-bottom: 10px;
    
    
    }
    
    
    .parent-header {font-weight: bold; background-color: #f0f0f0; text-align: center;}
    .child-header {text-algin: center;}
    .data-row {border 1px solid black;}
    .cell, .plot-cell { border-right: 1px solid #ddd; text-align: left; vertical-align: middle; padding: 5px;}
    .cell:last-child, .plot-cell:last-child {border-right: none;}
    .plot-cell {height: 50px; padding: 0; margin: 0; border: none;}
    
     a[data-value='dynamicTab_Mort'] {
            display: none !important;
     }
     
     a[data-value='dynamicTabMaster'] {
            display: none !important;
     }
     
     a[data-value='dynamicTabRII'] {
            display: none !important;
     }
     
     a[data-value='dynamicTabRIILOCALHEALTH'] {
            display: none !important;
     }
     
        
         a[data-value='dynamicTab'] {
            display: none !important;
        }
    
      .custom-close {
        cursor: pointer;
        position: absolute;
        color: white;
        top: 10px;
        right: 10px;
        z-index: 100;
      .custom-close:hover {
        color: darkred;
      }
      
      li.dropdown-header.optgroup-1 > span.text,
      li.dropdown-header.optgroup-2 > span.text  {
        font-size: 20px;
        font-weight: bold;
      }
      
    ")),
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
      
      tabItem(tabName = "MainMenu", 
              
              # fluidRow(
              #   box(title = "Welcome", status = "maroon", solidHeader = T, width = 12
              #       ,tags$h1("Welcome to the Health Inequalities Explorer"),
              #       tags$p("Thank you for using the Health Inequalities Explorer, a tool designed to provide insights into disparities in health outcomes.
              #              The app focuses on the Relative Index of Inequality (RII), offering users the ability to select a health metric of intrest and explore detialied breakdowns, including yearly trends and directly standardised rates. \n
              #              Begin by selecting a metric to see how inequalities have evolved over time.")
              #       )
              # ),
              fluidRow(
                box(title = div(style = "display: flex; padding-right: 200px",
                                span("Health Inequalities Dashboard"),
                                div(style = "margin-left: 100px",
                                materialSwitch(inputId = "rii_sii_icd10_switch2Master",
                                  label = "SII", inline = TRUE, value = T),
                                  span("RII"))), status = "maroon",
                    solidHeader = T, width = 12, 
                 
                  
                  
                  
                  
                
                    
                    uiOutput("masterdynamic_datatables_ui2") %>%
                      #dataTableOutput("dynamicContentRII_ICD10_2_Mort") %>% 
                      withSpinner(type = 3, color = '#3c8dbc', size = 1.5
                                  ,color.background ='#ecf0f5' )

                )
              )
              
      ),
      
      
      tabItem(tabName = "dynamicTabRII", actionButton("backRII", "Return",
           icon = icon("arrow-left"),
           style="color: white; background-color: #3c8dbc; border-color: #2e6da4"),
           uiOutput("dynamicContentRII2")
),



tabItem(tabName = "dynamicTab", actionButton("back", "Return",
                                             icon = icon("arrow-left"),
                                             style="color: white; background-color: #3c8dbc; border-color: #2e6da4; margin: 20px"),
        uiOutput("dynamicContent")
),


tabItem(tabName = "dynamicTabMaster", actionButton("backMaster", "Return",
                                             icon = icon("arrow-left"),
                                             style="color: white; background-color: #3c8dbc; border-color: #2e6da4; margin: 20px"),
        uiOutput("dynamicContentMaster")
),



      



tabItem(
  tabName = "RII_LOCALHEALTH",
  tags$head(
    tags$script(HTML("
          
          
                           $(document).on('click', '#DataTables_Table_0_wrapper, tbody tr', function(){
                           var rowIndex = $(this).index() +1;
                           Shiny.setInputValue('clickedRow', 'button_', + rowIndex);
                           
                           })
                           
                           ")),
    tags$style(
      HTML(

        " 
              .dataTables_wrapper . dataTables {
              font-size: 0.9em;
              
              }
              
              .dataTables_wrapper . dataTables  td{
              padding: 8px;
              
              }
              
              .dataTables_scrollHead .dataTables_scrollHeadInner thead {
              
              position: sticky;
              top: 0;
              z-index: 10;
              }
              
              table.dataTable thead .sorting,
              table.dataTable thead. sorting_asc,
              table.dataTable thead .sorting_desc{
              
              position: sticky;
              top: 0;
              z-index: 10;
              }
              
              body {
              transform: scale (0.8);
              transform-origin: top left;
              
              }
              
              .dataTable td {
              
              }
              
              .dataTable tr {
                  height: 30px;
              }
              
              
              table.dataTable.display tbody tr.odd {
                  height: 30px;
              }

              .sticky-header {

              }
              
              
              th { text-align: center;}
              .group-header {font-weight: bold;}
              
              
              
              .border-column {
                border-left: 1px solid black;
                border-right: 1px solid black;
                height: 100%
              }
              
              
              .column_w_bar {
                border-right-color: #eb4034;
                  border-right-width: 1px;
                border-right-style: solid;
              }
              
             
              
              .bordered-row {
                border: 1px solid #111d;
                margin-bottom: 10px;
              
              }
              
              .bordered-column {
              border: 1px soild #111;
              
              }
              
              .flex-container {
              display: flex; 
              flex-direction: column;
              height: 900px
              
              }
              
              .flex-grow {
              flex-grow: 1;
              }
              
              .bottom-button {
              
              margin-top: auto;
              }
              
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
      )
      
      
      
      
    )
  ),
  
  fluidPage(
    tags$head(tags$meta(name="viewport", content = "width=device-width, initial-scale =0.8")),
    fluidRow(class = "sticky-header",
             
             tagList(d3_dep_v4()),
             #uiOutput("dynamic_dropdown_ui"),
             dataTableOutput("dynamicContentRIILOCALHEALTH") %>% 
               withSpinner(type = 3, color = '#3c8dbc', size = 1.5
                           ,color.background ='#ecf0f5' ))
    
    
  )
),



tabItem(
  tabName = "ICD10_2",
  tags$head(
    tags$script(HTML("

                           $(document).on('click', '#DataTables_Table_0_wrapper, tbody tr', function(){
                           var rowIndex = $(this).index() +1;
                           Shiny.setInputValue('clickedRow', 'button_', + rowIndex);
                           
                           })
                           
                           ")),
    tags$style(
      HTML(

        " 
              .dataTables_wrapper . dataTables {
              font-size: 0.9em;
              
              }
              
              .dataTables_wrapper . dataTables  td{
              padding: 8px;
              
              }
              
              .dataTables_scrollHead .dataTables_scrollHeadInner thead {
              
              position: sticky;
              top: 0;
              z-index: 10;
              }
              
              table.dataTable thead .sorting,
              table.dataTable thead. sorting_asc,
              table.dataTable thead .sorting_desc{
              
              position: sticky;
              top: 0;
              z-index: 10;
              }
              
              body {
              transform: scale (0.8);
              transform-origin: top left;
              
              }
              
              .dataTable td {
              
              }
              
              
              
              .sticky-header {
            
              
              
              
              
              }
              
              
              th { text-align: center;}
              .group-header {font-weight: bold;}
              
              
              
              .border-column {
                border-left: 1px solid black;
                border-right: 1px solid black;
                height: 100%
              }
              
              
              .column_w_bar {
                border-right-color: #eb4034;
                  border-right-width: 1px;
                border-right-style: solid;
              }
              
             
              
              .bordered-row {
                border: 1px solid #111d;
                margin-bottom: 10px;
              
              }
              
              .bordered-column {
              border: 1px soild #111;
              
              }
              
              .flex-container {
              display: flex; 
              flex-direction: column;
              height: 900px
              
              }
              
              .flex-grow {
              flex-grow: 1;
              }
              
              .bottom-button {
              
              margin-top: auto;
              }
              
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
      )
    )
  ),
  
  fluidPage(
    tags$head(tags$meta(name="viewport2", content = "width=device-width, initial-scale =0.8")),
    fluidRow(class = "sticky-header",
             
             #tagList(d3_dep_v4()),
             uiOutput("dynamic_datatables_ui") %>%
             # dataTableOutput("dynamicContentRII_ICD10_2") %>%
               withSpinner(type = 3, color = '#3c8dbc', size = 1.5
                           ,color.background ='#ecf0f5' ))

  )
),





tabItem(
  tabName = "ICD10_2_Mort",
  tags$head(
    tags$script(HTML("

                           $(document).on('click', '#DataTables_Table_0_wrapper, tbody tr', function(){
                           var rowIndex = $(this).index() +1;
                           Shiny.setInputValue('clickedRow', 'button_', + rowIndex);
                           
                           })
                           
                           ")),
    tags$style(
      HTML(

        
        " 
              .dataTables_wrapper . dataTables {
              font-size: 0.9em;
              
              }
              
              .dataTables_wrapper . dataTables  td{
              padding: 8px;
              
              }
              
              .dataTables_scrollHead .dataTables_scrollHeadInner thead {
              
              position: sticky;
              top: 0;
              z-index: 10;
              }
              
              table.dataTable thead .sorting,
              table.dataTable thead. sorting_asc,
              table.dataTable thead .sorting_desc{
              
              position: sticky;
              top: 0;
              z-index: 10;
              }
              
              body {
              transform: scale (0.8);
              transform-origin: top left;
              
              }
              
              .dataTable td {
              
              }

              
              .sticky-header {

              
              }
              
              
              th { text-align: center;}
              .group-header {font-weight: bold;}
              
              
              
              .border-column {
                border-left: 1px solid black;
                border-right: 1px solid black;
                height: 100%
              }
              
              
              .column_w_bar {
                border-right-color: #eb4034;
                  border-right-width: 1px;
                border-right-style: solid;
              }
              
             
              
              .bordered-row {
                border: 1px solid #111d;
                margin-bottom: 10px;
              
              }
              
              .bordered-column {
              border: 1px soild #111;
              
              }
              
              .flex-container {
              display: flex; 
              flex-direction: column;
              height: 900px
              
              }
              
              .flex-grow {
              flex-grow: 1;
              }
              
              .bottom-button {
              
              margin-top: auto;
              }
              
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
      )
    )
  ),
  
  fluidPage(
    tags$head(tags$meta(name="viewport2", content = "width=device-width, initial-scale =0.8")),
    fluidRow(class = "sticky-header",
             
             #tagList(d3_dep_v4()),
             uiOutput("dynamic_datatables_ui_mort5") %>%
             #dataTableOutput("dynamicContentRII_ICD10_2_Mort") %>% 
               withSpinner(type = 3, color = '#3c8dbc', size = 1.5
                           ,color.background ='#ecf0f5' ))
    
    
  )
),

helpPage
,

tabItem(
  tabName = "Index2",

  tags$head(
    tags$script(HTML("

                           $(document).on('click', '#DataTables_Table_0_wrapper, tbody tr', function(){
                           var rowIndex = $(this).index() +1;
                           Shiny.setInputValue('clickedRow', 'button_', + rowIndex);
                           
                           })
                           
                           ")),
    tags$style(
      HTML(

        " 
              .dataTables_wrapper . dataTables {
              font-size: 0.9em;
              
              }
              
              .dataTables_wrapper . dataTables  td{
              padding: 8px;
              
              }
              
              .dataTables_scrollHead .dataTables_scrollHeadInner thead {
              
              position: sticky;
              top: 0;
              z-index: 10;
              }
              
              table.dataTable thead .sorting,
              table.dataTable thead. sorting_asc,
              table.dataTable thead .sorting_desc{
              
              position: sticky;
              top: 0;
              z-index: 10;
              }
              
              body {
              transform: scale (0.8);
              transform-origin: top left;
              
              }
              
              .dataTable td {
              
              }

              .sticky-header {

              
              }
              
              
              th { text-align: center;}
              .group-header {font-weight: bold;}
              
              
              
              .border-column {
                border-left: 1px solid black;
                border-right: 1px solid black;
                height: 100%
              }
              
              
              .column_w_bar {
                border-right-color: #eb4034;
                  border-right-width: 1px;
                border-right-style: solid;
              }
              
             
              
              .bordered-row {
                border: 1px solid #111d;
                margin-bottom: 10px;
              
              }
              
              .bordered-column {
              border: 1px soild #111;
              
              }
              
              .flex-container {
              display: flex; 
              flex-direction: column;
              height: 900px
              
              }
              
              .flex-grow {
              flex-grow: 1;
              }
              
              .bottom-button {
              
              margin-top: auto;
              }
              
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
        
        
        
        .direct-chat-contacts {
        z-index: 2 !important;
        position: absolute !important;
        }

        
        #dynamicContentRII_ICD10_2Index  #dynamicContentRII_ICD10_2riiIndex{
        z-index: 1 !important;
         
        
        }   
        
        #rii_sii_icd10_switch2 {
        background: black;
        }
        
        
        .svg-container svg {
          width: 100%;
          height: 80%;
        }
        

         .box.box-solid.box-primary>.box-header {

         background:#BB2882

                }

                .box.box-solid.box-primary{

                
                }



              "
      )
    )
  ),
  
  fluidPage(
    tags$head(tags$meta(name="viewport22", content = "width=device-width, initial-scale =0.8")),
    
      box(
        title = div(style = "display: flex; padding-right: 200px",
                    span(""),tags$div(style = "margin-left: 10px;",
                                      materialSwitch(inputId = "rii_sii_icd10_switch2", label = "SII", inline = TRUE, value = T),
                                      tags$span("RII"))#,
                    # checkboxInput(
                    #   "variableOptions", 
                    #   "Display all Sub ICD10 Chapters and Codes", TRUE)
        ),
                    
        closable = FALSE, 
        width = 12,
        status = "maroon", 
        solidHeader = TRUE, 
        collapsible = FALSE,
        height = '100%',
      
      
      # sidebar = boxSidebar(
      #   width = 25,
      #   startOpen = FALSE,
      #   id = "mycardsidebar2",
      #   background =  '#33adff',
      #   tags$div(style = "padding: 25px;",
      #            materialSwitch(inputId = "rii_sii_icd10_switch2", label = "SII", inline = TRUE, value = T),
      #            tags$span("RII")
      #   ),
      #   checkboxInput(
      #     "variableOptions", 
      #     "Display all Sub ICD10 Chapters and Codes", TRUE)
      #   ),
      
        uiOutput("dynamic_datatables_ui2") %>%
          withSpinner(type = 3, color = '#3c8dbc', size = 1.5
                    ,color.background ='#ecf0f5' )
      
          )
        )
      )
    )
  )  
)
