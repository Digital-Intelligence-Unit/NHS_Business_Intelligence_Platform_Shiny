library(shiny)
library(stringr)
library(DT)
library(readxl)
library(dplyr)
library(ggplot2)
library(shinyWidgets)
library(plotly)
library(shinyjs)

plot_w <- data.frame(1:1000)
colnames(plot_w)[1] <- "thousands"

first.word <- function(my.string){
  unlist(strsplit(my.string, ":"))[1]
}

HEER <- read_excel("./Health_Economics_Evidence_Resource_HEER_July19.xlsm", 
                   sheet = "Evidence")

HEER$`Benefit-cost ratio (BCR)` <- gsub("\\ to ", "-", HEER$`Benefit-cost ratio (BCR)`)
HEER$ICER <- gsub("\\ to ", "-", HEER$ICER)

regexAmount <- "[0-9\\.]+"
## Table Clean BCR values

HEER <- HEER  %>% mutate(Ratios2 =strsplit(`Benefit-cost ratio (BCR)`, "(?=[A-Za-z])(?<=[0-9])|(?=[0-9])(?<=[A-Za-z])", perl=TRUE))
HEER <- HEER %>% mutate(Ratios2 = stringr::str_extract(Ratios2, "^[^:]*"))

HEER$Ratios2 <- gsub("[^0-9.-]", "", HEER$Ratios2)

library(stringr)
HEER$Ratios3 <-str_split_fixed(HEER$Ratios2, fixed("-"), 2)[, 2]
HEER <- HEER  %>% mutate(Ratios2 =stringr::str_extract(Ratios2, regexAmount))
HEER <- HEER  %>% mutate(Ratios2 = ifelse(is.na(Ratios2),0,Ratios2))

HEER$Ratios <- ifelse(HEER$Ratios3 != "", (as.numeric(HEER$Ratios2) + as.numeric(HEER$Ratios3))/2 , HEER$Ratios2)

HEER <- HEER  %>% mutate(ICER2 =strsplit(`ICER`, "(?=[A-Za-z])(?<=[0-9])|(?=[0-9])(?<=[A-Za-z])", perl=TRUE))
HEER <- HEER %>% mutate(ICER2 = stringr::str_extract(ICER2, "^[^:]*"))

HEER$ICER2 <- gsub("[^0-9.-]", "", HEER$ICER2)

library(stringr)
HEER$ICER3 <-str_split_fixed(HEER$ICER2, fixed("-"), 2)[, 2]
HEER <- HEER  %>% mutate(ICER2 =stringr::str_extract(ICER2, regexAmount))
HEER <- HEER  %>% mutate(ICER2 = ifelse(is.na(ICER2),0,ICER2))

HEER$ICER_VALUES <- ifelse(HEER$ICER3 != "", (as.numeric(HEER$ICER2) + as.numeric(HEER$ICER3))/2 , HEER$ICER2)

ui <- fluidPage(
  
  tabsetPanel(id = "inTabset",
              tabPanel("Explore", fluid = TRUE,
                       tags$style(HTML('table.dataTable tr.active td {background-color: pink !important;}')),
                       
                       sidebarLayout(
                         sidebarPanel(

                           pickerInput("Theme","select theme", choices=unique(HEER$Theme), options = list(`actions-box` = TRUE),multiple = T),

                           helpText("Use radio buttons to select columns of intrest.
               \n buttons will filter out where target column is not blank."),
                           
                           prettyCheckbox(
                             inputId = "icer", label = "ICER",
                             shape = "round", outline = TRUE, status = "info"

                           ),
                           prettyCheckbox(
                             inputId = "bcr", label = "Benefit Cost-Ratio",
                             shape = "round", outline = TRUE, status = "info"
                           ),

                           #### ICER
                           
                           prettyCheckbox(
                             inputId = "ICER_Filter_YN", label = "Use ICER filter slider Y/N",
                             shape = "round", outline = TRUE, status = "info"),

                           sliderInput(
                             inputId = "ICER_FILTER",
                             label = "Filter by ICER in £1,000s",
                             value = c(0.1,2),
                             min = 0,
                             max = 2000,
                             step = 0.1,
                             pre ="£"
                           ),
                           
                           verbatimTextOutput("ICER_MULTI"),
                           
                           prettyCheckbox(
                             inputId = "Ratio_Filter_YN", label = "Use Ratio filter slider Y/N",
                             shape = "round", outline = TRUE, status = "info"),
      
                           sliderInput(
                             inputId = "Ratio_FILTER",
                             label = "Filter by Ratio",
                             value = c(5,10),
                             min = 0,
                             max = 250,
                             step = 0.1
                           ),

                         ),
                         mainPanel(
                           DTOutput('myDT'),
                           br(),
                           uiOutput("submit"),
                           br(),
                           br(),
                           fluidRow(
                             column(12,
                                    fluidRow(
                                      column(width = 6, 
                                             DTOutput('myresults')),
                                      column(width = 3,
                                             plotlyOutput("distPlot")),
                                      column(width = 3,
                                             plotlyOutput("distPlot2"))#)
                                                                          )))
                         )
                       ))
              ###### REPORT PAGE
              ,
              tabPanel("Report", value = "report", fluid = TRUE,
                       
                       tags$head(tags$style("#text1,#text2,#text3,#text4{color: white;
                                 font-size: 20px;
                                 font-style: italic;
                                 }
                                strong{color: white;
                                font-size: 20px;
                                font-style: italic;
                                }
                                
                                .shiny-text-output shiny-bound-output,well{
                                height:3%;
                                margin:0;
  padding-top:8px;
                                
                                }
                                
                                
                                "
                       )
                       ),
                       
                         mainPanel(div(style='width:100%',
                                       id = "mainOrder_CA",fluidRow(
                                         fluidRow(
                                           column(12,
                                                  
                                                  
                                                  wellPanel(
                                                    style = "background-color: #800020;",
                                                    column(12, offset = 8,
                                                           textOutput("text4")   
                                                    ),
                                                    textOutput("text1"),
                                                    textOutput("text2"),
                                                    textOutput("text3")),
                                                  
                                                  fluidRow(
                                                    column(12,
                                                           wellPanel(
                                                             style = "border-color: black;
                                  border-width: thin medium;
                                   margin-top:-2em;
                                   height:5%"
                                                             
                                                             ,
                                                             
                                                             fluidRow(
                                                               column(width = 7, 
                                                                      textOutput("text5")),
                                                               column(width = 5,
                                                                      textOutput("text6")))),
                                                           
                                                           fluidRow(
                                                             column(12,wellPanel(
                                                               style = "border-color: black;
                                  border-width: thin medium;
                                     margin-top:-2em;
                                     
                                   ",
                              
                                                               fluidRow(
                                                                 column(width = 3,
                                                                        wellPanel(
                                                                          style = "background-color: white;
                                            border: -5%;    
                                   padding: -5%;
                                   height:100%
                                                ",
                                                                          
                                                                          
                                                                          textOutput("text7"))),

                       
                                                                 column( width = 3,
                                                                         wellPanel(
                                                                           style = "background-color: white;
                                                border: -5%;
                                   padding: -5%;
                                   height:100%
                                                ",
                                                                           
                                                                           textOutput("text9"))),

                                                                 column(width = 3,
                                                                        wellPanel(
                                                                          
                                                                          style = "background-color: white;
                                            border: -5%;  
                                   padding: -5%;
                                   height:100%
                                              ",
                                                                          
                                                                          textOutput("text8"))),
                                                                 column(width = 3, 
                                                                        wellPanel(
                                                                          style = "background-color: white;
                                              border: -5%;
                                   padding: -5%;
                                   height:100%
                                              ",
                                                                          
                                                                          textOutput("text10"))))))),
                                                           

                                                           fluidRow(
                                                             column(12,
                                                                    wellPanel(
                                                                      style = "background-color: #800020;
                                            margin-top:-2em",
                                                                      strong("Summary of evidence"),
                                                                      fluidRow(column(width = 11,
                                                                                      wellPanel(
                                                                                        style = "background-color: white;
                                                              ",
                                                                                        
                                                                                        
                                                                                        textOutput("text11"))))))),
                                                           
                                                           fluidRow(
                                                             column(12,wellPanel(
                                                               style = "border-color: black;
                                  border-width: thin medium;
                                     margin-top:-2em",
                                                               
                                                               
                                                               fluidRow(
                                                                 column(width = 3,
                                                                        wellPanel(
                                                                          style = "background-color: white;
                                                ",
                                                                          
                                                                          
                                                                          textOutput("text12"))),
                                                                 
                                                                 column( width = 3,
                                                                         wellPanel(
                                                                           style = "background-color: white;
                                                 ",
                                                                           
                                                                           textOutput("text13"))),

                                                                 column(width = 3,
                                                                        wellPanel(
                                                                          
                                                                          style = "background-color: white;
                                                ",
                                                                          
                                                                          textOutput("text14"))),
                                                                 column(width = 3, 
                                                                        wellPanel(
                                                                          style = "background-color: white;
                                                ",
                                                                          
                                                                          textOutput("text15")))))))
                                                           
                                                           ,
                                                           
                                                           fluidRow(
                                                             column(12,
                                                                    wellPanel(
                                                                      style = "border-color: black;
                                  border-width: thin medium;
                                            margin-top:-2em",
                                                                      
                                                                      fluidRow(
                                                                        column(width = 7, 
                                                                               textOutput("text16")),
                                                                        column(width = 5,
                                                                               textOutput("text17"))))))

                                                           ,

                                                           fluidRow(
                                                             column(12,
                                                                    wellPanel(
                                                                      style = "background-color: #800020;
                                            margin-top:-2em",
                                                                      strong("Assumptions / Caveats"),
                                                                      fluidRow(column(width = 11,
                                                                                      wellPanel(
                                                                                        style = "background-color: white;
                                                              ",
                                                                                        
                                                                                        
                                                                                        textOutput("text18")))))))
                                                           
                                                           ,
                                                           
                                                           fluidRow(
                                                             column(12,wellPanel(
                                                               style = "border-color: black;
                                  border-width: thin medium;
                                     margin-top:-2em",
                                                               
                                                               
                                                               fluidRow(
                                                                 column(width = 3,
                                                                        wellPanel(
                                                                          style = "background-color: white;
                                                ",
                                                                          
                                                                          
                                                                          textOutput("text19"))),
                                                                 
                                                            
                                                                 column( width = 3,
                                                                         wellPanel(
                                                                           style = "background-color: white;
                                                 ",
                                                                           
                                                                           textOutput("text20"))),

                                                                 column(width = 3,
                                                                        wellPanel(
                                                                          
                                                                          style = "background-color: white;
                                                ",
                                                                          
                                                                          textOutput("text21"))),
                                                                 column(width = 3, 
                                                                        wellPanel(
                                                                          style = "background-color: white;
                                                ",
                                                                          
                                                                          textOutput("text22")))))))
                                                           
                                                           ,
                                                           
                                                           fluidRow(
                                                             column(12,
                                                                    wellPanel(
                                                                      style = "border-color: black;
                                  border-width: thin medium;
                                            margin-top:-2em",
                                                                      
                                                                      fluidRow(
                                                                        column(width = 7, 
                                                                               textOutput("text23")),
                                                                        column(width = 5,
                                                                               textOutput("text24"))))))
                                                           
                                                           ,
                                                           
                                                           fluidRow(
                                                             column(12,wellPanel(
                                                               style = "border-color: black;
                                  border-width: thin medium;
                                     margin-top:-2em",
                                                               
                                                               
                                                               fluidRow(
                                                                 column(width = 2,
                                                                        wellPanel(
                                                                          style = "background-color: white;
                                                ",
                                                                          
                                                                          
                                                                          textOutput("text25"))),
                                                                 
                                                      
                                                                 column( width = 2,
                                                                         wellPanel(
                                                                           style = "background-color: white;
                                                 ",
                                                                           
                                                                           textOutput("text26"))),

                                                                 column(width = 2,
                                                                        wellPanel(
                                                                          style = "background-color: white;
                                                ",
                                                                          
                                                                          
                                                                          textOutput("text27"))),
                                                                 
                                                                 
                                                              
                                                                 column( width = 2,
                                                                         wellPanel(
                                                                           style = "background-color: white;
                                                 ",
                                                                           
                                                                           textOutput("text28"))),
                                                                 
                                                                 
                                                                 
                                                                 column(width = 2,
                                                                        wellPanel(
                                                                          
                                                                          style = "background-color: white;
                                                ",
                                                                          
                                                                          textOutput("text29"))),
                                                                 column(width = 2, 
                                                                        wellPanel(
                                                                          style = "background-color: white;
                                                ",
                                                                          
                                                                          textOutput("text30")))))))
                                                           
                                                           ,
                                                           
                                                           
                                                           
                                                           fluidRow(
                                                             column(12,
                                                                    wellPanel(
                                                                      style = "background-color: #800020;",
                                                                      strong("Reference to original source"),
                                                                      fluidRow(column(width = 11,
                                                                                      wellPanel(
                                                                                        style = "background-color: white;",
                                                                                        
                                                                                        
                                                                                        textOutput("text31")))))))
                                                           
                                                           ,
                                                           
                                                           textInput("InputId2", "", value = "A1", width = "0px", placeholder = NULL)
                                                           
                                                           
                                                    ))
                                           )
                                         )
                                       )
                         )
                         )
                       
              ),
              
              tabPanel("Glossary", fluid = TRUE,
                       
                       titlePanel("Glossary"),
                       tags$style(HTML('table.dataTable tr.active td {background-color: pink !important;}')),

                       mainPanel(
                         
                         fluidRow(
                           column(12,
                                  wellPanel(
                                    style = "border-color: black;
                                  border-width: thin medium;
                                
                                   "
                                    
                                    ,
                                    
                                    fluidRow(style = "margin:2%;",
                                             column(width = 7, 
                                                    strong("Comparator",style = "color: BLACK;" )),
                                             column(width = 5,
                                                    textOutput("textG1")))
                                    
                                    ,
                                    
                                    fluidRow(style = "margin:2%;",
                                             column(width = 7, 
                                                    strong("Cost-benefit analysis (CBA)",style = "color: BLACK;" )),
                                             column(width = 5,
                                                    textOutput("textG2")))
                                    
                                    ,
                                    
                                    fluidRow(style = "margin:2%;",
                                             column(width = 7, 
                                                    strong("Cost-effectiveness",style = "color: BLACK;" )),
                                             column(width = 5,
                                                    textOutput("textG3")))
                                    
                                    ,
                                    
                                    fluidRow(style = "margin:2%;",
                                             column(width = 7, 
                                                    strong("Cost-effectiveness analysis (CEA)",style = "color: BLACK;" )),
                                             column(width = 5,
                                                    textOutput("textG4")))
                                    
                                    ,
                                    
                                    fluidRow(style = "margin:2%;",
                                             column(width = 7, 
                                                    strong("Cost-utility analysis (CUA)",style = "color: BLACK;" )),
                                             column(width = 5,
                                                    textOutput("textG5")))
                                    
                                    ,
                                    
                                    fluidRow(style = "margin:2%;",
                                             column(width = 7, 
                                                    strong("Cost saving",style = "color: BLACK;" )),
                                             column(width = 5,
                                                    textOutput("textG6")))
                                    
                                    
                                    
                                    
                                    ,
                                    
                                    fluidRow(style = "margin:2%;",
                                             column(width = 7, 
                                                    strong("Disability adjusted life year (DALY)",style = "color: BLACK;" )),
                                             column(width = 5,
                                                    textOutput("textG7")))
                                    
                                    ,
                                    
                                    fluidRow(style = "margin:2%;",
                                             column(width = 7, 
                                                    strong("Discount rate",style = "color: BLACK;" )),
                                             column(width = 5,
                                                    textOutput("textG8")))
                                    ,
                                    
                                    fluidRow(style = "margin:2%;",
                                             column(width = 7, 
                                                    strong("Dominance",style = "color: BLACK;" )),
                                             column(width = 5,
                                                    textOutput("textG9")))
                                    
                                    ,
                                    
                                    fluidRow(style = "margin:2%;",
                                             column(width = 7, 
                                                    strong("Equity",style = "color: BLACK;" )),
                                             column(width = 5,
                                                    textOutput("textG10")))
                                    
                                    ,
                                    
                                    fluidRow(style = "margin:2%;",
                                             column(width = 7, 
                                                    strong("Incremental analysis",style = "color: BLACK;" )),
                                             column(width = 5,
                                                    textOutput("textG11")))
                                    ,
                                    
                                    fluidRow(style = "margin:2%;",
                                             column(width = 7, 
                                                    strong("Incremental cost-effectiveness ratio (ICER)",style = "color: BLACK;" )),
                                             column(width = 5,
                                                    textOutput("textG12")))
                                    
                                    ,
                                    
                                    fluidRow(style = "margin:2%;",
                                             column(width = 7, 
                                                    strong("Net present value (NPV)",style = "color: BLACK;" )),
                                             column(width = 5,
                                                    textOutput("textG13")))
                                    
                                    
                                    ,
                                    
                                    fluidRow(style = "margin:2%;",
                                             column(width = 7, 
                                                    strong("Productivity gains",style = "color: BLACK;" )),
                                             column(width = 5,
                                                    textOutput("textG14")))
                                    
                                    ,
                                    
                                    fluidRow(style = "margin:2%;",
                                             column(width = 7, 
                                                    strong("Quality adjusted life year (QALY)",style = "color: BLACK;" )),
                                             column(width = 5,
                                                    textOutput("textG15")))
                                    
                                    ,
                                    
                                    fluidRow(style = "margin:2%;",
                                             column(width = 7, 
                                                    strong("Quasi-experiment",style = "color: BLACK;" )),
                                             column(width = 5,
                                                    textOutput("textG16")))
                                    
                                    ,
                                    
                                    fluidRow(style = "margin:2%;",
                                             column(width = 7, 
                                                    strong("Randomised controlled trial (RCT)",style = "color: BLACK;" )),
                                             column(width = 5,
                                                    textOutput("textG17")))
                                    
                                    
                                    ,
                                    
                                    fluidRow(style = "margin:2%;",
                                             column(width = 7, 
                                                    strong("Return on investment (ROI)",style = "color: BLACK;" )),
                                             column(width = 5,
                                                    textOutput("textG18")))
                                    
                                    
                                    ,
                                    
                                    fluidRow(style = "margin:2%;",
                                             column(width = 7, 
                                                    strong("Sensitivity analysis",style = "color: BLACK;" )),
                                             column(width = 5,
                                                    textOutput("textG19")))
                                    
                                    
                                    ,
                                    
                                    fluidRow(style = "margin:2%;",
                                             column(width = 7, 
                                                    strong("System model analysis",style = "color: BLACK;" )),
                                             column(width = 5,
                                                    textOutput("textG20")))
                                    
                                
                                    ,
                                    
                                    fluidRow(style = "margin:2%;",
                                             column(width = 7, 
                                                    strong("Willingness to pay",style = "color: BLACK;" )),
                                             column(width = 5,
                                                    textOutput("textG21")))
                                    
                                  )))
                         
                       ))
              
              
  )
)
