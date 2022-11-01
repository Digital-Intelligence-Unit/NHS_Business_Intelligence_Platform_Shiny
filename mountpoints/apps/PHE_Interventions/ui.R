library(shiny)

# Define UI ----

## ui.R ##

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


#HEER %>% select(`Benefit-cost ratio (BCR)`) %>% mutate(Ratios = ifelse( ':' %in% `Benefit-cost ratio (BCR)` ,sapply(`Benefit-cost ratio (BCR)`, first.word),`Benefit-cost ratio (BCR)`))
regexAmount <- "[0-9\\.]+"
## Table Clean BCR values

HEER <- HEER  %>% mutate(Ratios2 =strsplit(`Benefit-cost ratio (BCR)`, "(?=[A-Za-z])(?<=[0-9])|(?=[0-9])(?<=[A-Za-z])", perl=TRUE))
HEER <- HEER %>% mutate(Ratios2 = stringr::str_extract(Ratios2, "^[^:]*"))
#HEER <- HEER  %>% mutate(Ratios2 =stringr::str_extract(Ratios2, regexAmount))
HEER$Ratios2 <- gsub("[^0-9.-]", "", HEER$Ratios2)
#HEER <- HEER  %>% mutate(Ratios2 = ifelse(is.na(Ratios2),0,Ratios2))
library(stringr)
HEER$Ratios3 <-str_split_fixed(HEER$Ratios2, fixed("-"), 2)[, 2]
HEER <- HEER  %>% mutate(Ratios2 =stringr::str_extract(Ratios2, regexAmount))
HEER <- HEER  %>% mutate(Ratios2 = ifelse(is.na(Ratios2),0,Ratios2))

HEER$Ratios <- ifelse(HEER$Ratios3 != "", (as.numeric(HEER$Ratios2) + as.numeric(HEER$Ratios3))/2 , HEER$Ratios2)

#HEER$Ratios3 <- as.numeric(unlist(strsplit(HEER$Ratios2, "-")))[2]# + as.numeric(unlist(strsplit(HEER$Ratios2, "-")))[2]) / 2, HEER$Ratios2)
#mat  <- matrix(unlist(strsplit("1.46-2.01", "-")), ncol=2, byrow=FALSE)
#df   <- as.data.frame(mat)



HEER <- HEER  %>% mutate(ICER2 =strsplit(`ICER`, "(?=[A-Za-z])(?<=[0-9])|(?=[0-9])(?<=[A-Za-z])", perl=TRUE))
HEER <- HEER %>% mutate(ICER2 = stringr::str_extract(ICER2, "^[^:]*"))
#HEER <- HEER  %>% mutate(ICER2 =stringr::str_extract(ICER2, regexAmount))
HEER$ICER2 <- gsub("[^0-9.-]", "", HEER$ICER2)
#HEER <- HEER  %>% mutate(ICER2 = ifelse(is.na(ICER2),0,ICER2))
library(stringr)
HEER$ICER3 <-str_split_fixed(HEER$ICER2, fixed("-"), 2)[, 2]
HEER <- HEER  %>% mutate(ICER2 =stringr::str_extract(ICER2, regexAmount))
HEER <- HEER  %>% mutate(ICER2 = ifelse(is.na(ICER2),0,ICER2))

HEER$ICER_VALUES <- ifelse(HEER$ICER3 != "", (as.numeric(HEER$ICER2) + as.numeric(HEER$ICER3))/2 , HEER$ICER2)





# HEER <- HEER %>% mutate(ICER_VALUES = gsub(",", "", gsub("([a-zA-Z]),", "\\1 ", ICER)))
# HEER <- HEER %>% mutate(ICER_VALUES = stringr::str_extract(ICER_VALUES, "^[^:]*"))
# HEER <- HEER  %>% mutate(ICER_VALUES = stringr::str_extract(ICER_VALUES, "^[^-]*"))
# HEER <- HEER  %>% mutate(ICER_VALUES = stringr::str_extract(ICER_VALUES,  "[^ROI ]+$"))
# HEER <- HEER  %>% mutate(ICER_VALUES =stringr::str_extract(ICER_VALUES, regexAmount) %>% unlist %>% as.numeric)
# HEER <- HEER  %>% mutate(ICER_VALUES = ifelse(is.na(ICER_VALUES),0,ICER_VALUES))

# data_rows <- function(mtcars) {
#   
#   mymtcars <- mtcars
#   mymtcars[["Select"]] <- paste0('<input type="checkbox" name="row_selected" value=',1:nrow(mymtcars),' unchecked>')
#   #mymtcars[["Select"]] <- paste0('<input type="checkbox" name="row_selected" value=',(mymtcars$ID),' unchecked>')
#   mymtcars[["_id"]] <- paste0("row_", seq(nrow(mymtcars)))
#   return(mymtcars)
# }

#HEER <- data_rows(HEER)
# js <- "
# $(document).ready(function(){
#   $('#printPdf_CA').click(function () {
#     domtoimage.toPng(document.getElementById('mainOrder_CA'))
#       .then(function (blob) {
#         var pdf = new jsPDF('l', 'pt', [$('#mainOrder_CA').width(), $('#mainOrder_CA').height()]);
#         pdf.addImage(blob, 'PNG', (window.innerWidth*(1/6)), 0, 1000, window.innerHeight);
#         pdf.save('Report.pdf');
#         // that.options.api.optionsChanged(); what is that?
#       });
#   });
# });
# "
ui <- fluidPage(
  
 tabsetPanel(id = "inTabset",
               tabPanel("Explore", fluid = TRUE,
                        
                        titlePanel("PHE"),
                        tags$style(HTML('table.dataTable tr.active td {background-color: pink !important;}')),
                        
                        sidebarLayout(
                          sidebarPanel(
                            #helpText("Use filters to work out intervention."),
                            
                            #sliderInput("investment", "Amount invested in £1000s:",
                            #             min = 0, max = 1000,
                            #             value = 0, step = 10,
                            #             pre ="£"),
                            
                            pickerInput("Theme","select theme", choices=unique(HEER$Theme), options = list(`actions-box` = TRUE),multiple = T),
                            
                            
                            
                            
                            pickerInput("type","select type of analysis", choices=unique(HEER$`Type of analysis`), options = list(`actions-box` = TRUE),multiple = T),
                            
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
                            
                            # prettyCheckbox(
                            #    inputId = "Ratios_Filter_YN", label = "Use Ratio filter Y/N",
                            #    shape = "round", outline = TRUE, status = "info")
                            
                            #  ,
                            
                            # sliderInput(
                            #   inputId = "param_slide",
                            #   label = "Manual ratio change",
                            #   value = 60,
                            #   min = 0.25,
                            #   max = 200,
                            #   step = 0.25
                            # ),
                            # numericInput(
                            #   inputId = "param_numeric",
                            #   label = "Manual ratio change",
                            #   value = 60,
                            #   min = 0.25,
                            #   max = 200,
                            #   step = 0.25
                            # )
                            # 
                            # ,
                            # verbatimTextOutput("BCR_MULTI"),
                            
                            
                            
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
                              step = 0.1#,
                              #pre ="£"
                            ),
                            
                            
                            # numericInput(
                            #    inputId = "param_numeric",
                            #    label = "Manual ratio change",
                            #    value = 60,
                            #    min = 0.25,
                            #    max = 200,
                            #    step = 0.25,
                            #    pre ="£"
                            #  )
                            
                            
                            
                            
                          ),
                          
                          
                          
                          
                          mainPanel(
                            
                            #verbatimTextOutput("excludedRows"),
                            DTOutput('myDT'),
                            
                            
                            
                            br(),
                            
                            #actionButton("submit", "Produce Report (Select only 1)"),
                            #actionButton("resets", "reset results"),
                            
                            br(),
                            br(),
                            
                            fluidRow(
                              column(12,
                                     
                                     
                                     
                                     
                                     fluidRow(#style = "margin:2%;",
                                       column(width = 6, 
                                              DTOutput('myresults')),
                                       column(width = 3,
                                              plotlyOutput("distPlot")),
                                       #fluidRow(style = "margin:2%;",
                                       column(width = 3,
                                              plotlyOutput("distPlot2"))#)
                                       
                                     )))
                            
                            
                            
                            #,
                            #textOutput("excludedRows")
                          )))
               
               
               
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
                        
                        #tags$head(
                        #  tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/jspdf/1.5.3/jspdf.min.js"),
                        #  tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/dom-to-image/2.6.0/dom-to-image.min.js"),
                        #  tags$script(js)
                        #)
                        
                        #,
                        
                        sidebarLayout(
                          sidebarPanel(textInput("InputId2", "Enter ID", value = "A1", width = NULL, placeholder = NULL)
                                       ,actionButton("submit2", "Produce Report")),
                                       #actionButton("printPdf_CA", "Export page to PDF")
                          mainPanel(div(style='width:100%',
                                        id = "mainOrder_CA",fluidRow(
                                          fluidRow(
                                            column(12,
                                                   
                                                   
                                                   wellPanel(
                                                     style = "background-color: #800020;",
                                                     #firstInput("input1"),
                                                     #strong(helpText("R:")),
                                                     #tableOutput("checkboxes"),
                                                     #hidden(uiOutput("secondInput")),
                                                     
                                                     
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
                                                                  
                                                                  
                                                                  #),
                                                                  column( width = 3,
                                                                          wellPanel(
                                                                            style = "background-color: white;
                                                border: -5%;
                                   padding: -5%;
                                   height:100%
                                                ",
                                                                            
                                                                            textOutput("text9"))),
                                                                  
                                                                  # column(4,offset = 0,
                                                                  #         textOutput("text8")),
                                                                  
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
                                                                  
                                                                  
                                                                  #),
                                                                  column( width = 3,
                                                                          wellPanel(
                                                                            style = "background-color: white;
                                                 ",
                                                                            
                                                                            textOutput("text13"))),
                                                                  
                                                                  # column(4,offset = 0,
                                                                  #         textOutput("text8")),
                                                                  
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
                                                                  
                                                                  
                                                                  #),
                                                                  column( width = 3,
                                                                          wellPanel(
                                                                            style = "background-color: white;
                                                 ",
                                                                            
                                                                            textOutput("text20"))),
                                                                  
                                                                  # column(4,offset = 0,
                                                                  #         textOutput("text8")),
                                                                  
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
                                                                  
                                                                  
                                                                  #),
                                                                  column( width = 2,
                                                                          wellPanel(
                                                                            style = "background-color: white;
                                                 ",
                                                                            
                                                                            textOutput("text26"))),
                                                                  
                                                                  # column(4,offset = 0,
                                                                  #         textOutput("text8")),
                                                                  
                                                                  
                                                                  column(width = 2,
                                                                         wellPanel(
                                                                           style = "background-color: white;
                                                ",
                                                                           
                                                                           
                                                                           textOutput("text27"))),
                                                                  
                                                                  
                                                                  #),
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
                                                            
                                                     ))
                                            )
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
                          #helpText("Use filters to work out intervention."),
                          
                          ### Input Glossary
                          
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