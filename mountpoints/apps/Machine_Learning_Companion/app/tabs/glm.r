glm_vars = c("Sex"                                   ,"Risk Score Group",                     
"Asthma"                                ,"Coronary Artery Disease" ,             
"Congestive Heart Failure"              ,"Cancer" ,                              
"Chronic obstructive pulmonary disease" ,"Persistent depressive disorder",       
"Diabetes"                              ,"Hypertension" ,                        
"Atrial fibrillation"                   ,"Chronic kidney disease" ,              
"Dementia"                              ,"Epilepsy",                             
"Hypothyroid"                           ,"Mental health" ,                       
"Learning disability"                   ,"Osteoporosis" ,                        
"Peripheral artery disease"             ,"Rheumatoid arthritis" ,                
"palliative care flag"                  ,"Stroke" ,                              
"smoker"                                ,"substance misuse" ,                    
"psychotic disorder flag"               ,"cdiff flag",                           
"oxygen flag"                           ,"mosaic label"  ,                       
"community matron status"               ,"community matron status-type" ,        
"wardcode"                              ,"wardname",                             
"age markers"                           ,"age 55 and over" ,                     
"age 65 and over"                       ,"age 75 and over",                      
"age Children"                          ,"age 17-54",                            
"age band narrow"                       ,"age band broad" ,                      
"taxonomy"                              ,"area" ,                                
"top 20 percent deprived"               ,"deprivation decile" ,                  
"gp data feed"                          ,"fcvanguard",                           
"data date"                             ,"lsoa",                                 
"msoa"                                  ,"household category",                   
"household group"                       ,"household type" ,                      
"household description"                 ,"wellbeing acorn group" ,               
"wellbeing acorn type"                  ,"wellbeing acorn description",          
"ethniccategory"                        ,"du",                                   
"electoral ward or division" )

glm_targets = c("Age"                                    ,  "Sex" ,                                    
"Risk Score"                             ,  "Risk Score Int" ,                         
"IP Admissions in Last 12 Months"        ,  "IP Elective Admissions in Last 12 Months",
"OP Appointments in Last 12 Months"      ,  "AE Attendances in Last 12 Months"  ,      
"Asthma"                                 ,  "Coronary Artery Disease" ,                
"Congestive Heart Failure"               ,  "Cancer"  ,                                
"Chronic obstructive pulmonary disease"  ,  "Persistent depressive disorder" ,         
"Diabetes"                               ,  "Hypertension",                            
"Atrial fibrillation"                    ,  "Chronic kidney disease" ,                 
"Dementia"                               ,  "Epilepsy",                                
"Hypothyroid"                            ,  "Mental health" ,                          
"Learning disability"                    ,  "Osteoporosis",                            
"Peripheral artery disease"              ,  "Rheumatoid arthritis"  ,                  
"Stroke" )

glm <-tabPanel("GLM", fluid = TRUE,
         mainPanel(
           fluidRow(
             column(width = 6,
                    fluidRow(
                      box(width = 12, title = "Variable Setup",
                          column(width = 6,
                                 pickerInput(
                                   inputId = "glmVar1",
                                   label = "Select Fields",
                                   choices =glm_vars, 
                                   selected = 1,
                                   multiple = TRUE,
                                   options = list(`actions-box` = TRUE, size = 12, noneSelectedText = "Please select at least 1 option")
                                 ),                                             
                          ),
                          column(width = 6,
                                 pickerInput(
                                   inputId = "glm2Var2",
                                   label = "Target Variable",
                                   choices = glm_targets,
                                   selected = 1,
                                   options = list(`actions-box` = TRUE, size = 12, noneSelectedText = "Please select at least 1 option")
                                 ),
                          ),
                          column(width = 12,
                                 br(),
                                 fluidRow(
                                   column(width = 12,
                                          actionButton("glmgo",label = "Go!"),
                                          h4("Algorithm Parameters"),
                                          verbatimTextOutput("glmFamily")
                                   )),
                                 column(width = 12,
                                       checkboxInput("glmoddsratios", "Tick to show the odd ratio plot", value = FALSE)
                                 )
                          )
                      )
                    ),
                    fluidRow(
                      tableOutput("glmTable"),tableOutput("glmTableSig")
                    )

             ),

           column(width = 6,
       fluidRow(
         column(width = 12,
                div(
                  style = "width: 100%; height: 100%;",
                  br(),
                  br(),
                  uiOutput("glmoddsratiosUI")
                )
         )
       )
)
                ) # added closing parenthesis here
         )
) 
#)


