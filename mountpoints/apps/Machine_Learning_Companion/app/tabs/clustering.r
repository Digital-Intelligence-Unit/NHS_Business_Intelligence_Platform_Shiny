
clustering_cols = c("Risk Score"                              , "Risk Score Rank" ,                        
"Risk Score Trend"                        , "Risk Score Group"  ,                      
"Risk Score Int"                          , "Risk Segment" ,                           
"IP Admissions in Last 12 Months"         , "IP Elective Admissions in Last 12 Months",
"OP Appointments in Last 12 Months"       , "AE Attendances in Last 12 Months",        
"Asthma"                                  , "Coronary Artery Disease",                 
"Congestive Heart Failure"                , "Cancer"  ,                                
"Chronic obstructive pulmonary disease"   , "Persistent depressive disorder" ,         
"Diabetes"                                , "Hypertension",                            
"Atrial fibrillation"                     , "Chronic kidney disease"   ,               
"Dementia"                                , "Epilepsy",                                
"Hypothyroid"                             , "Mental health"   ,                        
"Learning disability"                     , "Osteoporosis" ,                           
"Peripheral artery disease"               , "Rheumatoid arthritis" ,                   
"palliative care flag"                    , "Stroke",                                  
"smoker"                                  , "substance misuse" ,                       
"psychotic disorder flag"                 , "cdiff flag",                              
"oxygen flag"                             , "mosaic label" ,                           
"average ip admission in following year"  , "average nel costs in following year",     
"community matron status"                 , "community matron status-type" ,           
"wardcode"                                , "wardname",                                
"age markers"                             , "age 55 and over" ,                        
"age 65 and over"                         , "age 75 and over" ,                        
"age Children"                            , "age 17-54"  ,                             
"age band narrow"                         , "age band broad" ,                         
"chronic condition count"                 , "taxonomy"  ,                              
"area"                                    , "top 20 percent deprived"   ,             
"deprivation decile"                      , "gp data feed" ,                           
"fcvanguard"                              , "data date" ,                              
"cpm risk score"                          , "lsoa",                                    
"msoa"                                    , "household category",                      
"household group"                         , "household type",                          
"household description"                   , "wellbeing acorn group",                   
"wellbeing acorn type"                    , "wellbeing acorn description" ,            
"ethniccategory"                          , "du"  ,                                    
"electoral ward or division")



clustering <- tabPanel("Clustering", value = 'clustering',fluid = TRUE,
                     mainPanel(
                       fluidRow(
                         column(width = 7,
                                fluidRow(
                                  box(width = 12, title = "Variable Setup",
                                      column(width = 6,
                                             pickerInput(
                                               inputId = "kmVar1",
                                               label = "Select Fields to cluster by",
                                               choices = clustering_cols,
                                               selected = 1,
                                               multiple = TRUE,
                                               options = list(`actions-box` = TRUE, size = 12, noneSelectedText = "Please select at least 1 option")
                                             ),
                                             pickerInput(
                                               inputId = "kmVar2",
                                               label = "Select Fields to group clusters by",
                                               choices =clustering_cols,
                                               selected = 2,
                                               multiple = TRUE,
                                               options = list(`actions-box` = TRUE, size = 12, noneSelectedText = "Please select at least 1 option")
                                             ),
                                             
                                             bsTooltip("kmVar2","Use to graph features that were not used during cluster fitting."),
                                          
                                             numericInput('clusters', 'Cluster count', 3, min = 1, max = 9),
                                            bsTooltip("clusters","The number of clusters to produce.")                                             
                                      ),
                                      
                                      column(width = 12,
                                             br(),
                                             fluidRow(
                                                        column(width = 6,
                                                               
                                                               checkboxInput(inputId ="tsnegr",
                                                                            label = "Produce Tsne graph",
                                                                            value = FALSE),
                                                               
                                                              #  checkboxInput(inputId ="umapgr",
                                                              #                label = "Produce Umap graph",
                                                              #                value = FALSE),
                                                               
                                                               actionButton("kmgo",label = "Go!"),
                                                               actionButton("kmgosh",label = "silhouette"),
                                                               bsTooltip("kmgosh", "Used to evaluate the quality of clusters. Can take some time to process.")
                                               )
                                               
                                              #  ,fluidRow(
                                              #         br(),br()
                                              #    ,uiOutput("plot5")
                                              #           )
                                               ,fluidRow(
                                                 br(),br()
                                                 ,uiOutput("plot3")
                                                 ,uiOutput("plot4")
                                                 )
                                             ))))),
                         
                         
           column(width = 5,
       fluidRow(
         column(width = 12,
                div(
                  style = "width: 100%; height: 100%;",
                  fluidRow(
                                                  uiOutput("plot1")
                                                        )
                                               ,fluidRow(
                                                      br(),br(),
                                                      uiOutput("plot2")
                                                      )
                )
         )
       )
)
                       )
                     )
)
           