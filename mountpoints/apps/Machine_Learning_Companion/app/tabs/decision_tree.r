require(visNetwork)

##Columns for UI
decision_tree_vars = c("Age"                                      , "Sex",                                     
 "Risk Score"                               , "Risk Score Rank"  ,                       
 "Risk Score Trend"                         , "Risk Score Group",                        
 "Risk Score Int"                           , "Risk Segment" ,                           
 "IP Admissions in Last 12 Months"          , "IP Elective Admissions in Last 12 Months",
 "OP Appointments in Last 12 Months"        , "AE Attendances in Last 12 Months",        
 "Asthma"                                   , "Coronary Artery Disease",                 
 "Congestive Heart Failure"                 , "Cancer",                                  
 "Chronic obstructive pulmonary disease"    , "Persistent depressive disorder" ,         
 "Diabetes"                                 , "Hypertension",                            
 "Atrial fibrillation"                      , "Chronic kidney disease",                  
 "Dementia"                                 , "Epilepsy" ,                               
 "Hypothyroid"                              , "Mental health",                           
 "Learning disability"                      , "Osteoporosis",                            
 "Peripheral artery disease"                , "Rheumatoid arthritis",                    
 "palliative care flag"                     , "Stroke",                                  
 "smoker"                                   , "substance misuse",                        
 "psychotic disorder flag"                  , "cdiff flag",                              
 "oxygen flag"                              , "mosaic label",                            
 "average ip admission in following year"   , "average nel costs in following year" ,    
 "community matron status"                  , "community matron status-type",            
 "wardcode"                                 , "wardname",                                
 "age markers"                              , "age 55 and over",                         
 "age 65 and over"                          , "age 75 and over",                         
 "age Children"                             , "age 17-54",                               
 "age band narrow"                          , "age band broad",                          
 "chronic condition count"                  , "taxonomy" ,                               
 "area"                                     , "top 20 percent deprived" ,                
 "deprivation decile"                       , "gp data feed" ,                           
 "fcvanguard"                               , "data date",                               
 "cpm risk score"                           , "lsoa" ,                                   
 "msoa"                                     , "household category" ,                     
 "household group"                          , "household type" ,                         
 "household description"                    , "wellbeing acorn group" ,                  
 "wellbeing acorn type"                     , "wellbeing acorn description",             
 "ethniccategory"                           , "du",                                      
 "electoral ward or division" )
 
 
decision_tree_targets = c("Age"                                     , "Sex" ,                                    
"Risk Score"                              , "Risk Score Rank",                         
"Risk Score Trend"                        , "Risk Score Group" ,                       
"Risk Score Int"                          , "Risk Segment" ,                           
"IP Admissions in Last 12 Months"         , "IP Elective Admissions in Last 12 Months",
"OP Appointments in Last 12 Months"       , "AE Attendances in Last 12 Months",        
"Asthma"                                  , "Coronary Artery Disease",                 
"Congestive Heart Failure"                , "Cancer",                                  
"Chronic obstructive pulmonary disease"   , "Persistent depressive disorder" ,         
"Diabetes"                                , "Hypertension",                            
"Atrial fibrillation"                     , "Chronic kidney disease" ,                 
"Dementia"                                , "Epilepsy",                                
"Hypothyroid"                             , "Mental health"  ,                         
"Learning disability"                     , "Osteoporosis",                            
"Peripheral artery disease"               , "Rheumatoid arthritis"  ,                  
"palliative care flag"                    , "Stroke" ,                                 
"smoker"                                  , "substance misuse"  ,                      
"psychotic disorder flag"                 , "cdiff flag" ,                             
"oxygen flag"                             , "deprivation decile" ,                     
"age band broad"                          , "age band narrow") 

decision_tree <- tabPanel("Decision Tree",
                     fluidRow(
                       box(background = "green",width = 4,
                           uiOutput("oneCoverBoxs9"),
                       ),
                       box(background = "green", width = 4,
                           uiOutput("oneCoverBoxDate10")
                       ),
                     ),
                     fluidRow(
                       column(width = 9,
                              fluidRow(
                                box(width = 12, title = "Variable Setup",
                                    column(width = 6,
                                           pickerInput(
                                             inputId = "twoCARTVar1",
                                             label = "Select Fields (to use to build the tree)",
                                             choices =decision_tree_vars,
                                             selected = 1,
                                             multiple = TRUE,
                                             options = list(`actions-box` = TRUE, size = 12, noneSelectedText = "Please select at least 1 option")
                                           ),
                                    ),
                                    column(width = 6,
                                           pickerInput(
                                             inputId = "twoCARTVar2",
                                             label = "Target Variable (to cluster by)",
                                             choices = decision_tree_targets,
                                             
                                             selected = 1,
                                             options = list(`actions-box` = TRUE, size = 12, noneSelectedText = "Please select at least 1 option")
                                           ),
                                    ),
                                    column(width = 12,
                                           br(),
                                           fluidRow(
                                             column(width = 6,
                                                    div(id="AlgorithmParameters",h4("Algorithm Parameters")),
                                             ),
                                               bsTooltip("AlgorithmParameters", title = "It is recommended these settings are left as default.")
                                             
                                           ),
                                           fluidRow(
                                             column(width = 6,
                                                    numericInput("twoCARTmaxdepth", label = "Maximum Depth", value = 3, min = 1, max = 50, step = 1)
                                             ),
                                             
                                              bsTooltip("twoCARTmaxdepth", title = "Maximum depth of tree (how many splits are allowed)"),
                    
                                           ),
                                           fluidRow(
                                             column(width = 6,
                                                    numericInput("twoCARTcp", label = "CP", value = 0.00001, min = 0, step = 0.00001),
                                             ),                                             
                                                bsTooltip("twoCARTcp", title = "Complexity parameter. Lower value can allow more splits."),
                                           ),
                                           fluidRow(
                                             column(width = 6,
                                                    numericInput("twoCARTminbucket", label = "Minbucket", value = 20, min = 1, step = 1),
                                             ),
                                                bsTooltip("twoCARTminbucket", title = "Minimum number of individual that must be in a leaf node at the end of the algorithm."),
                                           ),
                                           fluidRow(
                                             column(width = 6,
                                                    actionButton("twoCARTgo",label = "Go!")
                                             ),
                                             column(width = 6,
                                                    
                                             )
                                           )
                                    ),
                                    
                                ),
                              ),
                              fluidRow(
                                box(width = 12, title = "Decision Tree",
                                    visNetworkOutput("twoCARTTree"),
                                    column(width = 3#,
                                          # checkboxInput("twoCARTTreeRules", "Tick to show rules", value = FALSE),
                                    ),
                                    
                                    column(width = 3,
                                           checkboxInput("twoCARTTreeVARIMP", "Tick to show variable importance", value = FALSE),
                                    ),
                                   
                                    uiOutput("twoCARTTreeRulesTableUI"),
                                    textOutput("eCART")),
                                
                                uiOutput("twoCARTTreeRulesTableUIVARIMP"),                                
                              ),
                       ),
                       column(width = 3,
                              fluidRow(
                                div(
                                  box(width = 12, id = "CARTInfo",style = "overflow-y: scroll;", title = "CART Info",
                                      column(width = 12,
                                             fluidRow(
                                               p("Classification and Regression Tree (CART), referred to as 'Decision Trees' on this page, is a data-driven regression technique."),
                                               p("See ", a("Tree-Based Models", href="https://blog.dataiku.com/tree-based-models-how-they-work-in-plain-english"), " for how regression trees work."),
                                               p(""),
                                               p("Use decision trees either to segment the population, or to discover what attributes are most statictically significant in explaining differences (of the selected target variable)."),
                                               )
                                      )
                                  ), style = "position:fixed;  width:inherit;")
                             )
                       )
                     ),
            )
