# library(rpart)
# library(rpart.plot)
# library(plotly)
# library(shiny)
# library(shinyjs)
require(visNetwork)
library(stringr)

##Columns for UI
decision_tree_vars = append(str_to_sentence(c("Age"                                      , "Sex",                                     
 #"Risk Score"                               , 
 "Risk Score Rank"  ,                       
 "Risk Score Trend"                         , "Risk Score Group",                        
 "Risk Score Int"                           , "Risk Segment" ,                           
         
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
 "psychotic disorder flag"                  ,# "cdiff flag",                              
 "oxygen flag"                              , "mosaic label",                            
 "average ip admission in following year"   , "average nel costs in following year" ,    
 #"community matron status"                  , "community matron status-type",            
 #"wardcode"                                 , "wardname",                                
 #"age markers"                              , #"age 55 and over",                         
 #"age 65 and over"                          , "age 75 and over",                         
 #"age Children"                             , "age 17-54",                               
"age band broad",                          
 "chronic condition count"                  ,# "taxonomy" ,                               
 #"area"                                     
 "top 20 percent deprived" ,                
 "deprivation decile"                       , #"gp data feed" ,                           
 #"fcvanguard"                               , "data date",                               
 #"cpm risk score"                           , "lsoa" ,                                   
 #"msoa"                                     
 "household category" ,                     
 "household group"                          , #"household type" ,                         
 "household description"                    , "wellbeing acorn group" ,                  
 "wellbeing acorn type"                     , "wellbeing acorn description",             
 #"ethniccategory"                           , 
 "du",                                      
 "electoral ward or division" )),c("IP admissions in last 12 months"          , "IP elective admissions in last 12 months",
 "OP appointments in last 12 months"        , "AE attendances in last 12 months"))
 
 
decision_tree_targets = append(str_to_sentence(c(                      
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
"oxygen flag"                             )),
c("IP admissions in last 12 months"          , "IP elective admissions in last 12 months",
 "OP appointments in last 12 months"        , "AE attendances in last 12 months"))

decision_tree <- tabPanel("Decision Tree",
                     fluidRow(
                       column(width = 7,
                              fluidRow(
                                box(width = 12, title = "Variable Setup",
                                    column(width = 6,

 pickerInput("twoCARTVar1", 
              "Select Fields (to use to build the tree)", 
              choices = list(
                "Columns with dimension" = str_to_sentence(c("Age"                                 ,  "Sex"                                  ,
                                          "Risk Score Int"                      ,  "mosaic label"                         ,
                                          "deprivation decile"                  ,  
                                          "electoral ward or division"          ,  "Asthma"                               ,
                                          "Coronary Artery Disease"             ,  "Congestive Heart Failure"             ,
                                          "Cancer"                              ,  "Chronic obstructive pulmonary disease",
                                          "Persistent depressive disorder"      ,  "Diabetes"                             ,
                                          "Hypertension"                        ,  "Atrial fibrillation"                  ,
                                          "Chronic kidney disease"              ,  "Dementia"                             ,
                                          "Epilepsy"                            ,  "Hypothyroid"                          ,
                                          "Mental health"                       ,  "Learning disability"                  ,
                                          "Osteoporosis"                        ,  "Peripheral artery disease"            ,
                                          "Rheumatoid arthritis"                ,  
                                          "top 20 percent deprived"             ,
                                          #"age band narrow"                     ,
                                          "age band broad"                       )),
                "Columns without dimension" = setdiff(decision_tree_vars, str_to_sentence(c("Age"                                 ,  "Sex"                                  ,
                                          "Risk Score Int"                      ,  "mosaic label"                         ,
                                          "deprivation decile"                  ,  
                                          "electoral ward or division"          ,  "Asthma"                               ,
                                          "Coronary Artery Disease"             ,  "Congestive Heart Failure"             ,
                                          "Cancer"                              ,  "Chronic obstructive pulmonary disease",
                                          "Persistent depressive disorder"      ,  "Diabetes"                             ,
                                          "Hypertension"                        ,  "Atrial fibrillation"                  ,
                                          "Chronic kidney disease"              ,  "Dementia"                             ,
                                          "Epilepsy"                            ,  "Hypothyroid"                          ,
                                          "Mental health"                       ,  "Learning disability"                  ,
                                          "Osteoporosis"                        ,  "Peripheral artery disease"            ,
                                          "Rheumatoid arthritis"                ,  
                                          "Risk Score Group"                    ,  "top 20 percent deprived"              ,
                                          #"age band narrow"                     ,  
                                          "age band broad" )))
              ),
              options = list(`actions-box` = TRUE, size = 12, noneSelectedText = "Please select at least 1 option"),
              selected = 1,                             
              multiple = TRUE),


                                          #  pickerInput(
                                          #    inputId = "twoCARTVar1",
                                          #    label = "Select Fields (to use to build the tree)",
                                          #    choices =decision_tree_vars,
                                          #    selected = 1,
                                          #    multiple = TRUE,
                                          #    options = list(`actions-box` = TRUE, size = 12, noneSelectedText = "Please select at least 1 option")
                                          #  ),
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
                                           fluidRow(width = 12,
                                             column(width = 3,
                                                    div(id="AlgorithmParameters",h4("Algorithm Parameters")),
                                             #),
                                               bsTooltip("AlgorithmParameters", title = "It is recommended these settings are left as default.")
                                             
                                           ),
                                           #fluidRow(
                                             column(width = 3,
                                                    numericInput("twoCARTmaxdepth", label = "Maximum Depth", value = 3, min = 1, max = 50, step = 1),
                                             #),
                                             
                                              bsTooltip("twoCARTmaxdepth", title = "Maximum depth of tree (how many splits are allowed)"),
                    
                                           ),
                                           #fluidRow(
                                             column(width = 3,
                                                    numericInput("twoCARTcp", label = "CP", value = 0.00001, min = 0, step = 0.00001),
                                             #),                                             
                                                bsTooltip("twoCARTcp", title = "Complexity parameter. Lower value can allow more splits."),
                                           ),
                                           #fluidRow(
                                             column(width = 3,
                                                    numericInput("twoCARTminbucket", label = "Minbucket", value = 20, min = 1, step = 1),
                                             ),
                                                bsTooltip("twoCARTminbucket", title = "Minimum number of individual that must be in a leaf node at the end of the algorithm."),
                                           ),
                                           )
                                    ),
                                    
                                ),
                              ),
                              
                       ),
 
 
 fluidRow(
   box(width = 12, title = "Decision Tree",
       visNetworkOutput("twoCARTTree"),
       #column(width = 3#,
       # checkboxInput("twoCARTTreeRules", "Tick to show rules", value = FALSE),
       #),
       
       #column(width = 3,
       #       checkboxInput("twoCARTTreeVARIMP", "Tick to show variable importance", value = FALSE),
       #),
       
       uiOutput("twoCARTTreeRulesTableUI"),
       textOutput("eCART"))),
 fluidRow(
   column(width = 6,
          actionButton("twoCARTgo",label = "Go!")
   )
     
     #uiOutput("twoCARTTreeRulesTableUIVARIMP"),                                
   ),
                       column(width = 5,
       fluidRow(
         column(width = 12,
                div(
                  style = "width: 100%; height: 100%;",
                  br(),
                  br(),
                 checkboxInput("twoCARTTreeVARIMP", "Tick to show variable importance", value = FALSE),
                 uiOutput("twoCARTTreeRulesTableUIVARIMP")
                )
         )
       )
)
                ) # added closing parenthesis here
        #)