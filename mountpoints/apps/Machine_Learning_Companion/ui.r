pdf(file = NULL)
library(DBI)
library(RPostgreSQL)
library(shiny)
library(shinydashboard)
library(shinyBS)
#library(plotly)
library(treemap)
#library(RColorBrewer)
library(shinyTree)
library(ggplot2)
library(shinyWidgets)
library(dplyr)
library(scales)
library(cluster)
library(data.table)
library(DT)
library(epiDisplay)
library(bnlearn)
library(bnviewer)
library(fastcluster)
library(gridExtra)
library(kmed)
library(Rtsne)
library(umap)
library(rpart)
library(rpart.plot)

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

Bayesian_cols = c("Asthma"                               ,  "Coronary Artery Disease" ,             
"Congestive Heart Failure"             ,  "Cancer",                               
"Chronic obstructive pulmonary disease",  "Persistent depressive disorder" ,      
"Diabetes"                             ,  "Hypertension" ,                        
"Atrial fibrillation"                  ,  "Chronic kidney disease" ,              
"Dementia"                             ,  "Epilepsy",                             
"Hypothyroid"                          ,  "Mental health",                        
"Learning disability"                  ,  "Osteoporosis",                         
"Peripheral artery disease"            ,  "Rheumatoid arthritis",                 
"Stroke"                               ,  "Sex",                                  
"top 20 percent deprived"              ,  "age 55 and over",                      
"age 65 and over"                      ,  "age 75 and over" ,                     
"age Children"                         ,  "age 17-54")

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


source("./app/tabs/welcome.r")
source("./app/tabs/decision_tree.r")
source("./app/tabs/glm.r")
source("./app/tabs/bayesian_network.r")
source("./app/tabs/clustering.r")

ui <- fluidPage(
  tags$head(
    #Css styling
    tags$style(HTML("
                  .vis-network {
                  height: 100%
                  }
                  #bntext {
                    height:500px;
                    width: auto;
                    background-color: white
                    
                  }

                  #glmTable {
                    height:500px;
                    width: auto;
                    background-color: white;
                    overflow-y: auto
                  }
                  
                  #plot11{
                  margin:35px
                  }

                  #TABLE {
                    margin-top: 50px;
                    
                  }

                  #textbox_ui{
                    width: 100%;

                  }

                  #textbox_ui2{
                    width: 100%;

                  }

                  ")),
  ),
  # Welcome page
tabsetPanel(
      welcome,
      decision_tree,
      glm,
      bayesian_network,
      clustering                     
        ))
