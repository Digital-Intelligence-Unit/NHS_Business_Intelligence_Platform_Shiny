print("ui loading libs")
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
library(shinyjs)

source("./app/tabs/welcome.r")
source("./app/tabs/decision_tree.r")
source("./app/tabs/glm.r")
source("./app/tabs/bayesian_network.r")
source("./app/tabs/clustering.r")

#server_ready <- reactiveVal(FALSE)

jsCode <- '
Shiny.addCustomMessageHandler("addCohort", function(data) {
  const dataString = JSON.stringify(data[0]);
  const userID = data[2];
  const referrer = data[3];
  const APIData = {
    function:"createCVICohort",
    payload:{data:dataString,created: new Date(),name:data[1]},
    missing:["username"],
    validation:{"username":userID}
  }
  if(document.referrer.includes(referrer)){
    window.parent.postMessage(APIData,document.referrer);
  } else {
    console.log("incorrect referrer");
  }
});
'
ui <- fluidPage(
  useShinyjs(),
  extendShinyjs(text = jsCode, functions = c("addCohort")),
  tags$head(
    #Css styling
    tags$style(HTML("
      #PopSelect {
        background-color:blue;
        color:white
      }
      li.dropdown-header.optgroup-1 > span.text,
      li.dropdown-header.optgroup-2 > span.text  {
        font-size: 20px;
        font-weight: bold;
      }
      .navbar {
        padding-bottom: 20px;
      }
      .welcome-text {
        display: flex;
        flex-direction: column;
        justify-content: center;
        padding: 20px;
        background-color: rgba(255, 255, 255, 0.8);
        border-radius: 10px;
      }
      .welcome-image {
        position: relative;
        height: 200px;
        margin-bottom: 20px;
        border-radius: 10px;
      }
      .welcome-image img {
        height: 100%;
        width: 100%;
        object-fit: cover;
        border-radius: 10px;
      }
      .btn-header {
        background-color: transparent;
        color: #555555;
        font-size: 20px;
        font-weight: bold;
        border: none;
        border-bottom: 3px solid transparent;
        padding: 10px 20px;
        margin-right: 10px;
        cursor: pointer;
      }
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
    id = "my_tabs",
    welcome,
    decision_tree,
    glm,
    bayesian_network,
    clustering                     
  )
)