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

print("Sources to load ui")

source("./app/tabs/welcome.r")
source("./app/tabs/decision_tree.r")
source("./app/tabs/glm.r")
source("./app/tabs/bayesian_network.r")
source("./app/tabs/clustering.r")
source("./data.r")

print("Making UI")

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
