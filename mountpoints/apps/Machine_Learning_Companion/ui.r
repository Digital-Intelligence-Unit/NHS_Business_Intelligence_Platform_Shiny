print("ui loading libs")
pdf(file = NULL)
library(DBI)
library(RPostgreSQL)
library(shiny)
library(shinydashboard)
library(shinyBS)
library(plotly)
library(treemap)
library(RColorBrewer)
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
library(forcats)
library(stringr)

print("Sources to load ui")

source("./app/tabs/welcome.r", local = TRUE)
source("./app/tabs/decision_tree.r", local = TRUE)
source("./app/tabs/glm.r", local = TRUE)
source("./app/tabs/bayesian_network.r", local = TRUE)
source("./app/tabs/clustering.r", local = TRUE)
#source("./data.r")

print("Making UI")

ui <- fluidPage(
  tags$head(
    #Css styling
    tags$style(HTML("

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
        ))
