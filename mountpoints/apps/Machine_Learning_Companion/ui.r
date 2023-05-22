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

source("./app/tabs/welcome.R")
source("./app/tabs/decision_tree.R")
source("./app/tabs/glm.R")
source("./app/tabs/bayesian_network.R")
source("./app/tabs/clustering.R")

#server_ready <- reactiveVal(FALSE)

ui <- fluidPage(

  

  # Use a button that is disabled until the server is ready
  
#)

# Define the wait message UI element using a reactive expression



  tags$head(
    #Css styling

tags$script(HTML(
    '
         if (window.addEventListener) {
        window.addEventListener("message", (e) => {
        console.log(e);
        }, false);
        }'
  )),

    tags$style(HTML("

              
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
        ))