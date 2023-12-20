# Load required libraries
library(shiny)
library(shinyjs)
library(plotly)
library(dplyr)
library(ggplot2)
library(shinydashboard)
library(shinydashboardPlus)
library(shinycssloaders)
library(scales)
library(funnelR)
library(DT)
library(shinythemes)
library(shinyBS)
library(shinyWidgets)
library(PHEindicatormethods)
library(RColorBrewer)

library(NHSRplotthedots)
library(runcharter)
# Shiny server
server <- function(input, output, session) {
  
  ## practice/pcn tab
  observe({
    if(input$switchView == FALSE) { # GP
      # Adjust logic for GP
      # GP page
      source("gpserver.R", local = TRUE)
      
    } else if(input$switchView == TRUE) { # PCN
      
      source("pcnserver.R", local = TRUE)
      # Adjust logic for PCN
    }
  })
  
  ## PBC tab
  observe({
    if(input$switchViewPBC == FALSE) { # GP
      # Adjust logic for GP
      # GP page
      source("pbcserver.R", local = TRUE)
      
    } else if(input$switchViewPBC == TRUE) { # PCN
      
      source("pbcpcnserver.R", local = TRUE)
      # Adjust logic for PCN
    }
  })
  
  # PBC page
  
  #source("pbcserver.R", local = TRUE)
  
}


