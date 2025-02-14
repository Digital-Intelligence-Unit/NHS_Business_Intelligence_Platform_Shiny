library(readr)
library(tidyverse)
library(readxl)
library(tidyverse)
library(shinyWidgets)
library(DT)
library(d3r)

ICD10_Ref <- read_csv("Data/ICD10/ICD10_Ref.csv")
ChapterLookup <- read_csv("Data/ICD10/ChapterLookup.csv")

server <- function(input, output, session) {

  source("INDEXTABLES.R", local = TRUE)
  source("MasterINDEXTABLES.R", local = TRUE)
  
  output$pdfview <- renderUI({
      tags$iframe(style="height:1000px; width:100%", src="guide.pdf")
    })

}
