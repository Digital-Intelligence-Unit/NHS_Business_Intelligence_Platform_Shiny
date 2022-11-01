
library(shiny)

# Define UI ----

## ui.R ##

library(stringr)
library(DT)
library(readxl)
library(dplyr)
library(ggplot2)
library(shinyWidgets)
library(plotly)
library(shinyjs)


plot_w <- data.frame(1:1000)
colnames(plot_w)[1] <- "thousands"

first.word <- function(my.string){
  unlist(strsplit(my.string, ":"))[1]
}

HEER <- read_excel("./Health_Economics_Evidence_Resource_HEER_July19.xlsm", 
                   sheet = "Evidence")

HEER$`Benefit-cost ratio (BCR)` <- gsub("\\ to ", "-", HEER$`Benefit-cost ratio (BCR)`)
HEER$ICER <- gsub("\\ to ", "-", HEER$ICER)


#HEER %>% select(`Benefit-cost ratio (BCR)`) %>% mutate(Ratios = ifelse( ':' %in% `Benefit-cost ratio (BCR)` ,sapply(`Benefit-cost ratio (BCR)`, first.word),`Benefit-cost ratio (BCR)`))
regexAmount <- "[0-9\\.]+"
## Table Clean BCR values

HEER <- HEER  %>% mutate(Ratios2 =strsplit(`Benefit-cost ratio (BCR)`, "(?=[A-Za-z])(?<=[0-9])|(?=[0-9])(?<=[A-Za-z])", perl=TRUE))
HEER <- HEER %>% mutate(Ratios2 = stringr::str_extract(Ratios2, "^[^:]*"))
#HEER <- HEER  %>% mutate(Ratios2 =stringr::str_extract(Ratios2, regexAmount))
HEER$Ratios2 <- gsub("[^0-9.-]", "", HEER$Ratios2)
#HEER <- HEER  %>% mutate(Ratios2 = ifelse(is.na(Ratios2),0,Ratios2))
library(stringr)
HEER$Ratios3 <-str_split_fixed(HEER$Ratios2, fixed("-"), 2)[, 2]
HEER <- HEER  %>% mutate(Ratios2 =stringr::str_extract(Ratios2, regexAmount))
HEER <- HEER  %>% mutate(Ratios2 = ifelse(is.na(Ratios2),0,Ratios2))

HEER$Ratios <- ifelse(HEER$Ratios3 != "", (as.numeric(HEER$Ratios2) + as.numeric(HEER$Ratios3))/2 , HEER$Ratios2)

#HEER$Ratios3 <- as.numeric(unlist(strsplit(HEER$Ratios2, "-")))[2]# + as.numeric(unlist(strsplit(HEER$Ratios2, "-")))[2]) / 2, HEER$Ratios2)
#mat  <- matrix(unlist(strsplit("1.46-2.01", "-")), ncol=2, byrow=FALSE)
#df   <- as.data.frame(mat)



HEER <- HEER  %>% mutate(ICER2 =strsplit(`ICER`, "(?=[A-Za-z])(?<=[0-9])|(?=[0-9])(?<=[A-Za-z])", perl=TRUE))
HEER <- HEER %>% mutate(ICER2 = stringr::str_extract(ICER2, "^[^:]*"))
#HEER <- HEER  %>% mutate(ICER2 =stringr::str_extract(ICER2, regexAmount))
HEER$ICER2 <- gsub("[^0-9.-]", "", HEER$ICER2)
#HEER <- HEER  %>% mutate(ICER2 = ifelse(is.na(ICER2),0,ICER2))
library(stringr)
HEER$ICER3 <-str_split_fixed(HEER$ICER2, fixed("-"), 2)[, 2]
HEER <- HEER  %>% mutate(ICER2 =stringr::str_extract(ICER2, regexAmount))
HEER <- HEER  %>% mutate(ICER2 = ifelse(is.na(ICER2),0,ICER2))

HEER$ICER_VALUES <- ifelse(HEER$ICER3 != "", (as.numeric(HEER$ICER2) + as.numeric(HEER$ICER3))/2 , HEER$ICER2)





# HEER <- HEER %>% mutate(ICER_VALUES = gsub(",", "", gsub("([a-zA-Z]),", "\\1 ", ICER)))
# HEER <- HEER %>% mutate(ICER_VALUES = stringr::str_extract(ICER_VALUES, "^[^:]*"))
# HEER <- HEER  %>% mutate(ICER_VALUES = stringr::str_extract(ICER_VALUES, "^[^-]*"))
# HEER <- HEER  %>% mutate(ICER_VALUES = stringr::str_extract(ICER_VALUES,  "[^ROI ]+$"))
# HEER <- HEER  %>% mutate(ICER_VALUES =stringr::str_extract(ICER_VALUES, regexAmount) %>% unlist %>% as.numeric)
# HEER <- HEER  %>% mutate(ICER_VALUES = ifelse(is.na(ICER_VALUES),0,ICER_VALUES))

# data_rows <- function(mtcars) {
#   
#   mymtcars <- mtcars
#   mymtcars[["Select"]] <- paste0('<input type="checkbox" name="row_selected" value=',1:nrow(mymtcars),' unchecked>')
#   #mymtcars[["Select"]] <- paste0('<input type="checkbox" name="row_selected" value=',(mymtcars$ID),' unchecked>')
#   mymtcars[["_id"]] <- paste0("row_", seq(nrow(mymtcars)))
#   return(mymtcars)
# }

#HEER <- data_rows(HEER)
# js <- "
# $(document).ready(function(){
#   $('#printPdf_CA').click(function () {
#     domtoimage.toPng(document.getElementById('mainOrder_CA'))
#       .then(function (blob) {
#         var pdf = new jsPDF('l', 'pt', [$('#mainOrder_CA').width(), $('#mainOrder_CA').height()]);
#         pdf.addImage(blob, 'PNG', (window.innerWidth*(1/6)), 0, 1000, window.innerHeight);
#         pdf.save('Report.pdf');
#         // that.options.api.optionsChanged(); what is that?
#       });
#   });
#});
#"
# checks <- function(mymtcars) {
#   
#   
#   
#   
#   callback <- c(
#     sprintf("table.on('click', 'td:nth-child(%d)', function(){", 
#             which(names(mymtcars) == "Select")),
#     "  var checkbox = $(this).children()[0];",
#     "  var $row = $(this).closest('tr');",
#     "  if(checkbox.unchecked){",
#     "    $row.removeClass('excluded');",
#     "  }else{",
#     "    $row.addClass('excluded');",
#     "  }",
#     "  var excludedRows = [];",
#     "  table.$('tr').each(function(i, row){",
#     "    if($(this).hasClass('excluded')){",
#     "      excludedRows.push(parseInt($(row).attr('id').split('_')[1]));",
#     "    }",
#     "  });",
#     "  Shiny.onInputChange('excludedRows', excludedRows);",
#     "});"
#   )
#   
#   return(callback)
# }
# 
# checks2 <- function(mymtcars) {
#   
#   
#   
#   
#   callback <- c(
#     
#     sprintf("table.on('click', 'td:nth-child(%d)', function(){", 
#             which(names(mymtcars) == "Select")),
#     "  var checkbox = $(this).children()[0];",
#     "  var $row = $(this).closest('tr');",
#     "  if(checkbox.checked){",
#     "    $row.removeClass('excluded');",
#     "$row.addClass('unexcluded');",
#     "  }else{",
#     "    $row.addClass('excluded');",
#     "  }",
#     "  var excludedRows = [];",
#     "  table.$('tr').each(function(i, row){",
#     "    if($(this).hasClass('unexcluded')){",
#     "      excludedRows.push(parseInt($(row).attr('id').split('_')[1]));",
#     "    }",
#     "  });",
#     "  Shiny.onInputChange('excludedRows', excludedRows);",
#     "});"
#     
#   )
#   
#   return(callback)
# }

#library(pixiedust)
#library(shinydust)




















#tabTable(id="iris")



# Define server logic ----
server <- function(input, output, session) {
  
  
  
  
  
  
  
  
  observe({
    
    if(!is.null(input$Theme)) {
      
      #df <- HEER  %>% select(ID,Activity,ICER)
      if(isTRUE(input$icer) & isFALSE(input$bcr)){
        
        df <- HEER  %>% filter(Theme %in% input$Theme & `Type of analysis` %in% input$type) %>% #select(ID,Activity,Intervention,`Benefit-cost ratio (BCR)`,ICER) %>%
          filter(`ICER` != '-')
        #filter(if_all(ICER, ~ grepl("^-?\\d+(\\.\\d+)?$", ICER))) %>% 
        #mutate(across(ICER, ~ as.numeric(ICER)))
        #df <- df[grep(pattern = "^\\d+(\\.\\d+)?", df[,"ICER"]), ]
        
      }
      
      else if (isTRUE(input$bcr) & isFALSE(input$icer)){
        
        df <- HEER  %>% filter(Theme %in% input$Theme & `Type of analysis` %in% input$type) %>% #select(ID,Activity,Intervention,`Benefit-cost ratio (BCR)`,ICER) %>%
          filter(`Benefit-cost ratio (BCR)` != '-')
        
        
      }
      
      else if (isTRUE(input$bcr) & isTRUE(input$icer)){
        
        df <- HEER  %>% filter(Theme %in% input$Theme & `Type of analysis` %in% input$type) %>% #select(ID,Activity,Intervention,`Benefit-cost ratio (BCR)`,ICER) %>%
          filter(`Benefit-cost ratio (BCR)` != '-') %>%
          filter(`ICER` != '-')
        
        
      }
      #Ratios_Filter_YN
      
      
      else {
        
        
        #else {  
        df <- HEER  %>% filter(Theme %in% input$Theme & `Type of analysis` %in% input$type)# %>% select(ID,Activity,Intervention,`Benefit-cost ratio (BCR)`,ICER)
      }#}
      
      
      if (isTRUE(input$ICER_Filter_YN) & isFALSE(input$Ratio_Filter_YN)){
        
        df <- df  %>% filter(Theme %in% input$Theme & `Type of analysis` %in% input$type) %>%
          filter(as.numeric(ICER_VALUES) >= (input$ICER_FILTER[1] * 1000) & as.numeric(ICER_VALUES) <= (input$ICER_FILTER[2] * 1000)) %>% select(ID,Activity,Intervention,`Benefit-cost ratio (BCR)`,ICER)
      }
      
      else if (isTRUE(input$ICER_Filter_YN) & isTRUE(input$Ratio_Filter_YN))
      {
        
        df <- df  %>% filter(Theme %in% input$Theme & `Type of analysis` %in% input$type) %>%
          filter(as.numeric(ICER_VALUES) >= (input$ICER_FILTER[1] * 1000) & as.numeric(ICER_VALUES) <= (input$ICER_FILTER[2] * 1000)) %>% 
          filter(as.numeric(Ratios) >= (input$Ratio_FILTER[1]) & as.numeric(Ratios) <= (input$Ratio_FILTER[2])) %>% 
          select(ID,Activity,Intervention,`Benefit-cost ratio (BCR)`,ICER)
        
        
      }
      
      else if (isTRUE(input$Ratio_Filter_YN) & isFALSE(input$ICER_Filter_YN)){
        
        df <- df  %>% filter(Theme %in% input$Theme & `Type of analysis` %in% input$type) %>%
          #filter(ICER_VALUES >= (input$ICER_FILTER[1] * 1000) & ICER_VALUES <= (input$ICER_FILTER[2] * 1000)) %>% 
          filter(as.numeric(Ratios) >= (input$Ratio_FILTER[1]) & as.numeric(Ratios) <= (input$Ratio_FILTER[2])) %>% 
          select(ID,Activity,Intervention,`Benefit-cost ratio (BCR)`,ICER)
        
        
      }
      
      else {
        
        
        #else {  
        df <- df  %>% select(ID,Activity,Intervention,`Benefit-cost ratio (BCR)`,ICER)
      }#}  
      
      #Ratio_Filter_YN
      #Ratio_FILTER
      output$myDT <- DT::renderDataTable(df,selection = 'single', server = FALSE)
      
      
      
      
      output$excludedRows <- renderPrint({
        
        
        #ROWS <- as.data.frame()
        input$excludedRows
      })
      #toJSON(input$excludedRows)
      
      
      #})
      
      
      
      
      #ROWS <- as.list(input[["excludedRows"]])
      #results <- eventReactive(input$submit,{
      #  if(is.null(input[["excludedRows"]])){
      #    return()
      #  }
      
      
      
      
      #myList <- toJSON(input$excludedRows)
      #print(as.data.frame(fromJSON(myList))[1])
      
      
      output[["BCR_MULTI"]] <- renderText({
        
        #paste0("£",input[["param_numeric"]] * (input$investment*1000))
        paste0('Potential ROI: £',format(input[["param_numeric"]] * (input$investment*1000), big.mark=",", scientific=FALSE))
      })
      
      
      
      
      
      #df <- ICD_10_P#800020iction(input$variable,"Emran/ClinicalBERT_ICD10_Categories")
      
      #     filter(percentage >= input$range[1] & percentage <= input$range[2]) %>%
      #     arrange(desc(percentage))
      
      
      
      
      #minus <- input[["excludedRows"]]
      
      #IDS <- HEER %>% filter(ID %in% (df %>% filter(row_number() %in% (input$myDT_rows_selected)))) %>%
      #  select(ID) %>% pull
      
      output$myresults <- DT::renderDT({
        datatable(df2 <- HEER %>% filter(ID %in% (df %>% filter(row_number() %in% (input$myDT_rows_selected)))) %>%
                    select(ID,`Summary of evidence`,`Assumptions / Caveats`,Ratios))
        
        
        
        
        
        
      })
      
      
      
      
      # df2 <- HEER %>% filter(row_number() %in% (input[["excludedRows"]] - minus)) %>%
      #    select(ID,`Summary of evidence`,`Assumptions / Caveats`)
      #  
      #output$myresults <- DT::renderDT({
      #  datatable(df2,callback = JS(checks2(df2)))
      # df
      #})
      
      #shinyjs::runjs((checks(df)))
      
      #try(
      
      
      #rm(input$excludedRows) 
      #)
      
      # eventReactive(input$submit, {
      #   updateTabsetPanel(session, "inTabset",
      #                     selected = "report")
      #   
      #   updateTextInput(session,"InputId2",value = (HEER %>% filter(ID %in% (df %>% filter(row_number() %in% (input$myDT_rows_selected)))) %>%
      #                     select(ID) %>% pull))
      #   
      
      
      # RESULTS2 <- eventReactive(input$submit,{
      #  if(is.null(input$InputId2)){
      #    return()
      #  }
      
      #  df <- HEER %>% filter(ID == toupper(input$InputId2)) 
      
      
      
      observeEvent(input$myDT_rows_selected, {
        #req(input$myDT_rows_selected)
        
        
        # eventReactive(input$submit, {
        #   updateTabsetPanel(session, "inTabset",
        #                     selected = "report")
        #   
        updateTextInput(session,"InputId2",value = (HEER %>% filter(ID %in% (df %>% filter(row_number() %in% (input$myDT_rows_selected)))) %>%
                                                      select(ID) %>% pull))
        
        
        
        output$distPlot <- renderPlotly({
          
          
          
          RATIO <-  HEER %>% filter(ID %in% (df %>% filter(row_number() %in% (input$myDT_rows_selected)))) %>%
            select(Ratios) %>% pull
          
          plotsW <- plot_w %>% mutate(ROI = as.numeric(`thousands`) * as.numeric(RATIO) * 1000)
          
          #ggplot(plotsW, aes(thousands,ROI)) + 
          #  geom_line() + 
          #  geom_point() +
          #  labs(y = "ROI", x = "Investment in £1,000s")
          
          
          p <- ggplot(data = plotsW, aes(thousands,ROI,
                                         text = paste(paste0('Investment: £',format((thousands * 1000), big.mark=",", scientific=FALSE)),
                                                      paste0("<br>ROI: £",format((ROI), big.mark=",", scientific=FALSE))) 
                                         
                                         
                                         
          )) +
            geom_line(colour = "black") +
            geom_point(colour = "#408FA6")+
            labs(y = "ROI", x = "Investment in £1,000s") +
            ggtitle("Plot of investment in £1,000s by ROI")+
            theme(plot.title = element_text(size=8))
          ggplotly(p, tooltip = "text") 
          
        })
        
        
        
        
        output$distPlot2 <- renderPlotly({
          
          
          
          RATIO <-  HEER %>% filter(ID %in% (df %>% filter(row_number() %in% (input$myDT_rows_selected)))) %>%
            select(ICER_VALUES) %>% pull
          
          plotsW <- plot_w %>% mutate(QALYS = as.numeric(`thousands` * 1000) / as.numeric(RATIO))
          
          #ggplot(plotsW, aes(thousands,ROI)) + 
          #  geom_line() + 
          #  geom_point() +
          #  labs(y = "ROI", x = "Investment in £1,000s")
          
          
          p <- ggplot(data = plotsW, aes(thousands,QALYS,
                                         text = paste( paste0('Investment: £',format((thousands * 1000), big.mark=",", scientific=FALSE)),
                                                       "<br>Number of QALYS: ", QALYS) 
                                         
                                         
                                         
          )) +
            geom_line(colour = "black") +
            geom_point(colour = "#408FA6")+
            labs(y = "Number of QALYS", x = "Investment in £1,000s")+
            ggtitle("Plot of investment in £1,000s by QALYS gained")+
            theme(plot.title = element_text(size=8))
          ggplotly(p, tooltip = "text") 
          
        })
        
      })
      
      #text1
      
      #  })  
      
      #})
      
      #     if (!is.null(df2)) {
      #     
      #     updateSliderInput(session, "param_slide", value = (df2 %>%
      #                                                          select(Ratios)%>% pull))
      #     
      #     
      #     updateNumericInput(session, "param_numeric", value = (df2 %>%
      #                                                             select(Ratios)%>% pull))
      # }
      #return(IDS)
    }
    
    
    # updateSliderInput(session, "param_slide", value = (HEER %>% Filter(ID == IDS) %>%
    #                                                      select(Ratios)%>% pull))
    # 
    # updateNumericInput(session, "param_numeric", value = (HEER %>% Filter(ID == IDS) %>%
    #                                                         select(Ratios)%>% pull))
    # 
    # 
    
  })
  
  
  
  output[["ICER_MULTI"]] <- renderText({
    
    #paste0("£",input[["param_numeric"]] * (input$investment*1000))
    paste0('ICER filter ranage: £',format((input$ICER_FILTER[1]*1000), big.mark=",", scientific=FALSE), " to £",format((input$ICER_FILTER[2]*1000), big.mark=",", scientific=FALSE))
  })
  
  
  
  
  
  
  
  
  observeEvent(input$param_numeric, {
    
    updateSliderInput(session, "param_slide", value = input$param_numeric)
  }, ignoreInit = TRUE)
  observeEvent(input$param_slide, {
    updateNumericInput(session, "param_numeric", value = input$param_slide)
  }, ignoreInit = TRUE)
  
  
  
  
  #eventReactive(input$myDT, {
  
  
  
  
  
  
  
  
  #### PG 2 STUFF
  
  
  RESULTS2 <- eventReactive(input$submit2,{
    if(is.null(input$InputId2)){
      return()
    }
    
    df <- HEER %>% filter(ID == toupper(input$InputId2)) 
    
    
    #text1
    
  })  
  
  
  output$text1 <- renderText({paste("Theme: ", RESULTS2() %>% select(Theme) %>% pull)})  
  output$text2 <- renderText({paste("Activity: ", RESULTS2() %>% select(Activity) %>% pull)})
  output$text3 <- renderText({paste("Intervention: ", RESULTS2() %>% select(Intervention) %>% pull)})
  output$text4 <- renderText({paste("ID: ", RESULTS2() %>% select(ID) %>% pull)})
  
  output$text5 <- renderText({paste("Comparator: ", RESULTS2() %>% select(Comparator) %>% pull)})
  output$text6 <- renderText({paste("Benefit-cost ratio (BCR): ", RESULTS2() %>% select(`Benefit-cost ratio (BCR)`) %>% pull)})
  
  output$text7 <- renderText({paste("ICER: ")})
  output$text8 <- renderText({paste("Cost saving evidence: ")})
  output$text9 <- renderText({paste(RESULTS2() %>% select(ICER) %>% pull)})
  output$text10 <- renderText({paste(RESULTS2() %>% select(`Cost saving evidence`) %>% pull)})
  
  output$text11 <- renderText({paste(RESULTS2() %>% select(`Summary of evidence`) %>% pull)})
  
  output$text12 <- renderText({paste("Type of analysis: ")})
  output$text14 <- renderText({paste("Type of evidence: ")})
  output$text13 <- renderText({paste(RESULTS2() %>% select(`Type of analysis`) %>% pull)})
  output$text15 <- renderText({paste(RESULTS2() %>% select(`Type of evidence`) %>% pull)})
  
  output$text16 <- renderText({paste("Study population: ")})
  output$text17 <- renderText({paste(RESULTS2() %>% select(`Study population`) %>% pull)})
  
  output$text18 <- renderText({paste(RESULTS2() %>% select(`Assumptions / Caveats`) %>% pull)})
  
  output$text19 <- renderText({paste("Timeframe: ")})
  output$text21 <- renderText({paste("Who pays?: ")})
  output$text20 <- renderText({paste(RESULTS2() %>% select(`Timeframe`) %>% pull)})
  output$text22 <- renderText({paste(RESULTS2() %>% select(`Who pays?`) %>% pull)})
  
  output$text23 <- renderText({paste("Key outcome measure of evidence: ")})
  output$text24 <- renderText({paste(RESULTS2() %>% select(`Key outcome measure of evidence`) %>% pull)})
  
  output$text25 <- renderText({paste("Country: ")})
  output$text27 <- renderText({paste("Discount rate: ")})
  output$text29 <- renderText({paste("Currency: ")})
  output$text26 <- renderText({paste(RESULTS2() %>% select(`Country`) %>% pull)})
  output$text28 <- renderText({paste(RESULTS2() %>% select(`Discount rate`) %>% pull)})
  output$text30 <- renderText({paste(RESULTS2() %>% select(`Currency`) %>% pull)})
  
  output$text31 <- renderText({paste(RESULTS2() %>% select(`Reference to original source`) %>% pull)})
  
  ## glossary
  
  output$textG1 <- renderText({paste("The standard (for example, another intervention or usual care) against which an intervention is compared in a study. The comparator can be no intervention.")})
  output$textG2 <- renderText({paste("A form of economic evaluation that weighs the total expected costs against the total expected benefits by valuing both in monetary terms (for example, pounds sterling) to assess whether the benefits exceed the costs. It is measured using the benefit-cost ratio (BCR), which is the ratio of benefits relative to costs (benefits/costs).")})
  output$textG3 <- renderText({paste("The HEER defines an intervention as cost-effective if it is below the NICE cost per QALY threshold of £20,000.")})
  output$textG4 <- renderText({paste("Cost-effectiveness analysis (CEA) compares the relative costs and outcomes (effects) of two or more courses of action. The benefits are expressed in non-monetary terms related to health, such as symptom-free days or heart attacks avoided.")})
  output$textG5 <- renderText({paste("The most common approach to assess cost-effectiveness is to use cost-utility analysis (often interchanged with CEA), where the benefits are assessed in terms of both quality and duration of life, and expressed as quality-adjusted life years (QALYs) or disability-adjusted life years (DALYs). Utility refers to the measure of the preference or value that an individual or society places upon a particular health state.")})
  output$textG6 <- renderText({paste("An intervention is considered cost-saving when the monetary benefits of an intervention are greater than its costs. Monetary savings may not necessarily accrue as cash-releasing savings, for example due to reallocation of resources such as staffing.")})
  output$textG7 <- renderText({paste("A measure for the overall disease burden. It is designed to quantify the impact of premature death and disability on a population by combining them into a single, comparable measure.")})
  output$textG8 <- renderText({paste("A technique used to convert costs and benefits that occur in different time periods to 'present values', so that they can be compared. Since costs and benefits occurring in the future are generally considered less valuable than those occurring in the present, a discount rate is applied to future benefits.")})
  output$textG9 <- renderText({paste("When comparing tests or treatments, an option that is both less effective and costs more is said to be 'dominated' by the alternative. An option that is more effective and costs less is said to 'dominate' the alternative (see diagram in comment box).")})
  output$textG10 <- renderText({paste("How fair the distribution of resources is within society. ")})
  output$textG11<- renderText({paste("Also known as marginal analysis, it is an examination of the additional benefits of an activity compared to the additional costs incurred by that same activity.")})
  output$textG12 <- renderText({paste("The difference in the change in mean costs in the population of interest divided by the difference in the change in mean outcomes in the population of interest.  It provides the 'extra cost per extra unit of outcome', which can be compared to other interventions.")})
  output$textG13 <- renderText({paste("The present value of an investment's future net benefits minus net costs.")})
  output$textG14 <- renderText({paste("Productivity gains are the impact of an intervention on production in the economy.")})
  output$textG15 <- renderText({paste("A measure of the state of health of a person or group in which the benefits, in terms of length of life, are adjusted to reflect the quality of life. One QALY is equal to 1 year of life in perfect health. QALYs are calculated by estimating the years of life remaining for a patient following a particular treatment or intervention and weighting each year with a quality of life score (on a 0-1 scale).")})
  output$textG16 <- renderText({paste("Quasi-experiments contain a naturally occurring independent variable. However, in a quasi-experiment the naturally occurring independent variable is a difference between people that already exists (i.e. age or gender). The researcher examines the effect of this variable on the dependent variable.")})
  output$textG17 <- renderText({paste("A study in which a number of similar people are randomly assigned to two (or more) groups to test a specific drug, treatment or other intervention. One group (the experimental group) has the intervention being tested; the other (the comparison or control group) has an alternative intervention, a dummy intervention (placebo) or no intervention at all.")})
  output$textG18 <- renderText({paste("A general term encompassing the techniques for comparing the costs and benefits generated by an investment. The HEER uses the benefit-cost ratio as the measure of ROI.")})
  output$textG19 <- renderText({paste("A form of modelling that evaluates the impact of alternative values for some of the model parameters and other aspects of the analytical method. It is applied particularly to the investigation of uncertainty around the values of parameter inputs.")})
  output$textG20 <- renderText({paste("A computer simulation technique that allows the user to anticipate the effect of interventions in complex situations with interdependent variables.")})
  output$textG21 <- renderText({paste("The maximum amount an individual is willing to pay to acquire a good or a service, or the maximum amount an individual is willing to pay to avoid a prospective loss.")})
  #output$textG1 <- renderText({paste("The standard (for example, another intervention or usual care) against which an intervention is compared in a study. The comparator can be no intervention.")})
  
}

