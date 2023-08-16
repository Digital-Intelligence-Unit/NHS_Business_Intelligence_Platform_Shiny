library(shiny)
library(shinythemes)
library(dplyr)
library(ggplot2)
library(plotly)
library(tidyverse)
library(lubridate)
library(scales)
library(NHSRplotthedots)
library(runcharter)
library(qicharts2)
library(forecast)
library(prophet)



str_to_title_nhs_vectorized <- function(x) {
  words <- str_to_lower(x)
  words <- str_to_title(words)
  words <- str_replace_all(words, regex("nhs", ignore_case = TRUE), "NHS")
  words <- str_replace_all(words, regex("\\bAnd\\b", ignore_case = TRUE), "and")
  words <- str_replace_all(words, regex("\\bmri\\b", ignore_case = TRUE), "MRI")
  words <- str_replace_all(words, regex("\\bct\\b", ignore_case = TRUE), "CT")
  
  words
}



data <- read.csv("ENDOSCOPY_DM01_ALL_grouped_V3_15_08_23.csv", header=TRUE)

lookup <- data.frame(
  original = c("AUDIOLOGY_ASSESSMENTS", "BARIUM_ENEMA", "COLONOSCOPY", "CT", "CYSTOSCOPY",
               "DEXA_SCAN", "ECHOCARDIOGRAPHY", "ELECTROPHYSIOLOGY", "FLEXI_SIGMOIDOSCOPY",
               "GASTROSCOPY", "MRI", "NON_OBSTETRIC_ULTRASOUND", "PERIPHERAL_NEUROPHYS",
               "SLEEP_STUDIES", "TOTAL", "URODYNAMICS", "AUDIOLOGY - AUDIOLOGY ASSESSMENTS",
               "BARIUM ENEMA", "CARDIOLOGY - ECHOCARDIOGRAPHY", "CARDIOLOGY - ELECTROPHYSIOLOGY",
               "COMPUTED TOMOGRAPHY", "DEXA SCAN", "FLEXI SIGMOIDOSCOPY",
               "MAGNETIC RESONANCE IMAGING", "NEUROPHYSIOLOGY - PERIPHERAL NEUROPHYSIOLOGY",
               "NON-OBSTETRIC ULTRASOUND", "RESPIRATORY PHYSIOLOGY - SLEEP STUDIES",
               "URODYNAMICS - PRESSURES & FLOWS"),
  cleaned = c("AUDIOLOGY ASSESSMENTS", "BARIUM ENEMA", "COLONOSCOPY", "CT", "CYSTOSCOPY",
              "DEXA SCAN", "ECHOCARDIOGRAPHY", "ELECTROPHYSIOLOGY", "FLEXI SIGMOIDOSCOPY",
              "GASTROSCOPY", "MRI", "NON OBSTETRIC ULTRASOUND", "PERIPHERAL NEUROPHYS",
              "SLEEP STUDIES", "TOTAL", "URODYNAMICS", "AUDIOLOGY ASSESSMENTS",
              "BARIUM ENEMA", "ECHOCARDIOGRAPHY", "ELECTROPHYSIOLOGY", "CT", "DEXA SCAN",
              "FLEXI SIGMOIDOSCOPY", "MRI", "PERIPHERAL NEUROPHYS",
              "NON OBSTETRIC ULTRASOUND", "SLEEP STUDIES", "URODYNAMICS")
)

# Use left_join to join df with lookup
data <- data %>%
  mutate(Diagnostic.Tests = toupper(Diagnostic.Tests)) %>%
  left_join(lookup, by = c("Diagnostic.Tests" = "original")) %>%
  mutate(Diagnostic.Tests = ifelse(is.na(cleaned), Diagnostic.Tests, cleaned)) %>%
  rename_with(.fn = ~ gsub("\\.", " ", .)) %>%
  select(-cleaned) %>%
  filter(`Diagnostic Tests` != "TOTAL") %>%
  mutate(`Diagnostic Tests` = str_to_title_nhs_vectorized(`Diagnostic Tests`)) %>%
  mutate(`Provider Org Name` = str_to_title_nhs_vectorized(`Provider Org Name`)) %>%
  mutate(`Provider Parent Name` = str_to_title_nhs_vectorized(`Provider Parent Name`)) 



## clean duplicated test names



add_line_breaks <- function(string, words_per_line) {
  words <- strsplit(string, " ")[[1]]
  word_chunks <- split(words, ceiling(seq_along(words) / words_per_line))
  lines <- sapply(word_chunks, paste, collapse = " ")
  paste(lines, collapse = "\n")
}

get_rebase_dates <- function(shift_dates) {
  rebase_list <- shift_dates %>%
    group_by(group_by) %>%
    summarize(rebase_dates = list(as.Date(start_date))) %>%
    deframe()
  
  # Convert the list to a named list of date vectors
  rebase_list <- lapply(rebase_list, function(x) as.Date(x, origin = "1970-01-01"))
  
  return(rebase_list)
}


generate_spc_chart <- function(data, provider, test, date_range, rebase_dates, value_field,rebase_mode, run_length) {
   
  # Filter the data based on selected providers and tests
  data2 <- data %>%
    filter(`Provider Org Name` %in% provider,
           `Diagnostic Tests` %in% test,
           Date >= date_range[1],
           Date <= date_range[2]) %>%
    group_by(Date) %>%
    summarise(measure = sum(!!as.symbol(value_field)), .groups = "drop") %>%
    rename(date = Date) %>%
    mutate(date = as.Date(date, format="%Y-%m-%d")) %>%
    mutate(`Provider Parent Name` = 'ICB')

  # Add this block to determine rebase_dates based on rebase_mode
  if (value_field == "Total WL") {
    direction <- "decrease" 
  }else {
    direction <- "increase" 
  }
  if (rebase_mode == "Auto") {
    run_chart <- runcharter(data2,
                            med_rows = 12,
                            runlength = run_length,
                            direction = direction,
                            datecol = date, 
                            grpvar = `Provider Parent Name`,
                            yval = measure, 
                            chart_title = "Runs identified",
                            chart_subtitle = "Runs below the median signalling improvement")
    
    #significant_shifts <<- run_chart#$rule_violations
    #print(significant_shifts)
    rebase_dates <- run_chart$sustained$start_date
    print(rebase_dates)
  } else {
    rebase_dates <- as.Date(rebase_dates, format="%Y-%m-%d")
  }
  
  spc_chart <- ptd_spc(data2, measure, date, rebase = ptd_rebase(as.Date(rebase_dates, format="%Y-%m-%d")),improvement_direction = direction) %>% 
    plot(fixed_y_axis_multiple = FALSE, x_axis_breaks = "3 months",point_size = 1.5)
  
  
  spc_chart <- spc_chart + theme(axis.text.x = element_text(size = 6, angle = 45)) + ylab(value_field) + labs(
    title = paste("Selected Providers:", paste(provider, collapse = ", "),
                  "Selected Tests:", paste(test, collapse = ", "),
                  sep = "\n"))
  
  return(spc_chart)
}

generate_facet_spc_chart <- function(data, group_by, facet_field, date_range, rebase_dates, value_field, provider_filter, test_filter,rebase_mode,run_length) {
  #print(test_filter)
  
  # Filter the data
  df <- data %>%
    filter(`Provider Org Name` %in% provider_filter,
           `Diagnostic Tests` %in% test_filter,
           Date >= date_range[1],
           Date <= date_range[2]) %>%
    rename(date = Date) %>%
    mutate(date = as.Date(date, format="%Y-%m-%d"))

  data3 <- df %>%
    group_by(date,!!sym(facet_field)) %>%
    summarise(Total = sum(!!sym(value_field)), .groups = "drop")
  
   
  temp_data <- data3 %>%
    dplyr::select(date, !!as.symbol(facet_field), Total) %>%
    setNames(c("date", "group_by", "value_field"))
  
  total_string <- add_line_breaks(paste("SPC of ",value_field, paste0("(", paste(unique(df[[group_by]]), collapse = " + "), ")"), sep = " "),8)
  
  # Print the string
  print(total_string)
  
  if (value_field == "Total WL") {
    direction <- "decrease" 
  }else {
    direction <- "increase" 
  }
  if (rebase_mode == "Auto") {
    run_chart <- runcharter(temp_data,
                            med_rows = 12,
                            runlength = run_length,
                            direction = direction,
                            datecol = date, 
                            grpvar = group_by,
                            yval = value_field, 
                            chart_title = "Runs identified",
                            chart_subtitle = "Runs below the median signalling improvement")
    
    rebase_dates <- get_rebase_dates(run_chart$sustained)

    print(rebase_dates)
    
    spc_chart <- ptd_spc(data3, Total, date, facet_field = !!as.symbol(facet_field), rebase = do.call(ptd_rebase, rebase_dates), improvement_direction = "decrease") %>%
      plot(fixed_y_axis_multiple = FALSE, x_axis_breaks = "6 months",point_size = 1.5)
    
    spc_chart$facet <- facet_grid(f ~ ., scales = "free")
    spc_chart$data$f <- sapply(spc_chart$data$f, add_line_breaks, words_per_line = 3)
    
    spc_chart <- spc_chart + theme(axis.text.x = element_text(size = 6, angle = 45))+ ggtitle(total_string) + ylab(value_field)
  } else {
    rebase_dates <- as.Date(rebase_dates, format="%Y-%m-%d")
    
    spc_chart <- ptd_spc(data3, Total, date, facet_field = !!as.symbol(facet_field), rebase = ptd_rebase(as.Date(rebase_dates, format="%Y-%m-%d")), improvement_direction = "decrease") %>%
      plot(fixed_y_axis_multiple = FALSE, x_axis_breaks = "6 months",point_size = 1.5)
    
    spc_chart$facet <- facet_grid(f ~ ., scales = "free")
    spc_chart$data$f <- sapply(spc_chart$data$f, add_line_breaks, words_per_line = 3)
    
    spc_chart <- spc_chart + theme(axis.text.x = element_text(size = 6, angle = 45))+ ggtitle(total_string) + ylab(value_field)
  }
  
  
  
  return(spc_chart)
}


library(shinyWidgets)

server <- function(input, output, session) {
  
  reactiveData <- reactiveValues()

  reactiveDataSPC <- reactiveValues()

  decomposition_toggle <- TRUE
    
  observeEvent(input$commissioner, {
    # Filter the data based on the selected commissioner
    reactiveDataSPC$filtered_data <- data %>% filter(`Provider Parent Name` %in% input$commissioner)
    
    # Update the provider select input with the filtered data
    updateSelectInput(
      session,
      "provider",
      choices = unique(reactiveDataSPC$filtered_data$`Provider Org Name`),
      selected = unique(reactiveDataSPC$filtered_data$`Provider Org Name`)[1]
    )
  }, ignoreNULL = FALSE)
  
  output$provider_ui <- renderUI({
    selectInput("provider", "Provider",
                choices = unique(reactiveDataSPC$filtered_data$`Provider Org Name`),
                
                multiple = TRUE)
  })
  
  output$test_ui <- renderUI({
    selectInput("test", "Diagnostic Test",
                choices = unique(data$`Diagnostic Tests`),
                selected = 'CT',
                multiple = TRUE)
  })
  

  facet_field_choice <- reactiveVal("Provider Org Name")
  
  observe({
    if (input$facet_field) {
      facet_field_choice("Diagnostic Tests")
    } else {
      facet_field_choice("Provider Org Name")
    }
  })
  
  
  observeEvent(input$facet_toggle, {
    req(input$commissioner)
    updateSelectInput(session, "provider", selected = input$provider)
    updateSelectInput(session, "test", selected = input$test)
  })
  
  
  ### adding functionality 
  
  # Observe event for generating model
  observeEvent(input$submit, {
    
    id_message_spc <- showNotification(
      "Generating SPC... Please wait.",
      type = "message",
      duration = NULL # Makes the notification stay indefinitely
    )
    #showNotification("Generating SPC... Please wait.", type = "message")
    if (!input$facet_toggle) {
    spc_chart_data <- eventReactive(input$submit, {
      #if (input$facet_toggle) return(NULL)
      
      req(input$provider, input$test, input$date_range,input$rebase_mode)
      
      rebase_dates <- strsplit(input$rebase_dates, ",")[[1]]
      
      chart <- generate_spc_chart(data, input$provider, input$test, input$date_range, rebase_dates, input$value_field,input$rebase_mode,input$run_length)
      #chart$data$point_type <- str_to_title(gsub("_"," ",chart$data$point_type))
      removeNotification(id_message_spc)
      return(chart)
    }, ignoreNULL = TRUE)
    
    
    
    
    output$spc_chart <- renderPlotly({
      if (!input$facet_toggle) {
        
        req(spc_chart_data())
        ggplotly(spc_chart_data())
        
      }
    })
    
    output$facet_spc_chart <- renderPlotly({NULL})
    
    }else if (input$facet_toggle) {
    
    facet_spc_chart_data <- eventReactive(input$submit, {
      #if (!input$facet_toggle) return(NULL)
      
      req(input$date_range, facet_field_choice(),input$rebase_mode)
      rebase_dates <- strsplit(input$rebase_dates, ",")[[1]]
      group_bys <- setdiff(c("Provider Org Name", "Diagnostic Tests"), facet_field_choice())
      facet_field_choice <- facet_field_choice()
      print(facet_field_choice)
      chart <- generate_facet_spc_chart(data, group_bys, facet_field_choice, input$date_range, rebase_dates, input$value_field, input$provider, input$test,input$rebase_mode,input$run_length)
      #chart$data$point_type <- str_to_title(gsub("_"," ",chart$data$point_type))
      removeNotification(id_message_spc)
      #reactivet1$t1 <-chart

      return(chart)
    }, ignoreNULL = TRUE)
    
    output$facet_spc_chart <- renderPlotly({
      if (input$facet_toggle) {
       
        req(facet_spc_chart_data())
        ggplotly(facet_spc_chart_data())
        
      }
    })
    output$spc_chart <- renderPlotly({NULL})
    }
  })
  
  ############################## TIME SERIES PARTS #####################
  
  observeEvent(input$commissioner_ts, {
    # Filter the data based on the selected commissioner
    reactiveData$filtered_data_ts <- data %>% filter(`Provider Parent Name` %in% input$commissioner_ts)
    #filtered_data_ts <<- data %>% filter(`Provider Parent Name` %in% input$commissioner_ts)
    
    # Update the provider select input with the filtered data
    updateSelectInput(
      session,
      "provider_ts",
      choices = unique(reactiveData$filtered_data_ts$`Provider Org Name`)
    )
  }, ignoreNULL = FALSE)
  
  output$provider_ui_ts <- renderUI({
    selectInput("provider_ts", "Provider",
                choices = unique(reactiveData$filtered_data_ts$`Provider Org Name`),
                multiple = TRUE,
                selected = "Blackpool Teaching Hospitals NHS Foundation Trust")
  })
  
  output$test_ui_ts <- renderUI({
    selectInput("test_ts", "Diagnostic Test",
                choices = unique(data$`Diagnostic Tests`),
                multiple = TRUE,
                selected = "Colonoscopy")
  })
  
  # Observe event for generating model

  output$decomposition_plot_ui <- renderUI({
    if (decomposition_toggle) {
    if (input$model_type == "Auto ARIMA") {
      plotlyOutput("decomposition_plot")
    } else if (input$model_type == "Prophet") {
      plotlyOutput("decomposition_plot")
    }
    }
  })
  
  output$time_series_plot_ui <- renderUI({
   
      plotlyOutput("time_series_plot")
   
  })
  
  
  observeEvent(input$generate_model, {
    # Notify the user that the button was pressed
    plottitle <- input$value_field_ts
    id_message <- showNotification(
      "Generating model... Please wait.",
      type = "message",
      duration = NULL # Makes the notification stay indefinitely
    )
      
    
    filtered_data <- data %>%
      filter(
        `Provider Parent Name` %in% input$commissioner_ts,
        `Provider Org Name` %in% input$provider_ts,
        `Diagnostic Tests` %in% input$test_ts,
        Date >= input$date_range_ts[1],
        Date <= input$date_range_ts[2]
      ) %>%
      group_by(Date) %>%
      summarise(Total = sum(!!sym(input$value_field_ts)), .groups = "drop")
    
    
    # Prepare the time series data
    #ts_data <- ts(filtered_data[[input$value_field_ts]], frequency = 12) # Monthly frequency
    reactiveData$ts_data <- ts(filtered_data$Total, frequency = 12) # Monthly frequency
    #ts_data <<- ts(filtered_data$Total, frequency = 12) # Monthly frequency
    # Generate the model based on the selected type
    if (input$model_type == "Prophet") {
      prophet_data <- filtered_data %>%
        dplyr::rename(ds = Date, y = Total)
      
      
      output$model_summary <- renderText({
        NULL
      })
      
      # Generate Prophet model
      prophet_model <- prophet(df = prophet_data)
      
      #Make future dataframe
      future_df <- prophet::make_future_dataframe(prophet_model, periods = input$predict_months, freq = "month")
      
      # Forecast using the fitted model
      forecast_prophet <- predict(prophet_model, future_df)
            
        output$decomposition_plot <- renderPlotly({
          if (decomposition_toggle) {
          req(decomposition_toggle)
          
          if (decomposition_toggle) {
            # Check if the time series has a seasonal component
            if (frequency(reactiveData$ts_data) > 1) {
              decomposed <- tryCatch({
                stats::stl(reactiveData$ts_data, s.window = "periodic")
              }, error = function(e) {
                NULL
              })
              
              if (!is.null(decomposed)) {
                # Extract dates from original data frame
                historical_dates <- filtered_data$Date
                
                # Convert decomposed time series to data frame with Date column
                decomposed_df <- data.frame(Date = historical_dates, Decomposed = as.data.frame(decomposed$time.series))
                
                # Create separate plots for each component
                p1 <- plotly::plot_ly(data = decomposed_df, x = ~Date, y = ~Decomposed.seasonal, name = "Seasonal", type = "scatter", mode = "lines")
                p2 <- plotly::plot_ly(data = decomposed_df, x = ~Date, y = ~Decomposed.trend, name = "Trend", type = "scatter", mode = "lines")
                p3 <- plotly::plot_ly(data = decomposed_df, x = ~Date, y = ~Decomposed.remainder, name = "Remainder", type = "scatter", mode = "lines")
                
                # Combine plots into a single plot with multiple subplots
                subplots <- plotly::subplot(p1, p2, p3)
                subplots <- subplots %>% layout(title = "Decomposition Plots")
                subplots
              } else {
                "No seasonal component detected in the time series."
              }
            } else {
              "Time series has no frequency information. Unable to perform STL decomposition."
            }
          } else {
            NULL
          }
          
        }
        })

      
      # Generate time series plot with predictions
      output$time_series_plot <- renderPlotly({
        # Subset data into historical and forecasted
        historical_data <- prophet_data
        forecasted_data <- forecast_prophet[(nrow(forecast_prophet)-input$predict_months+1):nrow(forecast_prophet), ]
        
        # Create plot with plotly
        plotly_forecast <- plotly::plot_ly() %>%
          # Historical data
          plotly::add_trace(x = historical_data$ds, y = historical_data$y, name = "Historical", type = "scatter", mode = "lines", line = list(color = "blue")) %>%
          # Forecasted data
          plotly::add_trace(x = forecasted_data$ds, y = forecasted_data$yhat, name = "Forecast", type = "scatter", mode = "lines", line = list(color = "red")) %>%
          # Confidence intervals for forecasted data
          plotly::add_ribbons(x = forecasted_data$ds, ymin = forecasted_data$yhat_lower, ymax = forecasted_data$yhat_upper, name = "Confidence Interval", fillcolor = "rgba(255, 127, 80, 0.3)", line = list(color = "transparent")) %>%
          # Layout
          plotly::layout(title = "Prophet Model Forecast", yaxis = list(title = paste0(plottitle)), xaxis = list(title = "Date"))
        
        return(plotly_forecast)
      })
      
      
    } else if (input$model_type == "Auto ARIMA") {
      # Generate Auto ARIMA model
      model <- reactive({
        forecast::auto.arima(reactiveData$ts_data)
      })
      
      output$model_summary <- renderText({
        t1 <-summary(model())
        paste(t1)
      })
      
      # Generate decomposition plots if toggle is TRUE
      output$decomposition_plot <- renderPlotly({
        req(decomposition_toggle, model())
        
        if (decomposition_toggle) {
          # Check if the time series has a seasonal component
          if (frequency(reactiveData$ts_data) > 1) {
            decomposed <- tryCatch({
              stats::stl(reactiveData$ts_data, s.window = "periodic")
            }, error = function(e) {
              NULL
            })
            
            if (!is.null(decomposed)) {
              # Extract dates from original data frame
              historical_dates <- filtered_data$Date
              
              # Convert decomposed time series to data frame with Date column
              decomposed_df <- data.frame(Date = historical_dates, Decomposed = as.data.frame(decomposed$time.series))
              
              # Create separate plots for each component
              p1 <- plotly::plot_ly(data = decomposed_df, x = ~Date, y = ~Decomposed.seasonal, name = "Seasonal", type = "scatter", mode = "lines")
              p2 <- plotly::plot_ly(data = decomposed_df, x = ~Date, y = ~Decomposed.trend, name = "Trend", type = "scatter", mode = "lines")
              p3 <- plotly::plot_ly(data = decomposed_df, x = ~Date, y = ~Decomposed.remainder, name = "Remainder", type = "scatter", mode = "lines")
              
              # Combine plots into a single plot with multiple subplots
              subplots <- plotly::subplot(p1, p2, p3)
              subplots <- subplots %>% layout(title = "Decomposition Plots")
              subplots
            } else {
              "No seasonal component detected in the time series."
            }
          } else {
            "Time series has no frequency information. Unable to perform STL decomposition."
          }
        } else {
          NULL
        }
      })

      # Generate time series plot with predictions
      output$time_series_plot <- renderPlotly({
        req(model())
        
        # Forecast using the fitted model
        fore.xts <- forecast::forecast(model(), h = input$predict_months)
        
        # Extract dates from original data frame
        historical_dates <- as.Date(filtered_data$Date, format="%Y-%m-%d")
        
        # Generate future dates
        future_dates <- seq(max(historical_dates), length.out = input$predict_months + 1, by = "month")[-1]
        
        # Combine historical and forecasted values into data frames
        historical_df <- data.frame(Date = historical_dates, Value = reactiveData$ts_data)
        forecast_df <- data.frame(Date = future_dates, Value = as.vector(fore.xts$mean), Lower = fore.xts$lower[, 2], Upper = fore.xts$upper[, 2])
        
        # Create plot with plotly
        plotly_forecast <- plotly::plot_ly() %>%
          plotly::add_trace(data = historical_df, x = ~Date, y = ~Value, name = "Historical", type = "scatter", mode = "lines", line = list(color = "blue")) %>%
          plotly::add_trace(data = forecast_df, x = ~Date, y = ~Value, name = "Forecast", type = "scatter", mode = "lines", line = list(color = "red")) %>%
          plotly::add_ribbons(data = forecast_df, x = ~Date, ymin = ~Lower, ymax = ~Upper, name = "Confidence Interval", fillcolor = "rgba(255, 127, 80, 0.3)", line = list(color = "transparent")) %>%
          plotly::layout(title = paste("ARIMA Model Forecast"), yaxis = list(title = plottitle), xaxis = list(title = "Date"))
        
        return(plotly_forecast)
      })
      
      
    }
    
    removeNotification(id_message)
  })
  
  
}



