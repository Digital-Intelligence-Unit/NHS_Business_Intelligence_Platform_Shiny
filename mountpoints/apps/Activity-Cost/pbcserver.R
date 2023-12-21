########### PBC STUFF ##############
# output$dynamicTitleProgram <- renderUI({
#   tags$h1(paste0("PBC: ",input$program))
# })

# Function to calculate metrics
calculate_metrics <- function(data) {
  ss_total <- sum((data$`Total Cost` - mean(data$`Total Cost`))^2)
  ss_residual <- sum((data$`Total Cost` - data$predicted)^2)
  r2 <- 1 - (ss_residual / ss_total)
  
  list(
    r2 = r2,
    mae = Metrics::mae(data$`Total Cost`, data$predicted),
    rmse = Metrics::rmse(data$`Total Cost`, data$predicted)
  )
}



## cards

output$RsquaredBox_pbc <- renderValueBox({
  metrics <- calculate_metrics(df_pbc %>% filter(`Programme Category Name` == input$program, gpp_short_name %!in% (input$excludeGP)))
  valueBox(
    paste0(round(metrics$r2, 2)),
    "R-squared", 
    icon = icon('chart-line'),
    color = "green"
  )
})

output$MAEBox_pbc <- renderValueBox({
  metrics <- calculate_metrics(df_pbc %>% filter(`Programme Category Name` == input$program, gpp_short_name %!in% (input$excludeGP)))
  valueBox(
    paste0(round(metrics$mae, 2)),
    "Mean Absolute Error", 
    icon = icon('chart-area'),
    color = "yellow"
  )
})

output$RMSEBox_pbc <- renderValueBox({
  metrics <- calculate_metrics(df_pbc %>% filter(`Programme Category Name` == input$program, gpp_short_name %!in% (input$excludeGP)))
  valueBox(
    paste0(round(metrics$rmse, 2)),
    "Root Mean Squared Error", 
    icon = icon('chart-bar'),
    color = "red"
  )
})


## plots

output$programSPC <- renderPlotly({

  dollar_formatter <- scales::dollar_format(prefix = "£", big.mark = ",", accuracy = 1)
  
  data <- monthcostdf %>%
    left_join(gp_lookup) %>%
    left_join(pcn_lookup) %>%
    left_join(Programme_Budgeting_Mappings_Definitions , by = c("ProgrammeBudgetCode" = "PBC02")) %>%
    filter(`Programme Category Name` == input$program, gpp_short_name %!in% (input$excludeGP)) %>%
    arrange(ProgrammeBudgetCode, date) #%>% # Sort the dataframe by ID and date
    #group_by(ProgrammeBudgetCode, date) %>%
    #summarise(sum = sum(sum, na.rm=T)) %>% 
    #ungroup()# %>%
    
  
run_chart <- runcharter(data %>% group_by(ProgrammeBudgetCode, date) %>%
                          summarise(sum = sum(sum, na.rm=T)) %>% 
                          ungroup(),
                          med_rows = 12,
                          runlength = 6,
                          direction = 'decrease',
                          datecol = date, 
                          grpvar = `ProgrammeBudgetCode`,
                          yval = sum, 
                          chart_title = "Runs identified",
                          chart_subtitle = "Runs below the median signalling improvement")
  

  rebase_dates <- run_chart$sustained$start_date
  print(rebase_dates)
  spc_chart <- ptd_spc(data %>% group_by(ProgrammeBudgetCode, date) %>%
                         summarise(sum = sum(sum, na.rm=T)) %>% 
                         ungroup(), sum, date, rebase = ptd_rebase(as.Date(rebase_dates, format="%Y-%m-%d")),improvement_direction = 'increase') %>% 
    plot(fixed_y_axis_multiple = FALSE, x_axis_breaks = "3 months",point_size = 1.5)
  
  spc_chart <- spc_chart + theme(axis.text.x = element_text(size = 6, angle = 45)) + ylab('Cost') + labs(
    title = paste("SPC of ", input$program))+
    labs(color = "Point Type")+
    scale_y_continuous(labels = scales::dollar_format(prefix = "£", big.mark = ",", accuracy = 1)) +
    aes(text = paste("Point Type:", `point_type` , "\n", "Date:", as.Date(x), "\n" , "Cost:" ,
     dollar_formatter(y)))
  
  
  spc_chart$data <- spc_chart$data %>%
             mutate(point_type = case_when(
               point_type == "special_cause_concern" ~ "special cause low",
               point_type == "special_cause_improvement" ~ "special cause high",
               point_type == "common_cause" ~ "expected variation",
               TRUE ~ point_type
           ))
          
          
          spc_chart <-(spc_chart +
                 scale_color_manual(
                 values = c(
                       'common_cause' = '#7B7D7D', 
                       'special_cause_concern' = '#fab428', 
                       'special_cause_improvement' = '#289de0'
                     ),
                 labels = c(
                       'common_cause' = 'expected variation', 
                       'special_cause_concern' = 'special cause low', 
                       'special_cause_improvement' = 'special cause high'
                   )
                ))
          
          spc_chart <- ggplotly( spc_chart + scale_color_manual(
                         values = c(
                                  'expected variation' = '#7B7D7D', 
                                   'special cause low' = '#fab428', 
                                    'special cause high' = '#289de0'
                                      )),tooltip = c( "text"))
          
          pbc_selected <- input$GP_H  # Get the selected PBCs
          data$Legend <- ifelse(data$gpp_short_name %in% pbc_selected, 'Selected', 'Not Selected')
          if (any(data$Legend == "Selected")) {
            
            run_chart2 <- runcharter(subset(data, Legend == "Selected") %>% group_by(ProgrammeBudgetCode, date) %>%
                                       summarise(sum = sum(sum, na.rm=T)) %>% 
                                       ungroup(),
                                     med_rows = 12,
                                     runlength = 6,
                                     direction = 'decrease',
                                     datecol = date, 
                                     grpvar = `ProgrammeBudgetCode`,
                                     yval = sum, 
                                     chart_title = "Runs identified",
                                     chart_subtitle = "Runs below the median signalling improvement")
            
            
            rebase_dates2 <- run_chart2$sustained$start_date
            print(rebase_dates2)
            spc_chart2 <- ptd_spc(subset(data, Legend == "Selected")%>% group_by(ProgrammeBudgetCode, date) %>%
                                    summarise(sum = sum(sum, na.rm=T)) %>% 
                                    ungroup(), sum, date, rebase = ptd_rebase(as.Date(rebase_dates2, format="%Y-%m-%d")),improvement_direction = 'increase') %>% 
              plot(fixed_y_axis_multiple = FALSE, x_axis_breaks = "3 months",point_size = 1.5)
            
            spc_chart2 <- spc_chart2 + theme(axis.text.x = element_text(size = 6, angle = 45)) + ylab('Cost') + labs(
              title = paste("SPC of Highligted GPs", input$GP_H ))+
              labs(color = "Point Type")+
              scale_y_continuous(labels = scales::dollar_format(prefix = "£", big.mark = ",", accuracy = 1)) +
              aes(text = paste("Point Type:", `point_type` , "\n", "Date:", as.Date(x), "\n" , "Cost:" ,
                               dollar_formatter(y)))
            
            
            spc_chart2$data <- spc_chart2$data %>%
              mutate(point_type = case_when(
                point_type == "special_cause_concern" ~ "special cause low",
                point_type == "special_cause_improvement" ~ "special cause high",
                point_type == "common_cause" ~ "expected variation",
                TRUE ~ point_type
              ))
            
            
            spc_chart2 <-(spc_chart2 +
                            scale_color_manual(
                              values = c(
                                'common_cause' = '#7B7D7D', 
                                'special_cause_concern' = '#fab428', 
                                'special_cause_improvement' = '#289de0'
                              ),
                              labels = c(
                                'common_cause' = 'expected variation', 
                                'special_cause_concern' = 'special cause low', 
                                'special_cause_improvement' = 'special cause high'
                              )
                            ))
            
            spc_chart2 <- ggplotly( spc_chart2 + scale_color_manual(
              values = c(
                'expected variation' = '#7B7D7D', 
                'special cause low' = '#fab428', 
                'special cause high' = '#289de0'
              )),tooltip = c( "text")) 
            
            annotations <- list(
              list(
                text =  paste("SPC of ", input$program),
                font = list(size = 16),
                xref = "paper",
                yref = "paper",
                x = 0.25,  # Adjust this for your specific plot's alignment
                y = 1.05,  # This positions the title just above the plot
                xanchor = "center",
                yanchor = "bottom",
                showarrow = FALSE
              ),
              list(
                text = paste("SPC of Highlighted GPs"),
                font = list(size = 16),
                xref = "paper",
                yref = "paper",
                x = 0.75,  # Adjust this for your specific plot's alignment
                y = 1.05,
                xanchor = "center",
                yanchor = "bottom",
                showarrow = FALSE
              )
            )
            
            subplot(spc_chart  %>% layout(
              showlegend = TRUE,
              title = ""
            ),
            spc_chart2 %>% layout(
              showlegend = FALSE,
              title = ""#,
              
            ),
            nrows = 1, margin = 0.05) %>% 
              layout(annotations = annotations)
          }
          else{
            spc_chart
          }  
})


observe({
    if(input$pbclogs == FALSE && input$switchViewPBC == FALSE) { # GP

    output$programPlot1 <- renderPlotly({
  data <- df_pbc %>% filter(`Programme Category Name` == input$program, gpp_short_name %!in% (input$excludeGP))
  residuals <- data$`Total Cost` - data$predicted
  
  pbc_selected <- input$GP_H  # Get the selected PBCs
  data$Legend <- ifelse(data$gpp_short_name %in% pbc_selected, 'Selected', 'Not Selected')
  
  
  p <- ggplot(data, aes(x = `Total Cost`, y = predicted, text = paste("GP Code:", gpp_short_name,
            "\nActual:" , dollar_formatter(`Total Cost`), "\nPredicted:", dollar_formatter(predicted)))) +
    geom_point(aes(color = Legend)) +
    scale_color_manual(values = c('Selected' = 'red', 'Not Selected' = 'black')) +
    geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
    labs(x = "Actual", y = "Predicted") +
    scale_y_continuous(labels = scales::dollar_format(prefix = "£", big.mark = ",", accuracy = 1)) +
    scale_x_continuous(labels = scales::dollar_format(prefix = "£", big.mark = ",", accuracy = 1)) +
    ggtitle(paste("Actual vs Predicted by Program Code", input$program)) +
    theme_minimal()
  
  ggplotly(p, tooltip = "text")
})



    } else if(input$pbclogs == TRUE && input$switchViewPBC == FALSE) { # PCN

    output$programPlot <- renderPlotly({
  data <- df_pbc %>% filter(`Programme Category Name` == input$program, gpp_short_name %!in% (input$excludeGP))
  residuals <- data$`Total Cost` - data$predicted
  
  pbc_selected <- input$GP_H  # Get the selected PBCs
  data$Legend <- ifelse(data$gpp_short_name %in% pbc_selected, 'Selected', 'Not Selected')
  
  # Add a small constant if needed, for example:
  # data$`Total Cost` <- data$`Total Cost` + 0.01
  # data$predicted <- data$predicted + 0.01
  
  p <- ggplot(data, aes(x = `Total Cost`, y = predicted, text = paste("GP Code:", gpp_short_name,
     "\nActual:" , dollar_formatter(`Total Cost`), "\nPredicted:", dollar_formatter(predicted)))) +
    geom_point(aes(color = Legend)) +
    scale_color_manual(values = c('Selected' = 'red', 'Not Selected' = 'black')) +
    geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
    labs(x = "Actual", y = "Predicted") +
    scale_y_continuous(labels = scales::dollar_format(prefix = "£", big.mark = ",", accuracy = 1), trans = "log10") +
    scale_x_continuous(labels = scales::dollar_format(prefix = "£", big.mark = ",", accuracy = 1), trans = "log10") +
    ggtitle(paste("Actual vs Predicted by Program Code", input$program)) +
    theme_minimal()
  
  ggplotly(p, tooltip = "text")
})




}
  })




observe({
    if(input$absrelpbc == FALSE && input$switchViewPBC == FALSE) { # GP
      # Adjust logic for 

      output$programResidualPlot1 <- renderPlotly({
  data <- df_pbc %>% filter(`Programme Category Name` == input$program, gpp_short_name %!in% (input$excludeGP)) %>%
    arrange(`Place`, gpp_short_name)
  
  data$`gpp_short_name` <- factor(data$`gpp_short_name`, levels = rev(unique(data$`gpp_short_name`)))
  data$`Place` <- factor(data$`Place`, levels = rev(unique(data$`Place`)))
  
  pbc_selected <- input$GP_H  # Get the selected PBCs
  data$Legend <- ifelse(data$gpp_short_name %in% pbc_selected, 'Selected', 'Not Selected')
  residuals <- (data$`Total Cost` - data$predicted)
  
  palette_name <- "Set1"
  brewer_palette <- brewer.pal(min(12, length(unique(data$`Place`))), palette_name)
  color_ramp <- colorRampPalette(brewer_palette)
  
  num_categories <- length(unique(data$`Place`))
  my_colors <- color_ramp(num_categories)
  
  p <- ggplot(data, aes(x = `gpp_short_name`, y = residuals, color = Legend, text = paste("GP: ", `gpp_short_name`,
"\n", "Residuals:", dollar_formatter(residuals)))) +
    geom_col(position = "dodge", aes( fill = `Place`)) +  # Using geom_col() for bar charts and position="dodge" to have separate bars when there are multiple observations for the same `Programme Category Name`
    scale_color_manual(values = c('Selected' = 'red', 'Not Selected' = 'white')) +
    scale_y_continuous(labels = scales::dollar_format(prefix = "£", big.mark = ",", accuracy = 1)) +
    scale_fill_manual(values = my_colors) +
    labs(x = "GP", y = "Difference") +
    theme_minimal() +
    geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
    ggtitle(paste0("Budgeting Variations Across GP Practices")) +
    coord_flip()+
    guides(color = guide_legend(title = "Place")) +
    guides(fill = guide_legend(title = '',override.aes = list(colour = NA))) # Rotating x labels for better readability

layout(ggplotly(p, tooltip = c("text")), height = 2000)
})

      
    } else if(input$absrelpbc == TRUE && input$switchViewPBC == FALSE) { # PCN
      

      output$programResidualPlot <- renderPlotly({
  data <- df_pbc %>% filter(`Programme Category Name` == input$program, gpp_short_name %!in% (input$excludeGP)) %>%
    arrange(`Place`, gpp_short_name)
  
  data$`gpp_short_name` <- factor(data$`gpp_short_name`, levels = rev(unique(data$`gpp_short_name`)))
  data$`Place` <- factor(data$`Place`, levels = rev(unique(data$`Place`)))
 
  
  pbc_selected <- input$GP_H  # Get the selected PBCs
  data$Legend <- ifelse(data$gpp_short_name %in% pbc_selected, 'Selected', 'Not Selected')

  data$percent_residual <- ((data$`Total Cost`) / data$predicted) * 100
  desired_order <- rev(unique(data$gpp_short_name))
  
  # # Create the percentage bar chart
  # p <- ggplot(data, aes(x = `gpp_short_name`, y = percent_residual, fill = Legend)) +
  #   geom_bar(stat = "identity") +
  #   scale_fill_manual(values = c('Selected' = 'red', 'Not Selected' = 'black')) +
  #   geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  #   labs(x = "GPs", y = "Percentage Difference (%)") +
  #   theme_minimal() +
  #   theme(axis.text.x = element_blank())  # To rotate the x labels
  
  # ggplotly(p, tooltip = c("x", "y"))
  palette_name <- "Set1"
  brewer_palette <- brewer.pal(min(12, length(unique(data$`Place`))), palette_name)
  color_ramp <- colorRampPalette(brewer_palette)
  
  num_categories <- length(unique(data$`Place`))
  my_colors <- color_ramp(num_categories)  

if (any(data$Legend == "Selected")) {


p <- ggplot() +
    # First, plot the bigger red segment for 'Selected' Legend
    geom_segment(data = subset(data, Legend == "Selected"), 
                 aes(y = `gpp_short_name`, x = 100, xend = percent_residual, 
                     yend = `gpp_short_name`), 
                 color = "red", size = 3, show.legend = F) +
    geom_segment(data = data,aes(y = `gpp_short_name`, x = 100,color = Place, xend = percent_residual, yend = `gpp_short_name`, fill = Legend, 
                     text = paste("GPs:", `gpp_short_name`, "<br>Percentage:", round(percent_residual,2),"%")),
                 size = 2,position = position_dodge(2.5)) +  
    scale_color_manual(values = c('Selected' = 'red', 'Not Selected' = 'white')) +
    scale_color_manual(values = my_colors) +
    geom_vline(xintercept = 100, linetype = "dashed", color = "red") +  
    labs(y = "GPs", x = "Percentage Difference (%)") +
    theme_minimal() +
    guides(color = guide_legend(title = "Place")) +
    ggtitle(paste0("Budgeting Variations Across GP Practices"))+
  scale_y_discrete(limits = desired_order)


}

else {

p <- ggplot() +
    geom_segment(data = data,aes(y = `gpp_short_name`, x = 100,color = Place, xend = percent_residual, yend = `gpp_short_name`, fill = Legend, 
                     text = paste("GPs:", `gpp_short_name`, "<br>Percentage:", round(percent_residual,2),"%")),
                 size = 2,position = position_dodge(2.5)) +  
    scale_color_manual(values = c('Selected' = 'red', 'Not Selected' = 'white')) +
    scale_color_manual(values = my_colors) +
    geom_vline(xintercept = 100, linetype = "dashed", color = "red") +  
    labs(y = "GPs", x = "Percentage Difference (%)") +
    theme_minimal() +
    guides(color = guide_legend(title = "Place")) +
    ggtitle(paste0("Budgeting Variations Across GP Practices"))



}


layout(ggplotly(p, tooltip = "text"), height = 2000)


})
     
    }
  })

output$programHist <- renderPlotly({
  data <- df_pbc %>% filter(`Programme Category Name` == input$program, gpp_short_name %!in% (input$excludeGP))
  residuals <- data$`Total Cost` - data$predicted
  
  p <- ggplot(data, aes(x = residuals, text= paste0("Residals:", dollar_formatter(round(residuals,2))))) +
    geom_histogram() +
    scale_x_continuous(labels = scales::dollar_format(prefix = "£", big.mark = ",", accuracy = 1)) +
    labs(x = "Difference", y = "Frequency") +
    theme_minimal() +
    ggtitle(paste("Histogram of Prediction Differences by Program Code", input$program)) 
  
  ggplotly(p, tooltip = "text")
})

output$programDensity <- renderPlotly({
  data <- df_pbc %>% filter(`Programme Category Name` == input$program, gpp_short_name %!in% (input$excludeGP))
  
  binwidth <- (max(data$`Total Cost`) - min(data$`Total Cost`)) / 30
  
  p <- ggplot(data) +
    geom_histogram(aes(x = predicted, fill = 'predicted', text = paste0("Predicted:",dollar_formatter(predicted))), binwidth = binwidth, alpha = 0.5) +
    geom_histogram(aes(x = `Total Cost`, fill = 'Total Cost', text = paste0("Total Cost:",dollar_formatter(`Total Cost`))), binwidth = binwidth, alpha = 0.5) +
    labs(x = "Cost (£)", y = "Frequency", title = "Cost Analysis: Predicted vs. Actual") +
    
    theme_minimal() +
    scale_x_continuous(labels = scales::comma_format(prefix = "£")) +
    guides(fill = guide_legend(title = "Cost Type")) +
    scale_fill_manual(values = c('blue', 'red'), labels = c('Predicted', 'Total Cost'))
  
  ggplotly(p, tooltip= "text")
})


observeEvent(input$swich_id_front, {
  runjs('_clickOnFront($("#id1"))')
})

observeEvent(input$swich_id_back, {
  runjs('_clickOnBack($("#id1"))')
})

## dsrs

dsrs_reactive <- reactive({
  
  dsrs <- pop_total_cost %>% 
    select(gpp_short_name, age_bands, `Programme Category Name`,pop, total_cost, activity) %>%
    complete(gpp_short_name,  age_bands, `Programme Category Name`, fill = list(total_cost = 0, pop = NA, activity=0)) %>%
    group_by(gpp_short_name, age_bands) %>%
    # fills in population by gp age bands
    fill(pop, .direction = "downup") %>%
    ungroup() %>%
    filter(`Programme Category Name` %in% input$program) %>%
    filter(gpp_short_name %!in% c("Elizabeth St","Old Links Surgery",NA), gpp_short_name %!in% (input$excludeGP)) %>%
    arrange(gpp_short_name,  age_bands) %>%
    group_by(gpp_short_name) %>%
    phe_dsr(activity, pop, multiplier = 1000)
  
  PLOTSDATA <- PHEindicatormethods::calculate_funnel_points(data = dsrs, numerator = total_count, denominator = total_pop, rate = value, rate_type = 'dsr', multiplier =1, years_of_data = 1) 
  
  LIMITSDATA <-  dsrs %>% ungroup() %>% mutate(total_count = total_count,
     total_pop = total_pop, value = value) %>% calculate_funnel_limits_2(
    numerator = total_count, denominator = total_pop, rate = value, rate_type = 'dsr', 
    multiplier =1000, years_of_data = 1, statistic = 'rate',type = "full") 
  
  
  pbc_selected <- input$GP_H  # Get the selected PBCs
  PLOTSDATA$Legend <- ifelse(PLOTSDATA$gpp_short_name %in% pbc_selected, 'Selected', 'Not Selected')

  PLOTSDATA <- PLOTSDATA %>% left_join(df_pbc %>% filter(`Programme Category Name` %in% input$program)
                                       , by= "gpp_short_name" ) %>%#
    mutate(funnelcost = (`Total Cost` / predicted) * 100)
  print(summary(PLOTSDATA$funnelcost))
  

  
  limitsdatalonger <- LIMITSDATA %>% 
    select(Events,baseline ,lower_2s_limit, upper_2s_limit,lower_3s_limit, upper_3s_limit) %>%
    pivot_longer(!Events, names_to = "limit", values_to = "value") %>% 
    mutate(limit = gsub("_", " ", limit))
  
  list(dsrs = dsrs, PLOTSDATA = PLOTSDATA, LIMITSDATA = LIMITSDATA , limitsdatalonger = limitsdatalonger)
  
})

output$mytable <- renderDT({
  dsrs <- dsrs_reactive()$dsrs
  DT::datatable(dsrs, options = list(scrollX = TRUE, scrollY = "300px", scroller = TRUE,searching = FALSE,  # removes the search bar
                                     paging = FALSE,     # removes the page numbers
                                     info = FALSE))
})

output$programFunnelPlot <- renderPlotly({
  
  dsrs <- dsrs_reactive()$dsrs
  PLOTSDATA <- dsrs_reactive()$PLOTSDATA
  LIMITSDATA <- dsrs_reactive()$LIMITSDATA
  limitsdatalonger <- dsrs_reactive()$limitsdatalonger
  

  PLOTSDATA$Shape <- ifelse(PLOTSDATA$Legend == "Selected", 8, 19)
  # Create the ggplot object
  p <- ggplot() +
    geom_point(data = PLOTSDATA, aes(x = total_pop, y = value_chart,colour = funnelcost,
                                      shape = as.factor(Shape), text = paste("GP: ",gpp_short_name , "\n" ,
                                      "Cost Ratio:",round(funnelcost,2), "\n", "Population:",total_pop, "\n",
                                      "DSR:",round(value_chart,2) )), 
               size = 2.5) +
    labs(color = "Observed to Expected Cost Ratio (x100)\n100 = Expected Spend") +
    scale_color_gradient2(low = "green", mid = "gray", high = "purple", midpoint = 100) +

  
  
    #scale_fill_continuous(low='skyblue', high='midnightblue') +
    #scale_colour_manual(low='skyblue', high='midnightblue') +
    #scale_fill_identity(aesthetics = "fill")+ # Gradient scale for funnelcost
    #scale_color_manual(values = c("95% Confidence Interval" = "blue", "99.8% Confidence Interval" = "red")) +
    scale_shape_identity() +
    #geom_line(data = limitsdatalonger, aes(x = Population, y = value, linetype = limit)) +
    # geom_line(data = LIMITSDATA, aes(x = lower_2s_population_1_year, y = lower_2s_limit)) +
    # geom_line(data = LIMITSDATA, aes(x = upper_2s_population_1_year, y = upper_2s_limit)) +
    # geom_line(data = LIMITSDATA, aes(x = lower_3s_population_1_year, y = lower_3s_limit)) +
    # geom_line(data = LIMITSDATA, aes(x = upper_3s_population_1_year, y = upper_3s_limit)) +
    # geom_hline(yintercept = unique(LIMITSDATA$baseline)) +
    geom_line(data = LIMITSDATA, aes(x = lower_2s_population_1_year, y = lower_2s_limit, linetype = "Lower 2s")) +
    geom_line(data = LIMITSDATA, aes(x = upper_2s_population_1_year, y = upper_2s_limit, linetype = "Upper 2s")) +
    geom_line(data = LIMITSDATA, aes(x = lower_3s_population_1_year, y = lower_3s_limit, linetype = "Lower 3s")) +
    geom_line(data = LIMITSDATA, aes(x = upper_3s_population_1_year, y = upper_3s_limit, linetype = "Upper 3s")) +
    geom_hline(yintercept = unique(LIMITSDATA$baseline), aes(linetype = "Baseline")) +
    
    # Define the linetypes you want
    scale_linetype_manual(values = c("Lower 2s" = "dashed",
                                     "Upper 2s" = "solid",
                                     "Lower 3s" = "dotdash",
                                     "Upper 3s" = "longdash",
                                     "Baseline" = "twodash")) +
    #scale_color_manual(values = c("95% Confidence Interval" = "blue", "99.8% Confidence Interval" = "red")) +
    ylab("Standardized Activity Rate per 1000 Persons") + 
    xlab("Population") +
    ggtitle(paste0("Funnel plot with Poisson limits for PBC: ",input$program)) +
    theme_minimal() +
    ylim(0, max(PLOTSDATA$value, na.rm = TRUE)*1.1 ) #+
  #xlim(0, max(PLOTSDATA$denominator_derived, na.rm = TRUE) *1.1)
  
  # Convert to a plotly plot
  ggplotly(p, tooltip = "text")
  
})
