

## GP page

# output$dynamicTitleGPP <- renderUI({
#   tags$h1(paste0("GP: ",input$gpp))
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

output$RsquaredBox <- renderValueBox({
  metrics <- calculate_metrics(df_pbc %>% filter(gpp_short_name == input$gpp, `Programme Category Name` %!in% (input$excludePBC)))
  valueBox(
    paste0(round(metrics$r2, 2)),
    "R-squared", 
    icon = icon('chart-line'),
    color = "green"
  )
})

output$MAEBox <- renderValueBox({
  metrics <- calculate_metrics(df_pbc %>% filter(gpp_short_name == input$gpp, `Programme Category Name` %!in% (input$excludePBC)))
  valueBox(
    paste0(round(metrics$mae, 2)),
    "Mean Absolute Error", 
    icon = icon('chart-area'),
    color = "yellow"
  )
})

output$RMSEBox <- renderValueBox({
  metrics <- calculate_metrics(df_pbc %>% filter(gpp_short_name == input$gpp, `Programme Category Name` %!in% (input$excludePBC)))
  valueBox(
    paste0(round(metrics$rmse, 2)),
    "Root Mean Squared Error", 
    icon = icon('chart-bar'),
    color = "red"
  )
})

output$PredictedCostBox <- renderValueBox({
  data <- df_pbc %>% filter(gpp_short_name == input$gpp, `Programme Category Name` %!in% (input$excludePBC))
  predicted_cost <- dollar_format(prefix = "£")(data %>% group_by(gpp_short_name) %>% summarise(p = sum(predicted, na.rm=T)) %>% pull())
  valueBox(
    predicted_cost,
   
    subtitle = HTML(
    paste( "Predicted Cost", tags$br(), tags$small("Derived from the average spend per age and sex for a specific PBC."))
  ),
    icon = icon('money-bill-wave'),
    color = "blue"
  )
})

output$ActualCostBox <- renderValueBox({
  data <- df_pbc %>% filter(gpp_short_name == input$gpp, `Programme Category Name` %!in% (input$excludePBC))
  actual_cost <- dollar_format(prefix = "£")(data %>% group_by(gpp_short_name) %>% summarise(p = sum(`Total Cost`, na.rm = TRUE)) %>% pull())
  valueBox(
    actual_cost,
     
    subtitle = HTML(
    paste( "Actual Cost", tags$br(), tags$small(" "))
  ),
    icon = icon('money-bill-alt'),
    color = "blue"
  )
})


output$mostUnderspendingGroup <- renderValueBox({
  data <- df_pbc %>% filter(gpp_short_name == input$gpp, `Programme Category Name` %!in% (input$excludePBC))
  mostunder <- (data %>% arrange(diff) %>% select(`Programme Category Name`) %>% slice_head(n=1) %>% pull() ) 
  valueBox(
    mostunder,
    "Lowest Relative Spend PBC", 
    icon = icon('money-bill-alt'),
    color = "green"
  )
})


output$mostOverspendingGroup <- renderValueBox({
  data <- df_pbc %>% filter(gpp_short_name == input$gpp, `Programme Category Name` %!in% (input$excludePBC))
  mostover <- (data %>% arrange(diff) %>% select(`Programme Category Name`) %>% slice_tail(n=1) %>% pull() ) 
  valueBox(
    mostover,
    "Highest Relative Spend PBC", 
    icon = icon('money-bill-alt'),
    color = "red"
  )
})

## plots


output$gpSPC <- renderPlotly({
  
  data <- monthcostdf %>%
    left_join(Programme_Budgeting_Mappings_Definitions , by = c("ProgrammeBudgetCode" = "PBC02")) %>%
    mutate(`Programme Category Name` = ifelse(is.na(`Programme Category Name`), "Not coded", `Programme Category Name`)) %>%
    filter(gpp_short_name == input$gpp, `Programme Category Name` %!in% (input$excludePBC)) %>%
    arrange(gpp_short_name, date) #%>% # Sort the dataframe by ID and date
    #group_by(gpp_short_name, date) %>%
    #summarise(sum = sum(sum, na.rm=T)) %>% 
    #ungroup() #%>%
    #left_join(Programme_Budgeting_Mappings_Definitions , by = c("ProgrammeBudgetCode" = "PBC02")) %>%
    #filter(gpp_short_name == input$gpp)
  
run_chart <- runcharter(data %>% group_by(gpp_short_name, date) %>%
                          summarise(sum = sum(sum, na.rm=T)) %>% 
                          ungroup(),
                          med_rows = 12,
                          runlength = 6,
                          direction = 'decrease',
                          datecol = date, 
                          grpvar = `gpp_short_name`,
                          yval = sum, 
                          chart_title = "Runs identified",
                          chart_subtitle = "Runs below the median signalling improvement")
  

  rebase_dates <- run_chart$sustained$start_date
  print(rebase_dates)
  spc_chart <- ptd_spc(data %>% group_by(gpp_short_name, date) %>%
                         summarise(sum = sum(sum, na.rm=T)) %>% 
                         ungroup(), sum, date, rebase = ptd_rebase(as.Date(rebase_dates, format="%Y-%m-%d")),improvement_direction = 'increase') %>% 
    plot(fixed_y_axis_multiple = FALSE, x_axis_breaks = "3 months",point_size = 1.5)
  
  spc_chart <- spc_chart + theme(axis.text.x = element_text(size = 6, angle = 45)) + ylab('Cost') + labs(
    title = paste("SPC of ", input$gpp))+
    labs(color = "Point Type") +
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
          
          
          pbc_selected <- input$pbc  # Get the selected PBCs
          data$Legend <- ifelse(data$`Programme Category Name` %in% pbc_selected, 'Selected', 'Not Selected')
          
          if (any(data$Legend == "Selected")) {
            
            run_chart2 <- runcharter(subset(data, Legend == "Selected") %>% group_by(gpp_short_name, date) %>%
                                       summarise(sum = sum(sum, na.rm=T)) %>% 
                                       ungroup(),
                                     med_rows = 12,
                                     runlength = 6,
                                     direction = 'decrease',
                                     datecol = date, 
                                     grpvar = `gpp_short_name`,
                                     yval = sum, 
                                     chart_title = "Runs identified",
                                     chart_subtitle = "Runs below the median signalling improvement")
            
            
            rebase_dates2 <- run_chart2$sustained$start_date
            print(rebase_dates2)
            spc_chart2 <- ptd_spc(subset(data, Legend == "Selected")%>% group_by(gpp_short_name, date) %>%
                                    summarise(sum = sum(sum, na.rm=T)) %>% 
                                    ungroup(), sum, date, rebase = ptd_rebase(as.Date(rebase_dates2, format="%Y-%m-%d")),improvement_direction = 'increase') %>% 
              plot(fixed_y_axis_multiple = FALSE, x_axis_breaks = "3 months",point_size = 1.5)
            
            spc_chart2 <- spc_chart2 + theme(axis.text.x = element_text(size = 6, angle = 45)) + ylab('Cost') + labs(
              title = paste("SPC of Highligted PBCs",input$pbc ))+
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
                text =  paste("SPC of ", input$gpp),
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
                text = paste("SPC of Highlighted PBCs"),
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
    if(input$gplogs == FALSE && input$switchView == FALSE) { # GP



    output$gppPlot1 <- renderPlotly({
  data <- df_pbc %>% filter(gpp_short_name == input$gpp, `Programme Category Name` %!in% (input$excludePBC))
  residuals <- data$`Total Cost` - data$predicted
  
  pbc_selected <- input$pbc  # Get the selected PBCs
  data$Legend <- ifelse(data$`Programme Category Name` %in% pbc_selected, 'Selected', 'Not Selected')
  
  
  p <- ggplot(data, aes(x = `Total Cost`, y = predicted, text = paste("Programme Budgeting Code:", `Programme Category Name`,
                "\nActual:" , dollar_formatter(`Total Cost`), "\nPredicted:", dollar_formatter(predicted)))) +
    geom_point(aes(color = Legend)) +
    scale_color_manual(values = c('Selected' = 'red', 'Not Selected' = 'black')) +
    geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
    labs(x = "Actual", y = "Predicted") +
    scale_y_continuous(labels = scales::dollar_format(prefix = "£", big.mark = ",", accuracy = 1)) +
    scale_x_continuous(labels = scales::dollar_format(prefix = "£", big.mark = ",", accuracy = 1)) +
    ggtitle(paste("Actual vs Predicted by GPP Code", input$gpp)) +
    theme_minimal()
  
  ggplotly(p, tooltip = c("text"))
})


    } else if(input$gplogs == TRUE && input$switchView == FALSE) { # PCN

     output$gppPlot <- renderPlotly({
  data <- df_pbc %>% filter(gpp_short_name == input$gpp, `Programme Category Name` %!in% (input$excludePBC))
  residuals <- data$`Total Cost` - data$predicted
  
  pbc_selected <- input$pbc  # Get the selected PBCs
  data$Legend <- ifelse(data$`Programme Category Name` %in% pbc_selected, 'Selected', 'Not Selected')
  
  
  p <- ggplot(data, aes(x = `Total Cost`, y = predicted, text = paste("Programme Budgeting Code:", `Programme Category Name`,
     "\nActual:" , dollar_formatter(`Total Cost`), "\nPredicted:", dollar_formatter(predicted)))) +
    geom_point(aes(color = Legend)) +
    scale_color_manual(values = c('Selected' = 'red', 'Not Selected' = 'black')) +
    geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
    labs(x = "Actual", y = "Predicted") +
    scale_y_continuous(labels = scales::dollar_format(prefix = "£", big.mark = ",", accuracy = 1), trans = "log10") +
    scale_x_continuous(labels = scales::dollar_format(prefix = "£", big.mark = ",", accuracy = 1), trans = "log10") +
    ggtitle(paste("Actual vs Predicted by GPP Code", input$gpp)) +
    theme_minimal()
  
  ggplotly(p, tooltip = c("text"))
})


}
  })




observe({

  

    if(input$absrel == FALSE && input$switchView == FALSE) { # GP
      # Adjust logic for 

      output$gppResidualPlot1 <- renderPlotly({
  data <- df_pbc %>% filter(gpp_short_name == input$gpp, `Programme Category Name` %!in% (input$excludePBC)) %>%
  arrange(pbccode)

  data$`Programme Category Name` <- factor(data$`Programme Category Name`, levels = rev(unique(data$`Programme Category Name`)))


  residuals <- (data$`Total Cost` - data$predicted)
  
  pbc_selected <- input$pbc  # Get the selected PBCs
  data$Legend <- ifelse(data$`Programme Category Name` %in% pbc_selected, 'Selected', 'Not Selected')

  palette_name <- "Set1"
  brewer_palette <- brewer.pal(min(12, length(unique(data$`Programme_Display`))), palette_name)
  color_ramp <- colorRampPalette(brewer_palette)

  # Interpolate the desired number of colors
  num_categories <- length(unique(data$`Programme_Display`))
  my_colors <- color_ramp(num_categories)
  

p <- ggplot(data, aes(x = `Programme Category Name`, y = residuals, colour = Legend, text = paste("Programme Budgeting Code:", `Programme Category Name`,
"\n", "Residuals:", dollar_formatter(residuals)))) +
    geom_col(position = "dodge", aes( fill = `Programme_Display`)) +  # Using geom_col() for bar charts and position="dodge" to have separate bars when there are multiple observations for the same `Programme Category Name`
    scale_color_manual(values = c('Selected' = 'red', 'Not Selected' = 'white')) +
    scale_y_continuous(labels = scales::dollar_format(prefix = "£", big.mark = ",", accuracy = 1)) +
    scale_fill_manual(values = my_colors) +
    labs(x = "Programme Category Name", y = "Difference") +
    geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
    ggtitle(paste0("Budgeting Variations Across Programme Categories")) +
    theme_minimal() +
    coord_flip()+
    guides(color = guide_legend(title = "Main Programme")) +
    guides(fill = guide_legend(title = '',override.aes = list(colour = NA)))# Rotating x labels for better readability

layout(ggplotly(p, tooltip = c("text")), height=800)


})
    
      
      
    } else if(input$absrel == TRUE && input$switchView == FALSE) { # PCN
      

      output$gppResidualPlot <- renderPlotly({
  data <- df_pbc %>% filter(gpp_short_name == input$gpp, `Programme Category Name` %!in% (input$excludePBC)) %>%
  arrange(pbccode)

  data$`Programme Category Name` <- factor(data$`Programme Category Name`, levels = rev(unique(data$`Programme Category Name`)))
  # Calculate the percentage difference from predicted values
  data$percent_residual <- ((data$`Total Cost`) / data$predicted) * 100
  
  pbc_selected <- input$pbc  # Get the selected PBCs
  data$Legend <- ifelse(data$`Programme Category Name` %in% pbc_selected, 'Selected', 'Not Selected')
  desired_order <- rev(unique(data$`Programme Category Name`))

  palette_name <- "Set1"
  brewer_palette <- brewer.pal(min(12, length(unique(data$`Programme_Display`))), palette_name)
  color_ramp <- colorRampPalette(brewer_palette)

  # Interpolate the desired number of colors
  num_categories <- length(unique(data$`Programme_Display`))
  my_colors <- color_ramp(num_categories)
  
  if (any(data$Legend == "Selected")) {

  p <- ggplot() +
    # First, plot the bigger red segment for 'Selected' Legend
    geom_segment(data = subset(data, Legend == "Selected"), 
                 aes(y = `Programme Category Name`, x = 100, xend = percent_residual, 
                     yend = `Programme Category Name`), 
                 color = "red", size = 4, show.legend = F) +
  
    geom_segment(data = data, aes(y = `Programme Category Name`,color = `Programme_Display`, x = 100, xend = percent_residual, yend = `Programme Category Name`, fill = Legend, 
                     text = paste("Programme Category Name:", `Programme Category Name`, "<br>Percentage:", round(percent_residual,2),"%")),
                 size = 2.75) +  
    scale_color_manual(values = c('Selected' = 'red', 'Not Selected' = 'white')) +
    scale_color_manual(values = my_colors) +
    geom_vline(xintercept = 100, linetype = "dashed", color = "red") +  
    labs(y = "Programme Category Name", x = "Percentage Difference (%)") +
    ggtitle(paste0("Budgeting Variations Across Programme Categories")) +
    guides(color = guide_legend(title = "Main Programme")) +
    theme_minimal()+
    scale_y_discrete(limits = desired_order)

  }
  else {


    p <- ggplot() +
  
    geom_segment(data = data, aes(y = `Programme Category Name`,color = `Programme_Display`, x = 100, xend = percent_residual, yend = `Programme Category Name`, fill = Legend, 
                     text = paste("Programme Category Name:", `Programme Category Name`, "<br>Percentage:", round(percent_residual,2),"%")),
                 size = 2.75) +  
    scale_color_manual(values = c('Selected' = 'red', 'Not Selected' = 'white')) +
    scale_color_manual(values = my_colors) +
    geom_vline(xintercept = 100, linetype = "dashed", color = "red") +  
    labs(y = "Programme Category Name", x = "Percentage Difference (%)") +
    ggtitle(paste0("Budgeting Variations Across Programme Categories")) +
    guides(color = guide_legend(title = "Main Programme")) +
    theme_minimal()
  }
layout(ggplotly(p, tooltip = "text"),height=800)

})
     
    }
  })

output$gppHist <- renderPlotly({
  data <- df_pbc %>% filter(gpp_short_name == input$gpp, `Programme Category Name` %!in% (input$excludePBC))
  residuals <- data$`Total Cost` - data$predicted
  
  p <- ggplot(data, aes(x = residuals, text= paste0("Residals:", dollar_formatter(round(residuals,2))))) +
    geom_histogram() +
    scale_x_continuous(labels = scales::dollar_format(prefix = "£", big.mark = ",", accuracy = 1)) +
    labs(x = "Difference", y = "Frequency") +
    theme_minimal() +
    ggtitle(paste0("Histogram of Prediction Differences for GP: ",input$gpp)) 
  
  ggplotly(p, tooltip = "text")
})


observeEvent(input$swich_id_front_gp, {
  runjs('_clickOnFront($("#id2"))')
})

observeEvent(input$swich_id_back_gp, {
  runjs('_clickOnBack($("#id2"))')
})

### funnel plot

# output$gppFunnelPlot <- renderPlotly({
  
#   dsrs <- pop_total_cost %>% 
#     select(gpp_short_name, age_bands, `Programme Category Name`, pop, total_cost, activity) %>%
#     complete(gpp_short_name,  age_bands, `Programme Category Name`, fill = list(total_cost = 0, pop = NA)) %>%
#     group_by(gpp_short_name, age_bands) %>%
#     # fills in population by gp age bands
#     fill(pop, .direction = "downup") %>%
#     ungroup() %>%
#     filter(`gpp_short_name` %in% input$gpp) %>%
#     filter(`Programme Category Name` %!in% c("missing"), `Programme Category Name` %!in% (input$excludePBC)) %>%
#     arrange(`Programme Category Name`,  age_bands) %>%
#     group_by(`Programme Category Name`) %>%
#     phe_dsr(activity, pop, multiplier = 1000)

#   gp_dsr <<- dsrs
  
#   PLOTSDATA <- PHEindicatormethods::calculate_funnel_points(data = dsrs, numerator = total_count, denominator = total_pop, rate = value, rate_type = 'dsr', multiplier =1, years_of_data = 1) 
  
#   LIMITSDATA <-  dsrs %>% ungroup() %>% mutate(total_count = total_count, total_pop = total_pop, value = value) %>% calculate_funnel_limits_2(
#     numerator = total_count, denominator = total_pop, rate = value, rate_type = 'dsr', 
#     multiplier =1000, years_of_data = 1, statistic = 'rate',type = "standard") 
  
#   pbc_selected <- input$pbc  # Get the selected PBCs
#   PLOTSDATA$Legend <- ifelse(PLOTSDATA$`Programme Category Name` %in% pbc_selected, 'Selected', 'Not Selected')
  
#   PLOTSDATA <- PLOTSDATA %>% left_join(df_pbc %>% filter(`gpp_short_name` %in% input$gpp)
#                                        , by= "Programme Category Name" ) %>%#
#     mutate(funnelcost = (`Total Cost` / predicted) * 100)
#   print(summary(PLOTSDATA$funnelcost))
  
  
#   limitsdatalonger <- LIMITSDATA %>% 
#     select(Events,baseline ,lower_2s_limit, upper_2s_limit,lower_3s_limit, upper_3s_limit) %>%
#     pivot_longer(!Events, names_to = "limit", values_to = "value") %>% 
#     mutate(limit = gsub("_", " ", limit))
  
  
#   # p <- ggplot() +
#   #   geom_point(data = PLOTSDATA, aes(x = denominator_derived, y = value_chart,colour = funnelcost,
#   #                                    stroke = 2, text = paste("PBC:", `Programme Category Name`)),
#   #              shape = 21) +
#   #   #scale_fill_continuous(low='skyblue', high='midnightblue') +
#   #   #scale_colour_manual(low='skyblue', high='midnightblue') +
#   #   #scale_fill_identity(aesthetics = "fill")+ # Gradient scale for funnelcost
#   #   #scale_color_manual(values = c("95% Confidence Interval" = "blue", "99.8% Confidence Interval" = "red")) +
#   #   geom_line(data = limitsdatalonger, aes(x = Events, y = value, linetype = limit)) +
#   #   #geom_line(data = LIMITSDATA, aes(x = Events, y = upper_2s_limit), linetype = "dashed") +
#   #   #geom_line(data = LIMITSDATA, aes(x = Events, y = lower_3s_limit), linetype = "dashed") +
#   #   #geom_line(data = LIMITSDATA, aes(x = Events, y = upper_3s_limit), linetype = "dashed") +
#   #   #geom_hline(yintercept = unique(limitsdatalonger$baseline)) +
#   #   #scale_color_manual(values = c("95% Confidence Interval" = "blue", "99.8% Confidence Interval" = "red")) +
#   #   ylab("Standardized Activity Rate per 1000 Persons") + 
#   #   xlab("Number of Events") +
#   #   ggtitle(paste0("Funnel plot with Poisson limits for GP: ",input$gpp)) +
#   #   theme_minimal() +
#   #   ylim(0, max(PLOTSDATA$value, na.rm = TRUE)*1.1 ) +
#   #   xlim(0, max(PLOTSDATA$denominator_derived, na.rm = TRUE) *1.1)
  
#   # # Convert to a plotly plot
#   # ggplotly(p)

#   # Set shape based on Selection
# PLOTSDATA$Shape <- ifelse(PLOTSDATA$Legend == "Selected", 8, 19) # 19 is solid circle, 1 is empty circle

# p <- ggplot() +
#     geom_point(data = PLOTSDATA, aes(x = denominator_derived, y = value_chart, 
#                                      colour = funnelcost, shape = as.factor(Shape), 
#                                      text = paste("PBC:", `Programme Category Name`)), 
#                size = 2.5) + 
#     #scale_fill_continuous(low='skyblue', high='midnightblue') + 
#     scale_shape_identity() +  # Use provided shapes
#     geom_line(data = limitsdatalonger, aes(x = Events, y = value, linetype = limit)) +
#     ylab("Standardized Activity Rate per 1000 Persons") + 
#     xlab("Number of Events") +
#     ggtitle(paste0("Funnel plot with Poisson limits for GP: ",input$gpp)) +
#     theme_minimal() +
#     ylim(0, max(PLOTSDATA$value, na.rm = TRUE)*1.1 ) +
#     xlim(0, max(PLOTSDATA$denominator_derived, na.rm = TRUE) *1.1)
  
# # Convert to a plotly plot
# ggplotly(p)

  
  
# })


output$gppDensity <- renderPlotly({
  data <- df_pbc %>% filter(gpp_short_name == input$gpp, `Programme Category Name` %!in% (input$excludePBC))
  
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

