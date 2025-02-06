library(readr)
library(tidyverse)
library(readxl)
library(tidyverse)
library(shinyWidgets)
library(DT)
library(d3r)

SELECTED_DATASET_MASTER <- eventReactive(c(input$rii_sii_icd10_switch2Master),{
  if(input$rii_sii_icd10_switch2Master) {
    slopeorrelmaster <- "Relative"
    widthSlope <- 0.03
    df <- riidataset %>% 
      filter(`Metrics` == 'ALL_EVENTS') %>%
      select(c(-"RII_Plot", -"AVG_RII_PLOT", -"Title.y")) %>% 
      rename(RII = Value, `Inequality Change` = AVG_RII, `AVG lCI` = avg_rii_lower95_0cl,
             `AVG UCI` = avg_rii_upper95_0cl)%>% 
      select(c(-"AVG lCI", -"AVG UCI")) %>% 
      mutate(`Metrics` = option) %>%
      rename(Metrics =`Metrics`) %>%
      arrange(description)
    
  } else if (!input$rii_sii_icd10_switch2Master){
    slopeorrelmaster <- "Slope"
    widthSlope <- -0.00
    df <- siidataset %>% 
      filter(`Metrics` == 'ALL_EVENTS') %>%
      select(c(-"SII_Plot", -"AVG_RII_PLOT",-"Title.y")) %>% 
      rename(SII = Value, `Inequality Change` = AVG_RII, `AVG lCI` = avg_rii_lower95_0cl,
             `AVG UCI` = avg_rii_upper95_0cl)%>% 
      select(c(-"AVG lCI", -"AVG UCI")) %>% 
      mutate(`Metrics` = option) %>%
      rename(Metrics =`Metrics`)%>%
      arrange(description)
  } 
  
  list(df = df , slopeorrel = slopeorrelmaster, widthSlope = widthSlope)
  
  
})


output$masterdynamic_datatables_ui2 <- renderUI({
  
   fluidRow(column(12,
  
      dataTableOutput("masterdynamicContentRII_ICD10_2Index") %>% 
        withSpinner(type = 3, color = '#3c8dbc', size = 1.5
                    ,color.background ='#ecf0f5' ))
  )
      
})   
    
    
#SELECTED_DATASET_MASTER()$df
    output$masterdynamicContentRII_ICD10_2Index <- renderDataTable({
      
      group_column <- 'description'
      group_column_index <- which(names(SELECTED_DATASET_MASTER()$df) == group_column) -1
      
      dt1icd10 <- datatable(
        
        
        
        SELECTED_DATASET_MASTER()$df %>%
          mutate(Year = ifelse(nchar(Year) > 4,
                               str_replace_all(paste(substr(Year, 3, 4),'-', str_sub(Year, -2, -1)), " ", ""),
                               Year)),
        rownames = F,
        extensions = 'RowGroup',
        selection = 'single' ,
        options = list(fixedHeader = F, dom = 't',
                       rowGroup = list(dataSrc = group_column_index),
                       
                       columnDefs = list(list(
                         targets = c(1,7,8,9),
                         visible = F, filter = list(show = FALSE), targets = 1
                       )),
                       height = '30px',
                       autoWidth = F,
                       pageLength = nrow(SELECTED_DATASET_MASTER()$df),
                       scrollX = TRUE,
                       scrollY = '800px',
                       
                       initComplete = JS(paste(
                         "function(settings, json) {",
                         
                         
                         "$(this.api().table().header()).css({'z-index': '1', 'background-color': '#27205D', 'color': '#fff', 'font-size': '10pt', 'border': '10px solid white', 'border-color': 'white'});",
                         "var table = this.api().table().header();",
                         #"var newHeaderRow1 = $('<tr>').append('<th colspan=\"2\" style = \"text-align: center;  border: 3px solid white;  \">  </th><th id = \"th_title\" colspan=\"6\" style = \"text-align: center;  border: 3px solid white;\"></th>');",
                         "var newHeaderRow2 = $('<tr>').append('<th colspan=\"2\" style = \"text-align: center;  border: 3px solid white;\"><h5></h5></th><th colspan=\"3\" style = \"text-align: center;  border: 3px solid white;\">",SELECTED_DATASET_MASTER()$slopeorrel," Index of Inequality</th><th colspan=\"3\" style = \"text-align: center;  border: 3px solid white;\">Yearly Change</th>');",
                         "$(table).prepend(newHeaderRow2);",
                         #"$(table).prepend(newHeaderRow1);",
                         "var table = this.api();",
                        
                         #<h1>Non-elective Admissions per 1,000 Persons</h1>
                         
                         "}")),
                       rowCallback = htmlwidgets::JS(paste(
                         "
function(row, data, dataIndex, columnIndex) {

  $('td', row).addClass('clickable');
  
  const group = data[",group_column_index,"];
  
  const barColor = (group === 'Life Expectancy') ? '#FFB22B' : '#6BAED6';

  function xsc(d) {
    return d3.scaleLinear()
      .domain(d3.extent(d))
      .range([0,150])
  };
  
  
   var width = $('#masterdynamic_datatables_ui2').width()*(0.20+",SELECTED_DATASET_MASTER()$widthSlope,");
   
   //console.log(width)
   
  
  // add cis
  //old
   //var scales = this.api().columns().data().toArray().map(xsc);
   
   
   //new
   
   
   var columnData = this.api().columns().data().toArray();
   
   const colorScale2 = d3.scaleLog()
      .domain([d3.min(columnData[6]), 1])
      .range(['darkgreen','#e3ff33'])
      .clamp(true);
   
   const colorScale = d3.scaleLinear()
      .domain([1, d3.max(columnData[6].flat())])
      .range(['#ffe933','darkred'])
      .clamp(true); 
   
   
   function getRotationAngle(value) {
      const minValue = ",globalMin,";
      const maxValue = ",globalMax,";
      const normalizedValue = (value - minValue) / (maxValue - minValue);
      return -(normalizedValue)  * 180 - 90;
   
   }
   
   var scales = columnData.map(function(data, i) {
   
    if ( i === 2 || i === 5) {
      var combinedData = data;
      
      
      if (d3.min(columnData[i +2]) < 0) {
      
      
      return d3.scaleLinear()
       .domain([d3.min(columnData[i +2]), d3.max(columnData[i +3])])
       .range([0,width]);
       
      } else {
      
      return d3.scaleLinear()
       .domain([0, d3.max(columnData[i +3])])
       .range([0,width]);
      
      } 

    }  else {
      return d3.scaleLinear()
        .domain(d3.extent(data))
        .range([0,width]);
      
    }
   
   });
   
  
   function getGradientColor(value) {
   
      if (value < 1) return colorScale2(value);
      return colorScale(value);
    }
  // add tooltip

  // try to render a d3 axis in column header
  var heads = d3.select(row)
    .selectAll('td')
    .data(scales)
    
    
    
    
  heads.each(function(head, i) {
  
    var x = head(data[i+1]);
  
    //column select
    if(i !== 2  ) {return}
    if(d3.select(this).select('svg').size() === 0) {

      $('td:eq(' + i + ')', row).addClass('plot-column');

      var cell = d3.select(this);
      //cell.html('');
      
      cell.style('vertical-align','middle')
      .style('text-align','right');
      
      var lci = head(data[i +2])
      var uci = head(data[i +3])
      

      var tooltip = cell.append('div')
        .attr('class', 'tooltip')
        .style('position', 'absolute')
        .style('visibility', 'hidden')
        .style('padding', '10px')
        .style('background', 'rgba(200,255,255,0.8)')
        .style('border', '1px solid #ddd')
        .style('border-radius', '5px')
        .style('text-align', 'left')
        .style('pointer-events', 'none')
        .style('z-index', '1000');
        
        
        if(i === 2 || i === 8 ) {
        
        
        
        svg = cell
        .append('div')
        .attr('class', 'containerPlot')
        .style('position', 'relative')
        .style('display', 'flex')
        .style('vertical-align', 'middle')
        .style('padding', '0')
        .style('margin-bottom', '20')
        .append('svg')
        .style('position', 'absolute')
        .style('display', 'block')
        .style('vertical-align', 'middle')
        .style('padding', '0')
        .style('margin-bottom', '20')
   
        .attr(\"width\", width+20)
      	.attr(\"height\", 20)
        .append('g')
        .attr(\"width\", '100%')
        .classed('axis', true)
        
        // add bar
        .append('rect')
        .attr(\"x\", d => data[i +1] <1 ? head(data[i+1]) : head(1)) // start from point if <0
      	.attr(\"y\", 0)
      	.attr(\"width\", d => Math.abs(head(data[i +1]) - head(1))) 
      	.attr(\"height\", 20)
      	.attr(\"fill\", d => data[i +1] <1 ? \"#78909C\" : `${barColor}`);

        } else {

        svg = cell
        .append('div')
        .attr('class', 'containerPlot')
        .style('position', 'relative')
        .style('display', 'block')
        .style('vertical-align', 'middle')
        .style('padding', '0')
        .style('margin-bottom', '20')
        .append('svg')
        .style('position', 'absolute')
        .style('display', 'block')
        .style('vertical-align', 'middle')
        .style('padding', '0')
        .style('margin-bottom', '20')
    
        .attr(\"width\", width)
      	.attr(\"height\", 20)
        .append('g')
        .attr(\"width\", '100%')
        .classed('axis', true)
        
        // add bar
        .append('rect')
        .attr(\"x\", d => data[i+1] <0 ? head(data[i+1]) : head(0)) // start from point if <0
      	.attr(\"y\", 0)
      	.attr(\"width\", d => Math.abs(head(data[i+1]) - head(0))) 
      	.attr(\"height\", 20)
      	.attr(\"fill\", d => data[i+1] <0 ? \"#78909C\" : `${barColor}`);

        }
      	
      //column select
      var points = cell
        .select('svg')
        .select('g')
        .append('circle')
        .attr('class', 'point')
        .attr('r', 3)
        .attr('cx',x)
        .attr('cy',10)
        .attr(\"fill\", \"black\");
        
     
       var lines = cell
        .select('svg')
        .select('g')
        .append('line')
        .attr('class', 'error')
        .attr('x1', d => lci)
        .attr('x2', d => uci)
        .attr('y1', 10)
        .attr('y2', 10)
        .attr(\"stroke\", \"black\")
        .attr(\"stroke-width\", 2);
        
        
        var linesLCI = cell
        .select('svg')
        .select('g')
        .append('line')
        .attr('class', 'error')
        .attr('x1', head(data[i+2]))
        .attr('x2', head(data[i+2]))
        .attr('y1', 10 - 5)
        .attr('y2', 10 + 5)
        .attr(\"stroke\", \"black\")
        .attr(\"stroke-width\", 2);
      
      
      var linesUCI = cell
        .select('svg')
        .select('g')
        .append('line')
        .attr('class', 'error')
        .attr('x1', head(data[i+3]))
        .attr('x2', head(data[i+3]))
        .attr('y1', 10 - 5)
        .attr('y2', 10 + 5)
        .attr(\"stroke\", \"black\")
        .attr(\"stroke-width\", 2);
      
        cell.select('svg')
        .on('mouseover', function(event, d){
      
      if(data[i+2] == null ) {
      
        tooltip.html(`Value: ${data[i+1]}%`)
            .style('visibility', 'visible')
            .style('font-size', '12px')
            .style('color', 'black')
            .style('opacity', '1')
            //.style('left', `${x + 10}px`)
            //.style('top', `${y + 10}px`);
            
            } else {
      
          tooltip.html(`Value: ${data[i+1]}<br>LCI: ${data[i +2]} <br> UCI: ${data[i+3]}`)
            .style('visibility', 'visible')
            .style('font-size', '12px')
            .style('color', 'black')
            .style('opacity', '1')
            //.style('left', `${x + 10}px`)
            //.style('top', `${y + 10}px`);
            
      }
      
        })
        .on('mouseout', function(){
      
          tooltip.style('visibility', 'hidden')
          .style('opacity', '0');
        
        });
    
    
    //console.log(head())
    
    var ax = d3.select(this).select('svg .axis')

    ax.call(d3.axisBottom(head).ticks(4));

  }

     var arrows = d3.select(row)
    .selectAll('td')
    .data(scales)
    
  arrows.each(function(arrow, i) {
  
    var x = arrow(data[i+1]);
  
    //column select
    if(i !== 5 ) {return}
    if(d3.select(this).select('svg').size() === 0) {

      $('td:eq(' + i + ')', row).addClass('plot-column');
    

      var cell = d3.select(this);
    
      if (i == 5) {
        
        const value = data[i+1];
        
        if( value == null || value == '' || isNaN(value)) {
          return;
        }
        
        $('td:eq(' + i + ')', row).html(((value - 1)*100).toFixed(1) + '%');
        
        function getArrowClass(value) {
        if (value < 1) {
          return 'fas fa-arrow-down';
      
        } else if (value > 1) {
          return 'fas fa-arrow-up';
        } else {
          return 'fas fa-arrow-right';
        }
        
        }

        function getArrowColor(value) {
        if (value < 1 ) {
          return 'darked';
        
        }else if (value > 1 ) {
          return 'green';
        } else {
          return 'yellow';
        }
        }
    
      svg = cell
        .append('div')
        .style('text-align', 'center');
        
      svg.append('i')
        .attr('class', 'fas fa-arrow-left fa-rotate-by')
        .style('color', getGradientColor(value))
        .style('--fa-rotate-angle', `${getRotationAngle(value)}deg`)
        .style('display', 'inline-block')
        .style('font-size','20px');
        
      
        cell.style('color', getGradientColor(value));
        
        }
      }
  
  })
      
})

}
")))) %>%
        formatStyle('Metrics', width = '46%') %>%
        formatStyle('Year', width = '3%')%>%
        formatStyle('LCI', width = '2.5%')%>%
        formatStyle('UCI', width = '2.5%')%>%
        formatStyle('Inequality Change', width = '6%')%>%
        formatStyle('p Value', width = '8%')%>%
        #formatStyle('Inequality Change', width = '6%')%>%
        formatStyle(4, width = '31%') %>%
        #formatStyle('SII', width = '20%')
      DT::formatStyle(names(SELECTED_DATASET_MASTER()$df),lineHeight='10%') %>%
        formatRound(columns=c(4, 5, 6), digits=2) %>%
        DT::formatStyle(names(SELECTED_DATASET_MASTER()$df), fontSize = '100%')
      
      
      dt1icd10
      #reset("picker_ui2")
      
    })
  
  #### dynamic page
    
  observeEvent(input$masterdynamicContentRII_ICD10_2Index_rows_selected, {
    clickedRow <- input$masterdynamicContentRII_ICD10_2Index_rows_selected

    indicator <- SELECTED_DATASET_MASTER()$df %>% 
     select(option)%>% nth(clickedRow) %>% pull()
    
    qph <- if_else(grepl("Life Expectancy",indicator, ignore.case = T),"LE by Quintile Plot","DSRs by Quintile Plot")
    qphT <- if_else(grepl("Life Expectancy",indicator, ignore.case = T),"Add Total Population Life Expectancy:","Add Total Population DSRs:")
    OqphT <- if_else(grepl("Life Expectancy",indicator, ignore.case = T),"LE Total Population Plot","DSRs Total Population Plot") 
    tableD <- if_else(grepl("Life Expectancy",indicator, ignore.case = T),"LE","DSRs") 
    
    chapter <- unique(allSlopes %>% filter(name == indicator) %>% select(Chapter_group) %>% pull())
    
    updateTabItems(session, "sidebarID", selected = "dynamicTabMaster")
    
    output$dynamicContentMaster <- renderUI({

      tags$div(
      
      fluidRow(column(12,
                      box(
                        title = paste(indicator),
                        status = "maroon",
                        solidHeader = TRUE,
                        closable = FALSE,
                        id = paste0("box", clickedRow),
                        width = 12,
                        tags$div(
                          column(12, 
                                 sliderInput("siislideryearICCD10MASTER",
                                             "Year Selector",width = "100%",
                                             min = as.numeric(str_sub(min(allDsrs %>%
                                                filter(option == indicator) %>%
                                                  select(Year) %>%
                                                  pull()), end = 4)),
                                             max = as.numeric(str_sub(max(allDsrs %>%
                                                filter(option == indicator) %>%
                                                  select(Year) %>%
                                                  pull()), end = 4)),
                                             value = as.numeric(str_sub(max(allDsrs %>%
                                                filter(option == indicator) %>%
                                                  select(Year) %>%
                                                  pull()), end = 4)),
                                             step = 1, sep = "", ticks = F),
                                 column(6,
                                 box(title = "Metrics by Year" ,solidHeader = TRUE,width=12,status = "maroon",tabsetPanel(
                                   id = 'plots_tab',

                                   tabPanel("RII Plot", tags$hr(style = "border-top: 2px solid #ccc; margin-top: 20px; margin-bottom: 20px;"),
                                            plotOutput("SIITimeriiMASTER", height = '400px') %>% 
                                              withSpinner(type = 3, color = '#3c8dbc', size = 1.5
                                                          ,color.background ='#ecf0f5' ),
                                            column(12,
                                                   dataTableOutput("ICD10RIITABLEMASTER")),
                                            
                                   ),
                                   tabPanel("SII Plot", 
                                            tags$hr(style = "border-top: 2px solid #ccc; margin-top: 20px; margin-bottom: 20px;"),
                                            plotOutput("SIITimeMASTER", height = '400px') %>% 
                                              withSpinner(type = 3, color = '#3c8dbc', size = 1.5
                                                          ,color.background ='#ecf0f5' ),
                                            column(12,
                                                   dataTableOutput("ICD10SIITABLEMASTER"))),
                                   
                                   tabPanel(paste(qph), tags$hr(style = "border-top: 2px solid #ccc; margin-top: 20px; margin-bottom: 20px;"),
                                            fluidRow(
                                              column(3,
                                                     radioButtons(
                                                       inputId = "chart_typeMASTER",
                                                       label = "Select Chart Type:",
                                                       choices = c("Points" = "points", "Remove Error Bars" = "lines"),
                                                       selected = "points"
                                                     )),column(4,
                                                               radioButtons(
                                                                 inputId = "add_oaDSRMASTER",
                                                                 label = paste(qphT),
                                                                 choices = c("Yes" = "yes", "No" = "no"),
                                                                 selected = "no"
                                                               )),column(4,
                                                                         checkboxGroupInput(
                                                                           inputId = "quintilesMASTER",
                                                                           label = "Select IMD Quintiles:",
                                                                           choices = c("IMD Quintile 1 (Most Deprived)" = "1", "IMD Quintile 2" = "2","IMD Quintile 3" = "3","IMD Quintile 4" = "4","IMD Quintile 5 (Least Deprived)" = "5"),
                                                                           selected = unique(allDsrs$Quintile)
                                                                         )),
                                              column(1,tags$div(style = "margin-top: 15px; margin bottom: 15px;",
                                                #                 HTML(
                                                #                   '<div style="display: flex; align-items: center; margin-bottom: 10px; gap: 10px;">
                                                #  <div style="display: flex; flex-direction: column; align-items: center;"></div>
                                                #   <div style="font-size: 8px; writing-mode: vertical-rl;-webkit-text-orientation: sideways;text-orientation: sideways;">1 (Most Deprived) &rarr; 5 (Least Deprived)
                                                #   </div>
                                                # </div>'
                                                #                 )
                                                ))
                                            ),
                                            
                                            fluidRow(
                                              plotOutput("QPTimeriiMASTER", height = '400px') %>% 
                                                withSpinner(type = 3, color = '#3c8dbc', size = 1.5
                                                            ,color.background ='#ecf0f5' ),
                                              column(12,
                                                     dataTableOutput("ICD1QPTABLEMASTER")))),
                                   tabPanel(paste(OqphT), tags$hr(style = "border-top: 2px solid #ccc; margin-top: 20px; margin-bottom: 20px;"),
                                            plotOutput("ORTimeriiMASTER", height = '400px') %>% 
                                              withSpinner(type = 3, color = '#3c8dbc', size = 1.5
                                                          ,color.background ='#ecf0f5' ),
                                            column(12,
                                                   dataTableOutput("ICD10OATABLEMASTER")))
                                   
                                   ))),
                                 column(6,
                                        box(title = "Metrics by Quintile" ,solidHeader = TRUE,width=12,status = "maroon",plotOutput("ICDODSRPLOTSMASTER", height = '452px')%>% 
                                          withSpinner(type = 3, color = '#3c8dbc', size = 1.5
                                                      ,color.background ='#ecf0f5' ),dataTableOutput("ICD10DSRTABLEMASTER"))),
  
                          ))),

                      )),
      
      
      tags$div(
        style = "text-align: center; margin-top: 20px;",
        
      )
    )
    
    })
      
    output$ICD10SIITABLEMASTER  <- renderDataTable({
      
      datatable(allSlopes %>%
                  filter(name == 'ALL_EVENTS',option == indicator) %>%
                  select(Year , sii, sii_lower95_0cl, sii_upper95_0cl) %>%
                  rename(`SII` = sii, `95% Lower CI` = sii_lower95_0cl, `95% Upper CI` =sii_upper95_0cl )
                ,extensions = 'Buttons', options = list(
                  dom = 'Bt',
                  buttons = c('csv')
                ), rownames = F) %>%
        formatRound(columns=c('SII', '95% Lower CI', '95% Upper CI'), digits=2)

    })

    output$ICD10RIITABLEMASTER  <- renderDataTable({
      
      datatable(allSlopes %>%
                  filter(name == 'ALL_EVENTS',option == indicator) %>%
                  select(Year , rii, rii_lower95_0cl, rii_upper95_0cl) %>%
                  rename(`RII` = rii, `95% Lower CI` = rii_lower95_0cl, `95% Upper CI` =rii_upper95_0cl )
                ,extensions = 'Buttons', options = list(
                  dom = 'Bt',
                  buttons = c('csv')
                ), rownames = F) %>%
        formatRound(columns=c('RII', '95% Lower CI', '95% Upper CI'), digits=2)

    })
    
    output$ICD10DSRTABLEMASTER  <- renderDataTable({
      
      filtered_data <- allDsrs %>%
        filter(Chapter == 'ALL_EVENTS',option == indicator) %>%
        filter(as.numeric(str_sub(Year, end = 4)) == as.numeric(str_sub(input$siislideryearICCD10MASTER, end = 4))) %>%
        select(Quintile , Std_Rate, Std_LCL, Std_UCL) %>%
        rename(`DSR` = Std_Rate, `95% Lower CI` = Std_LCL, `95% Upper CI` =Std_UCL )
      
      colnames(filtered_data)[2] <- paste(tableD)
      
      datatable(filtered_data
                ,extensions = 'Buttons', options = list(
                  dom = 'Bt',
                  buttons = c('csv')
                ), rownames = F) %>%
        formatRound(columns=c(paste(tableD), '95% Lower CI', '95% Upper CI'), digits=2)
      
      
    })
    
    output$SIITimeMASTER <- renderPlot({
      filtered_data_rates <- allSlopes %>%
        filter(name == 'ALL_EVENTS',option == indicator)
      
      pbc_selected <- as.numeric(str_sub(input$siislideryearICCD10MASTER, end = 4))  # Get the selected PBCs
      filtered_data_rates$Legend <- ifelse(as.numeric(str_sub(filtered_data_rates$Year, end = 4)) %in% pbc_selected, 'Year Selected', 'Not Selected')
      
      model_data <- data.frame(Year = unique(filtered_data_rates$Year))
      if (length(model_data) == 1) {

        p <- ggplot(data = filtered_data_rates, aes(color = Legend,x = Year, y = sii)) +
        geom_bar(aes(y = sii), stat = "identity", fill = "lightblue") +
        geom_errorbar(width = 0.2, data = filtered_data_rates, 
                      aes(color = Legend, x = Year, ymin = sii - (sii  - sii_lower95_0cl), ymax = sii + (sii_upper95_0cl  - sii))) +
        #geom_line(data = model_data, aes(x = Year, y = Predicted, color = 'green')) +
        scale_color_manual(values = c('Year Selected' = 'red', 'Not Selected' = 'black')) +
        ggtitle(paste0("SII of ",paste(strwrap(indicator,45), collapse="\n")," by Year" )) +
        ylab(paste0("SII of ",paste(strwrap(indicator,45), collapse="\n"))) +
        xlab("Year") +
        theme_minimal() + 
        theme(plot.title = element_text(hjust = 0.5, size = 14))#+

      } else {

      model_data$Predicted <- predict(lm(sii ~ Year, data=filtered_data_rates), newdata=model_data)
      
      p <- ggplot(data = filtered_data_rates, aes(color = Legend,x = Year, y = sii)) +
        geom_bar(aes(y = sii), stat = "identity", fill = "lightblue") +
        geom_errorbar(width = 0.2, data = filtered_data_rates, 
                      aes(color = Legend, x = Year, ymin = sii - (sii  - sii_lower95_0cl), ymax = sii + (sii_upper95_0cl  - sii))) +
        geom_line(data = model_data, aes(x = Year, y = Predicted, color = 'green')) +
        scale_color_manual(values = c('Year Selected' = 'red', 'Not Selected' = 'black')) +
        ggtitle(paste0("SII of ",paste(strwrap(indicator,45), collapse="\n")," by Year" )) +
        ylab(paste0("SII of ",paste(strwrap(indicator,45), collapse="\n"))) +
        xlab("Year") +
        theme_minimal() + 
        theme(plot.title = element_text(hjust = 0.5, size = 14))#+

      }

      p
      
    })
    
    output$SIITimeriiMASTER <- renderPlot({
      filtered_data_rates <- allSlopes %>%
        filter(name == 'ALL_EVENTS',option == indicator)
      
      pbc_selected <- as.numeric(str_sub(input$siislideryearICCD10MASTER, end = 4)) # Get the selected PBCs
      filtered_data_rates$Legend <- ifelse(as.numeric(str_sub(filtered_data_rates$Year, end = 4)) %in% pbc_selected, 'Year Selected', 'Not Selected')
      
      model_data <- data.frame(Year = unique(filtered_data_rates$Year))

      if (length(model_data) == 1) {
        p <- ggplot(data = filtered_data_rates, aes(color = Legend,x = Year, y = (rii))) +
        geom_bar(aes(y = (rii)), stat = "identity", fill = "lightblue") +
        geom_errorbar(width = 0.2, data = filtered_data_rates, 
                      aes(color = Legend, x = Year, ymin = rii - (rii  - rii_lower95_0cl), ymax = rii + (rii_upper95_0cl  - rii))) +
        #geom_line(data = model_data, aes(x = Year, y = Predicted, color = 'green')) +
        scale_color_manual(values = c('Year Selected' = 'red', 'Not Selected' = 'black')) +
        ggtitle(paste0("RII of ",paste(strwrap(indicator,45), collapse="\n"), " by Year")) +
        ylab(paste0("RII of ",paste(strwrap(indicator,45), collapse="\n"))) +
        xlab("Year") +
        theme_minimal() + 
        theme(plot.title = element_text(hjust = 0.5, size = 14))#+

      } else {

      model_data$Predicted <- predict(lm(rii ~ Year, data=filtered_data_rates), newdata=model_data)
    
      p <- ggplot(data = filtered_data_rates, aes(color = Legend,x = Year, y = (rii))) +
        geom_bar(aes(y = (rii)), stat = "identity", fill = "lightblue") +
        geom_errorbar(width = 0.2, data = filtered_data_rates, 
                      aes(color = Legend, x = Year, ymin = rii - (rii  - rii_lower95_0cl), ymax = rii + (rii_upper95_0cl  - rii))) +
        geom_line(data = model_data, aes(x = Year, y = Predicted, color = 'green')) +
        scale_color_manual(values = c('Year Selected' = 'red', 'Not Selected' = 'black')) +
        ggtitle(paste0("RII of ",paste(strwrap(indicator,45), collapse="\n"), " by Year")) +
        ylab(paste0("RII of ",paste(strwrap(indicator,45), collapse="\n"))) +
        xlab("Year") +
        theme_minimal() + 
        theme(plot.title = element_text(hjust = 0.5, size = 14))#+

      }

      p
      
    })
    
    output$ICD10OATABLEMASTER  <- renderDataTable({
      
      dtd <- allDSRsOverarching %>%
        filter(Chapter == 'ALL_EVENTS',option == indicator) %>%
        #filter(as.numeric(str_sub(Year, end = 4)) == as.numeric(str_sub(input$siislideryearICCD10, end = 4))) %>%
        select(Year , Std_Rate, Std_LCL, Std_UCL) %>%
        rename( `DSR` = Std_Rate, `95% Lower CI` = Std_LCL, `95% Upper CI` =Std_UCL )
        
      colnames(dtd)[2] <- paste(tableD)
        
        datatable(dtd
                ,extensions = 'Buttons', options = list(
                  dom = 'Bt',
                  buttons = c('csv')
                ), rownames = F) %>%
        formatRound(columns=c(paste(tableD), '95% Lower CI', '95% Upper CI'), digits=2)
      
      
      
    })
    
    
    ##DSRS
    
    ### DSRS 
    
    output$QPTimeriiMASTER <- renderPlot({
      
      
      filtered_data <- allDsrs %>%
        filter(Chapter == 'ALL_EVENTS',option == indicator) %>%
        mutate(Quintile = as.factor(Quintile))
      
      filtered_data <- filtered_data[filtered_data$Quintile %in% input$quintilesMASTER, ]
      #filter(as.numeric(str_sub(Year, end = 4)) == as.numeric(str_sub(input$siislideryearICCD10, end = 4)))
      
      p <- ggplot(filtered_data, aes(x = Year, y= Std_Rate , color = Quintile , group = Quintile)) +
        labs(X = "Year", y = paste(strwrap(indicator,45), collapse="\n"), color = "IMD Quintile") +
        theme_minimal()
      
      if (input$chart_typeMASTER == "points" & input$add_oaDSRMASTER == 'no') {
        p <- p + geom_point() +
          geom_line() +
          geom_errorbar(data = filtered_data ,
                        aes(x = Year, ymin = Std_Rate - (Std_Rate - Std_LCL), ymax = Std_Rate + (Std_UCL - Std_Rate))) +
          ylim(0, max(allDsrs %>%
                        filter(Chapter == 'ALL_EVENTS',option == indicator) %>% 
                        select(Std_Rate) %>%
                        pull()) * 1.5) 
      } else if (input$chart_typeMASTER == "lines" & input$add_oaDSRMASTER == 'no') {
        p <- p + geom_line() +
          ylim(0, max(allDsrs %>%
                        filter(Chapter == 'ALL_EVENTS',option == indicator) %>% 
                        select(Std_Rate) %>%
                        pull()) * 1.5) #+
        # geom_errorbar(data = filtered_data ,
        #             aes(x = Year, ymin = Std_Rate - (Std_Rate - Std_LCL), ymax = Std_Rate + (Std_UCL - Std_Rate)))
      } else if (input$chart_typeMASTER == "points" & input$add_oaDSRMASTER == 'yes') {
        p <- p + geom_point() +
          geom_line() +
          geom_errorbar(data = filtered_data ,
                        aes(x = Year, ymin = Std_Rate - (Std_Rate - Std_LCL), ymax = Std_Rate + (Std_UCL - Std_Rate))) +
          geom_point(data = allDSRsOverarching %>%
                       filter(Chapter == 'ALL_EVENTS',option == indicator) %>%
                       mutate(Quintile = "Total Population"), aes(x = Year, y = Std_Rate)) +
          geom_errorbar(data = allDSRsOverarching %>%
                          filter(Chapter == 'ALL_EVENTS',option == indicator) %>%
                          mutate(Quintile = "Total Population"),
                        aes(x = Year, ymin = Std_Rate - (Std_Rate - Std_LCL), ymax = Std_Rate + (Std_UCL - Std_Rate))) +
          geom_line(data = allDSRsOverarching %>%
                      filter(Chapter == 'ALL_EVENTS',option == indicator) %>%
                      mutate(Quintile = "Total Population"),aes(x = Year, y= Std_Rate)) +
          ylim(0, max(allDsrs %>%
                        filter(Chapter == 'ALL_EVENTS',option == indicator) %>% 
                        select(Std_Rate) %>%
                        pull()) * 1.5) 
      } else if (input$chart_typeMASTER == "lines" & input$add_oaDSRMASTER == 'yes'){
        
        p <- p + geom_line() +
          geom_line(data = allDSRsOverarching %>%
                      filter(Chapter == 'ALL_EVENTS',option == indicator) %>%
                      mutate(Quintile = "Total Population"),aes(x = Year, y= Std_Rate)) +
          ylim(0, max(allDsrs %>%
                        filter(Chapter == 'ALL_EVENTS',option == indicator) %>% 
                        select(Std_Rate) %>%
                        pull()) * 1.5) 
      }
      print(p)
      
    })
    
    
    output$ORTimeriiMASTER <- renderPlot({
      
      
      filtered_data <- allDSRsOverarching %>%
        filter(Chapter == 'ALL_EVENTS',option == indicator)
      #filter(as.numeric(str_sub(Year, end = 4)) == as.numeric(str_sub(input$siislideryearICCD10, end = 4)))
      
      
      p <- ggplot(data = filtered_data, aes(x = Year, y = Std_Rate)) +
        geom_bar(aes( y = Std_Rate), fill="lightblue", stat="identity") +
        geom_errorbar(width = 0.2,
          aes(x = Year, ymin = Std_Rate - (Std_Rate - Std_LCL), ymax = Std_Rate + (Std_UCL - Std_Rate))) +
        geom_smooth(method = "lm", se = F, aes(group=1), color = 'green') +
        #scale_x_continuous(name = "Year", breaks = 1:5, limits = c(0, 5.5)) +
        ggtitle(paste0(tableD," Total Population of ",paste(strwrap(indicator,45), collapse="\n")," by Year")) +
        ylab(paste(strwrap(indicator,45), collapse="\n")) +
        ylim(0, max(allDsrs %>%
                      filter(Chapter == 'ALL_EVENTS',option == indicator) %>% 
                      select(Std_Rate) %>%
                      pull()) * 1.5) +
        theme_minimal() +
        theme(plot.title = element_text(hjust = 0.5, size = 14))
      p
      
      
    })
    

    output$ICDODSRPLOTSMASTER <- renderPlot({
      filtered_data <- allDsrs %>%
        filter(Chapter == 'ALL_EVENTS',option == indicator) %>%
        filter(as.numeric(str_sub(Year, end = 4)) == as.numeric(str_sub(input$siislideryearICCD10MASTER, end = 4)))
      
      model_data <- data.frame(Quintile = 1:5)
      model_data$Predicted <- predict(lm(Std_Rate ~ Quintile, data=filtered_data ), newdata=model_data)
      
      # Your provided plotting code adjusted for the filtered data
      p <- ggplot(data = filtered_data , aes(x = Quintile, y = Std_Rate)) +
        geom_bar(aes(y =Std_Rate), stat = "identity",fill = "lightblue" ) +
        geom_errorbar(data = filtered_data, width = 0.2,
                      aes(x = Quintile, ymin = Std_Rate - (Std_Rate - Std_LCL), ymax = Std_Rate + (Std_UCL - Std_Rate))) +
        geom_line(data = model_data, aes(x = Quintile, y = Predicted), color = 'green') +
        scale_x_continuous(name = paste("Quintile 1 (Most Deprived)", "\u2192" ,"5 (Least Deprived)"), breaks = 1:5, limits = c(0, 5.5)) +
        ggtitle(paste0(paste(strwrap(indicator,45), collapse="\n"), " by Quintile ", as.numeric(str_sub(input$siislideryearICCD10MASTER, end = 4)))) +
        ylab(paste0(paste(strwrap(indicator,45), collapse="\n"))) +
        ylim(0, max(allDsrs %>%
                                        filter(Chapter == 'ALL_EVENTS',option == indicator) %>% 
                                        select(Std_Rate) %>%
                                        pull()) * 1.5) +
        theme_minimal() +
        theme(plot.title = element_text(hjust = 0.5, size = 14))
      p
      
    })

  })

  observeEvent(input$backMaster, {
    updateTabItems(session, "sidebarID", selected = "MainMenu")
  })
  
  

