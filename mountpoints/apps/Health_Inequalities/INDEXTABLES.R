library(readr)
library(tidyverse)
library(readxl)
library(shinyWidgets)
library(DT)
library(d3r)
library(RPostgreSQL)
library(config)
config <- config::get(file = "config.yml")


stars.pval <- function(x){
  stars <- c("***", "**", "*", "n.s.")
  var <- c(0, 0.01, 0.05, 0.10, 1)
  i <- findInterval(x, var, left.open = T, rightmost.closed = T)
  stars[i]
}

calculate_p_values <- function(df, ratio_col, ci_lower_col, ci_upper_col) {
  df <- df %>%
    mutate(
    
    est = log(!!sym(ratio_col)),
    
    log_ci_lower = log(!!sym(ci_lower_col)),
    log_ci_upper = log(!!sym(ci_upper_col)),
    
    se = (log_ci_upper - log_ci_lower) / (2 * 1.96),
    
    z = abs(est/se),
    
    p = exp(-0.717 * z - 0.416 * z^2),
    
    p = format(round(p, 3),scientific = FALSE),
    
    `p Value` = paste(p, " (",stars.pval(p),")")
    
  )
  return(df)
}

library(DBI)

print("Testing connection to database...")
con <- dbConnect(
  PostgreSQL(),
  dbname = config$database,
  user = config$uid,
  host = config$server,
  password = config$pwd,
  port = config$port
)



allDsrs <- dbGetQuery(con, "SELECT *
	FROM public.\"HealthEquityDSRs\";") %>%
  mutate(Year = as.character(Year))


allSlopes <- dbGetQuery(con, "SELECT *
	FROM public.\"HealthEquitySlopes\";") %>%
  mutate(Year = as.character(Year))

allSlopes2 <- dbGetQuery(con, "SELECT *
	FROM public.\"HealthEquityRelativeChanage\";")


allDSRsOverarching <- dbGetQuery(con, "SELECT *
	FROM public.\"HealthEquityOverarching\";")


dbDisconnect(con)

allSlopes <- allSlopes %>%
  left_join(ICD10_Ref %>% select(ICD10Category2Code, ICD10ChapterCode) %>% distinct_all(),
            by = c("CodeRange" = "ICD10Category2Code"))

allSlopes2 <- calculate_p_values(allSlopes2, "rii", "rii_lower95_0cl", "rii_upper95_0cl")

#allSlopes2 <- allSlopes2 %>% mutate(rii = 1- rii)


allSlopes <- allSlopes %>%
  left_join(ChapterLookup, by = c("CodeRange" = "Block")) %>%
  mutate(CHAPTERS = ifelse(!is.na(ICD10ChapterCode),
                           ICD10ChapterCode,
                           ifelse(is.na(ICD10ChapterCode) & is.na(Chapter),
                                  ICD10ChapterCode,Chapter ))) %>%
  left_join(ChapterLookup, by = c("CHAPTERS" = "Chapter"))

siidataset <- allSlopes %>%
  group_by(name,option) %>%
  filter(Year == max(Year)) %>%
  ungroup() %>%
  left_join(allSlopes2 %>%
              select(rii,rii_lower95_0cl,
                     rii_upper95_0cl,
                     name,option,`p Value`) %>%
              rename(AVG_RII = rii, avg_rii_lower95_0cl = rii_lower95_0cl,
                     avg_rii_upper95_0cl = rii_upper95_0cl)) %>%
  mutate(SII_Plot = sii, AVG_RII_PLOT = AVG_RII) %>%
  select(name,description,Title.y,Year, sii, sii_lower95_0cl, sii_upper95_0cl,SII_Plot, AVG_RII, avg_rii_lower95_0cl, avg_rii_upper95_0cl,AVG_RII_PLOT, flag,option,`p Value`) %>%
  #mutate(across(where(is.numeric), sprintf, fmt = '%.2f')) %>%
  rename(`Metrics` = name, Value = sii, LCI = sii_lower95_0cl, UCI = sii_upper95_0cl)%>% 
  #mutate(`% Differnce` = (`National RII` - Value) / Value * 100) %>%
  mutate(across(where(is.numeric), sprintf, fmt = '%.2f'))%>% arrange(Title.y )

siidataset[, c("Value", "LCI", "UCI", "SII_Plot", "AVG_RII", "avg_rii_lower95_0cl","avg_rii_upper95_0cl", "AVG_RII_PLOT")] <- sapply(siidataset[, c("Value", "LCI", "UCI", "SII_Plot", "AVG_RII", "avg_rii_lower95_0cl","avg_rii_upper95_0cl", "AVG_RII_PLOT")], as.numeric)


riidataset <- allSlopes %>%
  group_by(name,option) %>%
  filter(Year == max(Year)) %>%
  ungroup() %>%
  left_join(allSlopes2 %>%
              select(rii,rii_lower95_0cl,
                     rii_upper95_0cl,
                     name,option,`p Value`) %>%
              rename(AVG_RII = rii, avg_rii_lower95_0cl = rii_lower95_0cl,
                     avg_rii_upper95_0cl = rii_upper95_0cl)) %>%
  mutate(RII_Plot = rii, AVG_RII_PLOT = AVG_RII) %>%
  select(name,description,Title.y,Year, rii, rii_lower95_0cl, rii_upper95_0cl,RII_Plot, AVG_RII, avg_rii_lower95_0cl, avg_rii_upper95_0cl,AVG_RII_PLOT, flag,option,`p Value`) %>%
  #mutate(across(where(is.numeric), sprintf, fmt = '%.2f')) %>%
  rename(`Metrics` = name, Value = rii, LCI = rii_lower95_0cl, UCI = rii_upper95_0cl)%>% 
  #mutate(`% Differnce` = (`National RII` - Value) / Value * 100) %>%
  mutate(across(where(is.numeric), sprintf, fmt = '%.2f'))%>% arrange(Title.y )
for (i in c("Value", "LCI", "UCI", "RII_Plot", "AVG_RII", "avg_rii_lower95_0cl","avg_rii_upper95_0cl", "AVG_RII_PLOT"))
{
  riidataset[[i]] <- as.numeric(riidataset[[i]])
}

## for the arrow rotations
globalMin <- min(allSlopes2$rii, na.rm=T)  
globalMax <- max(allSlopes2$rii, na.rm=T)  

SELECTED_DATASET <- eventReactive(c(input$rii_sii_icd10_switch2, input$variableOptions),{
  if(input$rii_sii_icd10_switch2 & input$variableOptions) {
    slopeorrel <- "Relative"
    widthSlope <- 0.03
    df <- riidataset %>% 
      
      select(c(-"RII_Plot", -"AVG_RII_PLOT", -"description")) %>% 
      filter(`Metrics` !="ALL_EVENTS") %>%
      rename(RII = Value, `Inequality Change` = AVG_RII, `AVG lCI` = avg_rii_lower95_0cl,
             `AVG UCI` = avg_rii_upper95_0cl) %>%
      select(c(-"AVG lCI", -"AVG UCI")) 
    
  } else if (!input$rii_sii_icd10_switch2 & input$variableOptions){
    slopeorrel <- "Slope"
    widthSlope <- 0.0
    df <- siidataset %>% 
      
      select(c(-"SII_Plot", -"AVG_RII_PLOT",-"description")) %>% 
      filter(`Metrics` !="ALL_EVENTS") %>%
      rename(SII = Value, `Inequality Change` = AVG_RII, `AVG lCI` = avg_rii_lower95_0cl,
             `AVG UCI` = avg_rii_upper95_0cl) %>%
      select(c(-"AVG lCI", -"AVG UCI")) 
  } else if (!input$variableOptions & input$rii_sii_icd10_switch2 ) {
    slopeorrel <- "Relative"
    widthSlope <- 0.03
    df <- riidataset %>% 
      select(c(-"RII_Plot", -"AVG_RII_PLOT",-"description")) %>% 
      filter(`Metrics` !="ALL_EVENTS") %>%
      rename(RII = Value, `Inequality Change` = AVG_RII, `AVG lCI` = avg_rii_lower95_0cl,
             `AVG UCI` = avg_rii_upper95_0cl) %>%
      select(c(-"AVG lCI", -"AVG UCI"))  %>%
      filter(flag == 'chapter')
    
  } else if (!input$variableOptions & !input$rii_sii_icd10_switch2) {
    slopeorrel <- "Slope"
    widthSlope <- 0.0
    df <- siidataset %>% 
      
      select(c(-"SII_Plot", -"AVG_RII_PLOT", -"description")) %>% 
      filter(`Metrics` !="ALL_EVENTS") %>%
      rename(SII = Value, `Inequality Change` = AVG_RII, `AVG lCI` = avg_rii_lower95_0cl,
             `AVG UCI` = avg_rii_upper95_0cl) %>%
      select(c(-"AVG lCI", -"AVG UCI"))  %>%
      filter(flag == 'chapter')
    
    
  }
  
  list(df = df , slopeorrel = slopeorrel, widthSlope = widthSlope)
 
         
})


output$dynamic_datatables_ui2 <- renderUI({
  
   fluidRow(column(12,
  dataTableOutput("dynamicContentRII_ICD10_2Index") %>% 
    withSpinner(type = 3, color = '#3c8dbc', size = 1.5
                ,color.background ='#ecf0f5' ))

  )
  
  
  
  
      
})   

observeEvent(input$rii_sii_icd10_switch2, {

    output$dynamicContentRII_ICD10_2Index <- renderDataTable({
      group_column <- 'Title.y'
      group_column_index <- which(names(SELECTED_DATASET()$df) == group_column) -1

      dt1icd10 <- datatable(
        
        SELECTED_DATASET()$df%>%
          mutate(Year = ifelse(nchar(Year) > 4,
                               paste(substr(Year, 3, 4),'-', str_sub(Year, -2, -1)),
                               Year)),
        rownames = F,
        extensions = 'RowGroup',
        selection = 'single' ,
        options = list(fixedHeader = F, dom = 't',
                       rowGroup = list(dataSrc = group_column_index),
                       
                       columnDefs = list(list(
                         targets = c(1,7,8),
                         visible = F, filter = list(show = FALSE), targets = 1
                       ),list(targets = 0, render = JS("function(data, type, row) {return '<div style=\"white-space: normal; line-height:1.5;\">'+ data + '</div>';}"))),
                       
                       autoWidth = F,
                       pageLength = nrow(SELECTED_DATASET()$df),
                       scrollX = TRUE,
                       scrollY = '800px',
                       
                       initComplete = JS(paste(
                         "function(settings, json) {",
                         
                         
                         "$(this.api().table().header()).css({'z-index': '1', 'background-color': '#005eb8', 'color': '#fff', 'font-size': '10pt', 'border': '10px solid white', 'border-color': 'white'});",
                         "var table = this.api().table().header();",
                         "var newHeaderRow1 = $('<tr>').append('<th colspan=\"2\" style = \"text-align: center;  border: 3px solid white;  \"><select id= \"group-by-select-add-mort_nonelectiverii\"></select>  </th><th id = \"th_title\" colspan=\"8\" style = \"text-align: center;  border: 3px solid white;\"></th>');",
                         "var newHeaderRow2 = $('<tr>').append('<th colspan=\"2\" style = \"text-align: center;  border: 3px solid white;\"><h5>Select ICD10 Chapter</h5> <select id= \"group-by-select\"></select></th><th colspan=\"3\" style = \"text-align: center;  border: 3px solid white;\">",SELECTED_DATASET()$slopeorrel," Index of Inequality</th><th colspan=\"3\" style = \"text-align: center;  border: 3px solid white;\">Yearly Change</th>');",
                         "$(table).prepend(newHeaderRow2);",
                         "$(table).prepend(newHeaderRow1);",
                         
                      

                         "var table = this.api();
                        table.column(8).search('Paediatric Admissions 16 Years and Under  per 100,000 Persons' ? '^' + 'Paediatric Admissions 16 Years and Under  per 100,000 Persons'  + '$' : '', true, false).draw(); 
                     
                      $('#group-by-select-add-mort_nonelectiverii').on('change', function() {
                        var val = $.fn.dataTable.util.escapeRegex($(this).val());
                        console.log(val);
                        table.column(8).search(val ? '^' + val + '$' : '', true, false).draw();
                      
                      });
                      
                      
                      table.column(8).data().unique().sort().reverse().each(function(d) {
                       $('#group-by-select-add-mort_nonelectiverii').append('<option value=\"' + d + '\">' + d + '</option>');
                      
                      
                      });",
                         
                         
                         #<h1>Non-elective Admissions per 100,000 Persons</h1>
                         
                         
                         
                         ## chapter select
                         
                         
                         
                         "$('#group-by-select').html('<option value = \"\">(All)</option>');",
                         
                         
                         
                         "var table = this.api();
                     
                      $('#group-by-select').on('change', function() {
                        var val = $.fn.dataTable.util.escapeRegex($(this).val());
                        
                        table.column(1).search(val ? '^' + val + '$' : '', true, false).draw();
                      
                      });
                      
                      
                      table.column(1).data().unique().sort().each(function(d) {
                       $('#group-by-select').append('<option value=\"' + d + '\">' + d + '</option>');
                      
                      
                      });
                      
                      ",
                         
                         "}")),
                       rowCallback = htmlwidgets::JS(paste(
                                                  "
function(row, data, dataIndex, columnIndex) {

  $('td', row).addClass('clickable');
  
  const group = data[8];
  
  let barColor;
  if(group === 'GP Fingertips Data') {
    barColor = '#FF0000';//RED
  } else if (group === 'Local Health Data') {
    barColor = '#008000';//green
  } else {
    barColor = '#1E90FF';//green
  }
  

  function xsc(d) {
    return d3.scaleLinear()
      .domain(d3.extent(d))
      .range([0,150])
  };
  
  
  var width = $('#dynamicContentRII_ICD10_2Index').width()*(0.17+",SELECTED_DATASET()$widthSlope,");
  
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
    if(i !== 2 ) {return}
    if(d3.select(this).select('svg').size() === 0) {
    
    
    
      $('td:eq(' + i + ')', row).addClass('plot-column');
    

      var cell = d3.select(this);
      //cell.html('');
  
    
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
        .style('z-index', '100000');
        
        
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
        
         $('td:eq(' + i + ')', row).html(((value -1)*100).toFixed(1) + '%');
        
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
    
      cell.style('color', getGradientColor(value));
    
      svg = cell
        .append('div')
        .style('text-align', 'center');
        
      svg.append('i')
        .attr('class', 'fas fa-arrow-left fa-rotate-by')
        .style('color', getGradientColor(value))
        .style('--fa-rotate-angle', `${getRotationAngle(value)}deg`)
        .style('display', 'inline-block')
        .style('font-size','20px');
        
        
        
        
        
        
      }
    }
    
  })
    
    
  })

}
")
      )
    )
  ) %>%
        formatStyle('Metrics', width = '45%') %>%
        formatStyle('Year', width = '2.5%')%>%
        formatStyle('LCI', width = '2.5%')%>%
        formatStyle('UCI', width = '2.5%')%>%
        formatStyle('Inequality Change', width = '9%')%>%
        formatStyle('p Value', width = '10%')%>%
        #formatStyle('Inequality Change', width = '6%')%>%
        formatStyle(4, width = '30%') %>%
        #formatStyle('SII', width = '20%')
        DT::formatStyle(names(SELECTED_DATASET()$df),lineHeight='10%')%>%
        formatRound(columns=c(4, 5, 6), digits=2) %>%
        DT::formatStyle(names(SELECTED_DATASET()$df), fontSize = '75%')
      
      dt1icd10
      #reset("picker_ui2")
      
    })
  
  
})
    
  
  #### dynamic page
  
  observeEvent(input$dynamicContentRII_ICD10_2Index_rows_selected, {
    clickedRow <- input$dynamicContentRII_ICD10_2Index_rows_selected
    #print(paste0("Row", clickedRow, "clicked"))
    
    if(is.null(input$variableOptions)) {
      
      indicator <- SELECTED_DATASET()$df %>% filter(flag == 'chapter')
      header <- indicator$option[clickedRow]
      indicator <- indicator$`Metrics`[clickedRow]
      
    } else {
      indicator <- SELECTED_DATASET()$df$`Metrics`[clickedRow]
      header <- SELECTED_DATASET()$df$option[clickedRow]
    }
    
    
    chapter <- unique(allSlopes %>% filter(name == indicator) %>% select(Chapter_group) %>% pull())
    
    updateTabItems(session, "sidebarID", selected = "dynamicTab")
    
    output$dynamicContent <- renderUI({
      #print(page1DataICD10$option[clickedRow])
      
      tags$div(
        
        # column(12, div(class = "uiheading", 
        #                HTML(paste(header, indicator, sep="<br/>")),
        #                tags$br()
        # )),
      
      fluidRow(column(12,
                      box(
                        title = paste(indicator, " (", header, ")"),
                        status = "primary",
                        solidHeader = TRUE,
                        closable = FALSE,
                        id = paste0("box", clickedRow),
                        width = 12,
                        tags$div(

                          column(12, 
                                 sliderInput("siislideryearICCD10","Year Selector",width = "100%", min = as.numeric(str_sub(min(allDsrs%>%
                                                                                                               filter(Chapter == indicator) %>% select(Year) %>% pull()), end = 4)), max= as.numeric(str_sub(max(allDsrs%>%
                                                                                                                                                                                   filter(Chapter == indicator) %>% select(Year) %>% pull()), end = 4)), value = as.numeric(str_sub(max(allDsrs%>%
                                                                                                                                                                                                                                                         filter(Chapter == indicator) %>% select(Year) %>% pull()), end = 4)), step = 1, sep = "", ticks = F),
                                 column(6,
                                 box(title = "Metrics by Year" ,solidHeader = TRUE,width=12,status = "primary",tabsetPanel(
                                   id = 'plots_tab',
                                   
                                   
                                   tabPanel("RII Plot", tags$hr(style = "border-top: 2px solid #ccc; margin-top: 20px; margin-bottom: 20px;"),
                                            plotOutput("SIITimerii", height = '400px') %>% 
                                              withSpinner(type = 3, color = '#3c8dbc', size = 1.5
                                                          ,color.background ='#ecf0f5' ),
                                            column(12,
                                                   dataTableOutput("ICD10RIITABLE"))),
                                   
                                   tabPanel("SII Plot", 
                                            tags$hr(style = "border-top: 2px solid #ccc; margin-top: 20px; margin-bottom: 20px;"),
                                            plotOutput("SIITime", height = '400px') %>% 
                                              withSpinner(type = 3, color = '#3c8dbc', size = 1.5
                                                          ,color.background ='#ecf0f5' ),
                                            column(12,
                                                   dataTableOutput("ICD10SIITABLE"))),
                                   
                                   tabPanel("DSRs by Quintile Plot", tags$hr(style = "border-top: 2px solid #ccc; margin-top: 20px; margin-bottom: 20px;"),
                                            fluidRow(
                                            column(3,
                                              radioButtons(
                                                inputId = "chart_type",
                                                label = "Select Chart Type:",
                                                choices = c("Points" = "points", "Remove Error Bars" = "lines"),
                                                selected = "points"
                                              )),column(4,
                                              radioButtons(
                                                inputId = "add_oaDSR",
                                                label = "Add Total Population DSRs:",
                                                choices = c("Yes" = "yes", "No" = "no"),
                                                selected = "no"
                                              )),column(4,
                                              checkboxGroupInput(
                                                inputId = "quintiles",
                                                label = "Select IMD Quintiles:",
                                                choices = c("IMD Quintile 1 (Most Deprived)" = "1", "IMD Quintile 2" = "2","IMD Quintile 3" = "3","IMD Quintile 4" = "4","IMD Quintile 5 (Least Deprived)" = "5"),
                                                selected = unique(allDsrs$Quintile)
                                              )),
                                              column(1,tags$div(style = "margin-top: 15px; margin bottom: 15px;",
                                              #                   HTML(
                                              #   '<div style="display: flex; align-items: center; margin-bottom: 10px; gap: 10px;">
                                              #    <div style="display: flex; flex-direction: column; align-items: center;"></div>
                                              #     <div style="font-size: 8px; writing-mode: vertical-rl;-webkit-text-orientation: sideways;text-orientation: sideways;">1 (Most Deprived) &rarr; 5 (Least Deprived)
                                              #     </div>
                                              #   </div>'
                                              # )
                                              ))
                                              ),
                                            
                                            fluidRow(
                                            plotOutput("QPTimerii", height = '400px') %>% 
                                              withSpinner(type = 3, color = '#3c8dbc', size = 1.5
                                                          ,color.background ='#ecf0f5' ),
                                            column(12,
                                                   dataTableOutput("ICD1QPTABLE")))),
                                   tabPanel("DSRs Total Population Plot", tags$hr(style = "border-top: 2px solid #ccc; margin-top: 20px; margin-bottom: 20px;"),
                                            plotOutput("ORTimerii", height = '400px') %>% 
                                              withSpinner(type = 3, color = '#3c8dbc', size = 1.5
                                                          ,color.background ='#ecf0f5' ),
                                            column(12,
                                                   dataTableOutput("ICD10OATABLE")))
                                           
                                                   ))),
                                 column(6,
                                        box(title = "Metrics by Quintile" ,solidHeader = TRUE,width=12,status = "primary",plotOutput("ICDODSRPLOTS", height = '452px')%>% 
                                          withSpinner(type = 3, color = '#3c8dbc', size = 1.5
                                                      ,color.background ='#ecf0f5' ),dataTableOutput("ICD10DSRTABLE"))),
                                 
                          )))
                    
                      )),

      tags$div(
        style = "text-align: center; margin-top: 20px;",
        
      )
    )
    
    })
      
      ### need to combine all subchapters together
    drilldown_state <- reactiveVal("subchapter 3rd")
    code_range <- reactiveVal(NULL)
    
    output$ICD10SIITABLE  <- renderDataTable({
      
      datatable(allSlopes %>%
                  filter(name == indicator, option == header) %>%
                  select(Year , sii, sii_lower95_0cl, sii_upper95_0cl) %>%
                  rename(`SII` = sii, `95% Lower CI` = sii_lower95_0cl, `95% Upper CI` =sii_upper95_0cl )
                ,extensions = 'Buttons', options = list(
                  dom = 'Bt',
                  buttons = c('csv')
                ), rownames = F) %>%
        formatRound(columns=c('SII', '95% Lower CI', '95% Upper CI'), digits=2)

    })

    output$ICD10RIITABLE  <- renderDataTable({
      
      datatable(allSlopes %>%
                  filter(name == indicator, option == header) %>%
                  select(Year , rii, rii_lower95_0cl, rii_upper95_0cl) %>%
                  rename(`RII` = rii, `95% Lower CI` = rii_lower95_0cl, `95% Upper CI` =rii_upper95_0cl )
                ,extensions = 'Buttons', options = list(
                  dom = 'Bt',
                  buttons = c('csv')
                ), rownames = F) %>%
        formatRound(columns=c('RII', '95% Lower CI', '95% Upper CI'), digits=2)

    })
    
    output$ICD10DSRTABLE  <- renderDataTable({

      datatable(filtered_data <- allDsrs %>%
                  filter(Chapter == indicator, option == header) %>%
                  filter(as.numeric(str_sub(Year, end = 4)) == as.numeric(str_sub(input$siislideryearICCD10, end = 4))) %>%
                  select(Quintile , Std_Rate, Std_LCL, Std_UCL) %>%
                  rename(`DSR` = Std_Rate, `95% Lower CI` = Std_LCL, `95% Upper CI` =Std_UCL )
                ,extensions = 'Buttons', options = list(
                  dom = 'Bt',
                  buttons = c('csv')
                ), rownames = F) %>%
        formatRound(columns=c('DSR', '95% Lower CI', '95% Upper CI'), digits=2)

    })
    
    
    output$ICD10OATABLE  <- renderDataTable({
      
      datatable(allDSRsOverarching %>%
                  filter(Chapter == indicator, option == header) %>%
                  #filter(as.numeric(str_sub(Year, end = 4)) == as.numeric(str_sub(input$siislideryearICCD10, end = 4))) %>%
                  select(Year , Std_Rate, Std_LCL, Std_UCL) %>%
                  rename(`DSR` = Std_Rate, `95% Lower CI` = Std_LCL, `95% Upper CI` =Std_UCL )
                ,extensions = 'Buttons', options = list(
                  dom = 'Bt',
                  buttons = c('csv')
                ), rownames = F) %>%
        formatRound(columns=c('DSR', '95% Lower CI', '95% Upper CI'), digits=2)
      
    })
    

    
    
    output$SIITime <- renderPlot({
      filtered_data_rates <- allSlopes %>%
        filter(name == indicator, option == header) 
      
      pbc_selected <- as.numeric(str_sub(input$siislideryearICCD10, end = 4))  # Get the selected PBCs
      filtered_data_rates$Legend <- ifelse(as.numeric(str_sub(filtered_data_rates$Year, end = 4)) %in% pbc_selected, 'Year Selected', 'Not Selected')

      model_data <- data.frame(Year = unique(filtered_data_rates$Year))

      if (length(model_data) == 1) {

        p <- ggplot(data = filtered_data_rates, aes(color = Legend,x = Year, y = sii)) +
        geom_bar(aes( y = sii), stat = "identity", fill = "lightblue") +
        geom_errorbar(width = 0.2,data = filtered_data_rates, 
                      aes(color = Legend, x = Year, ymin = sii - (sii  - sii_lower95_0cl), ymax = sii + (sii_upper95_0cl  - sii))) +
        #geom_line(data = model_data, aes(x = Year, y = Predicted, color = 'green')) +
        
        scale_color_manual(values = c('Year Selected' = 'red', 'Not Selected' = 'black')) +
        ggtitle(paste0("SII of ",paste(strwrap(header,45), collapse="\n"), " by Year")) +
        ylab(paste0("SII of ",paste(strwrap(header,45), collapse="\n"))) +
        xlab("Year") +
        theme_minimal() + 
        theme(plot.title = element_text(hjust = 0.5, size =14))
      } else {

      model_data$Predicted <- predict(lm(sii ~ Year, data=filtered_data_rates), newdata=model_data)

      p <- ggplot(data = filtered_data_rates, aes(color = Legend,x = Year, y = sii)) +
        geom_bar(aes( y = sii), stat = "identity", fill = "lightblue") +
        geom_errorbar(width = 0.2,data = filtered_data_rates, 
                      aes(color = Legend, x = Year, ymin = sii - (sii  - sii_lower95_0cl), ymax = sii + (sii_upper95_0cl  - sii))) +
        geom_line(data = model_data, aes(x = Year, y = Predicted, color = 'green')) +
        
        scale_color_manual(values = c('Year Selected' = 'red', 'Not Selected' = 'black')) +
        ggtitle(paste0("SII of ",paste(strwrap(header,45), collapse="\n"), " by Year")) +
        ylab(paste0("SII of ",paste(strwrap(header,45), collapse="\n"))) +
        xlab("Year") +
        theme_minimal() + 
        theme(plot.title = element_text(hjust = 0.5, size =14))
      }

      p

    })

    output$SIITimerii <- renderPlot({
      filtered_data_rates <- allSlopes %>%
        filter(name == indicator, option == header) 

      pbc_selected <- as.numeric(str_sub(input$siislideryearICCD10, end = 4)) # Get the selected PBCs
      filtered_data_rates$Legend <- ifelse(as.numeric(str_sub(filtered_data_rates$Year, end = 4)) %in% pbc_selected, 'Year Selected', 'Not Selected')

      model_data <- data.frame(Year = unique(filtered_data_rates$Year))

      if (length(model_data) == 1) {

         p <- ggplot(data = filtered_data_rates, aes(color = Legend,x = Year, y = (rii))) +
        geom_bar(aes( y = (rii)), stat = "identity", fill = "lightblue") +
        geom_errorbar(width = 0.2,data = filtered_data_rates, 
                      aes(color = Legend, x = Year, ymin = rii - (rii  - rii_lower95_0cl), ymax = rii + (rii_upper95_0cl  - rii))) +
        #geom_line(data = model_data, aes(x = Year, y = Predicted, color = 'green')) +
        scale_color_manual(values = c('Year Selected' = 'red', 'Not Selected' = 'black')) +
        ggtitle(paste0("RII of ",paste(strwrap(header,45), collapse="\n"), " by Year")) +
        ylab(paste0("RII of ",paste(strwrap(header,45), collapse="\n"))) +
        xlab("Year") +
        theme_minimal() + 
        theme(plot.title = element_text(hjust = 0.5, size =14))

      } else {
      model_data$Predicted <- predict(lm(rii ~ Year, data=filtered_data_rates), newdata=model_data)

      p <- ggplot(data = filtered_data_rates, aes(color = Legend,x = Year, y = (rii))) +
        geom_bar(aes( y = (rii)), stat = "identity", fill = "lightblue") +
        geom_errorbar(width = 0.2,data = filtered_data_rates, 
                      aes(color = Legend, x = Year, ymin = rii - (rii  - rii_lower95_0cl), ymax = rii + (rii_upper95_0cl  - rii))) +
        geom_line(data = model_data, aes(x = Year, y = Predicted, color = 'green')) +
        scale_color_manual(values = c('Year Selected' = 'red', 'Not Selected' = 'black')) +
        ggtitle(paste0("RII of ",paste(strwrap(header,45), collapse="\n"), " by Year")) +
        ylab(paste0("RII of ",paste(strwrap(header,45), collapse="\n"))) +
        xlab("Year") +
        theme_minimal() + 
        theme(plot.title = element_text(hjust = 0.5, size =14))
      }

      p

    })
    
    
    ### DSRS 
    
    output$QPTimerii <- renderPlot({
      
      
      filtered_data <- allDsrs %>%
        filter(Chapter == indicator, option == header) %>%
        mutate(Quintile = as.factor(Quintile))
      
      filtered_data <- filtered_data[filtered_data$Quintile %in% input$quintiles, ]
        #filter(as.numeric(str_sub(Year, end = 4)) == as.numeric(str_sub(input$siislideryearICCD10, end = 4)))
      
      p <- ggplot(filtered_data, aes(x = Year, y= Std_Rate , color = Quintile , group = Quintile)) +
        labs(X = "Year", y = paste(strwrap(header,45), collapse="\n"), color = "IMD Quintile") +
        theme_minimal()
      
      if (input$chart_type == "points" & input$add_oaDSR == 'no') {
        p <- p + geom_point() +
          geom_line() +
          geom_errorbar(data = filtered_data ,
                        aes(x = Year, ymin = Std_Rate - (Std_Rate - Std_LCL), ymax = Std_Rate + (Std_UCL - Std_Rate))) +
          ylim(0, max(allDSRsOverarching %>%
                        filter(Chapter == indicator, option == header) %>% 
                        select(Std_Rate) %>%
                        pull()) * 1.5) 
      } else if (input$chart_type == "lines" & input$add_oaDSR == 'no') {
        p <- p + geom_line() +
          ylim(0, max(allDSRsOverarching %>%
                        filter(Chapter == indicator, option == header) %>% 
                        select(Std_Rate) %>%
                        pull()) * 1.5) #+
         # geom_errorbar(data = filtered_data ,
           #             aes(x = Year, ymin = Std_Rate - (Std_Rate - Std_LCL), ymax = Std_Rate + (Std_UCL - Std_Rate)))
      } else if (input$chart_type == "points" & input$add_oaDSR == 'yes') {
        p <- p + geom_point() +
          geom_line() +
          geom_errorbar(data = filtered_data ,
                        aes(x = Year, ymin = Std_Rate - (Std_Rate - Std_LCL), ymax = Std_Rate + (Std_UCL - Std_Rate))) +
          geom_point(data = allDSRsOverarching %>%
                       filter(Chapter == indicator, option == header) %>%
                       mutate(Quintile = "Total Population"), aes(x = Year, y = Std_Rate)) +
          geom_errorbar(data = allDSRsOverarching %>%
                          filter(Chapter == indicator, option == header)%>%
                          mutate(Quintile = "Total Population"),
            aes(x = Year, ymin = Std_Rate - (Std_Rate - Std_LCL), ymax = Std_Rate + (Std_UCL - Std_Rate))) +
          geom_line(data = allDSRsOverarching %>%
                      filter(Chapter == indicator, option == header)%>%
                      mutate(Quintile = "Total Population"),aes(x = Year, y= Std_Rate)) +
          ylim(0, max(allDSRsOverarching %>%
                        filter(Chapter == indicator, option == header) %>% 
                        select(Std_Rate) %>%
                        pull()) * 1.5) 
          
      } else if (input$chart_type == "lines" & input$add_oaDSR == 'yes'){
        
        p <- p + geom_line() +
          geom_line(data = allDSRsOverarching %>%
                      filter(Chapter == indicator, option == header)%>%
                      mutate(Quintile = "Total Population"),aes(x = Year, y= Std_Rate)) +
          ylim(0, max(allDSRsOverarching %>%
                        filter(Chapter == indicator, option == header) %>% 
                        select(Std_Rate) %>%
                        pull()) * 1.5) 
      }
      print(p)
      
    })
    
    
    output$ORTimerii <- renderPlot({
      
      
      filtered_data <- allDSRsOverarching %>%
        filter(Chapter == indicator, option == header)
      #filter(as.numeric(str_sub(Year, end = 4)) == as.numeric(str_sub(input$siislideryearICCD10, end = 4)))
    
      
      p <- ggplot(data = filtered_data, aes(x = Year, y = Std_Rate)) +
        geom_bar(aes(y = Std_Rate), fill = 'lightblue', stat="identity") +
        geom_errorbar(width = 0.2,
                      aes(x = Year, ymin = Std_Rate - (Std_Rate - Std_LCL), ymax = Std_Rate + (Std_UCL - Std_Rate))) +
        geom_smooth(method = "lm", se = F, aes(group=1), color = 'green') +
        #scale_x_continuous(name = "Year", breaks = 1:5, limits = c(0, 5.5)) +
        ggtitle(paste0("DSRs Total Population of ",paste(strwrap(header,45), collapse="\n", " by Year"))) +
        ylab(paste(strwrap(header,45), collapse="\n")) +
        ylim(0, max(allDsrs %>%
                      filter(Chapter == indicator, option == header) %>% 
                      select(Std_Rate) %>%
                      pull()) * 1.5) +
        theme_minimal() +
        theme(plot.title = element_text(hjust = 0.5, size =14))
      p
      
      
    })
    
    
    output$ICDODSRPLOTS <- renderPlot({
      
      filtered_data <- allDsrs %>%
        filter(Chapter == indicator, option == header) %>%
        filter(as.numeric(str_sub(Year, end = 4)) == as.numeric(str_sub(input$siislideryearICCD10, end = 4)))
      
      model_data <- data.frame(Quintile = 1:5)
      model_data$Predicted <- predict(lm(Std_Rate ~ Quintile, data=filtered_data ), newdata=model_data)
      
      # Your provided plotting code adjusted for the filtered data
      p <- ggplot(data = filtered_data , aes(x = Quintile, y = Std_Rate)) +
        geom_bar(aes(y =Std_Rate), stat = "identity", fill = "lightblue" ) +
        geom_errorbar(data = filtered_data ,width = 0.2,
                      aes(x = Quintile, ymin = Std_Rate - (Std_Rate - Std_LCL), ymax = Std_Rate + (Std_UCL - Std_Rate))) +
        geom_line(data = model_data, aes(x = Quintile, y = Predicted), color = 'green') +
        scale_x_continuous(name = paste("Quintile 1 (Most Deprived)", "\u2192" ,"5 (Least Deprived)"), breaks = 1:5, limits = c(0, 5.5)) +
        ggtitle(paste0(paste(strwrap(header,45), collapse="\n"), " by Quintile ", as.numeric(str_sub(input$siislideryearICCD10, end = 4)))) +
        ylab(paste(strwrap(header,45), collapse="\n")) +
        ylim(0, max(allDsrs %>%
                                        filter(Chapter == indicator, option == header) %>% 
                                        select(Std_Rate) %>%
                                        pull()) * 1.5) +
        theme_minimal() +
        theme(plot.title = element_text(hjust = 0.5, size =14))
      p

    })

  })
  
  observeEvent(input$back, {
    updateTabItems(session, "sidebarID", selected = "Index2")
  })
  
  

