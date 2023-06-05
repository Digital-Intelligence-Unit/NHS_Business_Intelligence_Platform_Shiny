source("./data.r",local=TRUE)

# code functions
source("./app/code/mod_summary.r",local = TRUE)

require(shiny)
require(visNetwork)
library(rpart)
library(xml2)
library(rvest)
library(jsonlite)

convert_bayesian_output_to_df <- function(output_string, column_names) {
  # Split the input string by new lines
  lines <- strsplit(output_string, "\n")[[1]]
  
  # Initialize an empty data frame
  df <- data.frame(
    Node = character(),
    Parent = character(),
    Value = numeric(),
    stringsAsFactors = FALSE
  )
  
  current_node <- ""
  current_parent <- ""
  
  # Prepare a regular expression pattern for column names
  column_name_pattern <- paste(column_names, collapse = "|")
  
  # Loop through each line
  for (line in lines) {
    # Check if the line contains node information
    if (grepl("Parameters of node", line)) {
      current_node <- gsub("Parameters of node | \\(.*", "", line)
    }
    
    # Check if the line contains parent information
    if (grepl(column_name_pattern, line) && !grepl("Conditional probability table", line)) {
      current_parent <- gsub("^\\s*|\\s*$", "", line)
    }
    
    # Check if the line contains probability values
    if (grepl("^\\s*[0-9.]+\\s*[0-9.]+", line)) {
      values <- as.numeric(unlist(strsplit(gsub("^\\s*|\\s*$", "", line), " ")))
      
      temp_df <- data.frame(
        Node = current_node,
        Parent = current_parent,
        Value = values,
        stringsAsFactors = FALSE
      )
      df <- rbind(df, temp_df)
    }
  }
  return(df)
}

strip_html <- function(s) {
  html_text(read_html(s))
}

convert_to_json <- function(filter_string, column_names) {
  # Split the input string by new lines
  lines <- strsplit(filter_string, "\n")[[1]]
  
  # Initialize an empty list to store the filters
  filters <- list()
  
  # Loop through each line
  for (line in lines) {
    if (grepl(" = ", line) && grepl(" to ", line)) {
      # Extract the column name, lower bound, and upper bound
      column_name <- trimws(gsub("=.*$", "", line))
      bounds <- strsplit(gsub("^.*= |$", "", line), " to ")[[1]]
      lower_bound <- as.numeric(trimws(bounds[1]))
      upper_bound <- as.numeric(trimws(bounds[2]))

      # Store the range filter in the list
      filters[[column_name]] <- list(c(lower_bound, upper_bound))
    } else if (grepl("<", line)) {
      # Upper bound only
      column_name <- trimws(gsub("<.*$", "", line))
      upper_bound <- round(as.numeric(gsub("^.*<|$", "", line)),0)
      
      # Store the upper bound in the list
      filters[[column_name]] <- list(c(0,upper_bound))
    } else if (grepl(">=", line)) {
      # Lower bound only
      column_name <- trimws(gsub(">=.*$", "", line))
      lower_bound <- round(as.numeric(gsub("^.*>=\\s*", "", line)),0)
      
      # Store the lower bound in the list
      filters[[column_name]] <- list(c(lower_bound,120))
    } else if (grepl(" or ", line)) {
      # where condition is contains or
      matching_col <- column_names[sapply(column_names, function(x) grepl(x, line))]
      
      if (length(matching_col) > 0 ) {
        column_value <- gsub(paste0(matching_col, "\\s*=\\s*"), "", line)
        #column_value <- gsub(matching_col, "", line)
        column_value <- unlist(strsplit(column_value, "or"))
        column_value <- column_value[column_value != " "]
        column_value <- trimws(column_value)
        filters[[matching_col]] <- (column_value)
        
      }
    } else if (grepl("=", line)) {
      # Upper bound only
      column_name <- trimws(gsub("=.*$", "", line))
      column_value <- as.character(gsub("^.*=\\s*", "", line))
      column_value <- trimws(column_value)
      
      # Store the upper bound in the list
      filters[[column_name]] <- list(column_value)
    } else {
      # Find the matching column name in the list of known column names
      matching_col <- column_names[sapply(column_names, function(x) grepl(x, line))]
      
      if (length(matching_col) > 0) {
        # Extract the column value, removing any "=" character that follows the column name
        column_value <- gsub(paste0(matching_col, "\\s*=\\s*"), "", line)
        
        # Trim leading and trailing whitespaces from the column_value
        column_value <- unlist(strsplit(column_value, " "))
        
        # Remove any extra whitespace between words
        column_value <- column_value[column_value != ""]
        
        # Store the column value in the list using 'matching_col' as the index
        filters[[matching_col]] <- list(column_value)
      }
    }
  }
  
  # Convert the list to JSON
  json_output <- toJSON(filters, auto_unbox = TRUE)
  
  return(json_output)
}

server <- function(input,output,session){

  queryParam <- reactive({
    # Get the query string
    query <- parseQueryString(session$clientData$url_search)
  })
  # ... other server code ...


  observeEvent(input$tab1_button1, {
    updateTabsetPanel(
      session,
      "my_tabs",
      selected = "Decision Tree"
    )
  })

  observeEvent(input$tab2_button2, {
    updateTabsetPanel(
      session,
      "my_tabs",
      selected = "GLM"
    )
  })

  observeEvent(input$tab3_button3, {
    updateTabsetPanel(
      session,
      "my_tabs",
      selected = "Bayesian network"
    )
  })

  observeEvent(input$tab4_button4, {
    updateTabsetPanel(
      session,
      "my_tabs",
      selected = "clustering"
    )
  })


  observeEvent(input$tab1_button12, {
    updateTabsetPanel(
      session,
      "my_tabs",
      selected = "Decision Tree"
    )
  })

  observeEvent(input$tab2_button22, {
    updateTabsetPanel(
      session,
      "my_tabs",
      selected = "GLM"
    )
  })


  observeEvent(input$tab3_button32, {
    updateTabsetPanel(
      session,
      "my_tabs",
      selected = "Bayesian network"
    )
  })

  observeEvent(input$tab4_button42, {
    updateTabsetPanel(
      session,
      "my_tabs",
      selected = "clustering"
    )
  })

  #Cart
  observeEvent(input$twoCARTgo, {
    if(!is.null(input$twoCARTVar1)) {
      showModal(
        modalDialog(
          HTML("<h2><strong>Please wait while the data is segmented.</strong></h2>"),
          p("This should finish in no more than a minute or two when using the default settings. Growing larger trees may take more time."),
          footer = tagList()
        )
      )
      try({
        add_sco <- function(x){
          gsub(" ", "_", x, fixed = T)
        }
        
        data <- as.data.frame(Query %>%
        select_at(c(unique(input$twoCARTVar1), input$twoCARTVar2)))
        data <- data %>% 
        mutate_if(
          function(x) is.factor(x) && length(levels(x)) == 2 && all(grepl("^\\d*\\.?\\d*$", levels(x))),
          function(x) as.numeric(as.character(x))
        )

        contols <- rpart.control(
          minbucket=input$twoCARTminbucket,
          maxdepth=input$twoCARTmaxdepth,
          cp=input$twoCARTcp
        )
        
        print("CART model")
        model<-rpart(as.formula(paste("`",input$twoCARTVar2,"`", " ~ .", sep = "")),data=data ,control=contols)

        #converting varribles that can work when converted
        
        print("CART model end")
        ##NEED TO REMOVE THIS TESTING ONLY!!
        data2e <<- data
        rules_df <- as.data.frame(rpart.plot::rpart.rules(model))
        rules_df$rules <- apply(rules_df[, -c(1)], 1, function(x) paste(x, collapse = " "))
        rules_df <- rules_df %>% select(rules) %>%
          tibble::rownames_to_column(var = "node_id")
        rules_df$rules <- gsub("when", "", rules_df$rules)
        rules_df$rules <- gsub(" is ", " = ", rules_df$rules)
        #rules_df$rules <- gsub(" <", " ≤ ", rules_df$rules)
        #rules_df$rules <- gsub(" >", " ≥ ", rules_df$rules)
        rules_df$rules <- gsub(" & ", "\n <br>", rules_df$rules)
        
        output$twoCARTTree <- renderVisNetwork({
          tree <- visTree(
            model,
            # width = "100%", 
            main = "classification Tree", 
            colorVar = c("lightgreen", "yellow", "orange","red"),
            colorY = c("green","red"),
            rules = TRUE,
            simplifyRules = TRUE,
            shapeVar = "box",
            shapeY = "square",
            export = FALSE,
            nodesPopSize = TRUE,
            fallenLeaves = TRUE,
            maxNodeSize = 30,
            collapse = list(
              enabled = FALSE,
              fit = TRUE,
              resetHighlight = TRUE,
              clusterOptions = list(
                fixed = TRUE,
                physics = FALSE
              )
            ),
            submain = list(
              text = paste0("Predicitng ",input$twoCARTVar2),
              style = "font-family:Arial;color:black;font-size:15px;text-align:center;"
            ), 
            width = "100%"
          ) %>% 
          visOptions(highlightNearest = list(enabled = T, degree = 2, hover = T)) %>%
          visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE ) %>%
          visEvents(
            click = "function(properties) {
              var nodeId = properties.nodes[0];
              var nodeData = this.body.data.nodes.get(nodeId);
              if (nodeData && nodeData.title) {
                var rules = nodeData.title;
                /*alert('Rules: ' + rules);*/
                
                Shiny.setInputValue('clicked_nodeId', rules, {priority: 'event'});
                // Open the Bootstrap modal dialog box
                
                Shiny.setInputValue('clicked_nodeId_leaf', nodeId, {priority: 'event'});
              }
            }"
          )
          ## Rpart rules
          
          # Modify the title of each node to include the extracted rules
          for (i in 1:nrow(rules_df)) {
            # Find the index of the node in the visTree object
            node_idx <- which(tree$x$nodes$id == rules_df$node_id[i])
            
            # Extract current rules from node title
            #current_rules <- gsub(".*<div class=\"showMeRpartTTp2\" style=\"display:none;\"><b>(.*)</b></div></div>\n\n</div>$", "\\1", tree$x$nodes$title[node_idx])
            
            # Generate new title for the node, including the extracted rules
            new_title <- paste0(
              # Keep the original title up to "<hr class = \"rPartvisNetwork\">"
              gsub("<hr.*", "", tree$x$nodes$title[node_idx]), 
              "<hr class = \"rPartvisNetwork\">\n",
              "<div class =\"showOnMe2\"><div style=\"text-align:center;\"><U style=\"color:black;\"  onmouseover=\"this.style.cursor='pointer';\" onmouseout=\"this.style.cursor='default';\">Rules</U></div>\n",
              #"<div class=\"showMeRpartTTp2\" style=\"display:none;\">\n",
              # Add the new rules
              "<b>", paste(rules_df$rules[i], sep=""), "</b>",
              "</div></div>\n\n</div>"
            )
            # Update the node title with the new title
            tree$x$nodes$title[node_idx] <- new_title
          }
          
          # Return the modified visTree object
          tester_tree <<- tree$x$nodes
          tree
        })
        
        observeEvent(input$clicked_nodeId,{
          node <-read_html(gsub('^.*Rules\\s*|\\s*\\$.*$', '', input$clicked_nodeId)) %>% html_text
          filter_obj <- convert_to_json(node,colnames(data))
          append_to_col_lookup <- function(col_lookup, new_columns) {
              for (col in new_columns) {
                if (!col %in% names(col_lookup)) {
                  col_lookup[[col]] <- "DONTUSE"
                }
              }
            return(col_lookup)
          }
          col_lookup <- c(
            "Age" = "AgeDimension",
            "Sex" = "SexDimension",
            "Risk Score Int" = "RskDimension",
            "mosaic label" = "MDimension",
            "deprivation decile" = "DDimension",
            "household type" = "ADimension" ,
            "electoral ward or division" = "WDimension",
            "Asthma" = "Asthma",
            "Coronary Artery Disease" = "Coronary Artery Disease",
            "Congestive Heart Failure" = "Congestive Heart Failure",
            "Cancer" = "Cancer",                                  
            "Chronic obstructive pulmonary disease" = "COPD",
            "Persistent depressive disorder" = "Depression",         
            "Diabetes" = "Diabetes",
            "Hypertension" = "Hypertension",                            
            "Atrial fibrillation" = "Atrial Fibrillation",
            "Chronic kidney disease" = "Chronic Kidney Disease",                  
            "Dementia" = "Dementia",
            "Epilepsy" = "Epilepsy",                               
            "Hypothyroid" = "Hypothyroid",
            "Mental health" = "Mental Health",                           
            "Learning disability" = "Learning Disabilities",
            "Osteoporosis" = "Osteoporosis",                            
            "Peripheral artery disease" = "Peripheral Artery Disease",
            "Rheumatoid arthritis" = "Rheumatoid Arthritis",
            #"Risk Score" = "RskDimension","Risk Score Group" = "RskDimension",
            "top 20 percent deprived" = "DDimension",
            "age band narrow" = "AgeDimension",
            "age band broad" = "AgeDimension",
            "age markers" = "AgeDimension",
            "age 55 and over" = "AgeDimension",
            "age 65 and over" = "AgeDimension",
            "age 75 and over" = "AgeDimension",
            "age 17-54" = "AgeDimension",
            "age Children" = "AgeDimension"
          )
          #### for deriving columns
          if ("Risk Score Group" %in% names(fromJSON(filter_obj))) {
            # Extract what groups are being used in string
            json_list <- fromJSON(filter_obj)
            # Function to convert the string into integer pairs
            string_to_groups <- function(input_str) {
              split_input <- strsplit(input_str, " ")[[1]]
              groups <- list()
              for (item in split_input) {
                temp <- as.integer(strsplit(item, "_")[[1]])
                groups <- append(groups, list(c(temp[1], temp[2])))
              }
              return(groups)
            }
            # Convert the "Risk Score Group" values and replace them in the list
            json_list$`Risk Score Group` <- lapply(json_list$`Risk Score Group`, string_to_groups)

            # Convert back to JSON
            filter_obj <- toJSON(json_list, auto_unbox = TRUE)
          }

          if ("top 20 percent deprived" %in% names(fromJSON(filter_obj))) {
            core20 <- function(json_string) {
              # Parse the JSON string into a list
              json_list <- fromJSON(json_string)
              # Replace the "top 20" value with the desired list
              if (json_list$`top 20 percent deprived`[[1]] == "1") {
                json_list$`top 20 percent deprived` <- c("1", "2")
              } else if (json_list$`top 20 percent deprived`[[1]] == "0") {
                json_list$`top 20 percent deprived` <- c("3", "4", "5", "6", "7", "8", "9", "10")
              }
              # Convert the modified list back into a JSON string
              modified_json_string <- toJSON(json_list)
              return(modified_json_string)
            }
            # Modify the JSON strings
            filter_obj <- core20(filter_obj)
          }

          # Function to convert the string into integer pairs
          string_to_groups_age <- function(input_str) {
            split_input <- strsplit(input_str, " ")[[1]]
            groups <- list()
            for (item in split_input) {
              temp <- as.integer(strsplit(item, "-")[[1]])
              groups <- append(groups, list(temp))
            }
            return(groups)
          }
          
          # Parse the initial JSON string into a list
          ######## AGE BANDS IS ANOTHER!
          if ("age band narrow" %in% names(fromJSON(filter_obj))) {
            # extract what groups are being used in string
            json_list <- fromJSON(filter_obj)
            # Convert the "groups" values and replace them in the list
            json_list$`age band narrow` <- string_to_groups_age(paste(unlist(json_list$`age band narrow`), collapse = " "))
            # Convert the modified list back into a JSON string
            filter_obj <- toJSON(json_list)
          }

          if ("age band broad" %in% names(fromJSON(filter_obj))) {
            # extract what groups are being used in string
            json_list <- fromJSON(filter_obj)
            # Convert the "groups" values and replace them in the list
            json_list$`age band broad` <- string_to_groups_age(paste(unlist(json_list$`age band broad`), collapse = " "))
            # Convert the modified list back into a JSON string
            filter_obj <- toJSON(json_list)
          }


          if ("age markers" %in% names(fromJSON(filter_obj))) {
            agemarkers <- function(json_string) {
              # Parse the JSON string into a list
              json_list <- fromJSON(json_string)
              
              # Replace the "top 20" value with the desired list
              if (json_list$`age markers`[[1]] == "Children") {
                json_list$`age markers` <- list(c(0, 16))
              } else if (json_list$`age markers`[[1]] == "17-54") {
                json_list$`age markers` <-  list(c(17, 54))
              } else if (json_list$`age markers`[[1]] == "55 and over") {
                json_list$`age markers` <-  list(c(55, 64))
              } else if (json_list$`age markers`[[1]] == "65 and over") {
                json_list$`age markers` <-  list(c(65, 74))
              } else if (json_list$`age markers`[[1]] == "75 and over") {
                json_list$`age markers` <-  list(c(75, 120))
              }
              # Convert the modified list back into a JSON string
              modified_json_string <- toJSON(json_list)
              return(modified_json_string)
            }
            # Modify the JSON strings
            filter_obj <- agemarkers(filter_obj)
          }
          
          if ("age 55 and over" %in% names(fromJSON(filter_obj))) {
            agemarkers_55 <- function(json_string) {
              # Parse the JSON string into a list
              json_list <- fromJSON(json_string)
              # Replace the "top 20" value with the desired list
              if (json_list$`age 55 and over`[[1]] == "1") {
                json_list$`age 55 and over` <- list(c("55", "64"))
              } else if (json_list$`age 55 and over`[[1]] == "0") {
                json_list$`age 55 and over` <-  list(c("65","120"),c("0", "54"))
              }
              # Convert the modified list back into a JSON string
              modified_json_string <- toJSON(json_list)
              return(modified_json_string)
            }
            # Modify the JSON strings
            filter_obj <- agemarkers_55(filter_obj)
          }

          if ("age Children" %in% names(fromJSON(filter_obj))) {
            agemarkers_Children <- function(json_string) {
              # Parse the JSON string into a list
              json_list <- fromJSON(json_string) 
              # Replace the "top 20" value with the desired list
              if (json_list$`age Children`[[1]] == "1") {
                json_list$`age Children` <- list(c("0", "16"))
              } else if (json_list$`age Children`[[1]] == "0") {
                json_list$`age Children` <-   list(c("17","120"))
              }
              # Convert the modified list back into a JSON string
              modified_json_string <- toJSON(json_list)
              return(modified_json_string)
            }
            # Modify the JSON strings
            filter_obj <- agemarkers_Children(filter_obj)
          }
          
          if ("age 65 and over" %in% names(fromJSON(filter_obj))) {
            agemarkers_65 <- function(json_string) {
              # Parse the JSON string into a list
              json_list <- fromJSON(json_string)
              
              # Replace the "top 20" value with the desired list
              if (json_list$`age 65 and over`[[1]] == "1") {
                json_list$`age 65 and over` <- list(c("65", "74"))
              } else if (json_list$`age 65 and over`[[1]] == "0") {
                json_list$`age 65 and over` <-   list(c("0","64"),c("75","120"))
              }
              # Convert the modified list back into a JSON string
              modified_json_string <- toJSON(json_list)
              
              return(modified_json_string)
            }
            # Modify the JSON strings
            filter_obj <- agemarkers_65(filter_obj)
          }
          
          if ("age 75 and over" %in% names(fromJSON(filter_obj))) {
            agemarkers_75 <- function(json_string) {
              # Parse the JSON string into a list
              json_list <- fromJSON(json_string) 
              # Replace the "top 20" value with the desired list
              if (json_list$`age 75 and over`[[1]] == "1") {
                json_list$`age 75 and over` <- list(c("75", "120"))
              } else if (json_list$`age 75 and over`[[1]] == "0") {
                json_list$`age 75 and over` <-   list(c("0","74"))
              }
              # Convert the modified list back into a JSON string
              modified_json_string <- toJSON(json_list)  
              return(modified_json_string)
            }
            # Modify the JSON strings
            filter_obj <- agemarkers_75(filter_obj)
          }
          
          if ("age 17-54" %in% names(fromJSON(filter_obj))) {
            agemarkers_17 <- function(json_string) {
              # Parse the JSON string into a list
              json_list <- fromJSON(json_string)
              # Replace the "top 20" value with the desired list
              if (json_list$`age 17-54`[[1]] == "1") {
                json_list$`age 17-54` <- list(c("17", "54"))
              } else if (json_list$`age 17-54`[[1]] == "0") {
                json_list$`age 17-54` <-   list(c("0","16"),c("55","120"))
              }
              # Convert the modified list back into a JSON string
              modified_json_string <- toJSON(json_list)
              return(modified_json_string)
            }
            # Modify the JSON strings
            filter_obj <- agemarkers_17(filter_obj)
          }
          # Print the modified JSON string
          col_lookup <- append_to_col_lookup(col_lookup, colnames(data))
          #json_str <- convert_to_json("Petal.Length = 1.3 to 1.5\nSpecies = setosa", col_lookup)
          filters <- fromJSON(filter_obj)
          # Rename the columns using the lookup table
          names(filters) <- unlist(col_lookup[names(filters)])
          # Convert the list back to JSON
          json_output <- toJSON(filters, auto_unbox = TRUE)
          json_list <- fromJSON(json_output)
          # filter the elements based on the parent key
          json_list <- json_list[!grepl("^DONTUSE", names(json_list))]
          # convert the filtered list back to a JSON object
          filtered_json_str <<- toJSON(json_list)
          UpdatedRulesPHI <<- paste(names(json_list))
      
          # Group columns
          group_cols <- c("Asthma", "Coronary Artery Disease", "Congestive Heart Failure", "Cancer", "COPD", "Depression", "Diabetes", "Hypertension", "Atrial Fibrillation", "Chronic Kidney Disease","Dementia", "Epilepsy", "Hypothyroid", "Mental Health", "Learning Disabilities", "Osteoporosis", "Peripheral Artery Disease", "Rheumatoid Arthritis")
          # Function to process JSON
          modify_json <- function(json_str, group_cols) {
            # Parse the JSON string into a list
            json_list <- fromJSON(json_str)
            # Find all the columns that are not in group_cols
            non_group_cols <- setdiff(names(json_list), group_cols)
            # Create a new list to store the modified JSON
            modified_json_list <- list()
            # Copy over the non-group columns to the modified JSON list
            for (col in non_group_cols) {
              modified_json_list[[col]] <- json_list[[col]]
            }
            # Find the values of the group columns that are equal to 1
            group_col_values <- sapply(unlist(json_list[group_cols]), function(x) x == 1)
            # Extract the names of the group columns that have a value of 1
            selected_group_cols <-names(which(unlist(group_col_values)))
            # Check if vector contains both 1 and 0
            if (any(group_col_values == 1) && any(group_col_values == 0)) {
              UpdatedRulesPHI <<- "Sorry but we cant filter by LTCs at this time."
            } else {
              if (length(selected_group_cols) > 0) {
                modified_json_list[["LTCs2Dimension"]] <- unname(selected_group_cols)
              } else {
                modified_json_list[["LTCs2Dimension"]] <- setdiff(c(group_cols, "None"), names(json_list[!group_col_values])) 
                modified_json_list[["NumSelectedLtcs"]] <- c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14")
              }
            }
            # Convert the modified JSON list back to a JSON string
            modified_json_str <- toJSON(modified_json_list)
            return(modified_json_str)
          }

          consolidate_dimensions <- function(json_string) {
            # Import the required library
            library(jsonlite)
            # Parse the JSON string
            json_data <- fromJSON(json_string, simplifyVector = FALSE)
            # Get the dimension names without trailing numbers
            dimension_names <- gsub("\\.\\d+$", "", names(json_data))
            # Initialize new data list
            new_data <- list()
            # Loop over unique dimension names
            for (dim_name in unique(dimension_names)) {
              # Get all matching dimensions
              matching_dims <- json_data[startsWith(names(json_data), dim_name)]
              # Concatenate the vectors without additional nesting
              consolidated_dim <- unlist(matching_dims, use.names = FALSE)
              # Assign the consolidated dimension to the new data
              new_data[[dim_name]] <- consolidated_dim
            }
            # Convert back to JSON
            json_string <- toJSON(new_data, auto_unbox = TRUE)
            return(json_string)
          }
          filtered_json_str <- consolidate_dimensions(filtered_json_str)
          check_span <- function(vec) {
            max_val <- max(vec)
            min_val <- min(vec)
            length_val <- length(unique(as.list((vec))))
            
            is_sequential <- (max_val - min_val + 1) == length_val
            
            return(is_sequential)
          }
          check_overlap_remove_dimension <- function(json_string, dim) {
            # Import the required library
            library(jsonlite)
            
            # Parse the JSON string
            json_data <- fromJSON(json_string, simplifyVector = FALSE)
            
            # Get the dimension vector
            dimension <- json_data[[dim]]
            
            # Check if the vector length is even and has pairs
            if (is.vector(dimension) && length(dimension) %% 2 == 0) {
              pairs <- split(dimension, ceiling(seq_along(dimension)/2))
              
              # Convert the pairs to sequential ranges
              ranges <- lapply(pairs, function(pair) {
                seq_val <- seq(pair[[1]], pair[[2]])
                return(seq_val)
              })
              
              # Flatten the structure and convert to the expected format
              ranges <- lapply(ranges, function(seq_val) {
                if (length(seq_val) == 1) {
                  return(list(seq_val))
                } else {
                  return(seq_val)
                }
              })
              
              # Assign the ranges to the dimension
              #json_data[[dim]] <- ranges
              #print(seq(min(unlist(ranges)),max(unlist(ranges))))
              # Check if the ranges are sequential
              is_sequential <- check_span(unlist(ranges,use.names = F))
              if (!is_sequential) {
                json_data <- fromJSON(json_string)
                json_data[[dim]] <- NULL
                json_string <- toJSON(json_data)
              } else if (is_sequential) {
                json_data <- fromJSON(json_string)
                json_data[[dim]] <- list(c(min(unlist(ranges)), max(unlist(ranges))))
                json_string <- toJSON(json_data)
              }
            }
            return(json_string)
          }
          if ("AgeDimension" %in% names(fromJSON(filtered_json_str))){
            filtered_json_str <- check_overlap_remove_dimension(filtered_json_str,"AgeDimension")
          }
          if ("RskDimension" %in% names(fromJSON(filtered_json_str))){
            filtered_json_str <- check_overlap_remove_dimension(filtered_json_str,"RskDimension")
          }
          # Process JSON and print result
          if (any(group_cols %in% names(fromJSON(filtered_json_str)))){
            modified_json_str <<- modify_json(filtered_json_str, group_cols)
          } else {
            modified_json_str <<- filtered_json_str
          }

          ### RiskScore just use the round function to 0 dp.
          json_list <- fromJSON(modified_json_str)

          # check and create the Age range
          # Create an empty variable to hold all the results
          output_string <- ""

          # Check and create the Age range
          if ("AgeDimension" %in% names(json_list)){
            age_range <- json_list$AgeDimension[1]:json_list$AgeDimension[2]
            output_string <- paste(output_string, paste("Age from", min(age_range), "to", max(age_range)), sep = "<br>")
          }

          # Check and create the D vector
          if ("DDimension" %in% names(json_list)){
            d_vector <- as.integer(json_list$DDimension)
            output_string <- paste(output_string, paste("Deprivation =", paste(d_vector, collapse = ",")), sep = "<br>")
          }

          # Check and create the Risk range
          if ("RskDimension" %in% names(json_list)){
            risk_range <- json_list$RskDimension[1]:json_list$RskDimension[2]
            output_string <- paste(output_string, paste("Risk from", min(risk_range), "to", max(risk_range)), sep = "<br>")
          }

          # Check and create the Sex vector
          if ("SexDimension" %in% names(json_list)){
            s_vector <- as.character(json_list$SexDimension)
            output_string <- paste(output_string, paste("Sex =", paste(s_vector, collapse = ",")), sep = "<br>")
          }

          # Check and create the Mosiac vector
          if ("MDimension" %in% names(json_list)){
            m_vector <- as.character(json_list$MDimension)
            output_string <- paste(output_string, paste("Mosiac =", paste(m_vector, collapse = ",")), sep = "<br>")
          }

          # Check and create the Acorn vector
          if ("ADimension" %in% names(json_list)){
            a_vector <- as.character(json_list$ADimension)
            output_string <- paste(output_string, paste("Acorn =", paste(a_vector, collapse = ",")), sep = "<br>")
          }

          if ("NumSelectedLtcs" %in% names(json_list) && "LTCs2Dimension" %in% names(json_list)){
            not_selected_ltcs <- as.character(json_list$LTCs2Dimension)
            not_selected_ltcs <- setdiff(group_cols, not_selected_ltcs)
            output_string <- paste(output_string, paste("LTCs not selected", paste(not_selected_ltcs, collapse = ",")), sep = "<br>")
              
          }

          # case where only "LTCs2Dimension" is present
          else if ("LTCs2Dimension" %in% names(json_list)){
            selected_ltcs <- as.character(json_list$LTCs2Dimension)
            output_string <- paste(output_string, paste("LTCs selected", paste(selected_ltcs, collapse = ",")), sep = "<br>")
            
          }

          if ("WDimension" %in% names(json_list)){
            w_vector <- as.character(json_list$WDimension)
            output_string <- paste(output_string, paste("Wards selected", paste(w_vector, collapse = ",")), sep = "<br>")
          }
          # Print the output
          output_string <<- output_string
          modified_json_str
        })

        observeEvent(input$clicked_nodeId_leaf, {
          check_ids <- tester_tree %>% filter(level ==max(tester_tree$level)) %>% dplyr::select(id) %>% unlist
          #print(tester_tree$x$nodes)
          if (input$clicked_nodeId_leaf %in%  check_ids && input$clicked_nodeId_leaf %in% rules_df$node_id) {
            footer_elements <- list(modalButton("Close"))
            if (nchar(output_string) > 0) {
              footer_elements <- append(footer_elements, list(actionButton("ApplyRules", "Apply")))
            }

            showModal(modalDialog(
              title = "Do you want to apply these rules to PHI?",
              HTML(paste0("These are the rules from the leaf node selected: <br>",rules_df %>% filter(node_id == input$clicked_nodeId_leaf) %>% select(rules) %>% pull()),
                "<br> <br> <b> However, PHI can only use these following parts of the rules. </b> <br>",
                output_string  
              ), 
              footer = tagList(footer_elements),
              easyClose = TRUE
            ))

            observeEvent(input$ApplyRules, {
              # Remove the first modal before opening the second one
              removeModal()
              
              showModal(modalDialog(
                title = "Apply Cohort",
                textInput("cohortName", "Enter cohort name:"),
                actionButton("cohortSubmit", "Submit"),
                footer = NULL,
                easyClose = FALSE
              ))
            })
  
            observeEvent(input$cohortSubmit, {
              if(input$cohortName == "") {
                showModal(modalDialog(
                  title = "Error",
                  "Please enter a cohort name.",
                  easyClose = TRUE,
                  footer = NULL
                ))
              } else {
                # User input is valid, you can continue your operations here
                print(paste("You entered:", input$cohortName))

                session$sendCustomMessage(type = 'addCohort', message = c(list(modified_json_str,input$cohortName,queryParam()$user_id,queryParam()$referrer)))

                removeModal()
              }
            })
          }
        })

        dat2$cartModel <<- model
          ids <- sort(unique(dat2$cartModel$where))
          rules <- rpart.rules(dat2$cartModel)#cover = TRUE,
          newIDs <- 1:length(ids)
          names(newIDs) <- ids
          newSeg <- newIDs[as.character(dat2$cartModel$where)]
          dat2$segMem[[3]] <<- newSeg
            updateCheckboxInput(session, "twoCARTTreeRules", value = FALSE)
            updateCheckboxInput(session, "twoCARTTreeVARIMP", value = FALSE)
          })
          removeModal()
        }
      })
      
      #show_results
      output$twoCARTTreeRulesTableUIVARIMP <- renderUI({
        if(input$twoCARTTreeVARIMP) {
          column(width = 12,
            plotOutput("twoCARTTreeRulesTableVARIMP2"),style = "overflow-y: scroll;overflow-x: scroll;",
          )
        } else {
          plotOutput("twoCARTTreeRulesTableUIBlankVAR")
        }
      })

      observeEvent(input$twoCARTTreeVARIMP, ignoreInit = T, {
        if(input$twoCARTTreeVARIMP) {
          output$twoCARTTreeRulesTableVARIMP2 <- renderPlot({

            df <- data.frame(imp = dat2$cartModel$variable.importance)
            df2 <- df %>% 
              tibble::rownames_to_column() %>% 
              dplyr::rename("variable" = rowname) %>% 
              dplyr::arrange(imp) %>%
              dplyr::mutate(variable = forcats::fct_inorder(variable))
            
            ggplot2::ggplot(df2) +
              geom_segment(aes(x = variable, y = 0, xend = variable, yend = imp), 
                          size = 1.5, alpha = 0.7) +
              geom_point(aes(x = variable, y = imp, col = variable), 
                        size = 4, show.legend = F) +
              coord_flip() +
              theme_bw()        
            
          })
        }
      })

      #GLMS
      observeEvent(input$glmgo, {
        if(!is.null(input$glmVar1)) {
          showModal(
            modalDialog(
              HTML("<h2><strong>Please wait while the data is segmented.</strong></h2>"),
              p("This should finish in no more than a minute or two."),
              footer = tagList()
            )
          )
          try({
            add_sco <- function(x){
              gsub(" ", "_", x, fixed = T)
            }
            updateCheckboxInput(session, "glmoddsratios", value = FALSE)

            data <- as.data.frame(Query %>% select_at(c(unique(input$glmVar1), input$glm2Var2)))

            dichotomous <- colnames(data %>%
              as_data_frame() %>%
              select(-c(input$glm2Var2)) %>%
              select_if(~ is.factor(.) & nlevels(.) > 2)
            )

            onehot <- colnames(data %>%
              as_data_frame() %>%
              select(-c(dichotomous),-c(input$glm2Var2))
            )
                    
            mtype <- modeltype %>% filter(lookup == input$glm2Var2) %>% select (type )%>% pull

            if(mtype == "binomial"){
              
              model <- glm(as.formula(paste("`",input$glm2Var2,"`", " ~ .", sep = "")), family=(modeltype %>% filter(lookup == input$glm2Var2) %>% select (type )%>% pull), data=data)             
                        
              output[["glmFamily"]] <- renderText({  
                paste0('Model Family ', (modeltype %>% filter(lookup == input$glm2Var2) %>% select (type )%>% pull))
              })
              
              `mod summary stars` <- mod_summary(model)
              
              output$glmTable <- renderTable({
                table <- logistic.display(model, simplified=TRUE,decimal  =2)
                table <- as.data.frame(table$table)
                ordered<- rownames(table)
                table <- base::merge(table, as.data.frame(`mod summary stars`), by=0, all=TRUE) 
                table <-table[match(ordered, table$Row.names),]

                for(i2 in unique(onehot)){
                  table <- table %>%
                  mutate(across(Row.names, ~ ifelse(startsWith(.x, i2),substring(.x, 1, nchar(.x)-1),
                  ifelse(startsWith(.x, paste0("`",i2)),substring(.x, 1, nchar(.x)-1), .x))))
                }

                table <- na.omit(table,) %>% as.data.frame(row.names = 1:nrow(.))
                table <-table %>%  
                    mutate(across(OR:upper95ci, ~ifelse(.x > 1000,formatC(.x, format="e", digits=2),round(.x,2))))
                
                table$OR <- as.character(table$OR)
                table$Row.names <- gsub("`", "", table$Row.names, fixed = TRUE)

                table$lower95ci <- as.character(table$lower95ci)
                table$`Pr(>|Z|)` <- as.character(table$`Pr(>|Z|)`)
                table$upper95ci <- as.character(table$upper95ci)
                table$`mod summary stars` <- as.character(table$`mod summary stars`)

                for(i in unique(dichotomous)){
                  lv <-  as.integer(round(length(unique(levels(data[[i]])))/2) +1) 
                
                  rt <- which(table$Row.names == paste0(i,levels(data[[i]])[lv]))[1]
                  table <-table %>% add_row(Row.names = paste(c(": \t", levels(data[[i]])[1]),collapse = '\t'), OR = "Reference", lower95ci = "", `Pr(>|Z|)` = "", upper95ci="", `mod summary stars` ="", .before = rt)

                  rt2 <- which(table$Row.names == paste0(i,levels(data[[i]])[2]))[1]
                  

                  table <- table %>% rowwise() %>% #paste(c("blah","blah"), collapse = "    ")
                    mutate(across(Row.names, ~ ifelse(startsWith(.x, i),paste(c(": \t", substring(.x, nchar(i)+1)),collapse = '\t'),.x)))

                  table <-table %>% add_row(Row.names = paste0(i), OR = "Categorical" , lower95ci = "", `Pr(>|Z|)` = "", upper95ci="", `mod summary stars` ="",.before = rt2)
                }

                table <- table %>% 
                  rename(
                    `variables` = `Row.names`
                    )

                table
              }, 	
              include.rownames=TRUE) 
              glm_plots$table <<- logistic.display(model, simplified=TRUE)
              glm_plots$table <<- as.data.frame(glm_plots$table$table)   
            } else if (mtype == "poisson") {
              names(data)<-make.names(names(data))
              model.poisson <- glm(as.formula(paste("`",make.names(input$glm2Var2),"`", " ~ .", sep = "")),
              family=(modeltype %>% filter(lookup == input$glm2Var2) %>% select (type )%>% pull),
              data=data)          
              
              output[["glmFamily"]] <- renderText({
                paste0('Model Family ', (modeltype %>% filter(lookup == input$glm2Var2) %>% select (type )%>% pull))
              })
              
              output$glmTable <- renderTable({

                table <- idr.display(model.poisson)
                table <- as.data.frame(table$table)
                table <- cbind(variables = rownames(table), data.frame(table, row.names=NULL))
                
                table$variables <- as.character(table$variables)
                table$variables <- gsub(".", " ", table$variables, fixed = TRUE)
                table <- table %>% 
                  rename(
                    `crude IDR 95 CI` = `crude.IDR.95.CI.`,
                    `adj IDR 95 CI` = `adj..IDR.95.CI.`,
                    `P Wald s test` = `P.Wald.s.test.`,
                    `P LR test` =  `P.LR.test.`
                  ) %>%
                  filter(`crude IDR 95 CI` !="" | `P LR test` !="")
                table
              },
              include.rownames=FALSE)
            } else if (mtype == "gaussian") {
              model <- glm(as.formula(paste("`",input$glm2Var2,"`", " ~ .", sep = "")), family=(modeltype %>% filter(lookup == input$glm2Var2) %>% select (type )%>% pull), data=data)             
              output[["glmFamily"]] <- renderText({
                paste0('Model Family ', (modeltype %>% filter(lookup == input$glm2Var2) %>% select (type )%>% pull))
              })
              `mod summary stars` <- mod_summary(model)
              
              output$glmTable <- renderTable({

                table <- regress.display(model, simplified=TRUE,decimal  =2)
                table <- as.data.frame(table$table)
                ordered<- rownames(table)
                table <- base::merge(table, as.data.frame(`mod summary stars`), by=0, all=TRUE) 
                table <-table[match(ordered, table$Row.names),]

                for(i2 in unique(onehot)){
                  table <- table %>%
                  mutate(across(Row.names, ~ ifelse(startsWith(.x, i2),substring(.x, 1, nchar(.x)-1),
                  ifelse(startsWith(.x, paste0("`",i2)),substring(.x, 1, nchar(.x)-1), .x))))
                }

                table <- na.omit(table,) %>% as.data.frame(row.names = 1:nrow(.))
                table <-table %>%  
                    mutate(across(Coeff:upper095ci, ~ifelse(.x > 1000,formatC(.x, format="e", digits=2),round(.x,2))))
                
                table$Coeff <- as.character(table$Coeff)
                table$Row.names <- gsub("`", "", table$Row.names, fixed = TRUE)

                #table$Coeff <- as.character(table$Coeff)
                table$lower095ci <- as.character(table$lower095ci)
                table$upper095ci <- as.character(table$upper095ci)
                table$`Pr>|t|` <- as.character(table$`Pr>|t|`)
                table$`mod summary stars` <- as.character(table$`mod summary stars`)
                
                for(i in unique(dichotomous)){         
                  lv <-  as.integer(round(length(unique(levels(data[[i]])))/2) +1) 
                
                  rt <- which(table$Row.names == paste0(i,levels(data[[i]])[lv]))[1]
                  table <-table %>% add_row(Row.names = paste(c(": \t", levels(data[[i]])[1]),collapse = '\t'), Coeff = "Reference" ,lower095ci="",upper095ci="",`Pr>|t|`="",`mod summary stars`="", .before = rt)

                  rt2 <- which(table$Row.names == paste0(i,levels(data[[i]])[2]))[1]
                  
                  table <- table %>% rowwise() %>%
                    mutate(across(Row.names, ~ ifelse(startsWith(.x, i),paste(c(": \t", substring(.x, nchar(i)+1)),collapse = '\t'),.x)))

                  table <-table %>% add_row(Row.names = paste0(i), Coeff = "Categorical" ,lower095ci="",upper095ci="",`Pr>|t|`="",`mod summary stars`="", .before = rt2)
                }

                table <- table %>% 
                  rename(
                    `variables` = `Row.names`
                    )

                table 
              }, 
              include.rownames=TRUE) 
            }
          })
          # eCART add in error message?
          removeModal()
        }
      })

      output$glmoddsratiosUI <- renderUI({
        if(input$glmoddsratios) {
          column(width = 12,
                plotOutput("glmoddsratios2"),#,style = "overflow-y: scroll;overflow-x: scroll;",
          )
        } else {
          plotOutput("glmoddsratiosBlank")
        }
      })

      observeEvent(input$glmoddsratios, ignoreInit = T, {
        if((modeltype %>% filter(lookup == input$glm2Var2) %>% select (type )%>% pull) == "binomial") {
          output$glmoddsratios2 <- renderPlot({
            
            plot1 <- ggplot(glm_plots$table, aes(y = 1:nrow(glm_plots$table), x = OR)) +
              geom_point(shape = 18, size = 5) +  
              geom_errorbarh(aes(xmin = lower95ci, xmax = upper95ci), height = 0.25) +
              geom_vline(xintercept = 1, color = "red", linetype = "dashed", cex = 1, alpha = 0.5) +
              scale_y_continuous(name = "", breaks=1:nrow(glm_plots$table), labels = rownames(glm_plots$table), trans = "reverse") +
              #scale_x_continuous(trans = 'log10') +
              xlab("Odds Ratio (95% CI)") + 
              ylab(" ") + 
              theme_bw() +
              theme(panel.border = element_blank(),
                panel.background = element_blank(),
                panel.grid.major = element_blank(), 
                panel.grid.minor = element_blank(), 
                axis.line = element_line(colour = "black"),
                axis.text.y = element_text(size = 12, colour = "black"),
                axis.text.x.bottom = element_text(size = 12, colour = "black"),
                axis.title.x = element_text(size = 12, colour = "black")
              )
            plot1
          })
        } else {
          output$glmoddsratios2 <- renderPlot({})
        }
      })
    
      ### BN
  
      observeEvent(input$bngo, {
        if(!is.null(input$bnVar1)) {
          showModal(
            modalDialog(
              HTML("<h2><strong>Please wait while the data is segmented.</strong></h2>"),
              p("This should finish in no more than a minute or two."),
              footer = tagList()
            )
          )
          try({
            add_sco <- function(x){
              gsub(" ", "_", x, fixed = T)
            }
        
            counter2 <- reactiveValues(n = 0)
            prevcount2 <-reactiveValues(n = 0)
            counter <- reactiveValues(n = 0)
            prevcount <-reactiveValues(n = 0)
            output$textbox_ui <- renderUI(NULL)
            output$textbox_ui2 <- renderUI(NULL)
            output$BNCOMFIRM <- renderUI(NULL)

            data <- as.data.frame(Query %>% select_at(c(unique(input$bnVar1))))
        

            dummys <- colnames(data %>%
              as_data_frame() %>%
              select_if(~ is.factor(.) & nlevels(.) > 2))

            if (length(dummys) > 0) {
              data <- fastDummies::dummy_cols(data,select_columns = dummys ,remove_selected_columns = T)               
              data<-data.frame(lapply(data,factor))
              colnames(data) <-gsub("\\_", " ", colnames(data))
              names(data) <- gsub("\\.", " ", names(data))
            }
            
            res <- hc(data)
        
            # Track the number of input boxes to render
            counter <- reactiveValues(n = 0)
            
            #Track the number of input boxes previously
            prevcount <-reactiveValues(n = 0)
            
            observeEvent(input$add_btn, {
              counter$n <- counter$n + 1
              prevcount$n <- counter$n - 1
            })
            
            observeEvent(input$rm_btn, {
              if (counter$n > 0) {
                counter$n <- counter$n - 1 
                prevcount$n <- counter$n + 1
              }
            })
        
            output$counter <- renderPrint(print(counter$n))
        
            textboxes <- reactive({
              n <- counter$n
              
              if (n > 0) {
                # If the no. of textboxes previously where more than zero, then 
                #save the text inputs in those text boxes 
                if(prevcount$n > 0){
                  
                  vals = c()
                  if(prevcount$n > n){
                    lesscnt <- n
                    isInc <- FALSE
                  }else{
                    lesscnt <- prevcount$n
                    isInc <- TRUE
                  }
                  for(i in 1:lesscnt){
                    inpid = paste0("textin",i)
                    vals[i] = input[[inpid]] 
                  }
                  if(isInc){
                    vals <- c(vals, input$textin[[i]])
                  }
                  
                  lapply(seq_len(n), function(i) {
                    div(class = 'bnpickers',
                    pickerInput(
                      inputId = paste0("textin", i),
                      label = "From",
                      choices =unique(colnames(data)),
                      selected = vals[i],
                      multiple = FALSE,
                      #value = vals[i],
                      options = list(`actions-box` = TRUE, size = 12, noneSelectedText = "Please select at least 1 option")
                    )
                    )
                  })
                } else{
                  lapply(seq_len(n), function(i) {
                    div(class = 'bnpickers',
                    pickerInput(
                      inputId = paste0("textin", i),
                      label = "From",
                      choices =unique(colnames(data)),
                      selected = 1,
                      multiple = FALSE,
                      options = list(`actions-box` = TRUE, size = 12, noneSelectedText = "Please select at least 1 option")
                    )
                    )
                  }) 
                }
              } 
            })
        
            output$textbox_ui <- renderUI({ textboxes() })
            
            ## Text box 2
            # Track the number of input boxes to render
            counter2 <- reactiveValues(n = 0)
            
            #Track the number of input boxes previously
            prevcount2 <-reactiveValues(n = 0)
            
            observeEvent(input$add_btn, {
              counter2$n <- counter2$n + 1
              prevcount2$n <- counter2$n - 1})
            
            observeEvent(input$rm_btn, {
              if (counter2$n > 0) {
                counter2$n <- counter2$n - 1 
                prevcount2$n <- counter2$n + 1
              }
              
            })
            
            output$counter2 <- renderPrint(print(counter2$n))
        
            textboxes2 <- reactive({
              n <- counter2$n
              if (n > 0) {
                # If the no. of textboxes previously where more than zero, then 
                #save the text inputs in those text boxes 
                if(prevcount2$n > 0){
                  
                  vals = c()
                  if(prevcount2$n > n){
                    lesscnt <- n
                    isInc <- FALSE
                  }else{
                    lesscnt <- prevcount2$n
                    isInc <- TRUE
                  }
                  for(i in 1:lesscnt){
                    inpid = paste0("textinN",i)
                    vals[i] = input[[inpid]] 
                  }
                  if(isInc){
                    vals <- c(vals, input$textinN[[i]])
                  }
                  
                  lapply(seq_len(n), function(i) {
                    div(class = 'bnpickers',
                    pickerInput(
                      inputId = paste0("textinN", i),
                      label = "To",
                      choices =unique(colnames(data)),
                      selected = vals[i],
                      multiple = FALSE,
                      #value = vals[i],
                      options = list(`actions-box` = TRUE, size = 12, noneSelectedText = "Please select 1 option")
                    )
                    )
                  })
                } else{
                  lapply(seq_len(n), function(i) {
                    div(class = 'bnpickers',
                    pickerInput(
                      inputId = paste0("textinN", i),
                      label = "To",
                      choices =unique(colnames(data)),
                      selected = 1,
                      multiple = FALSE,
                      options = list(`actions-box` = TRUE, size = 12, noneSelectedText = "Please select 1 option")
                    )
                    )
                  }) 
                }
              }
            })
        
            output$textbox_ui2 <- renderUI({ textboxes2() })

            output$BNCOMFIRM <- renderUI({
              actionButton("BNCOMFIRM", label = "Press To Fit Model")
            })
        
            if(length(colnames(data)) > 5) {
          
              output$TABLE <- renderUI({
                viewer(res,
                  bayesianNetwork.width = "100%",
                  bayesianNetwork.height = "500px",
                  bayesianNetwork.layout = "layout_in_circle",
                  node.colors = list(
                    background = "#f4bafd",
                    border = "#2b7ce9",
                    highlight = list(
                      background = "#97c2fc",
                      border = "#2b7ce9"
                    )
                  )
                )
              })
            } else {
              output$TABLE <- renderUI({
              viewer(res,
                bayesianNetwork.width = "100%",
                bayesianNetwork.height = "500px",
                bayesianNetwork.layout = "layout_on_grid",
                node.colors = list(
                  background = "#f4bafd",
                  border = "#2b7ce9",
                  highlight = list(
                    background = "#97c2fc",
                    border = "#2b7ce9"
                  )
                )   
            )
          })
        }
        
        observeEvent(input$BNCOMFIRM, {
          if(!is.null(counter$n >0)) {
            try({
              add_sco <- function(x){
                gsub(" ", "_", x, fixed = T)
              }
              if (counter$n >0){
                for (i in seq.int(1,counter$n , by=1)) {
                  print(i)
                  res <- drop.arc(res,input[[paste0("textin", i)]],input[[paste0("textinN", i)]])
                }
              }  
              if(length(colnames(data)) > 5) {
                output$TABLE <- renderUI({
                  viewer(res,
                    bayesianNetwork.width = "100%",
                    bayesianNetwork.height = "500px",
                    bayesianNetwork.layout = "layout_in_circle",
                    node.colors = list(
                      background = "#f4bafd",
                      border = "#2b7ce9",
                      highlight = list(
                        background = "#97c2fc",
                        border = "#2b7ce9"
                      )
                    ) 
                  )
                })
              } else {
                output$TABLE <- renderUI({
                  viewer(res,
                    bayesianNetwork.width = "100%",
                    bayesianNetwork.height = "500px",
                    bayesianNetwork.layout = "layout_on_grid",
                    node.colors = list(
                      background = "#f4bafd",
                      border = "#2b7ce9",
                      highlight = list(
                        background = "#97c2fc",
                        border = "#2b7ce9"
                      )
                    )
                  )
                })
              }
              fittedbn <- bn.fit(res, data = data)
              output$bntext <- renderPrint({ fittedbn })
              cpt_list <- list()

              # Loop through each node
              for (node in names(fittedbn)) {
                # Extract the CPT for the current node
                cpt <- as.data.frame(coef(fittedbn[[node]]), stringsAsFactors = FALSE)
                # Reset row names
                row.names(cpt) <- NULL
                # Assign proper column names based on the node
                colnames(cpt) <- c(paste0(node, ".Parent"), paste0(node, ".Probability"))
                # Add a column for the parent node(s)
                parents <- parents(res, node)
                if (length(parents) == 1) {
                  cpt$ParentNode <- parents
                } else {
                  cpt$ParentNode <- paste(parents, collapse = ", ")
                }
                # Add the current CPT to the list
                cpt_list[[node]] <- cpt
              }

              # Print the CPTs with parent nodes
              for (node in names(cpt_list)) {
                cat(paste("CPT for node", node, "with parent node(s):", parents(res, node), "\n"))
                print(cpt_list[[node]])
                cat("\n")
              }
              counter2 <- reactiveValues(n = 0)
              prevcount2 <-reactiveValues(n = 0)
              counter <- reactiveValues(n = 0)
              prevcount <-reactiveValues(n = 0)

            })
          } else {
            fittedbn <- bn.fit(res, data = data)
            output$bntext <- renderPrint({ fittedbn })
            cpt_list <- list()
            # Loop through each node
            for (node in names(fittedbn)) {
              # Extract the CPT for the current node
              cpt <- as.data.frame(coef(fittedbn[[node]]), stringsAsFactors = FALSE)
              
              # Reset row names
              row.names(cpt) <- NULL
              
              # Assign proper column names based on the node
              colnames(cpt) <- c(paste0(node, ".Parent"), paste0(node, ".Probability"))
              
              # Add a column for the parent node(s)
              parents <- parents(res, node)
              if (length(parents) == 1) {
                cpt$ParentNode <- parents
              } else {
                cpt$ParentNode <- paste(parents, collapse = ", ")
              }
              
              # Add the current CPT to the list
              cpt_list[[node]] <- cpt
            }

            # Print the CPTs with parent nodes
            for (node in names(cpt_list)) {
              cat(paste("CPT for node", node, "with parent node(s):", parents(res, node), "\n"))
              print(cpt_list[[node]])
              cat("\n")
            }
          }
        })
      
        # eCART add in error message?
        removeModal()
      
        counter2 <- reactiveValues(n = 0)
        prevcount2 <-reactiveValues(n = 0)
        counter <- reactiveValues(n = 0)
        prevcount <-reactiveValues(n = 0)
      })
    } 
  })
  
  ## Clustering

  observeEvent(input$kmgo, {
    if(!is.null(input$kmVar1)) {
      showModal(
        modalDialog(
          HTML("<h2><strong>Please wait while the data is segmented.</strong></h2>"),
          p("This should finish in no more than a minute or two."),
          footer = tagList()
        )
      )
      try({
        add_sco <- function(x){
          gsub(" ", "_", x, fixed = T)
        }
        data <- as.data.frame(Query %>% select_at(c(unique(input$kmVar1),unique(input$kmVar2))))
        data <-data[sample(nrow(data), 3000), ]
        Numericals <- colnames(dplyr::select_if(data, is.numeric))
        if (!is.null(input$kmVar1)){
          gower_dist <- daisy(data %>% select(input$kmVar1), metric = "gower")
          gower_mat <- as.matrix(gower_dist)
          k <- as.numeric(input$clusters)
          pam_fit <- pam(gower_dist, diss = TRUE, k)
          
          results <- data %>%
            mutate(cluster = pam_fit$clustering)
          
          results$cluster <- as.factor(results$cluster)
         
          plot_list = list()
          for(i in Numericals) {
            p<- ggplot(results, aes(x=cluster, y=!!as.symbol(i))) +
            geom_violin(trim=FALSE, fill='#A4A4A4', color="darkred")+
            geom_boxplot(width=0.1) + theme_minimal()
            plot_list[[i]] = p
          }
          
          plot_list2 = list()
          Cats <- colnames(results %>% select(-Numericals,-cluster))

          for(i in Cats) {
            counts <- table(results %>% select(!!as.symbol(i), cluster)) 
            p <- ggplot(as.data.frame(counts)  %>%
              rename_all(~str_replace_all(.,"\\."," ")), aes(cluster, Freq, fill = !!as.symbol(i))) +
              geom_col() +
              ylab("Population") +
              coord_flip() +
              theme_bw()
            plot_list2[[i]] = p
          }

          output$plot2 <- renderUI({
            if(input$tsnegr) {
              plotOutput("plot21")
            } else {
              uiOutput("plot21UIBlank")
            }
          })
          if(input$tsnegr) {
            output$plot21 <- renderPlot({
              tsne_obj <- Rtsne(gower_dist, is_distance = TRUE)
              tsne_data <- tsne_obj$Y %>%
                data.frame() %>%
                setNames(c("tsne dimension 1", "tsne dimension 2")) %>%
                mutate(cluster = factor(pam_fit$clustering))
              ggplot(aes(x = `tsne dimension 1`, y = `tsne dimension 2`), data = tsne_data) +
                geom_point(aes(color = cluster))
            })
          }
          if (length(Numericals) >0) {
            output$plot3 <- renderUI({
              plotOutput("plot31")
            })
            output$plot31 <- renderPlot({grid.arrange(grobs = plot_list)})
          }
          
          if (length(Cats) >0) {
            output$plot4 <- renderUI({
              plotOutput("plot41")
            })
            output$plot41 <- renderPlot({grid.arrange(grobs = plot_list2)})
          }
        }
      })
      removeModal()
    }
  })
  
  ##sh
  observeEvent(input$kmgosh, {
    if(!is.null(input$kmVar1)) {
      showModal(
        modalDialog(
          HTML("<h2><strong>Please wait while the data is segmented.</strong></h2>"),
          p("This should finish in no more than a minute or two."),
          footer = tagList()
        )
      )
      try({
        add_sco <- function(x){
          gsub(" ", "_", x, fixed = T)
        }

        data <- as.data.frame(Query %>% select_at(c(unique(input$kmVar1))))
        data <-data[sample(nrow(data), 3000), ]
        
        gower_dist <- daisy(data, metric = "gower")
        gower_mat <- as.matrix(gower_dist) 
        sil_width <- c(NA)
        for(i in 2:8){  
          pam_fit <- pam(gower_dist, diss = TRUE, k = i)  
          sil_width[i] <- pam_fit$silinfo$avg.width  
        }

        output$plot1 <- renderUI({
          fluidPage(plotOutput("plot11"))
        })
          
        output$plot11 <- renderPlot({
          plot(1:8, sil_width,
                xlab = "Number of clusters",
                ylab = "Silhouette Width")
          lines(1:8, sil_width)
        })
      })
      # eCART add in error message?
      removeModal()
    }
  })
}
