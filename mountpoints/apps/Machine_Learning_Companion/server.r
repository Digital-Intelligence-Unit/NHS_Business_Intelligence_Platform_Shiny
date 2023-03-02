print("Loads libs server")

#source("./app/ui.r")

pdf(file = NULL)

# code functions
source("./app/code/mod_summary.r", local = TRUE)
#source("./app/code/show_results.R")

server <- function(input,output,session){
source("./data.r", local = TRUE)
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
        
        data <- as.data.frame(Query %>% select_at(c(unique(input$twoCARTVar1), input$twoCARTVar2)))

        contols <- rpart.control(minbucket=input$twoCARTminbucket,
                                  maxdepth=input$twoCARTmaxdepth,
                                  cp=input$twoCARTcp)
        
        print("CART model")
        model<-rpart(as.formula(paste("`",input$twoCARTVar2,"`", " ~ .", sep = "")),data=data ,
                     control=contols)
        print("CART model end")
        
        output$twoCARTTree <- renderPlot({

          node.fun1 <- function(x, labs, digits, varlen)

          {
            paste(labs, "\nn =", x$frame$n2)
          }
          
          milfun <- function(x) {ifelse(x>1e5,paste0(round(x/1e6,3),"M"),x)}
          
          model$frame$n2 <- sapply(model$frame$n,milfun)
          
        if ((sapply(rev(data)[1], class))[1] == "integer" | (sapply(rev(data)[1], class))[1] == "numeric") {  
          
         extras <- 100 
        }
          
        else {
          extras <- 109
        }
          
        prp(model,
            type = 1, # left and right split labels (see Figure 2)
            clip.right.labs = FALSE, # full right split labels
            extra = extras , # show nbr of obs and percentages (see Figure 3)
            node.fun = node.fun1,
            under = FALSE, # position extra info _under_ the boxes
            under.cex = .8, # size of text under the boxes (default is .8)
            fallen.leaves = TRUE, # put leaves at the bottom of plot
            box.palette = "GnYlRd", # color of the boxes
            branch = .3, # branch lines with narrow shoulders and down slopes
            branch.type = 5,
            branch.tweak =0.25 ,
            faclen = 0,
            varlen = 0,
            #nn.font = 5,
            round = 0, # no rounding of node corners i.e. use rectangles
            leaf.round = 2, # round leaf nodes (for leaves, this supersedes the round arg)
            #prefix = "ozone\n", # prepend this string to the node labels
            tweak = 1.2,
            xcompact = TRUE,
            xcompact.ratio = 1,
            cex.main = 1.5, # use big text for main title
            branch.col = "gray", # color of branch lines
            branch.lwd = 2)
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
  output$twoCARTTreeRulesTableUI <- renderUI({
    if(input$twoCARTTreeRules) {
      column(width = 12,
             tableOutput("twoCARTTreeRulesTable"),style = "overflow-y: scroll;overflow-x: scroll;",
      )
    } else {
      uiOutput("twoCARTTreeRulesTableUIBlank")
    }
  })
  observeEvent(input$twoCARTTreeRules, ignoreInit = T, {
    if(input$twoCARTTreeRules & !is.na(dat2$cartModel)) {
      output$twoCARTTreeRulesTable <- renderTable({
        rules <- rpart.rules(dat2$cartModel, nn = TRUE)
        newIDs <- 1:nrow(rules)
        names(newIDs) <- sort(as.integer(rules$nn))
        newSeg <- newIDs[as.character(rules$nn)]
        rules <- rules[,-1]
        colnames(rules)[1:2] <- c(colnames(rules)[1], "Rule")
        colnames(rules)[3:ncol(rules)] <- " "
        cbind("Segment" = paste0("Segment ", newSeg), rules)
      })
    }
  })

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
                  select_if(~ is.factor(.) & nlevels(.) > 2))

        onehot <- colnames(data %>%
                  as_data_frame() %>%
                  select(-c(dichotomous),-c(input$glm2Var2)))
                
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
            table <- merge(table, as.data.frame(`mod summary stars`), by=0, all=TRUE) 
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
            
        }
        
        else if (mtype == "poisson") {
          
          names(data)<-make.names(names(data))

           model.poisson <- glm(as.formula(paste("`",make.names(input$glm2Var2),"`", " ~ .", sep = "")),
           family=(modeltype %>% filter(lookup == input$glm2Var2) %>% select (type )%>% pull),
           data=data)          
          
          output[["glmFamily"]] <- renderText({
            
            paste0('Model Family ', (modeltype %>% filter(lookup == input$glm2Var2) %>% select (type )%>% pull))
          })
          
          #mod_summary_stars <- mod_summary(model.poisson)
          
          output$glmTable <- renderTable({

           table <- idr.display(model.poisson)
           table <- as.data.frame(table$table)

           
           #table <- table[table$`crude.IDR.95.CI.`!='' &  table$`P.LR.test.` !="", ]
           table <- cbind(variables = rownames(table), data.frame(table, row.names=NULL))
           
           table$variables <- as.character(table$variables)
           table$variables <- gsub(".", " ", table$variables, fixed = TRUE)

          #  table <- table %>% 
          #   filter(`P.LR.test.` !="" & `crude.IDR.95.CI.` !="")
           table <- table %>% 
              rename(
                `crude IDR 95 CI` = `crude.IDR.95.CI.`,
                `adj IDR 95 CI` = `adj..IDR.95.CI.`,
                `P Wald s test` = `P.Wald.s.test.`,
                `P LR test` =  `P.LR.test.`
                ) %>%
                #mutate(flag = ifelse(`crude IDR 95 CI` !="" | `P LR test` !="",1,0)) 
            filter(`crude IDR 95 CI` !="" | `P LR test` !="")
            #table$`crude IDR 95 CI` <- as.character(table$`crude IDR 95 CI`)
            #table$`P LR test` <- as.character(table$`P LR test`)
            #table <- table %>% 
            # filter((`P LR test` !="" &`crude IDR 95 CI` !=""))
           			           
            #table <- table %>% rowwise() %>% 
             #  mutate(across(variables, ~ ifelse(startsWith(.x,"X "),paste(c(": \t", substring(.x, nchar(.x)-2)),collapse = '\t'),.x)))

            

          #print(table)
          table
          },
          include.rownames=FALSE)
          
          #output$glmTableSig <- renderTable({
            
          #table <- as.data.frame(mod_summary_stars)
          #}, 
          #include.rownames=TRUE) 
          
        }
        
        else if (mtype == "gaussian") {
          
          model <- glm(as.formula(paste("`",input$glm2Var2,"`", " ~ .", sep = "")), family=(modeltype %>% filter(lookup == input$glm2Var2) %>% select (type )%>% pull), data=data)             
          
          output[["glmFamily"]] <- renderText({
            
            paste0('Model Family ', (modeltype %>% filter(lookup == input$glm2Var2) %>% select (type )%>% pull))
          })
          `mod summary stars` <- mod_summary(model)
          
          output$glmTable <- renderTable({

            table <- regress.display(model, simplified=TRUE,decimal  =2)
            table <- as.data.frame(table$table)
            ordered<- rownames(table)
            table <- merge(table, as.data.frame(`mod summary stars`), by=0, all=TRUE) 
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
        
       # plot1 <- 
       ggplot(glm_plots$table, aes(y = 1:nrow(glm_plots$table), x = OR)) +
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
                axis.title.x = element_text(size = 12, colour = "black"))
        
        #plot1
        
      })
    }
    
    else {
      
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
          prevcount$n <- counter$n - 1})
        
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
              
            }else{
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
              
            }else{
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
                   
                   node.colors = list(background = "#f4bafd",
                                      border = "#2b7ce9",
                                      highlight = list(background = "#97c2fc",
                                                       border = "#2b7ce9"))
            )
          })
        }
        
        else {
          output$TABLE <- renderUI({

            viewer(res,
                   bayesianNetwork.width = "100%",
                   bayesianNetwork.height = "500px",
                   bayesianNetwork.layout = "layout_on_grid",
                   node.colors = list(background = "#f4bafd",
                                      border = "#2b7ce9",
                                      highlight = list(background = "#97c2fc",
                                                       border = "#2b7ce9"))
                   
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
                   
                   node.colors = list(background = "#f4bafd",
                                      border = "#2b7ce9",
                                      highlight = list(background = "#97c2fc",
                                                       border = "#2b7ce9"))
                   
            )
          })
        }
        
        else {
        output$TABLE <- renderUI({

          viewer(res,
                 bayesianNetwork.width = "100%",
                 bayesianNetwork.height = "500px",
                 bayesianNetwork.layout = "layout_on_grid",
                 
                 node.colors = list(background = "#f4bafd",
                                    border = "#2b7ce9",
                                    highlight = list(background = "#97c2fc",
                                                     border = "#2b7ce9"))
                 
          )
          
      })
    }

        fittedbn <- bn.fit(res, data = data)
       
        output$bntext <- renderPrint({ fittedbn })
        
        counter2 <- reactiveValues(n = 0)
        prevcount2 <-reactiveValues(n = 0)
        counter <- reactiveValues(n = 0)
        prevcount <-reactiveValues(n = 0)

        })
   
      }
      else {
        fittedbn <- bn.fit(res, data = data)
        
        output$bntext <- renderPrint({ fittedbn })

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
        else {
          
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
                 ylab = "Silhouette Width")#;lines(1:8, sil_width)
          })

      })
      # eCART add in error message?

      removeModal()
    }
    
  })
  
}
