show_results <- function(id,style = "overflow-y: scroll;overflow-x: scroll;",check,returns,blanks) {
#    ns <- NS(id)
#  tagList(
#     tableOutput(ns(paste0(returns))),
#     uiOutput(ns(paste0(blanks)))
#   )
#}

#show_results <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {

        output[[paste0(id)]] <- renderUI({
            if(check) {
            column(width = 12,
                    tableOutput(paste0(returns)),style = style,
            )
            } else {
            uiOutput(paste0(blanks))
            }
  })

    # observeEvent(check, ignoreInit = T, {
    #     if(check) {
    #     output[[paste0(returns)]] <- code
    #     }
    # })

    
})
}



#  show_results(id = "twoCARTTreeRulesTableUI", check = input$twoCARTTreeRules
#   , returns = "twoCARTTreeRulesTable", blanks = "twoCARTTreeRulesTableUIBlank", codes = renderTable({
#             rules <- rpart.rules(dat2$cartModel, nn = TRUE)
#             newIDs <- 1:nrow(rules)
#             names(newIDs) <- sort(as.integer(rules$nn))
#             newSeg <- newIDs[as.character(rules$nn)]
#             rules <- rules[,-1]
#             colnames(rules)[1:2] <- c(colnames(rules)[1], "Rule")
#             colnames(rules)[3:ncol(rules)] <- " "
#             cbind("Segment" = paste0("Segment ", newSeg), rules)
#        })
#        )