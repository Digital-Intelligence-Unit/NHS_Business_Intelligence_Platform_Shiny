show_results <- function(id,style = "overflow-y: scroll;overflow-x: scroll;",check,returns,blanks) {
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
    
})
}

