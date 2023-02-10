arc_buttons <- function(id,counter,prevcount,cols, label) {
        

        moduleServer(
        id,
        
        function(input, output, session) {

        

        textboxes <- reactive({
          
          
          n <- counter$n
          print
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
                inpid = paste0(id,i)
                vals[i] = input[[inpid]] 
              }
              if(isInc){
                vals <- c(vals, input[[paste0(id,i)]])
              }
              
              lapply(seq_len(n), function(i) {
                
                pickerInput(
                  inputId = paste0(id, i),
                  label = label,
                  choices =cols,
                  selected = vals[i] ,
                  multiple = FALSE,
                  #value = vals[i],
                  options = list(`actions-box` = TRUE, size = 12, noneSelectedText = "Please select at least 1 option")
                )
                
              
              })
              
            }else{
              lapply(seq_len(n), function(i) {
                
                pickerInput(
                  inputId = paste0(id, i),
                  label = label,
                  choices =cols,
                  selected = 1,
                  multiple = FALSE,
                  options = list(`actions-box` = TRUE, size = 12, noneSelectedText = "Please select at least 1 option")
                )
                
              }) 
            }
            
          }
          
        })
        
       
       
       
        }
    ) 
}