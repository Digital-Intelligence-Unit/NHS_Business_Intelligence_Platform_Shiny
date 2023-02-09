glm <-tabPanel("GLM", fluid = TRUE,
                     mainPanel(
                       fluidRow(
                         column(width = 9,
                                fluidRow(
                                  box(width = 12, title = "Variable Setup",
                                      column(width = 6,
                                             pickerInput(
                                               inputId = "glmVar1",
                                               label = "Select Fields",
                                               choices =glm_vars, 
                                               selected = 1,
                                               multiple = TRUE,
                                               options = list(`actions-box` = TRUE, size = 12, noneSelectedText = "Please select at least 1 option")
                                             ),                                             
                                      ),
                                      column(width = 6,
                                             pickerInput(
                                               inputId = "glm2Var2",
                                               label = "Target Variable",
                                               choices = glm_targets,
                                               selected = 1,
                                               options = list(`actions-box` = TRUE, size = 12, noneSelectedText = "Please select at least 1 option")
                                             ),
                                      ),
                                      column(width = 12,
                                             br(),
                                             fluidRow(
                                               column(width = 12,
                                                      actionButton("glmgo",label = "Go!"),
                                                      h4("Algorithm Parameters"),
                                                      verbatimTextOutput("glmFamily"),
                                               )),
                                             column(width = 12,
                                                   checkboxInput("glmoddsratios", "Tick to show the odd ratio plot", value = FALSE),
                                             ),)
                                      )
                                  ),
                                fluidRow(
                                           tableOutput("glmTable"),tableOutput("glmTableSig")
                                ),
                                fluidRow(
                                  br(),
                                  uiOutput("glmoddsratiosUI")
                                )

                              ),
                         column(width = 3,
                                fluidRow(
                                  div(
                                    box(width = 12, id = "OddsInfo",style = "overflow-y: scroll;", title = "GLMs",
                                        column(width = 12,
                                               fluidRow(
                                                 p("Generalized Linear Models (GLM), a generalization of ordinary linear regression that allows for response variables that have error distribution models other than a normal distribution like Gaussian distribution."),
                                                 p("See ", a("GLM in R", href="https://www.datacamp.com/tutorial/generalized-linear-models"), " for a tutorial."),
                                                 p("Why use Glm's?"),
                                                 p("GLM models allow us to build a linear relationship between the response and predictors, even though their underlying relationship is not linear. This is made possible by using a link function, which links the response variable to a linear model."),
                                                 p("See ", a("Generalized Linear Model", href="https://www.mygreatlearning.com/blog/generalized-linear-models/#:~:text=GLM%20models%20allow%20us%20to,variable%20to%20a%20linear%20model."), " for more information."),
                                               )
                                        )
                                    ), style = "position:fixed;  width:inherit;")
                                )
                         )
                        )
                     )
          )