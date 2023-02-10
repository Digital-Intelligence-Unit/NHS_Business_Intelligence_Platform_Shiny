bayesian_network <-tabPanel("Bayesian network", fluid = TRUE,
                     mainPanel(
                       fluidRow(
                         column(width = 9,
                                fluidRow(
                                  box(width = 12, title = "Variable Setup",
                                      column(width = 6,
                                             pickerInput(
                                               inputId = "bnVar1",
                                               label = "Select Fields",
                                               choices =Bayesian_cols,
                                               selected = 1,
                                               multiple = TRUE,
                                               options = list(`actions-box` = TRUE, size = 12, noneSelectedText = "Please select at least 1 option")
                                             ),
                                             actionButton("add_btn", "Add Deletion of Arc"),
                                             bsTooltip("add_btn", "Use to remove branches/connections."),
                                             actionButton("rm_btn", "Remove Deletion of Arc"),
                                             div(style="display:flex",
                                                 uiOutput("textbox_ui"),
                                                 uiOutput("textbox_ui2"),
                                             )                                      
                                      ),
                                    
                                      column(width = 12,
                                             br(),
                                             fluidRow(
                                               column(width = 6,
                                                      verbatimTextOutput("bnFamily"),
                                                                                                        
                                                      fluidRow(
                                                        column(width = 6,
                                                               actionButton("bngo",label = "Go!"),
                                                               uiOutput("BNCOMFIRM")
                                                        )   
                                               )
                                          )

                                   )
                                   
                                   ,uiOutput(outputId = "TABLE")
                                   ,br()
                                   ,verbatimTextOutput("bntext")
                                   
                                   )))),
                         column(width = 3,
                                fluidRow(
                                  div(
                                    box(width = 12, id = "bnInfo",style = "overflow-y: scroll;", title = "Bayesian network",
                                        column(width = 12,
                                               fluidRow(
                                                 p("A Bayesian network is a probabilistic graphical model that represents a set of variables and their conditional dependencies via a directed acyclic graph."),
                                                 p("See ", a("Bayesian network", href="https://en.wikipedia.org/wiki/Bayesian_network"), " for more information."),
                                                 p("Why use BN's?"),
                                                 p("Bayesian networks are a type of Probabilistic Graphical Model that can be used to build models from data and/or expert opinion.
They can be used for a wide range of tasks including diagnostics, reasoning, causal modeling, decision making under uncertainty, anomaly detection, automated insight and prediction."),
                                                p("See ", a("Bayesian networks - an introduction", href="https://www.bayesserver.com/docs/introduction/bayesian-networks/#:~:text=Bayesian%20networks%20are%20a%20type,detection%2C%20automated%20insight%20and%20prediction."), " for more information."),
                                                 
                                               )
                                        )
                                    ), style = "position:fixed;  width:inherit;")
                                )
                         )
                         )
                     )
            )