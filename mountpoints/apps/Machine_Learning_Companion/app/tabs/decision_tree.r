decision_tree <- tabPanel("Decision Tree",
                     fluidRow(
                       box(background = "green",width = 4,
                           uiOutput("oneCoverBoxs9"),
                       ),
                       box(background = "green", width = 4,
                           uiOutput("oneCoverBoxDate10")
                       ),
                     ),
                     fluidRow(
                       column(width = 9,
                              fluidRow(
                                box(width = 12, title = "Variable Setup",
                                    column(width = 6,
                                           pickerInput(
                                             inputId = "twoCARTVar1",
                                             label = "Select Fields (to use to build the tree)",
                                             choices =decision_tree_vars,
                                             selected = 1,
                                             multiple = TRUE,
                                             options = list(`actions-box` = TRUE, size = 12, noneSelectedText = "Please select at least 1 option")
                                           ),
                                    ),
                                    column(width = 6,
                                           pickerInput(
                                             inputId = "twoCARTVar2",
                                             label = "Target Variable (to cluster by)",
                                             choices = decision_tree_targets,
                                             
                                             selected = 1,
                                             options = list(`actions-box` = TRUE, size = 12, noneSelectedText = "Please select at least 1 option")
                                           ),
                                    ),
                                    column(width = 12,
                                           br(),
                                           fluidRow(
                                             column(width = 6,
                                                    div(id="AlgorithmParameters",h4("Algorithm Parameters")),
                                             ),
                                               bsTooltip("AlgorithmParameters", title = "It is recommended these settings are left as default.")
                                             
                                           ),
                                           fluidRow(
                                             column(width = 6,
                                                    numericInput("twoCARTmaxdepth", label = "Maximum Depth", value = 3, min = 1, max = 50, step = 1)
                                             ),
                                             
                                              bsTooltip("twoCARTmaxdepth", title = "Maximum depth of tree (how many splits are allowed)"),
                    
                                           ),
                                           fluidRow(
                                             column(width = 6,
                                                    numericInput("twoCARTcp", label = "CP", value = 0.00001, min = 0, step = 0.00001),
                                             ),                                             
                                                bsTooltip("twoCARTcp", title = "Complexity parameter. Lower value can allow more splits."),
                                           ),
                                           fluidRow(
                                             column(width = 6,
                                                    numericInput("twoCARTminbucket", label = "Minbucket", value = 20, min = 1, step = 1),
                                             ),
                                                bsTooltip("twoCARTminbucket", title = "Minimum number of individual that must be in a leaf node at the end of the algorithm."),
                                           ),
                                           fluidRow(
                                             column(width = 6,
                                                    actionButton("twoCARTgo",label = "Go!")
                                             ),
                                             column(width = 6,
                                                    
                                             )
                                           )
                                    ),
                                    
                                ),
                              ),
                              fluidRow(
                                box(width = 12, title = "Decision Tree",
                                    plotOutput("twoCARTTree",
                                               width = "100%",
                                               height = "500px",
                                               click = NULL,
                                               dblclick = NULL,
                                               hover = TRUE,
                                               brush = NULL,
                                               inline = FALSE),
                                    column(width = 3,
                                           checkboxInput("twoCARTTreeRules", "Tick to show rules", value = FALSE),
                                    ),
                                    
                                    column(width = 3,
                                           checkboxInput("twoCARTTreeVARIMP", "Tick to show variable importance", value = FALSE),
                                    ),
                                   
                                    uiOutput("twoCARTTreeRulesTableUI"),
                                    textOutput("eCART")),
                                
                                uiOutput("twoCARTTreeRulesTableUIVARIMP"),                                
                              ),
                       ),
                       column(width = 3,
                              fluidRow(
                                div(
                                  box(width = 12, id = "CARTInfo",style = "overflow-y: scroll;", title = "CART Info",
                                      column(width = 12,
                                             fluidRow(
                                               p("Classification and Regression Tree (CART), referred to as 'Decision Trees' on this page, is a data-driven regression technique."),
                                               p("See ", a("Tree-Based Models", href="https://blog.dataiku.com/tree-based-models-how-they-work-in-plain-english"), " for how regression trees work."),
                                               p(""),
                                               p("Use decision trees either to segment the population, or to discover what attributes are most statictically significant in explaining differences (of the selected target variable)."),
                                               )
                                      )
                                  ), style = "position:fixed;  width:inherit;")
                             )
                       )
                     ),
            )