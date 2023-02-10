welcome <-tabPanel("Welcome", fluid = TRUE,
                     mainPanel(
                      #  p("Please select a date ranage using the range tool below. The min date is ",mindate, "and the latest date is ",maxdate,"."),
                      #  dateRangeInput(
                      #    "dateRange",
                      #    label = "select ranage",
                      #    start = mindate,
                      #    end = maxdate,
                      #    min = mindate,
                      #    max = maxdate,
                      #    format = "yyyy-mm-dd",
                      #    startview = "month",
                      #    weekstart = 0,
                      #    language = "en",
                      #    separator = " to ",
                      #    width = NULL,
                      #    autoclose = TRUE
                      #  )
                      fluidRow(
                       column(width = 9,
                          h1("Welcome to the Machine Learning Companion App.")
                       ),
                       column(width = 5, p("For more informtation please vist the user guide here:"))
                      )



                     )
            )