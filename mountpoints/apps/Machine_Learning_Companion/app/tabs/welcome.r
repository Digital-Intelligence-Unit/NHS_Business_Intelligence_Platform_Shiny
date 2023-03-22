welcome <-tabPanel(
      "Welcome",
      fluidRow(
        column(width = 6,
          #div(class = "welcome-image"#,
            #tags$img(src = "./app/tabs/www/image1.png", alt="Image of a Decision Tree")
          #),
          div(class = "welcome-text",
           actionButton("tab1_button1", "Decision Tree", class = "btn-header"),
            #tags$h2(id = "header", "Decision Tree"),
            #h4("Decision Tree"),
            p("Classification and Regression Tree (CART), referred to as 'Decision Trees' on this page, is a data-driven regression technique."),
            p("See ", a("Tree-Based Models", href="https://blog.dataiku.com/tree-based-models-how-they-work-in-plain-english"), " for how regression trees work."),
            p("Use decision trees either to segment the population, or to discover what attributes are most statically significant in explaining differences (of the selected target variable).")
          )
        ),
        column(width = 6,
          #div(class = "welcome-image"#,
             #tags$img(src = "./app/tabs/www/image2.jpg", alt="Image of a Generalized Linear Model")
          #),
          div(class = "welcome-text",
           #tags$h2(id = "header", "GLM"),
           actionButton("tab2_button2", "GLM", class = "btn-header"),
            # h4("GLM"),
            p("Generalized Linear Models (GLM), a generalization of ordinary linear regression that allows for response variables that have error distribution models other than a normal distribution like Gaussian distribution."),
            p("See ", a("GLM in R", href="https://www.datacamp.com/tutorial/generalized-linear-models"), " for a tutorial."),
            p("GLM models allow us to build a linear relationship between the response and predictors, even though their underlying relationship is not linear. This is made possible by using a link function, which links the response variable to a linear model.")
          )
        )
      ),
      fluidRow(
        column(width = 6,
          #div(class = "welcome-image"#,
             #tags$img(src = "./app/tabs/www/image3.png", alt="Image of a Bayesian Network")
          #),
          div(class = "welcome-text",
            #tags$h2(id = "header", "Bayesian Network"),
            actionButton("tab3_button3", "Bayesian Network", class = "btn-header"),
            #h4("Bayesian Network"),
            p("A Bayesian network is a probabilistic graphical model that represents a set of variables and their conditional dependencies via a directed acyclic graph."),
            p("See ", a("Bayesian network", href="https://en.wikipedia.org/wiki/Bayesian_network"), " for more information."),
            p("Bayesian networks are a type of Probabilistic Graphical Model that can be used to build models from data and/or expert opinion. They can be used for a wide range of tasks including diagnostics, reasoning, causal modeling, decision making under uncertainty, anomaly detection, automated insight and prediction.")
          )
        ),


        column(width = 6,
          #div(class = "welcome-image"#,
             #tags$img(src = "./app/tabs/www/image4.png", alt="Image of a group of clusters")
          #),
          div(class = "welcome-text",
            actionButton("tab4_button4", "Clustering", class = "btn-header"),
            #h4("Clustering"),
            p("PAM (partition around medoids) clustering, is an iterative, data-partitioning algorithm that assigns n observations to exactly one of k clusters defined by medoids, where k is chosen before the algorithm starts."),
            p("See ", a("PAM", href="https://www.cs.umb.edu/cs738/pam1.pdf"), " for more information."),
            p("Why use PAM?"),
            p("The PAM clustering algorithm is used to find groups which have not been explicitly labeled in the data. This can be used to confirm business assumptions about what types of groups exist or to identify unknown groups in complex data sets."),
            p("See ", a("Introduction to PAM Clustering", href="https://www.datanovia.com/en/lessons/k-medoids-in-r-algorithm-and-practical-examples/"), " for more information.")
          )
        )
      )        
)
