tabBox(title = "", width = 12,id = "tabset1",
       tabPanel("Signal",withSpinner(plotlyOutput("signal"))),
       tabPanel("1st derivative",withSpinner(plotlyOutput("first_der"))),
       tabPanel(HTML("T<sub>m</sub> (1st der. peak) versus [Denaturant]"),withSpinner(plotlyOutput("tm_vs_den"))),
       tabPanel("Initial signal versus [Denaturant]",withSpinner(plotlyOutput("initial_fluo_vs_den"))))

