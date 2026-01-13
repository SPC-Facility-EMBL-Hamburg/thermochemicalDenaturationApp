tabBox(title = "", width = 12,id = "tabset_fit",

    tabPanel("Fitted signal",withSpinner(plotlyOutput("fitted_signal"))),
    tabPanel("Fitted params",tableOutput("fitted_params")),
    # DG versus temperature plot
    tabPanel("DG vs T",withSpinner(plotlyOutput("dg_vs_temp")))

)

