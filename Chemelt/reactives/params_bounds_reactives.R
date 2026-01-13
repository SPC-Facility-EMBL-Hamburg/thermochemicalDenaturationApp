observeEvent(input$show_advanced_options_fit,{

    showModal(

        modalDialog(

            fluidRow(

                column(
                    width = 4,
                    p(HTML("<b>ΔC<sub>p</sub></b>"),
                        span(shiny::icon("info-circle"), id = "info_uuL-1"),

                        selectInput("fix_cp_option", NULL,
                            choices=c(
                                "Fixed value" = "fix_cp", 
                                "Set bounds" = "set_cp_bounds",
                                "Automatic bounds" = "auto_cp_bounds"
                            ),
                            selected = reactives$fix_cp_option
                        ),
                        
                        tippy::tippy_this(
                            elementId = "info_uuL-1",
                            tooltip = "Decide if the value of ΔCp should be fixed, 
                            or limited within bounds. You can either
                            set bounds manually, or let Chemelt set automatic bounds.",
                            placement = "right"
                        )
                    )
                ),

                column(
                    width = 8,

                    conditionalPanel(
                        condition = "input.fix_cp_option == 'fix_cp'",

                        column(
                            width = 6,
                            p(HTML("<b>Value</b>"),
                              numericInput("cp_value",NULL,value=reactives$cp_value,min=0,max=20,step=0.1)
                            )
                        )
                    ),

                    conditionalPanel(

                        condition = "input.fix_cp_option == 'set_cp_bounds'",

                        # Numeric Input - Cp lower limit
                            column(6, p(
                            HTML("<b>Lower limit</b>"),
                            span(shiny::icon("info-circle"), id = "info_uu-cp_lower_limit"),
                            numericInput("cp_lower_limit", NULL, value = reactives$cp_low_bound, step = 0.1),
                            tippy::tippy_this(
                                elementId = "info_uu-cp_lower_limit",
                                tooltip = "Set a lower limit for the heat capacity change (Cp) during fitting.
                                This can help prevent the fitting algorithm from exploring unphysical values.
                                Units are in kcal/(mol*K).",
                                placement = "right")
                            )),

                            # Numeric Input - Cp upper limit
                            column(6, p(
                            HTML("<b>Upper limit</b>"),
                            span(shiny::icon("info-circle"), id = "info_uu-cp_upper_limit"),
                            numericInput("cp_upper_limit", NULL, value = reactives$cp_upp_bound, step = 0.1),
                            tippy::tippy_this(
                                elementId = "info_uu-cp_upper_limit",
                                tooltip = "Set an upper limit for the heat capacity change (Cp) during fitting.
                                This can help prevent the fitting algorithm from exploring unphysical values.
                                Units are in kcal/(mol*K).",
                                placement = "right")
                            ))

                    )

                )

            ),

            fluidRow(

                column(
                    width = 4,
                    p(HTML("<b>T<sub>m</sub></b>"),
                        span(shiny::icon("info-circle"), id = "info_uuL-2"),

                        selectInput("fix_tm_option", NULL,
                            choices=c(
                                "Set bounds" = "set_tm_bounds",
                                "Automatic bounds" = "auto_tm_bounds"
                            ),
                            selected = reactives$fix_tm_option
                        ),

                        tippy::tippy_this(
                            elementId = "info_uuL-2",
                            tooltip = "Set bounds manually, or let Chemelt set automatic bounds.",
                            placement = "right"
                        )
                    )
                ),

                column(
                    width = 8,

                    conditionalPanel(

                        condition = "input.fix_tm_option == 'set_tm_bounds'",

                        column(6, p(
                          HTML("<b>T<sub>m</sub> lower limit (ºC)</b>"),
                          span(shiny::icon("info-circle"), id = "info_uu-tm_lower_limit"),
                          numericInput("tm_lower_limit", NULL, value = reactives$tm_low_bound, step = 1),
                          tippy::tippy_this(
                            elementId = "info_uu-tm_lower_limit",
                            tooltip = "Set a lower limit for the melting temperature (Tm) during fitting.
                            This can help prevent the fitting algorithm from exploring unphysical values.",
                            placement = "right")
                        )),

                        # Numeric input - Tm upper limit
                        column(6, p(
                          HTML("<b>T<sub>m</sub> upper limit (ºC)</b>"),
                          span(shiny::icon("info-circle"), id = "info_uu-tm_upper_limit"),
                          numericInput("tm_upper_limit", NULL, value = reactives$tm_upp_bound, step = 1),
                          tippy::tippy_this(
                            elementId = "info_uu-tm_upper_limit",
                            tooltip = "Set an upper limit for the melting temperature (Tm) during fitting.
                            This can help prevent the fitting algorithm from exploring unphysical values.",
                            placement = "right")
                        ))

                    )

                )

            ),

            fluidRow(

                column(
                    width = 4,
                    p(HTML("<b>ΔH</b>"),
                        span(shiny::icon("info-circle"), id = "info_uuL-3"),

                        selectInput("fix_dh_option", NULL,
                            choices=c(
                                "Set bounds" = "set_dh_bounds",
                                "Automatic bounds" = "auto_dh_bounds"
                            ),
                            selected = reactives$fix_dh_option
                        ),

                        tippy::tippy_this(
                            elementId = "info_uuL-3",
                            tooltip = "Set bounds manually, or let Chemelt set automatic bounds.",
                            placement = "right"
                        )
                    )
                ),

                column(
                    width = 8,

                    conditionalPanel(

                        condition = "input.fix_dh_option == 'set_dh_bounds'",

                        column(6, p(
                          HTML("<b>ΔH lower limit</b>"),
                          span(shiny::icon("info-circle"), id = "info_uu-dh_lower_limit"),
                          numericInput("dh_lower_limit", NULL, value = reactives$dh_low_bound, step = 10),
                          tippy::tippy_this(
                            elementId = "info_uu-dh_lower_limit",
                            tooltip = "Set a lower limit for the enthalpy change (ΔH) during fitting.
                            This can help prevent the fitting algorithm from exploring unphysical values.
                            Units are in kcal/mol.",
                            placement = "right")
                        )),

                        # Numeric input - DH upper limit
                        column(6, p(
                          HTML("<b>ΔH upper limit</b>"),
                          span(shiny::icon("info-circle"), id = "info_uu-dh_upper_limit"),
                          numericInput("dh_upper_limit", NULL, value = reactives$dh_upp_bound, step = 10),
                          tippy::tippy_this(
                            elementId = "info_uu-dh_upper_limit",
                            tooltip = "Set an upper limit for the enthalpy change (ΔH) during fitting.
                            This can help prevent the fitting algorithm from exploring unphysical values.
                            Units are in kcal/mol.",
                            placement = "right")
                        ))

                    )

                )

            ),

            footer=tagList(
                modalButton('Close')
            )

        )
    )

})

observeEvent(input$fix_cp_option, {
    reactives$fix_cp_option <- input$fix_cp_option
})

observeEvent(input$cp_value, {
    reactives$cp_value <- input$cp_value
})

observeEvent(input$cp_lower_limit, {
    reactives$cp_low_bound <- input$cp_lower_limit
})

observeEvent(input$cp_upper_limit, {
    reactives$cp_upp_bound <- input$cp_upper_limit
})

observeEvent(input$fix_tm_option, {
    reactives$fix_tm_option <- input$fix_tm_option
})

observeEvent(input$cp_lower_limit, {
    reactives$tm_low_bound <- input$tm_lower_limit
})

observeEvent(input$tm_upper_limit, {
    reactives$tm_upp_bound <- input$tm_upper_limit
})

observeEvent(input$fix_dh_option, {
    reactives$fix_dh_option <- input$fix_dh_option
})

observeEvent(input$dh_lower_limit, {
    reactives$dh_low_bound <- input$dh_lower_limit
})

observeEvent(input$dh_upper_limit, {
    reactives$dh_upp_bound <- input$dh_upper_limit
})