box(title = "2a. Settings", width = 6, solidHeader = T, status = "primary",

    fluidRow(

        # Temperature window for baseline estimation, slider between 5 and 30, only one value is selected
        column(6, p(
            HTML("<b>Window for baseline estimation (ºC) - Native</b>"),
            span(shiny::icon("info-circle"), id = "info_uu-baseline_window_native"),
            sliderInput("baseline_window_native", NULL, min = 5, max = 20, value = c(5, 10)),
            tippy::tippy_this(
            elementId = "info_uu-baseline_window_native",
            tooltip = "Select a temperature range for the initial baseline estimation of the native state.
            It is used to provide good initial values to the fitting algorithm.",
            placement = "right"))
        ),

        # Temperature window for baseline estimation, slider between 5 and 30, only one value is selected
        column(6, p(
            HTML("<b>Window for baseline estimation (ºC) - Unfolded</b>"),
            span(shiny::icon("info-circle"), id = "info_uu-baseline_window_unfolded"),
            sliderInput("baseline_window_unfolded", NULL, min = 5, max = 20, value = c(15, 20)),
            tippy::tippy_this(
            elementId = "info_uu-baseline_window_unfolded",
            tooltip = "Select a temperature range for the initial baseline estimation of the unfolded state.
            It is used to provide good initial values to the fitting algorithm.",
            placement = "right"))
        ),

        # Column to input the number of residues
        column(6,p(
            HTML("<b>Number of residues</b>"),
            span(shiny::icon("info-circle"), id = "info_uu-n_residues"),
            numericInput("n_residues", NULL, value = 100, min = 1, step = 1),
            tippy::tippy_this(
            elementId = "info_uu-n_residues",
            tooltip = "Input the number of residues in the protein sequence.
            This is used to provide an initial value for ΔC<sub>p</sub> during fitting.",
            placement = "right"))
        ),

                # button to run the fit
        column(6, p(HTML('<p style="margin-bottom:0px;"><br></p>'),
            actionButton(
            inputId = "show_advanced_options_fit",label = "Set params bounds",
            icon("gear")))
        )

    ),

    fluidRow(

        # Checkbox to show advanced options
        column(6, p(HTML("<b>Fit data subset</b>"),
            checkboxInput("fit_subset", NULL, value = TRUE)
        )),

        conditionalPanel(
            'input.fit_subset',
            column(6, p(
                HTML("<b>Max points per curve</b>"),
                span(shiny::icon("info-circle"), id = "info_uu-max_points_per_curve"),
                numericInput("max_points_per_curve", NULL, value = 100, step = 10),
                tippy::tippy_this(
                elementId = "info_uu-max_points_per_curve",
                tooltip = "Fit the data using at most this number of points per curve (evenly spaced).",
                placement = "right")
            ))
        )


    )

)