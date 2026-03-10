box(title = "2. Fitting", width = 12, solidHeader = T, status = "primary",

    fluidRow(

        # Temperature window for baseline estimation, slider between 5 and 30, only one value is selected
        column(2, p(
            HTML("<b>2a. Window for baseline estimation (ºC) - Native</b>"),
            span(shiny::icon("info-circle"), id = "info_uu-baseline_window_native"),
            sliderInput("baseline_window_native", NULL, min = 5, max = 20, value = 10),
            tippy::tippy_this(
            elementId = "info_uu-baseline_window_native",
            tooltip = "Select a temperature range for the initial baseline estimation of the native state.
            It is used to provide good initial values to the fitting algorithm.",
            placement = "right"))
        ),

        # Temperature window for baseline estimation, slider between 5 and 30, only one value is selected
        column(2, p(
            HTML("<b>2b. Window for baseline estimation (ºC) - Unfolded</b>"),
            span(shiny::icon("info-circle"), id = "info_uu-baseline_window_unfolded"),
            sliderInput("baseline_window_unfolded", NULL, min = 5, max = 20, value = 10),
            tippy::tippy_this(
            elementId = "info_uu-baseline_window_unfolded",
            tooltip = "Select a temperature range for the initial baseline estimation of the unfolded state.
            It is used to provide good initial values to the fitting algorithm.",
            placement = "right"))
        ),

        # To select type of fitting, between
        # A) global thermodynamic parameters (but local baselines and slopes)
        # B) global thermodynamic parameters and global slopes (but local baselines)
        # C) with global thermodynamic parameters, global slopes and global baselines 3D fitting

        column(3,p(
            HTML("<b>2c. Model</b>"),
            span(shiny::icon("info-circle"), id = "info_uu-unfolding_model"),
            selectInput("unfolding_model", NULL,choices =
                c("Local intercepts and slopes" = "global-local-local",
                "Global slopes and local intercepts" = "global-global-local",
                "Global slopes and global intercepts" = "global-global-global"
                ),selectize=FALSE),
            tippy::tippy_this(
            elementId = "info_uu-unfolding_model",
            tooltip = "If possible, use the model with global slopes.",placement = "right"))
        ),

        conditionalPanel(
          condition = "input.unfolding_model == 'global-global-global'",
          column(2,p(
                HTML("<b>Fit scale factor</b>"),
                span(shiny::icon("info-circle"), id = "info_uu-scale_factor"),
                checkboxInput("fit_scale_factor", NULL, value = TRUE),
                tippy::tippy_this(
                elementId = "info_uu-scale_factor",
                tooltip = "Apply a small rescaling of fluorescence intensities to correct for
                protein concentration errors,
                differences in detection sensitivity across positions, or artefacts (e.g., dust).
                If you measure the exact same sample in all positions and the unfolding curves do not overlap,
                enable the scale factor.",placement = "right")
          ))
        ),

        # Column to input the number of residues
        column(2,p(
            HTML("<b>Number of residues</b>"),
            span(shiny::icon("info-circle"), id = "info_uu-n_residues"),
            numericInput("n_residues", NULL, value = 100, min = 1, step = 1),
            tippy::tippy_this(
            elementId = "info_uu-n_residues",
            tooltip = "Input the number of residues in the protein sequence.
            This is used to provide an initial value for ΔC<sub>p</sub> during fitting.",
            placement = "right"))
        )

    ),

    fluidRow(

        column(3,p(
            HTML("<b>2d. Signal dependence (native)</b>"),
            span(shiny::icon("info-circle"), id = "info_uu-native_dependence"),
            selectInput("native_dependence", NULL,choices =
                c(
                "constant" = "constant",
                "linear"   = "linear",
                "quadratic"= "quadratic",
                "exponential"= "exponential"
                ),selectize=FALSE),
            tippy::tippy_this(
            elementId = "info_uu-native_dependence",
            tooltip = "Set to linear if there's a linear dependence
            between the signal and the temperature for the
            native state.
            Set to quadratic if there's a quadratic dependence.",placement = "right"))
        ),

        column(3,p(
            HTML("<b>2e. Signal dependence (unfolded)</b>"),
            span(shiny::icon("info-circle"), id = "info_uu-unfolded_dependence"),
            selectInput(
                "unfolded_dependence",
                NULL,
                choices =
                  c(
                  "constant" = "constant",
                  "linear"   = "linear",
                  "quadratic"= "quadratic",
                  "exponential" = "exponential"
                  ),
                selectize=FALSE
            ),
            tippy::tippy_this(
            elementId = "info_uu-unfolded_dependence",
            tooltip = "Set to linear if there's a linear dependence
            between the signal and the temperature for the
            unfolded state.
            Set to quadratic if there's a quadratic dependence.",placement = "right"))
        ),

        # button to run the fit
        column(2, p(HTML('<p style="margin-bottom:0px;"><br></p>'),
            actionButton(
            inputId = "btn_call_fit",label = "Run Fitting!",
            icon("meteor"),
            style="color: #fff; background-color: #337ab7;
            border-color: #2e6da4"))
        ),

        # Little hack to use the withBusyIndicatorUI function (loading spinner)
        column(1, p(HTML('<p style="margin-bottom:0px;"><br></p>'),
            withBusyIndicatorUI(
            shinyjs::hidden(actionButton("Go","",class = "btn-primary"))))
        ),

        # button to run the fit
        column(2, p(HTML('<p style="margin-bottom:0px;"><br></p>'),
            actionButton(
            inputId = "show_advanced_options_fit",label = "Set params bounds",
            icon("gear")))
        ),

    ),


    fluidRow(

        # Checkbox to show advanced options
        column(2, p(HTML("<b>Fit data subset</b>"),
            checkboxInput("fit_subset", NULL, value = FALSE)
        )),

        conditionalPanel(
            'input.fit_subset',
            column(2, p(
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