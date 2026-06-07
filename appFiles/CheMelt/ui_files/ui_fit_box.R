box(title = "2b. Fitting", width = 6, solidHeader = T, status = "primary",

    fluidRow(

        # To select type of fitting, between
        # A) global thermodynamic parameters (but local baselines and slopes)
        # B) global thermodynamic parameters and global slopes (but local baselines)
        # C) with global thermodynamic parameters, global slopes and global baselines 3D fitting

        column(6,p(
            HTML("<b>Model</b>"),
            span(shiny::icon("info-circle"), id = "info_uu-unfolding_model"),
            selectInput("unfolding_model", NULL,choices =
                c("Local intercepts and slopes" = "global-local-local",
                "Global slopes and local intercepts" = "global-global-local",
                "Global slopes and global intercepts" = "global-global-global",
                "Compare models" = "compare-many-models"
                ),selectize=FALSE),
            tippy::tippy_this(
            elementId = "info_uu-unfolding_model",
            tooltip = "If the unfolding curves were obtained under the same experimental conditions,
            such as protein concentration, instrument type, instrument power, etc., use the models with global slopes.
            ",placement = "right"))
        ),

        conditionalPanel(
          condition = "input.unfolding_model == 'global-global-global'",
          column(6,p(
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
        )

    ),

    fluidRow(

        conditionalPanel(
          condition = "input.unfolding_model != 'compare-many-models'",

            column(6,p(
                HTML("<b>Native baseline</b>"),
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
                Set to exponential (or quadratic) if there's a non-linear dependence.",placement = "right"))
            ),

            column(6,p(
                HTML("<b>Unfolded baseline</b>"),
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
                Set to exponential (or quadratic) if there's a non-linear dependence.",placement = "right"))
            )

        ),

        # button to run the fit
        column(6, p(HTML('<p style="margin-bottom:0px;"><br></p>'),
            actionButton(
            inputId = "btn_call_fit",label = "Run Fitting!",
            icon("meteor"),
            style="color: #fff; background-color: #337ab7;
            border-color: #2e6da4"))
        ),

        # Little hack to use the withBusyIndicatorUI function (loading spinner)
        column(2, p(HTML('<p style="margin-bottom:0px;"><br></p>'),
            withBusyIndicatorUI(
            shinyjs::hidden(actionButton("Go","",class = "btn-primary"))))
        )

    ),


    fluidRow(

        # Panel to allow computing asymmetric confidence intervals
        conditionalPanel(
            'output.fitting_done',
                        # button to run the fit
            column(6, p(HTML('<p style="margin-bottom:0px;"><br></p>'),
                actionButton(
                inputId = "btn_cal_conf_interval",
                label = "Calculate CI95",
                icon("flag"),
                style="color: #fff; background-color: #337ab7;
                border-color: #2e6da4"))
            ),

            # Little hack to use the withBusyIndicatorUI function (loading spinner)
            column(2, p(HTML('<p style="margin-bottom:0px;"><br></p>'),
                withBusyIndicatorUI(
                shinyjs::hidden(actionButton("Go2","",class = "btn-primary"))))
            )

        )

    )

)