box(title = "Plot options", width = 12, solidHeader = T, status = "primary",
    fluidRow(

        column(2, p(HTML("<b>Font size</b>"),
            numericInput('plot_axis_size',NULL, 16,min = 4, max = 40))
        ),

        column(3, p(HTML("<b>Legend x-axis position</b>"),
            sliderInput('x_legend_pos',NULL, 1,min = 0, max = 1,step = 0.1))
        ),

        column(3, p(HTML("<b>Legend y-axis position</b>"),
            sliderInput('y_legend_pos',NULL, 1,min = 0, max = 1,step = 0.1))
        ),

        column(2, p(HTML("<b>Show X-grid</b>")),
            checkboxInput('show_grid_x',NULL,FALSE)
        ),

        column(2, p(HTML("<b>Show Y-grid</b>")),
            checkboxInput('show_grid_y',NULL,FALSE)
        )

    ),

    fluidRow(

        #marker size
        column(2, p(HTML("<b>Marker size</b>"),
            sliderInput('plot_marker_size',NULL, 7,min = 2, max = 12,step = 0.5))
        ),


        column(3, p(HTML("<b>Color bar orientation</b>"),
            selectInput("color_bar_orientation", NULL,
            c("horizontal" = "h",
            "vertical"  = "v"
            )))
        ),

        column(3, p(HTML("<b>Color bar length</b>"),
            sliderInput('color_bar_length',NULL, 0.4,min = 0, max = 1,step = 0.1))
        ),

        # Max points
        column(2, p(HTML("<b>Max points</b>"),
            sliderInput('max_points',NULL, 2000,min = 100, max = 1e4,step = 100))
        ),

        column(2, p(HTML("<b>File type</b>"),
            selectInput("plot_type", NULL,
            c("PNG"                 = "png",
            "SVG"    = "svg",
            "JPEG"    = "jpeg")))
        )

    ),

    fluidRow(

        # Line width
        column(2, p(HTML("<b>Line width</b>"),
            sliderInput('plot_line_width',NULL, 3,min = 0.5, max = 10,step = 0.5))
        ),


        column(2, p(HTML("<b>Width</b>"),
            span(shiny::icon("info-circle"), id = "info_uu1-13"),
            numericInput('plot_width',NULL, 18,min = 1, max = 100),
            tippy::tippy_this(elementId = "info_uu1-13",
            tooltip = "Units are pixels * 50",placement = "right"))
        ),

        column(2, p(HTML("<b>Height</b>"),
            span(shiny::icon("info-circle"), id = "info_uu1-14"),
            numericInput('plot_height',NULL, 11,min = 1, max = 100),
            tippy::tippy_this(elementId = "info_uu1-14",
            tooltip = "Units are pixels * 50",placement = "right"))
        )


    )
)

