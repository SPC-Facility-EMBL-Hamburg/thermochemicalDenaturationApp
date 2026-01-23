box(title = "Plot options", width = 10, solidHeader = T, status = "primary",

    fluidRow(

        column(
          width=3,
          p(HTML('<p style="margin-bottom:0px;"></p>'),
            actionButton(
              inputId = "configure_legend_fit",
              label = "Colorbar",
              icon("palette"),
              style="color: #fff; background-color: #337ab7;
              border-color: #2e6da4")
          )
        ),

        column(
          width=3,
          p(HTML('<p style="margin-bottom:0px;"></p>'),
            actionButton(
              inputId = "configure_appearance_fit",
              label = "Font, markers and lines",
              icon("marker"),
              style="color: #fff; background-color: #337ab7;
              border-color: #2e6da4")
          )
        ),

        column(
          width=3,
          p(HTML('<p style="margin-bottom:0px;"></p>'),
            actionButton(
              inputId = "configure_axis_fit",
              label = "Axis",
              icon("grip"),
              style="color: #fff; background-color: #337ab7;
              border-color: #2e6da4")
          )
        ),

        column(
          width=3,
          p(HTML('<p style="margin-bottom:0px;"></p>'),
            actionButton(
              inputId = "configure_export_fit",
              label = "Export",
              icon("maximize"),
              style="color: #fff; background-color: #337ab7;
              border-color: #2e6da4")
          )
        )

    )

)
