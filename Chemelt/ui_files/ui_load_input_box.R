box(title = "Input", width = 3, solidHeader = T, status = "primary", 
    fluidRow(
      
      column(11, p(HTML("<b>1. DSF files </b>"),
                   span(shiny::icon("info-circle"), id = "info_uu1-1"),
                   fileInput("dsf_input_files", NULL,
                   accept   = c(".xlsx",".zip",".xls",".csv",".txt",'.json','.supr'),
                   multiple = TRUE),
                   tippy::tippy_this(elementId = "info_uu1-1",
                   tooltip = "Check the User Guide to understand the format of the input files. 
                   Hint: Multiple files can be uploaded at once.",
                   placement = "right"))),

      column(4, p(HTML("<b>Signal</b>"),
                  selectInput("which", NULL,
                              c("Ratio"="Ratio","350nm" = "350nm","330nm" = "330nm","Scattering"="Scattering")))),

      # Checkbox input to rescale between 0 and 100
      column(
        6, p(HTML("<b>Normalise</b>"),
        span(shiny::icon("info-circle"), id = "info_uu1-rescale"),
        checkboxInput("rescale", NULL, TRUE),
        tippy::tippy_this(
        elementId="info_uu1-rescale",
        tooltip = "If selected, the signal will be divided by the global maximum and multiplied by 100",placement = "right")
        )
      ),

      column(12, p(HTML("<b>\nTemperature range (ºC)</b>"),
                   span(shiny::icon("info-circle"), id = "info_uu1-3"),
                   
                   #Change colour of slider (this code should be re-written in a cleaner way. For now, it works)
                   tags$style(HTML('.js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar {
                                                            
                                                            background: #00829c;
                                                            border-top: 1px solid #00829c ;
                                                            border-bottom: 1px solid #00829c ;}
          
                                      /* changes the colour of the number tags */
                                     .irs-from, .irs-to, .irs-single { background: #00829c }'
                   )),
                   sliderInput("sg_range", NULL,min = 5, max = 110,value = c(25,90)),
                   tippy::tippy_this(elementId = "info_uu1-3",
                                     tooltip = "Remove temperature data far from the melting transition. 
                We recommend selecting a temperature range that covers not more than 45 degrees",placement = "right"))),

      conditionalPanel(condition = "input.fill_table",
                       column(4, p(HTML("<b>Init. [Denaturant] (M)</b>"),
                                   numericInput('initial_denaturant',NULL, 6,min = 1, max = 12)))),

      conditionalPanel(condition = "input.fill_table",
                       column(4, p(HTML("<b>#Replicates</b>"),numericInput('n_replicates',NULL, 2,min = 1, max = 6)))),
      
      conditionalPanel(condition = "input.fill_table", 
                       column(4, p(HTML("<b>Dilution Factor</b>"),numericInput('dil_factor',NULL, 2,min = 1, max = 10)))),

      conditionalPanel(condition = "input.fill_table", 
                       column(4, p(HTML("<b>Reverse Order</b>"),checkboxInput("rev_order", "", FALSE)))),
      
      column(6, p(HTML("<b>Autofill Table</b>"),
                  span(shiny::icon("info-circle"), id = "info_uu1-5"),
                  checkboxInput("fill_table", "", FALSE),
                  tippy::tippy_this(elementId = "info_uu1-5",
                                    tooltip = "Select this option to complete the \'Position versus Concentration\'
                                            Table using a constant dilution factor
                                            and an initial ligand concentration",placement = "right")))
      
      
    ))