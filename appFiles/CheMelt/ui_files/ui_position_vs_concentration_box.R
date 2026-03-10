box(title = "[Denaturant] (M)", width = 9, solidHeader = T, status = "primary",
    style = "height:600px; overflow-y:auto; overflow-x:hidden;",
    fluidRow(
      column(width = 3,rHandsontableOutput('table1')),
      column(width = 3,rHandsontableOutput('table2')),
      column(width = 3,rHandsontableOutput('table3')),
      column(width = 3,rHandsontableOutput('table4'))
      
    ))