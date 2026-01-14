source("ui_files/theme.R")
source("ui_files/logo.R")
source("ui_files/busy_indicator.R")

shinyUI(
    dashboardPage(
        title = appName,
  
        dashboardHeader(
            title = logo_grey_light, 
            titleWidth = 200),
        
        dashboardSidebar(
            collapsed = F,
            width = 200,

            sidebarMenu(

                menuItem("1. Import data", icon = icon("file-circle-plus"), tabName = "menu_input"),
                menuItem("2. Fitting", icon = icon("chart-line"), tabName = "menu_fit"),
                menuItem("3. Export results",        icon = icon("file-export"),            tabName = "menu_export"),
                #menuItem("Simulate data",            icon = icon("magnifying-glass-chart"), tabName = "menu_simulate"),
                menuItem("User guide",               icon = icon("user-astronaut"),         tabName = "menu_user_guide"),
                menuItem("Tutorial",                 icon = icon("book-open"),              tabName = "menu_tutorial"),
                menuItem("About", icon = icon("circle-info"), tabName = "menu_about")
            )
        ),

        dashboardBody(
            theme_grey_light,

            includeHTML("www/banners.html"),
            includeScript("www/banner.js"),

            tabItems(
                tabItem(tabName = "menu_input",
                    fluidRow(

                        #Custom CSS to increase plot height
                        tags$head(tags$style("
                        #signal{height:650px !important;}
                        #first_der{height:650px !important;}
                        #tm_vs_den{height:650px !important;}
                        #initial_fluo_vs_den{height:650px !important;}
                        ")),

                        source("ui_files/ui_load_input_box.R",local = TRUE)$value,
                        source("ui_files/ui_position_vs_concentration_box.R",local = TRUE)$value,
                        source("ui_files/ui_signal_tab_box.R",local = TRUE)$value,
                        source("ui_files/ui_plot_options_box.R",local = TRUE)$value
                    )
                ),

                tabItem(tabName = "menu_fit",
                    fluidRow(

                        #Custom CSS to increase plot height
                        tags$head(tags$style("
                        #fitted_signal{height:650px !important;}
                        #dg_vs_temp{height:650px !important;}
                        #fitted_signal_den_in_x_axis{height:650px !important;}
                        ")),

                        source("ui_files/ui_fit_box.R",local = TRUE)$value,
                        source("ui_files/ui_signal_fit_tab_box.R",local = TRUE)$value,
                        source("ui_files/ui_plot_options_box_fit.R",local = TRUE)$value
                    )
                ),

                tabItem(tabName = "menu_export",

                    fluidRow(
                    source("ui_files/ui_export_fitting_information.R",local = TRUE)$value
                    )
                ),

                tabItem(tabName = "menu_user_guide", includeHTML("www/docs/user_guide.html")),
                tabItem(tabName = "menu_tutorial", includeHTML("www/docs/tutorial.html")),
                tabItem(tabName = "menu_about", includeHTML("www/docs/about.html"))

            )
        )
    )
)