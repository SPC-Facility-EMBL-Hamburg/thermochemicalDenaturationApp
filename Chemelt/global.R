packages <- c(
    'reshape2','tidyverse','reticulate',"plotly","shinyalert",
    "shinydashboard","shinycssloaders","rhandsontable"
)

invisible(lapply(packages, library, character.only = TRUE))

appName     <- "Chemelt"
user        <- Sys.info()['user']

# Detect if we have macbook or linux
if (Sys.info()['sysname'] == "Darwin") {
  reticulate::use_python(paste0("/Users/",user,"/myenv/bin/python"), required = TRUE)
  base_dir <- paste0("/Users/",user,"/Desktop/arise/spc_shiny_servers/thermochemicalDenaturationApp/appFiles/",appName,"/")
} else {
  reticulate::use_python(paste0("/home/",user,"/myenv/bin/python"), required = TRUE)
  base_dir <- paste0("/home/",user,"/spc_shiny_servers/thermochemicalDenaturationApp/appFiles/",appName,"/")
}

# path for the docker user
if (user == 'shiny') base_dir <- paste0("/home/shiny/",appName,'/')

tick_length_cst <- 8

