options(browser='google-chrome-stable')
# Extract the directory where this script is located
library(tidyverse)

getCurrentFileLocation <-  function()
{
    this_file <- commandArgs() %>%
    tibble::enframe(name = NULL) %>%
    tidyr::separate(col=value, into=c("key", "value"), sep="=", fill='right') %>%
    dplyr::filter(key == "--file") %>%
    dplyr::pull(value)
    if (length(this_file)==0)
    {
      this_file <- rstudioapi::getSourceEditorContext()$path
    }
    return(dirname(this_file))
}

appDir <- getCurrentFileLocation()

shiny::runApp(appDir)
