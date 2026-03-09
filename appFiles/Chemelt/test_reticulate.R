gc()
rm(list=ls())
library(reticulate)


appName     <- "Chemelt"
user        <- Sys.info()['user']

# Detect if we have macbook or linux
if (Sys.info()['sysname'] == "Darwin") {
  reticulate::use_python(paste0("/Users/",user,"/myenv/bin/python"), required = TRUE)
  base_dir <- paste0("/Users/",user,"/Desktop/arise/thermochemicalDenaturationApp/appFiles/",appName,"/")
} else {
  reticulate::use_python(paste0("/home/",user,"/myenv/bin/python"), required = TRUE)
  base_dir <- paste0("/home/",user,"/thermochemicalDenaturationApp/appFiles/",appName,"/")
}


setwd(base_dir)

library(tidyverse)
pyChemelt <- import("pychemelt")

file_example <- "./www/nDSFdemoFile.xlsx"

pySample <- pyChemelt$Monomer('test')
pySample$read_multiple_files(file_example)
pySample$set_signal(pySample$signals[1])


pySample$set_denaturant_concentrations()
pySample$select_conditions(normalise_to_global_max=TRUE)
pySample$estimate_derivative()
pySample$guess_Tm()


library(plotly)

source("./helpers_R/helpers.R")
source("./helpers_R/plot_helpers.R")

pySample$reset_fittings_results()
pySample$estimate_derivative()


signal_df     <- pySample$signal_to_df()
derivative_df <- pySample$signal_to_df(signal_type = "derivative")

signal_df     <- pandas_to_r(signal_df)
derivative_df <- pandas_to_r(derivative_df)




