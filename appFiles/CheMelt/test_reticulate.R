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
pySample$set_signal(list('350nm','330nm'))



pySample$set_denaturant_concentrations()
conditions = c(rep(FALSE,24),rep(TRUE,14),rep(FALSE,20))
pySample$select_conditions(normalise_to_global_max=TRUE)
pySample$estimate_derivative()
pySample$guess_Tm()
pySample$reset_fittings_results()
pySample$n_residues <- 100


pySample$guess_initial_parameters(
native_baseline_type     = 'linear',
unfolded_baseline_type   = 'exponential',
window_range_native = 10,
window_range_unfolded = 10
)

print(pySample$signal_names)

quit()


pySample$fit_thermal_unfolding_global()
pySample$fit_thermal_unfolding_global_global()

print(pySample$params_df)


library(plotly)

source("./helpers_R/helpers.R")
source("./helpers_R/plot_helpers.R")



