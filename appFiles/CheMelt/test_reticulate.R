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
pySample$set_signal(list('Ratio'))


pySample$set_denaturant_concentrations()
conditions = c(rep(TRUE,16),rep(FALSE,8),rep(FALSE,24))
pySample$select_conditions(normalise_to_global_max=FALSE)
pySample$estimate_derivative()
pySample$guess_Tm()
pySample$reset_fittings_results()
pySample$n_residues <- 100
pySample$max_points <- 100


pySample$guess_initial_parameters(
native_baseline_type     = 'linear',
unfolded_baseline_type   = 'linear',
window_range_native = 10,
window_range_unfolded = 10
)


source('./helpers_R/helpers.R')

pySample$estimate_baseline_parameters(
    native_baseline_type = 'linear',
    unfolded_baseline_type = 'linear',
    window_range_native = 10,
    window_range_unfolded = 10
)

pySample$fit_thermal_unfolding_global()
pySample$fit_thermal_unfolding_global_global()
print(pySample$params_df)

pySample$leave_one_out_cross_validation()

print(pySample$loo_df)