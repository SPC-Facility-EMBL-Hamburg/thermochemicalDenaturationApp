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

file_example <- "./www/20191202_ACBP_15C_95C_processed.xlsx"

pySample <- pyChemelt$Monomer('test')
pySample$read_multiple_files(file_example)
pySample$set_signal(list('330nm','350nm'))


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
source('./helpers_R/plot_helpers.R')

pySample$estimate_baseline_parameters(
    native_baseline_type = 'linear',
    unfolded_baseline_type = 'exponential',
    window_range_native = 10,
    window_range_unfolded = 10
)

pySample$fit_thermal_unfolding_global()
#pySample$fit_thermal_unfolding_global_global()
print(pySample$baseline_df)

signal_df <- pySample$signal_to_df()
fitted_df <- pySample$signal_to_df(signal_type = "fitted")
baseline_df <- pySample$baseline_df

library(plotly)
fig <- plot_fluo_signal(
        signal_df              = signal_df,
        unfolding_fitted_data              = fitted_df,
        baseline_df            = baseline_df)

fig