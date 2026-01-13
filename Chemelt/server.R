options(shiny.maxRequestSize=100*1024^2)
options(stringsAsFactors = F)

#source_python("fitting_helpers_thermal_unfolding.py")
#source_python("fitting_helpers_unfolded_fraction.py")
#source_python("foldAffinity.py")

pyChemelt <- import("pychemelt")

source("helpers_R/helpers.R")
source("helpers_R/load_input_helpers.R")
source("helpers_R/plot_helpers.R")

#source("server_files/simulation_helpers.R")
#source("server_files/plot_functions.R")
#source("server_files/fitting_helpers.R")
### End of variables to change

function(input, output, session) {

    pySample <- pyChemelt$Sample('test')

    source(paste0(base_dir,"reactives/reactive_values.R"),      local = T)
    source(paste0(base_dir,"reactives/logbook_reactives.R"),    local = T)
    source(paste0(base_dir,"reactives/load_input_reactives.R"), local = T)
    source(paste0(base_dir,"reactives/plot_reactives.R"),       local = T)
    source(paste0(base_dir,"reactives/analysis_reactives.R"),   local = T)
    source(paste0(base_dir,"reactives/params_bounds_reactives.R"), local = T)
    source(paste0(base_dir,"reactives/download_reactives.R"),   local = T)


}


