library(reticulate)
pyDen <- import("pyChemelt")

sample = pyDen$Sample('test')
sample$read_file('/home/os/Downloads/demo.xlsx')

bool_lst  <- rep(FALSE, 24)
bool_lst <- c(bool_lst, rep(TRUE, 12))
bool_lst <- c(bool_lst, rep(FALSE, 12))

bool_lst <- list(bool_lst)

signals <- sample$signals[0]

sample$set_signal(signals)

sample$set_denaturant_concentrations()

sample$select_conditions(bool_lst)

print('here')


sample$set_temperature_range(30,80)

print('here')

sample$estimate_derivative()
sample$guess_Tm()
sample$estimate_baseline_parameters(poly_order_native=1, poly_order_unfolded=1)
sample$fit_thermal_unfolding_local()
sample$guess_Cp()
sample$fit_thermal_unfolding_global()

#sample$set_signal_id()
#sample$fit_thermal_unfolding_global_global()

#sample$fit_thermal_unfolding_global_global_global()