import pyChemelt

file = '/home/os/Downloads/20191202_ACBP_15C_95C_processed.xlsx'

sample = pyChemelt.Sample('test')

sample.read_file(file)

sample.set_signal(sample.signals[1])

print(sample.signals)

sample.set_denaturant_concentrations()
sample.select_conditions()

sample.estimate_derivative()
sample.guess_Tm()

sample.guess_initial_parameters(
    10,
    10
)

sample.estimate_baseline_parameters(
    12,
    12,
    2,
    2
)

sample.set_signal_id()
sample.fit_thermal_unfolding_global()
sample.fit_thermal_unfolding_global_global()
