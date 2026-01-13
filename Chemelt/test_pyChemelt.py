# Test the module pyChemelt
import numpy as np
import pyChemelt
import matplotlib.pyplot as plt

file1 = '/home/os/Downloads/demo.xlsx'
file2 = '/home/os/Downloads/quantStudio.txt'

file1 = "/home/os/Downloads/250813_binder_stabil_titer1.xlsx"

sample = pyChemelt.Sample('test')
sample.read_multiple_files(file1)

print(sample.signals)

#sample.read_multiple_files(file2)

sample.set_signal(sample.signals[0])
sample.set_denaturant_concentrations()
sample.select_conditions(normalise_to_global_max=T)
sample.set_temperature_range(30,80)

sample.estimate_derivative()
sample.guess_Tm()

sample.set_signal(sample.signals[0])
sample.set_denaturant_concentrations()
sample.select_conditions(normalise_to_global_max=True)

sample.estimate_derivative()
sample.guess_Tm()

sample.signal_to_df()
sample.signal_to_df('derivative')
