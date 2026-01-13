import numpy as np
import pandas as pd
import os
import pyChemelt
import matplotlib.pyplot as plt

from pyChemelt import residuals_squares_sum

file = '/Users/oburastero/Desktop/arise/spc_shiny_servers/thermochemicalDenaturationApp/appFiles/Chemelt/www/nDSFdemoFile.xlsx'

sample = pyChemelt.Sample('name')
sample.read_file(file)


pon = 2
pou = 2

def plot_signal_df(signal_df, lines_df=None, save_path=None,show=False):

    plt.figure(figsize=(8, 6))
    scatter = plt.scatter(
        signal_df['Temperature'],
        signal_df['Signal'],
        c=signal_df['Denaturant'],
        cmap='viridis',
        edgecolor='none'
    )

    # Plot black lines for each ID in lines_df
    if lines_df is not None:
        for _, group in lines_df.groupby('ID'):
            plt.plot(group['Temperature'], group['Signal'], color='black', linewidth=0.7, alpha=0.7)

    plt.xlabel('Temperature')
    plt.ylabel('Signal')
    plt.title('Signal vs Temperature colored by Denaturant')
    cbar = plt.colorbar(scatter)
    cbar.set_label('Denaturant Concentration')

    if save_path:
        os.makedirs(os.path.dirname(save_path), exist_ok=True)
        plt.savefig(save_path, dpi=300, bbox_inches='tight')

    if show:
        plt.show()
    else:
        plt.close()
        return None

n1 = 4
n2 = 10

l1 = [False for _ in range(n1)]
l2 = [True for _ in range(n2)]
l3 = [False for _ in range(48-n1-n2)]

bool_lst = l1 + l2 + l3

signals = sample.signals[2]

sample.set_signal(signals)
sample.set_denaturant_concentrations()
sample.select_conditions(bool_lst)
sample.set_temperature_range(20,100)

sample.n_residues = 129
sample.max_points = 120
sample.pre_fit = False

sample.estimate_derivative()
sample.guess_Tm()
sample.estimate_baseline_parameters(poly_order_native=pon, poly_order_unfolded=pou)
sample.fit_thermal_unfolding_local()
sample.guess_Cp()

# Global fit

# Create a vector between 1.7/2 and 1.7*2
cp_fixed_vec = np.linspace(1.7/2,1.7*2,14)

results = []


for cp_fixed in cp_fixed_vec:

    sample.fit_thermal_unfolding_global(
        cp_value=cp_fixed,
        dh_limits=[30,400])

    sample.set_signal_id()
    sample.fit_thermal_unfolding_global_global()
    sample.fit_thermal_unfolding_global_global_global()

    signal_df = sample.signal_to_df(signal_type='raw')
    fitted_df = sample.signal_to_df(signal_type='fitted')
    #plot_signal_df(signal_df, fitted_df,show=False)

    # Extract RSS
    rss = residuals_squares_sum(
        sample.signal_lst_expanded,
        sample.predicted_lst_multiple
    )

    row = {
        'Tm (K)': sample.params_df.iloc[0, 1],
        'DH (kcal/mol)': sample.params_df.iloc[1, 1],
        'm-value': sample.params_df.iloc[2, 1],
        'Cp fixed': cp_fixed,
        'RSS': rss
    }

    results.append(row)

df = pd.DataFrame(results)
print(df)

# Plot RSS versus Cp fixed and show it
plt.figure(figsize=(6, 4))
plt.scatter(df['Cp fixed'], df['RSS'], c='C0', s=20)
plt.xlabel('Cp fixed')
plt.ylabel('RSS')
plt.title('RSS vs Cp fixed')
plt.tight_layout()
plt.show()
