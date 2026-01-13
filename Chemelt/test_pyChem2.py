# Test the module pyChemelt
import numpy as np
import pyChemelt
import matplotlib.pyplot as plt

file = '/Users/oburastero/Downloads/demo.xlsx'
file = '/home/os/Downloads/demo.xlsx'
file = "/home/os/Downloads/Titer data intensities-20251013T134434Z-1-001/Titer data intensities/titer4.xlsx"
file = '/home/os/Downloads/20191202_ACBP_15C_95C_processed.xlsx'

sample = pyChemelt.Sample('name')
sample.read_file(file)

quit()

import matplotlib.pyplot as plt
import matplotlib as mpl

import os

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
        return None

def plot_dg_df(dg_df, save_path=None,show=False):

    plt.figure(figsize=(8, 6))
    plt.plot(dg_df['Temperature (°C)'], dg_df['DG (kcal/mol)'], color='black')
    plt.xlabel('Temperature (°C)')
    plt.ylabel('ΔG (kcal/mol)')
    plt.title('ΔG vs Temperature')
    plt.grid(True)
    if save_path:
        os.makedirs(os.path.dirname(save_path), exist_ok=True)
        plt.savefig(save_path, dpi=300, bbox_inches='tight')

    if show:
        plt.show()
    else:
        return None

file = "/home/os/Downloads/Titer data intensities-20251013T134434Z-1-001/Titer data intensities/titer4.xlsx"

names = ['A4A','A4B']

bool_lst_A4A = [True for _ in range(22)] + [False for _ in range(22)]


bool_lst_A4B = [False for _ in range(22)] + [True for _ in range(15)] + [False] + [True for _ in range(6)]


for name, bool_lst in zip(names, [bool_lst_A4A, bool_lst_A4B]):

    sample = pyChemelt.Sample(name)
    sample.read_file(file)

    signals = sample.signals[1]

    sample.set_signal(signals)
    sample.set_denaturant_concentrations()
    sample.select_conditions(bool_lst)
    sample.set_temperature_range(20,100)

    sample.n_residues = 139
    sample.max_points = 120
    sample.pre_fit = False

    for pon in [1,2]:

        for pou in [1,2]:

            try:

                sample.estimate_derivative()
                sample.guess_Tm()
                sample.estimate_baseline_parameters(poly_order_native=pon, poly_order_unfolded=pou)
                sample.fit_thermal_unfolding_local()
                sample.guess_Cp()

                # Global fit

                sample.fit_thermal_unfolding_global(
                    user_cp_limits=[0.5,4.5],
                    user_dh_limits=[50,400])

                sample.create_dg_df()

                # find dg at 5K
                df_5K = sample.dg_df[sample.dg_df['Temperature (°C)'] == 5]

                value = df_5K['DG (kcal/mol)'].values[0]

                if value > 0:

                    save_path = "/home/os/Downloads/dsf_results/" + name + "/dg1_" + str(pon) + "_" + str(pou) + ".png"
                    plot_dg_df(sample.dg_df,save_path=save_path)

                    signal_df = sample.signal_to_df(signal_type='raw')
                    fitted_df = sample.signal_to_df(signal_type='fitted')
                    save_path = "/home/os/Downloads/dsf_results/" + name + "/fit1_" + str(pon) + "_" + str(pou) + ".png"
                    plot_signal_df(signal_df, fitted_df,save_path=save_path)

                    sample.params_df.to_csv("/home/os/Downloads/dsf_results/" + name + "/params1_" + str(pon) + "_" + str(pou) + ".csv")

                # Global global fit

                sample.set_signal_id()
                sample.fit_thermal_unfolding_global_global()

                sample.create_dg_df()

                # find dg at 5K
                df_5K = sample.dg_df[sample.dg_df['Temperature (°C)'] == 5]

                value = df_5K['DG (kcal/mol)'].values[0]

                if value > 0:

                    save_path = "/home/os/Downloads/dsf_results/" + name + "/dg2_" + str(pon) + "_" + str(pou) + ".png"
                    plot_dg_df(sample.dg_df,save_path=save_path)

                    signal_df = sample.signal_to_df(signal_type='raw')
                    fitted_df = sample.signal_to_df(signal_type='fitted')
                    save_path = "/home/os/Downloads/dsf_results/" + name + "/fit2_" + str(pon) + "_" + str(pou) + ".png"
                    plot_signal_df(signal_df, fitted_df,save_path=save_path)

                    sample.params_df.to_csv("/home/os/Downloads/dsf_results/" + name + "/params2_" + str(pon) + "_" + str(pou) + ".csv")

                # Global global global fit

                sample.fit_thermal_unfolding_global_global_global(model_scale_factor=True)

                sample.create_dg_df()

                # find dg at 5K
                df_5K = sample.dg_df[sample.dg_df['Temperature (°C)'] == 5]

                value = df_5K['DG (kcal/mol)'].values[0]

                if value > 0:

                    save_path = "/home/os/Downloads/dsf_results/" + name + "/dg3_" + str(pon) + "_" + str(pou) + ".png"
                    plot_dg_df(sample.dg_df,save_path=save_path)

                    signal_df = sample.signal_to_df(signal_type='raw',scaled=True)
                    fitted_df = sample.signal_to_df(signal_type='fitted',scaled=True)
                    save_path = "/home/os/Downloads/dsf_results/" + name + "/fit3_" + str(pon) + "_" + str(pou) + ".png"
                    plot_signal_df(signal_df, fitted_df,save_path=save_path)

                    sample.params_df.to_csv("/home/os/Downloads/dsf_results/" + name + "/params3_" + str(pon) + "_" + str(pou) + ".csv")

            except:

                pass
