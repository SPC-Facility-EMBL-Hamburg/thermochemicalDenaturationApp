reactives <- reactiveValues(
    logbook = list(),# record data manipulation steps
    update_plots = NULL,  # show plots/tables
    signal_df = NULL,  # signal data frame
    derivative_df = NULL,  # derivative data frame
    signal_df_fitted = NULL,  # fitted signal data frame
    signal_df_scaled = NULL,  # scaled signal data frame
    signal_df_fitted_scaled = NULL,  # fitted scaled signal data frame
    find_initial_params = TRUE, # trigger to find initial params
    scaled_tab_shown = FALSE, # whether the scaled tab is shown
    n_rows_conditions_table = 24, # number of rows in the conditions table (default 24, can be modified)
    tm_value = NULL, # Tm value - if given, used as fixed param
    dh_value = NULL, # DH value - if given, used as fixed param
    cp_value = NULL, # Cp value - if given, used as fixed param
    fix_cp_option='auto_cp_bounds', # option to fix Cp value or set bounds
    fix_tm_option='auto_tm_bounds', # option to fix Tm value or set bounds
    fix_dh_option='auto_dh_bounds', # option to fix DH value or set bounds
    tm_low_bound = 60,  # Tm lower bound
    tm_upp_bound = 130, # Tm upper bound
    dh_low_bound = 10, # DH lower bound
    dh_upp_bound = 450, # DH upper bound
    cp_low_bound = 0.1, # Cp lower bound
    cp_upp_bound = 6, # Cp upper bound
)

