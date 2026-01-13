observeEvent(input$n_residues,{

    reactives$cp_value <- input$n_residues * 0.0148 - 0.1267

})

observeEvent(input$btn_call_fit,{

    req(input$table1)
    req(reactives$signal_df)

    native_dependence   <- input$native_dependence
    unfolded_dependence <- input$unfolded_dependence

    reactives$signal_df_fitted <- NULL
    output$fitted_params       <- NULL

    # Do a prefit if the user wants to fit the whole dataset (and not a subset)
    pySample$pre_fit <- !input$fit_subset

    # do not use ifelse one liner because input$max_points_per_curve is only available if input$fit_subset is TRUE in the UI
    if (input$fit_subset) {
        max_points <- input$max_points_per_curve
    } else {
        max_points <- NULL
    }

    pySample$max_points <- max_points

    pySample$reset_fittings_results()


    # Return an error if the user selected the global global global model but wants to use the Ratio signal
    if (input$unfolding_model == "global-global-global" & "Ratio" %in% pySample$signal_names) {
        popUpWarning(
            "⚠ Error: The 'global-global-global' model cannot be used with the 'Ratio' signal type.
            Please select a different signal."
        )
        return(NULL)
    }

    # give a warning if the ratio signal is being used
    if ("Ratio" %in% pySample$signal_names) {

        popUpWarning(
            "⚠ Warning: The 'Ratio' signal type is not recommended for the fitting of unfolding curves.
            Please consider using an extensive property of the system, such as the fluorescence intensity at a given wavelength."
        )

    }

    withBusyIndicatorServer("Go",{

        # if tab panel for scaled data is present delete it first
        if (reactives$scaled_tab_shown) {

            removeTab('tabset_fit',target = "Fitted signal (rescaled)")
            reactives$scaled_tab_shown <- FALSE
        }

        popUpInfo(
          'Fitting started.
          The plot below will be updated when the fitting is finished.
          Please wait some minutes...'
        )

        native_baseline_type <- input$native_dependence
        unfolded_baseline_type <- input$unfolded_dependence

        pySample$n_residues <- input$n_residues

        if (reactives$find_initial_params) {
        # Use a simple model to guess good initial thermodynamic parameters

              pySample$guess_initial_parameters(
              native_baseline_type     = 'linear',
              unfolded_baseline_type   = 'linear',
              window_range_native = input$baseline_window_native,
              window_range_unfolded = input$baseline_window_unfolded
              )

            if (input$unfolding_model != "global-local-local") {
                popUpInfo('The search for the initial parameters is finished. Please wait some minutes...')
            }

            reactives$find_initial_params <- FALSE
        }


        pySample$estimate_baseline_parameters(
            native_baseline_type     = native_baseline_type,
            unfolded_baseline_type   = unfolded_baseline_type,
            window_range_native     = input$baseline_window_native,
            window_range_unfolded   = input$baseline_window_unfolded
        )

        user_cp_limits <- NULL
        user_dh_limits <- NULL
        user_tm_limits <- NULL
        cp_value <- NULL

        bounds_options_open <- !is.null(input$fix_cp_option)

        if (bounds_options_open) {

            if (input$fix_cp_option == 'set_cp_bounds') {

                user_cp_limits <- c(input$cp_lower_limit, input$cp_upper_limit)

            }

            if (input$fix_tm_option == 'set_tm_bounds') {

                user_tm_limits <- c(input$tm_lower_limit, input$tm_upper_limit)

            }

            if (input$fix_dh_option == 'set_dh_bounds') {

                user_dh_limits <- c(input$dh_lower_limit, input$dh_upper_limit)

            }

          # If cp value is given, it will be used as fixed parameter
          # The bounds are ignored in this case
          if (input$fix_cp_option == 'fix_cp') cp_value <- input$cp_value

        }

        pySample$fit_thermal_unfolding_global(
            cp_limits = user_cp_limits,
            dh_limits = user_dh_limits,
            tm_limits = user_tm_limits,
            cp_value = cp_value
        )

        if (input$unfolding_model %in% c("global-global-local", "global-global-global")) {

            pySample$set_signal_id()

            result <- tryCatch(
                {

                    pySample$fit_thermal_unfolding_global_global()

                }, error = function(e) {
                    if (inherits(e, "python.builtin.RuntimeError")) {
                    err <- py_last_error()
                    popUpWarning(
                        paste0("⚠ Fitting error: ", err$value)
                    )
                    return('Error')
                    } else {
                    stop(e) # rethrow non-Python errors
                    }
                }
            )

            if (!is.null(result)) return(NULL)

        }


        if (input$unfolding_model == "global-global-global") {
            popUpInfo('The fitting with global slopes and local intercepts is finished.
            Now the data will be fitted with global slopes and global intercepts. Please wait...')

            result <- tryCatch(
                {

                    pySample$fit_thermal_unfolding_global_global_global(
                      model_scale_factor = input$fit_scale_factor
                    )

                }, error = function(e) {
                    if (inherits(e, "python.builtin.RuntimeError")) {
                    err <- py_last_error()
                    popUpWarning(paste0("⚠ Fitting error: ", err$value))
                    return('Error')
                    } else {
                    stop(e) # rethrow non-Python errors
                    }
                }
            )

            if (!is.null(result)) return(NULL)

            if (input$fit_scale_factor) {

                tab_panel_to_add <- tabPanel(
                    "Fitted signal (rescaled)",
                    withSpinner(plotlyOutput("fitted_signal_rescaled"))
                )

                appendTab('tabset_fit',tab_panel_to_add)

                reactives$scaled_tab_shown <- TRUE

                df_scaled <- pySample$signal_to_df(scaled = TRUE)
                reactives$signal_df_scaled <- df_scaled

                fitted_df_scaled <- pySample$signal_to_df(signal_type = "fitted",scaled = TRUE)
                reactives$signal_df_fitted_scaled <- fitted_df_scaled

            }

        }

        # We create the dataset again, in case a subset of data was used
        if (input$fit_subset) {
            reactives$signal_df <- pySample$signal_to_df()
        }

        fitted_df <- pySample$signal_to_df(signal_type = "fitted")

        reactives$signal_df_fitted <- fitted_df

        # Include the fitted parameters into a Table
        fitted_parameters <- pySample$params_df

        output$fitted_params <- renderTable({
            fitted_parameters
        },digits=4)

        popUpSuccess('✅ Fitting completed!')

    })

})