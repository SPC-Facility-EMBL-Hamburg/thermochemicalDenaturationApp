observeEvent(input$n_residues,{

    reactives$cp_value <- input$n_residues * 0.0148 - 0.1267

})


observeEvent(input$btn_call_fit,{

    req(input$table1)
    req(reactives$signal_df)

    unfolding_model <- input$unfolding_model

    fixed_cp <- input$fix_cp_option == 'fix_cp'

    reactives$fitting_done <- FALSE

    write_logbook( "Fitting process started")

    native_baseline_type <- input$native_dependence
    unfolded_baseline_type <- input$unfolded_dependence

    write_logbook(paste0("Baseline window (native): ",input$baseline_window_native))
    write_logbook(paste0("Baseline window (unfolded): ",input$baseline_window_unfolded))

    # print baseline type if the compare options is not selected
    if (unfolding_model != "compare-many-models") {
        write_logbook(paste0("Native baseline dependence set to: ",native_baseline_type))
        write_logbook(paste0("Unfolded baseline dependence set to: ",unfolded_baseline_type))
    }

    write_logbook(paste0("Number of residues set to: ",input$n_residues))

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

    logbook_txt <- paste0("Maximum points per curve set to: ", ifelse(is.null(max_points), "All points", max_points))
    write_logbook(logbook_txt,include_time = FALSE)

    pySample$reset_fittings_results()

    c1 <- unfolding_model == "global-global-global"
    # Return an error if the user selected the global global global model but wants to use the Ratio signal
    if (c1 & "Ratio" %in% pySample$signal_names) {
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

        # if tab panel for confidence intervals is present delete it first
        if (reactives$conf_interval_calculated) {
            removeTab('tabset_fit',target = "Confidence intervals")
            reactives$conf_interval_calculated <- FALSE
        }

        # Select the popup message based on the model selected
        if (unfolding_model != "compare-many-models") {
            pop_msg <- 'Fitting started. The plot below will be updated when the fitting is finished. Please wait some minutes...'
            popUpInfo(pop_msg)
            write_logbook(paste0("Fitting model selected: ",unfolding_model))

        }

        pySample$n_residues <- input$n_residues

        if (reactives$find_initial_params) {
        # Use a simple model to guess good initial thermodynamic parameters

              pySample$guess_initial_parameters(
              native_baseline_type     = 'linear',
              unfolded_baseline_type   = 'linear',
              window_range_native      = input$baseline_window_native,
              window_range_unfolded    = input$baseline_window_unfolded
              )

            reactives$find_initial_params <- FALSE
        }

        user_cp_limits <- NULL
        user_dh_limits <- NULL
        user_tm_limits <- NULL
        cp_value <- NULL

        bounds_options_open <- !is.null(input$fix_cp_option)

        if (bounds_options_open) {

            if (input$fix_cp_option == 'set_cp_bounds') {

                user_cp_limits <- c(input$cp_lower_limit, input$cp_upper_limit)
                write_logbook(paste0("User-defined Cp limits: ",paste(user_cp_limits,collapse = " - ")))

            }

            if (input$fix_tm_option == 'set_tm_bounds') {

                user_tm_limits <- c(input$tm_lower_limit, input$tm_upper_limit)
                write_logbook(paste0("User-defined Tm limits: ",paste(user_tm_limits,collapse = " - ")))

            }

            if (input$fix_dh_option == 'set_dh_bounds') {

                user_dh_limits <- c(input$dh_lower_limit, input$dh_upper_limit)
                write_logbook(paste0("User-defined ΔH limits: ",paste(user_dh_limits,collapse = " - ")))

            }

          # If cp value is given, it will be used as fixed parameter
          # The bounds are ignored in this case
          if (fixed_cp) {
            
              cp_value <- input$cp_value
              write_logbook(paste0("Cp fixed to user-defined value: ",cp_value))

          }

        }

        reactives$user_cp_limits <- user_cp_limits
        reactives$user_dh_limits <- user_dh_limits
        reactives$user_tm_limits <- user_tm_limits
        reactives$cp_value <- cp_value

        # Compare models if the user selected the option to compare models
        if (unfolding_model == "compare-many-models") {

            # Create a modal dialog to ask for the types of baselines to fit
            # Three select inout with multiple selection allowed: one for the model and two for the baselines
            # constant, linear, quadratic, exponential
            showModal(modalDialog(
                title = "Model comparison",
                # Small text to explain the user what to do

                # Checkbox input if slopes are shared or not across conditions
                column(12,
                # tooltip to explain the user what global slopes means
                p(
                    HTML("<b>Compare models with global slopes (recommended)</b>"),
                    span(shiny::icon("info-circle"), id = "info_compare_global_slopes"),
                    checkboxInput("compare_global_slopes", label=NULL, value = TRUE),

                    tippy::tippy_this(
                    elementId = "info_compare_global_slopes",
                    tooltip = "If enabled, the models with global slopes will be included in the comparison.
                    The models with global slopes are recommended because they are more robust and provide more accurate thermodynamic parameters. 
                    However, if the unfolding curves have very different shapes across conditions, the models with global slopes might not fit well and the user might want to compare them with models with local slopes.
                    As a guide, to analyse datasets obtained under the same experimental conditions (e.g., same protein concentrations, same instrument, same laser power), the models with global slopes should be preferred. 
                    If the dataset includes unfolding curves obtained under different conditions (e.g., different instruments, different instrument setup), the models with local slopes might fit better.",
                    placement = "right")

                )),

                selectInput("native_baselines_to_compare", "Select one or two native baselines to compare:",
                            choices = c("constant", "linear", "quadratic", "exponential"),
                            selected = NULL,
                            multiple = TRUE),
                selectInput("unfolded_baselines_to_compare", "Select one or two unfolded baselines to compare:",
                            choices = c("constant", "linear", "quadratic", "exponential"),
                            selected = NULL,
                            multiple = TRUE),
                easyClose = FALSE,
                footer = tagList(
                    modalButton("Cancel"),
                    actionButton("confirm_model_comparison", "Confirm")
                )
            ))

            return(NULL)
        }

        pySample$estimate_baseline_parameters(
            native_baseline_type    = native_baseline_type,
            unfolded_baseline_type  = unfolded_baseline_type,
            window_range_native     = input$baseline_window_native,
            window_range_unfolded   = input$baseline_window_unfolded
        )

        pySample$fit_thermal_unfolding_global(
            cp_limits = user_cp_limits,
            dh_limits = user_dh_limits,
            tm_limits = user_tm_limits,
            cp_value = cp_value
        )


        condition <- unfolding_model %in% c("global-global-local", "global-global-global")

        if (condition) {

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

        condition <- unfolding_model == "global-global-global"
        if (condition) {
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

                write_logbook(paste0("Scale factor fitting activated."))

                tab_panel_to_add <- tabPanel(
                    "Fitted signal (rescaled)",
                    withSpinner(plotlyOutput("fitted_signal_rescaled"))
                )

                appendTab('tabset_fit',tab_panel_to_add)

                reactives$scaled_tab_shown <- TRUE

                df_scaled <- pySample$signal_to_df(scaled = TRUE)
                df_scaled <- pandas_to_r(df_scaled)

                reactives$signal_df_scaled <- df_scaled

                fitted_df_scaled <- pySample$signal_to_df(signal_type = "fitted",scaled = TRUE)
                fitted_df_scaled <- pandas_to_r(fitted_df_scaled)

                reactives$signal_df_fitted_scaled <- fitted_df_scaled

            }

        }

        # We create the dataset again, in case a subset of data was used
        if (input$fit_subset) {
            signal_df <- pySample$signal_to_df()
            signal_df <- pandas_to_r(signal_df)
            reactives$signal_df <- signal_df
        }

        fitted_df <- pySample$signal_to_df(signal_type = "fitted")
        fitted_df <- pandas_to_r(fitted_df)

        reactives$signal_df_fitted <- fitted_df

        # Include the fitted parameters into a Table
        fitted_parameters <- pySample$params_df
        fitted_parameters <- pandas_to_r(fitted_parameters)

        output$fitted_params <- renderTable({
            fitted_parameters
        },digits=4)

       dg_df <- pySample$dg_df
       dg_df <- pandas_to_r(dg_df)
       reactives$dg_df <- dg_df

        reactives$fitting_done <- TRUE
        popUpSuccess('✅ Fitting completed!')

        pySample$create_fit_report()
        report_string <- pySample$fit_report
        report_string <- highlight_cp_line(report_string)

        output$fitReport <- renderUI({
            HTML(
                paste0(
                "<pre style='font-size:13px;'>",
                report_string,
                "</pre>"
                )
            )
        })

        write_logbook("Fitting completed successfully.")

    })

})

observeEvent(input$btn_cal_conf_interval,{

    withBusyIndicatorServer("Go2",{

        # if tab panel for confidence intervals is present delete it first
        if (reactives$conf_interval_calculated) {
            removeTab('tabset_fit',target = "Confidence intervals")
            reactives$conf_interval_calculated <- FALSE
        }

        popUpInfo('Calculating asymmetric confidence intervals. 
        Please wait some minutes...')

        write_logbook("Calculating asymmetric confidence intervals.")
        pySample$calculate_confidence_intervals()

        conf_int_df <- pySample$ci_df
        conf_int_df <- pandas_to_r(conf_int_df)

        reactives$conf_interval_calculated <- TRUE

        # append a tab panel with the confidence intervals table
        tab_panel_to_add <- tabPanel(
            "Confidence intervals",
            withSpinner(tableOutput("conf_int_table"))
        )

        appendTab('tabset_fit',tab_panel_to_add)
        output$conf_int_table <- renderTable({
            conf_int_df
        })
        
        popUpSuccess('✅ Confidence intervals calculated successfully!')

    })

})

observeEvent(input$confirm_model_comparison,{

    removeModal()

    native_baselines_to_compare <- input$native_baselines_to_compare
    unfolded_baselines_to_compare <- input$unfolded_baselines_to_compare

    if (is.null(native_baselines_to_compare) || is.null(unfolded_baselines_to_compare)) {
        popUpWarning("⚠ Please select at least one native baseline and one unfolded baseline to compare.")
        return(NULL)
     }

    write_logbook(paste0("User selected the following native baselines to compare: ", paste(native_baselines_to_compare, collapse = ", ")))
    write_logbook(paste0("User selected the following unfolded baselines to compare: ", paste(unfolded_baselines_to_compare, collapse = ", ")))

    native_baselines_to_compare <- as.list(native_baselines_to_compare)
    unfolded_baselines_to_compare <- as.list(unfolded_baselines_to_compare)

    # Verify that they have at most 5 options in total otherwise the comparison will take too long and might not be useful
    if (length(native_baselines_to_compare) + length(unfolded_baselines_to_compare) > 5) {
        popUpWarning("⚠ Please select at most five options in total for the native baselines and unfolded baselines to compare to avoid an excessively long fitting time.")
        return(NULL)
    }

    global_model_types <- if (input$compare_global_slopes) {
        list("global_global", "global_global_global")
    } else {
        list("global")
    }

    withBusyIndicatorServer("Go",{

        pySample$compare_models(
            native_baseline_types = native_baselines_to_compare,
            unfolded_baseline_types = unfolded_baselines_to_compare,
            global_model_types=global_model_types,
            cp_limits = reactives$user_cp_limits,
            dh_limits = reactives$user_dh_limits,
            tm_limits = reactives$user_tm_limits,
            cp_value = reactives$cp_value
        )

    })

    # If present remove the model comparison Table before adding the new one
    if (reactives$comparison_table_shown) {
        removeTab('tabset_fit',target = "Model comparison")
        reactives$comparison_table_shown <- FALSE
    }

    # Print results
    model_comparison_df <- pySample$comparison_df
    model_comparison_df <- pandas_to_r(model_comparison_df)

    # We need duplicates, one for the modal and another for the tab
    output$model_comparison_table <- renderTable({
        model_comparison_df
    },digits=2)

    output$model_comparison_table_2 <- renderTable({
        model_comparison_df
    },digits=3)

    # Show the model comparison results in a modal dialog
    showModal(modalDialog(
        title = "Model comparison results",
        div(style = "overflow-x: auto;",
            withSpinner(tableOutput("model_comparison_table"))
        ),
        easyClose = TRUE,
        footer = modalButton("Close"),
        size = "l"
    ))

    # Append also the model comparison tab to the Tabpanel
    tab_panel_to_add <- tabPanel(
        "Model comparison",
        withSpinner(tableOutput("model_comparison_table_2"))
    )

    appendTab('tabset_fit',tab_panel_to_add)

    reactives$comparison_table_shown <- TRUE

    # Plot and show the stats of the best model according to the EBIC criteria
    compare_py_fit_objects <- pySample$fit_objects
    best_py_fit_obj <- pySample$fit_objects[[1]]

    if (input$fit_subset) {
        signal_df <- best_py_fit_obj$signal_to_df()
        signal_df <- pandas_to_r(signal_df)
        reactives$signal_df <- signal_df
    }

    fitted_df <- best_py_fit_obj$signal_to_df(signal_type = "fitted")
    fitted_df <- pandas_to_r(fitted_df)

    reactives$signal_df_fitted <- fitted_df

    # Include the fitted parameters into a Table
    fitted_parameters <- best_py_fit_obj$params_df
    fitted_parameters <- pandas_to_r(fitted_parameters)

    output$fitted_params <- renderTable({
        fitted_parameters
    },digits=4)
        
    dg_df <- best_py_fit_obj$dg_df
    dg_df <- pandas_to_r(dg_df)
    reactives$dg_df <- dg_df

    reactives$fitting_done <- TRUE

    best_py_fit_obj$create_fit_report()
    report_string <- best_py_fit_obj$fit_report
    report_string <- highlight_cp_line(report_string)

    output$fitReport <- renderUI({
        HTML(
            paste0(
            "<pre style='font-size:13px;'>",
            report_string,
            "</pre>"
            )
        )
    })
    
    # Find if the best model is with a rescale and plot it
    selected_model <- model_comparison_df[1,3]
    
    if (grepl("global intercepts", selected_model)) {
    
        tab_panel_to_add <- tabPanel(
            "Fitted signal (rescaled)",
            withSpinner(plotlyOutput("fitted_signal_rescaled"))
        )

        appendTab('tabset_fit',tab_panel_to_add)

        reactives$scaled_tab_shown <- TRUE

        df_scaled <- best_py_fit_obj$signal_to_df(scaled = TRUE)
        df_scaled <- pandas_to_r(df_scaled)

        reactives$signal_df_scaled <- df_scaled

        fitted_df_scaled <- best_py_fit_obj$signal_to_df(signal_type = "fitted",scaled = TRUE)
        fitted_df_scaled <- pandas_to_r(fitted_df_scaled)

        reactives$signal_df_fitted_scaled <- fitted_df_scaled

    }

    # End of - Plot and show the stats of the best model according to the EBIC criteria

    # Set pySample to be the best fit object - so the user can compute confidence intervals
    
    # Caution - we are changing the global pySample object to be the best fit model
    pySample <<- best_py_fit_obj   

})