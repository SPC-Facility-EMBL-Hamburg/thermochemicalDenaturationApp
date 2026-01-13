reset_signal_df <- function(){
    reactives$signal_df     <- NULL
    reactives$derivative_df <- NULL
    return(NULL)
}

generate_signal_df <- function(){

    reactives$signal_df     <- pySample$signal_to_df()
    reactives$derivative_df <- pySample$signal_to_df(signal_type = "derivative")

    return(NULL)
}

observeEvent(input$dsf_input_files, {

    reactives$update_plots <- NULL

    reset_signal_df()

    files_path <- input$dsf_input_files$datapath
    read_file_status <- pySample$read_multiple_files(files_path)

    if (!read_file_status) {
        popUpWarning("Error reading one or more files. Please check that all files share the same signal type (e.g., 350nm)")
    }

    logbook_txt <- paste0("Files imported: ",paste(input$dsf_input_files$name, collapse = ", "))

    write_logbook(logbook_txt,include_time = TRUE)

    updateSelectInput(session, "which",choices  = pySample$signals)

    pySample$set_signal(pySample$signals[1])

    # Find max and min temperature
    min_temp <- floor(pySample$global_min_temp) - 5
    max_temp <- ceiling(pySample$global_max_temp) + 5

    # Update the slider range
    updateSliderInput(session, "sg_range", min = min_temp, max = max_temp,value = c(min_temp, max_temp))

    conditions  <- unlist(pySample$conditions)

    n_conditions <- length(conditions)

    if (n_conditions > 24 * 4) {

        reactives$n_rows_conditions_table <- ifelse(n_conditions <= 192,48,96)

    }

    tables <- get_renderRHandsontable_list_filled(conditions,reactives$n_rows_conditions_table,pySample$labels)

    output$table4 <- tables[[4]]
    output$table3 <- tables[[3]]
    output$table2 <- tables[[2]]
    output$table1 <- tables[[1]]

    pySample$set_denaturant_concentrations()
    pySample$select_conditions(normalise_to_global_max=input$rescale)
    pySample$estimate_derivative()
    pySample$guess_Tm()

    generate_signal_df()

    Sys.sleep(0.5)
    reactives$update_plots <- TRUE
    reactives$signal_df_fitted <- NULL
    reactives$find_initial_params <- TRUE
})


observeEvent(list(input$which,input$rescale,input$sg_range), {

    req(reactives$update_plots)
    req(input$table1)
    reactives$update_plots <- NULL
    pySample$set_signal(input$which)

    logbook_txt <- paste0("Temperature range set to: ",paste(input$sg_range[1], input$sg_range[2], sep = " - "))
    write_logbook(logbook_txt,include_time = FALSE)

    include_and_conc_vectors <- get_include_and_conc_vectors(
        input$table1,input$table2,input$table3,input$table4,
        pySample$conditions,reactives$n_rows_conditions_table)

    pySample$select_conditions(include_and_conc_vectors$include_vector,normalise_to_global_max=input$rescale)
    pySample$set_temperature_range(input$sg_range[1], input$sg_range[2])

    logbook_txt <- paste0("Temperature range set to: ",paste(input$sg_range[1], input$sg_range[2], sep = " - "))
    write_logbook(logbook_txt,include_time = FALSE)

    pySample$estimate_derivative()
    pySample$guess_Tm()

    pySample$reset_fittings_results()

    generate_signal_df()
    reactives$update_plots <- TRUE
    reactives$signal_df_fitted <- NULL
    reactives$find_initial_params <- TRUE

})

# Autocomplete the concentration versus position table
observe({

    req(input$table1)
    if (input$fill_table) {

        conditions <- c(pySample$conditions)
        labels <- c(pySample$labels)

        tables <- get_renderRHandsontable_list_autofill(
            conditions,labels,reactives$n_rows_conditions_table,
            input$initial_denaturant,input$n_replicates,
            input$dil_factor,input$rev_order)

        output$table4 <- tables[[4]]
        output$table3 <- tables[[3]]
        output$table2 <- tables[[2]]
        output$table1 <- tables[[1]]
  }

})

# Update the concentrations of the pySample object, if the user changes the tables
observeEvent(list(input$table1,input$table2,input$table3,input$table4), {

    req(input$table1)

    include_and_conc_vectors <- get_include_and_conc_vectors(
        input$table1,input$table2,input$table3,input$table4,
        pySample$conditions,reactives$n_rows_conditions_table)

    n_conditions <- sum(include_and_conc_vectors$include_vector)>0

    reactives$update_plots <- NULL

    req(n_conditions > 0) # Need at least one conditions to plot

    pySample$set_denaturant_concentrations(include_and_conc_vectors$concentration_vector)

    logbook_txt <- paste0("Denaturant concentrations set to: ",paste(include_and_conc_vectors$concentration_vector, collapse = ", "))
    write_logbook(logbook_txt,include_time = FALSE)

    pySample$select_conditions(include_and_conc_vectors$include_vector,normalise_to_global_max=input$rescale)
    pySample$estimate_derivative()
    pySample$guess_Tm()

    pySample$reset_fittings_results()

    generate_signal_df()

    reactives$update_plots <- TRUE
    reactives$signal_df_fitted <- NULL
    reactives$find_initial_params <- TRUE

})