# ObserveEvents for plot configuration (syncs both regular and fit inputs)
observeEvent(input$plot_width, {
    if (!isTRUE(all.equal(reactives$plot_width_val, input$plot_width))) {
        reactives$plot_width_val <- input$plot_width
        updateNumericInput(session, "plot_width_fit", value = input$plot_width)
    }
}, ignoreInit = TRUE)

observeEvent(input$plot_width_fit, {
    if (!isTRUE(all.equal(reactives$plot_width_val, input$plot_width_fit))) {
        reactives$plot_width_val <- input$plot_width_fit
        updateNumericInput(session, "plot_width", value = input$plot_width_fit)
    }
}, ignoreInit = TRUE)

observeEvent(input$plot_height, {
    if (!isTRUE(all.equal(reactives$plot_height_val, input$plot_height))) {
        reactives$plot_height_val <- input$plot_height
        updateNumericInput(session, "plot_height_fit", value = input$plot_height)
    }
}, ignoreInit = TRUE)

observeEvent(input$plot_height_fit, {
    if (!isTRUE(all.equal(reactives$plot_height_val, input$plot_height_fit))) {
        reactives$plot_height_val <- input$plot_height_fit
        updateNumericInput(session, "plot_height", value = input$plot_height_fit)
    }
}, ignoreInit = TRUE)

observeEvent(input$plot_type, {
    if (!identical(reactives$plot_type_val, input$plot_type)) {
        reactives$plot_type_val <- input$plot_type
        updateSelectInput(session, "plot_type_fit", selected = input$plot_type)
    }
}, ignoreInit = TRUE)

observeEvent(input$plot_type_fit, {
    if (!identical(reactives$plot_type_val, input$plot_type_fit)) {
        reactives$plot_type_val <- input$plot_type_fit
        updateSelectInput(session, "plot_type", selected = input$plot_type_fit)
    }
}, ignoreInit = TRUE)

observeEvent(input$plot_axis_size, {
    if (!isTRUE(all.equal(reactives$plot_axis_size_val, input$plot_axis_size))) {
        reactives$plot_axis_size_val <- input$plot_axis_size
        updateNumericInput(session, "plot_axis_size_fit", value = input$plot_axis_size)
    }
}, ignoreInit = TRUE)

observeEvent(input$plot_axis_size_fit, {
    if (!isTRUE(all.equal(reactives$plot_axis_size_val, input$plot_axis_size_fit))) {
        reactives$plot_axis_size_val <- input$plot_axis_size_fit
        updateNumericInput(session, "plot_axis_size", value = input$plot_axis_size_fit)
    }
}, ignoreInit = TRUE)

observeEvent(input$x_legend_pos, {
    if (!isTRUE(all.equal(reactives$x_legend_pos_val, input$x_legend_pos))) {
        reactives$x_legend_pos_val <- input$x_legend_pos
        updateNumericInput(session, "x_legend_pos_fit", value = input$x_legend_pos)
    }
}, ignoreInit = TRUE)

observeEvent(input$x_legend_pos_fit, {
    if (!isTRUE(all.equal(reactives$x_legend_pos_val, input$x_legend_pos_fit))) {
        reactives$x_legend_pos_val <- input$x_legend_pos_fit
        updateNumericInput(session, "x_legend_pos", value = input$x_legend_pos_fit)
    }
}, ignoreInit = TRUE)

observeEvent(input$y_legend_pos, {
    if (!isTRUE(all.equal(reactives$y_legend_pos_val, input$y_legend_pos))) {
        reactives$y_legend_pos_val <- input$y_legend_pos
        updateNumericInput(session, "y_legend_pos_fit", value = input$y_legend_pos)
    }
}, ignoreInit = TRUE)

observeEvent(input$y_legend_pos_fit, {
    if (!isTRUE(all.equal(reactives$y_legend_pos_val, input$y_legend_pos_fit))) {
        reactives$y_legend_pos_val <- input$y_legend_pos_fit
        updateNumericInput(session, "y_legend_pos", value = input$y_legend_pos_fit)
    }
}, ignoreInit = TRUE)

observeEvent(input$color_bar_length, {
    if (!isTRUE(all.equal(reactives$color_bar_length_val, input$color_bar_length))) {
        reactives$color_bar_length_val <- input$color_bar_length
        updateNumericInput(session, "color_bar_length_fit", value = input$color_bar_length)
    }
}, ignoreInit = TRUE)

observeEvent(input$color_bar_length_fit, {
    if (!isTRUE(all.equal(reactives$color_bar_length_val, input$color_bar_length_fit))) {
        reactives$color_bar_length_val <- input$color_bar_length_fit
        updateNumericInput(session, "color_bar_length", value = input$color_bar_length_fit)
    }
}, ignoreInit = TRUE)

observeEvent(input$color_bar_orientation, {
    if (!identical(reactives$color_bar_orientation_val, input$color_bar_orientation)) {
        reactives$color_bar_orientation_val <- input$color_bar_orientation
        updateSelectInput(session, "color_bar_orientation_fit", selected = input$color_bar_orientation)
    }
}, ignoreInit = TRUE)

observeEvent(input$color_bar_orientation_fit, {
    if (!identical(reactives$color_bar_orientation_val, input$color_bar_orientation_fit)) {
        reactives$color_bar_orientation_val <- input$color_bar_orientation_fit
        updateSelectInput(session, "color_bar_orientation", selected = input$color_bar_orientation_fit)
    }
}, ignoreInit = TRUE)

observeEvent(input$show_colorbar, {
    if (!identical(reactives$show_colorbar_val, input$show_colorbar)) {
        reactives$show_colorbar_val <- input$show_colorbar
        updateCheckboxInput(session, "show_colorbar_fit", value = input$show_colorbar)
    }
}, ignoreInit = TRUE)

observeEvent(input$show_colorbar_fit, {
    if (!identical(reactives$show_colorbar_val, input$show_colorbar_fit)) {
        reactives$show_colorbar_val <- input$show_colorbar_fit
        updateCheckboxInput(session, "show_colorbar", value = input$show_colorbar_fit)
    }
}, ignoreInit = TRUE)

observeEvent(input$show_grid_x, {
    if (!identical(reactives$show_grid_x_val, input$show_grid_x)) {
        reactives$show_grid_x_val <- input$show_grid_x
        updateCheckboxInput(session, "show_grid_x_fit", value = input$show_grid_x)
    }
}, ignoreInit = TRUE)

observeEvent(input$show_grid_x_fit, {
    if (!identical(reactives$show_grid_x_val, input$show_grid_x_fit)) {
        reactives$show_grid_x_val <- input$show_grid_x_fit
        updateCheckboxInput(session, "show_grid_x", value = input$show_grid_x_fit)
    }
}, ignoreInit = TRUE)

observeEvent(input$show_grid_y, {
    if (!identical(reactives$show_grid_y_val, input$show_grid_y)) {
        reactives$show_grid_y_val <- input$show_grid_y
        updateCheckboxInput(session, "show_grid_y_fit", value = input$show_grid_y)
    }
}, ignoreInit = TRUE)

observeEvent(input$show_grid_y_fit, {
    if (!identical(reactives$show_grid_y_val, input$show_grid_y_fit)) {
        reactives$show_grid_y_val <- input$show_grid_y_fit
        updateCheckboxInput(session, "show_grid_y", value = input$show_grid_y_fit)
    }
}, ignoreInit = TRUE)

observeEvent(input$plot_marker_size, {
    if (!isTRUE(all.equal(reactives$plot_marker_size_val, input$plot_marker_size))) {
        reactives$plot_marker_size_val <- input$plot_marker_size
        updateNumericInput(session, "plot_marker_size_fit", value = input$plot_marker_size)
    }
}, ignoreInit = TRUE)

observeEvent(input$plot_marker_size_fit, {
    if (!isTRUE(all.equal(reactives$plot_marker_size_val, input$plot_marker_size_fit))) {
        reactives$plot_marker_size_val <- input$plot_marker_size_fit
        updateNumericInput(session, "plot_marker_size", value = input$plot_marker_size_fit)
    }
}, ignoreInit = TRUE)

observeEvent(input$plot_line_width, {
    if (!isTRUE(all.equal(reactives$plot_line_width_val, input$plot_line_width))) {
        reactives$plot_line_width_val <- input$plot_line_width
        updateNumericInput(session, "plot_line_width_fit", value = input$plot_line_width)
    }
}, ignoreInit = TRUE)

observeEvent(input$plot_line_width_fit, {
    if (!isTRUE(all.equal(reactives$plot_line_width_val, input$plot_line_width_fit))) {
        reactives$plot_line_width_val <- input$plot_line_width_fit
        updateNumericInput(session, "plot_line_width", value = input$plot_line_width_fit)
    }
}, ignoreInit = TRUE)

observeEvent(input$max_points, {
    if (!isTRUE(all.equal(reactives$max_points_val, input$max_points))) {
        reactives$max_points_val <- input$max_points
        updateNumericInput(session, "max_points_fit", value = input$max_points)
    }
}, ignoreInit = TRUE)

observeEvent(input$max_points_fit, {
    if (!isTRUE(all.equal(reactives$max_points_val, input$max_points_fit))) {
        reactives$max_points_val <- input$max_points_fit
        updateNumericInput(session, "max_points", value = input$max_points_fit)
    }
}, ignoreInit = TRUE)

observeEvent(input$n_xticks, {
    if (!isTRUE(all.equal(reactives$n_xticks_val, input$n_xticks))) {
        reactives$n_xticks_val <- input$n_xticks
        updateNumericInput(session, "n_xticks_fit", value = input$n_xticks)
    }
}, ignoreInit = TRUE)

observeEvent(input$n_xticks_fit, {
    if (!isTRUE(all.equal(reactives$n_xticks_val, input$n_xticks_fit))) {
        reactives$n_xticks_val <- input$n_xticks_fit
        updateNumericInput(session, "n_xticks", value = input$n_xticks_fit)
    }
}, ignoreInit = TRUE)

observeEvent(input$n_yticks, {
    if (!isTRUE(all.equal(reactives$n_yticks_val, input$n_yticks))) {
        reactives$n_yticks_val <- input$n_yticks
        updateNumericInput(session, "n_yticks_fit", value = input$n_yticks)
    }
}, ignoreInit = TRUE)

observeEvent(input$n_yticks_fit, {
    if (!isTRUE(all.equal(reactives$n_yticks_val, input$n_yticks_fit))) {
        reactives$n_yticks_val <- input$n_yticks_fit
        updateNumericInput(session, "n_yticks", value = input$n_yticks_fit)
    }
}, ignoreInit = TRUE)

observeEvent(input$tick_length, {
    if (!isTRUE(all.equal(reactives$tick_length_val, input$tick_length))) {
        reactives$tick_length_val <- input$tick_length
        updateNumericInput(session, "tick_length_fit", value = input$tick_length)
    }
}, ignoreInit = TRUE)

observeEvent(input$tick_length_fit, {
    if (!isTRUE(all.equal(reactives$tick_length_val, input$tick_length_fit))) {
        reactives$tick_length_val <- input$tick_length_fit
        updateNumericInput(session, "tick_length", value = input$tick_length_fit)
    }
}, ignoreInit = TRUE)

observeEvent(input$tick_width, {
    if (!isTRUE(all.equal(reactives$tick_width_val, input$tick_width))) {
        reactives$tick_width_val <- input$tick_width
        updateNumericInput(session, "tick_width_fit", value = input$tick_width)
    }
}, ignoreInit = TRUE)

observeEvent(input$tick_width_fit, {
    if (!isTRUE(all.equal(reactives$tick_width_val, input$tick_width_fit))) {
        reactives$tick_width_val <- input$tick_width_fit
        updateNumericInput(session, "tick_width", value = input$tick_width_fit)
    }
}, ignoreInit = TRUE)

# Render signal plot
output$signal <- renderPlotly({
  
    req(input$table1)
    req(reactives$update_plots)
    req(reactives$signal_df)

    fig <- plot_fluo_signal(
        signal_df               = reactives$signal_df,
        plot_width              = reactives$plot_width_val,
        plot_height             = reactives$plot_height_val,
        plot_type               = reactives$plot_type_val,
        axis_size               = reactives$plot_axis_size_val,
        unfolding_fitted_data   = NULL,
        x_legend_pos            = reactives$x_legend_pos_val,
        y_legend_pos            = reactives$y_legend_pos_val,
        color_bar_length        = reactives$color_bar_length_val,
        color_bar_orientation   = reactives$color_bar_orientation_val,
        show_colorbar           = reactives$show_colorbar_val,
        show_grid_x             = reactives$show_grid_x_val,
        show_grid_y             = reactives$show_grid_y_val,
        y_axis_label            = input$which,
        marker_size             = reactives$plot_marker_size_val,
        line_width              = reactives$plot_line_width_val,
        max_points              = reactives$max_points_val,
        n_xticks                = reactives$n_xticks_val,
        n_yticks                = reactives$n_yticks_val,
        tick_length             = reactives$tick_length_val,
        tick_width              = reactives$tick_width_val
        )
  
    return(fig)
})

output$first_der <- renderPlotly({

    req(input$table1)
    req(reactives$update_plots)
    req(reactives$derivative_df)

    fig <- plot_fluo_signal(
        signal_df               = reactives$derivative_df,
        plot_width              = reactives$plot_width_val,
        plot_height             = reactives$plot_height_val,
        plot_type               = reactives$plot_type_val,
        axis_size               = reactives$plot_axis_size_val,
        unfolding_fitted_data   = NULL,
        x_legend_pos            = reactives$x_legend_pos_val,
        y_legend_pos            = reactives$y_legend_pos_val,
        color_bar_length        = reactives$color_bar_length_val,
        color_bar_orientation   = reactives$color_bar_orientation_val,
        show_colorbar           = reactives$show_colorbar_val,
        show_grid_x             = reactives$show_grid_x_val,
        show_grid_y             = reactives$show_grid_y_val,
        y_axis_label            = input$which,
        marker_size             = NULL,
        line_width              = reactives$plot_line_width_val,
        max_points              = reactives$max_points_val,
        n_xticks                = reactives$n_xticks_val,
        n_yticks                = reactives$n_yticks_val,
        tick_length             = reactives$tick_length_val,
        tick_width              = reactives$tick_width_val,
        derivative              = TRUE
        )

    return(fig)
})

output$tm_vs_den <- renderPlotly({

    req(input$table1)
    req(reactives$update_plots)

    df = pySample$t_melting_df_multiple[[1]]

    fig <- plot_2d(
        df                      = df,
        plot_width              = reactives$plot_width_val,
        plot_height             = reactives$plot_height_val,
        plot_type               = reactives$plot_type_val,
        axis_size               = reactives$plot_axis_size_val,
        show_grid_x             = reactives$show_grid_x_val,
        show_grid_y             = reactives$show_grid_y_val,
        marker_size             = reactives$plot_marker_size_val,
        n_xticks                = reactives$n_xticks_val,
        n_yticks                = reactives$n_yticks_val,
        tick_length             = reactives$tick_length_val,
        tick_width              = reactives$tick_width_val
    )

    return(fig)
})

output$initial_fluo_vs_den <- renderPlotly({

    req(input$table1)
    req(reactives$update_plots)
    req(reactives$signal_df)

    fig <- plot_initial_signal_versus_denaturant(
        signal_df               = reactives$signal_df,
        plot_width              = reactives$plot_width_val,
        plot_height             = reactives$plot_height_val,
        plot_type               = reactives$plot_type_val,
        axis_size               = reactives$plot_axis_size_val,
        show_grid_x             = reactives$show_grid_x_val,
        show_grid_y             = reactives$show_grid_y_val,
        marker_size             = reactives$plot_marker_size_val,
        n_xticks                = reactives$n_xticks_val,
        n_yticks                = reactives$n_yticks_val,
        tick_length             = reactives$tick_length_val,
        tick_width              = reactives$tick_width_val
    )

    return(fig)
})

# Render signal plot
output$fitted_signal <- renderPlotly({

    req(input$table1)
    req(reactives$update_plots)
    req(reactives$signal_df)
    req(reactives$signal_df_fitted)

    fig <- plot_fluo_signal(
        signal_df               = reactives$signal_df,
        plot_width              = reactives$plot_width_val,
        plot_height             = reactives$plot_height_val,
        plot_type               = reactives$plot_type_val,
        axis_size               = reactives$plot_axis_size_val,
        unfolding_fitted_data   = reactives$signal_df_fitted,
        x_legend_pos            = reactives$x_legend_pos_val,
        y_legend_pos            = reactives$y_legend_pos_val,
        color_bar_length        = reactives$color_bar_length_val,
        color_bar_orientation   = reactives$color_bar_orientation_val,
        show_colorbar           = reactives$show_colorbar_val,
        show_grid_x             = reactives$show_grid_x_val,
        show_grid_y             = reactives$show_grid_y_val,
        y_axis_label            = input$which,
        marker_size             = reactives$plot_marker_size_val,
        line_width              = reactives$plot_line_width_val,
        max_points              = reactives$max_points_val,
        n_xticks                = reactives$n_xticks_val,
        n_yticks                = reactives$n_yticks_val,
        tick_length             = reactives$tick_length_val,
        tick_width              = reactives$tick_width_val
        )

    return(fig)
})

output$fitted_signal_rescaled <- renderPlotly({

    req(input$table1)
    req(reactives$update_plots)
    req(reactives$signal_df_scaled)
    req(reactives$signal_df_fitted_scaled)

    fig <- plot_fluo_signal(
        signal_df               = reactives$signal_df_scaled,
        plot_width              = reactives$plot_width_val,
        plot_height             = reactives$plot_height_val,
        plot_type               = reactives$plot_type_val,
        axis_size               = reactives$plot_axis_size_val,
        unfolding_fitted_data   = reactives$signal_df_fitted_scaled,
        x_legend_pos            = reactives$x_legend_pos_val,
        y_legend_pos            = reactives$y_legend_pos_val,
        color_bar_length        = reactives$color_bar_length_val,
        color_bar_orientation   = reactives$color_bar_orientation_val,
        show_colorbar           = reactives$show_colorbar_val,
        show_grid_x             = reactives$show_grid_x_val,
        show_grid_y             = reactives$show_grid_y_val,
        y_axis_label            = input$which,
        marker_size             = reactives$plot_marker_size_val,
        line_width              = reactives$plot_line_width_val,
        max_points              = reactives$max_points_val,
        n_xticks                = reactives$n_xticks_val,
        n_yticks                = reactives$n_yticks_val,
        tick_length             = reactives$tick_length_val,
        tick_width              = reactives$tick_width_val
        )

    return(fig)
})

output$dg_vs_temp <- renderPlotly({

    req(input$table1)
    req(reactives$update_plots)
    req(reactives$signal_df_fitted)

    fig <- plot_2d(
        df                      = pySample$dg_df,
        plot_width              = reactives$plot_width_val,
        plot_height             = reactives$plot_height_val,
        plot_type               = reactives$plot_type_val,
        axis_size               = reactives$plot_axis_size_val,
        show_grid_x             = reactives$show_grid_x_val,
        show_grid_y             = reactives$show_grid_y_val,
        marker_size             = reactives$plot_marker_size_val,
        n_xticks                = reactives$n_xticks_val,
        n_yticks                = reactives$n_yticks_val,
        tick_length             = reactives$tick_length_val,
        tick_width              = reactives$tick_width_val,
        x_label                 = "Temperature (°C)",
        y_label                 = "ΔG (kcal/mol)",
        filename                = "dg_vs_temp"
    )

    return(fig)
})