# Render signal plot
output$signal <- renderPlotly({
  
    req(input$table1)
    req(reactives$update_plots)
    req(reactives$signal_df)

    fig <- plot_fluo_signal(
        signal_df               = reactives$signal_df,
        plot_width              = input$plot_width,
        plot_height             = input$plot_height,
        plot_type               = input$plot_type,
        axis_size               = input$plot_axis_size,
        unfolding_fitted_data   = NULL,
        x_legend_pos            = input$x_legend_pos,
        y_legend_pos            = input$y_legend_pos,
        color_bar_length        = input$color_bar_length,
        color_bar_orientation   = input$color_bar_orientation,
        show_grid_x             = input$show_grid_x,
        show_grid_y             = input$show_grid_y,
        y_axis_label            = input$which,
        marker_size             = input$plot_marker_size,
        line_width              = input$plot_line_width,
        max_points              = input$max_points
        )
  
    return(fig)
})

output$first_der <- renderPlotly({

    req(input$table1)
    req(reactives$update_plots)
    req(reactives$derivative_df)

    fig <- plot_fluo_signal(
        signal_df               = reactives$derivative_df,
        plot_width              = input$plot_width,
        plot_height             = input$plot_height,
        plot_type               = input$plot_type,
        axis_size               = input$plot_axis_size,
        unfolding_fitted_data   = NULL,
        x_legend_pos            = input$x_legend_pos,
        y_legend_pos            = input$y_legend_pos,
        color_bar_length        = input$color_bar_length,
        color_bar_orientation   = input$color_bar_orientation,
        show_grid_x             = input$show_grid_x,
        show_grid_y             = input$show_grid_y,
        y_axis_label            = input$which,
        marker_size             = NULL,
        line_width              = input$plot_line_width,
        max_points              = input$max_points,
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
        plot_width              = input$plot_width,
        plot_height             = input$plot_height,
        plot_type               = input$plot_type,
        axis_size               = input$plot_axis_size,
        show_grid_x             = input$show_grid_x,
        show_grid_y             = input$show_grid_y,
        marker_size             = input$plot_marker_size
    )

    return(fig)
})

output$initial_fluo_vs_den <- renderPlotly({

    req(input$table1)
    req(reactives$update_plots)
    req(reactives$signal_df)

    fig <- plot_initial_signal_versus_denaturant(
        signal_df               = reactives$signal_df,
        plot_width              = input$plot_width,
        plot_height             = input$plot_height,
        plot_type               = input$plot_type,
        axis_size               = input$plot_axis_size,
        show_grid_x             = input$show_grid_x,
        show_grid_y             = input$show_grid_y,
        marker_size             = input$plot_marker_size
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
        plot_width              = input$plot_width_fit,
        plot_height             = input$plot_height_fit,
        plot_type               = input$plot_type_fit,
        axis_size               = input$plot_axis_size_fit,
        unfolding_fitted_data   = reactives$signal_df_fitted,
        x_legend_pos            = input$x_legend_pos_fit,
        y_legend_pos            = input$y_legend_pos_fit,
        color_bar_length        = input$color_bar_length_fit,
        color_bar_orientation   = input$color_bar_orientation_fit,
        show_grid_x             = input$show_grid_x_fit,
        show_grid_y             = input$show_grid_y_fit,
        y_axis_label            = input$which,
        marker_size             = input$plot_marker_size_fit,
        line_width              = input$plot_line_width_fit,
        max_points              = input$max_points_fit
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
        plot_width              = input$plot_width_fit,
        plot_height             = input$plot_height_fit,
        plot_type               = input$plot_type_fit,
        axis_size               = input$plot_axis_size_fit,
        unfolding_fitted_data   = reactives$signal_df_fitted_scaled,
        x_legend_pos            = input$x_legend_pos_fit,
        y_legend_pos            = input$y_legend_pos_fit,
        color_bar_length        = input$color_bar_length_fit,
        color_bar_orientation   = input$color_bar_orientation_fit,
        show_grid_x             = input$show_grid_x_fit,
        show_grid_y             = input$show_grid_y_fit,
        y_axis_label            = input$which,
        marker_size             = input$plot_marker_size_fit,
        line_width              = input$plot_line_width_fit,
        max_points              = input$max_points_fit
        )

    return(fig)
})

output$dg_vs_temp <- renderPlotly({

    req(input$table1)
    req(reactives$update_plots)
    req(reactives$signal_df_fitted)

    fig <- plot_2d(
        df                      = pySample$dg_df,
        plot_width              = input$plot_width_fit,
        plot_height             = input$plot_height_fit,
        plot_type               = input$plot_type_fit,
        axis_size               = input$plot_axis_size_fit,
        show_grid_x             = input$show_grid_x_fit,
        show_grid_y             = input$show_grid_y_fit,
        marker_size             = input$plot_marker_size_fit,
        x_label                 = "Temperature (°C)",
        y_label                 = "ΔG (kcal/mol)",
        filename                = "dg_vs_temp"
    )

    return(fig)
})