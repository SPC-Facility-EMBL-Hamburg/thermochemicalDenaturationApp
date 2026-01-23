observeEvent(list(input$configure_legend,input$configure_legend_fit),{

    showModal(modalDialog(
        title = "Configure the colorbar",
        div(style = "width:300px; max-width:90vw;",
            sliderInput(
                inputId   = "legend_x_pos_input",
                label     = "X Position",
                value     = reactives$x_legend_pos_val,
                min       = 0,
                max       = 1.1,
                step      = 0.1
            ),
            sliderInput(
                inputId   = "legend_y_pos_input",
                label     = "Y Position",
                value     = reactives$y_legend_pos_val,
                min       = 0,
                max       = 1.1,
                step      = 0.1
            ),
            selectInput(
                inputId = "legend_colorbar_orientation_input",
                label = "Colorbar orientation",
                choices = c("horizontal" = "h", "vertical" = "v"),
                selected = reactives$color_bar_orientation_val
            ),
            sliderInput(
                inputId = "legend_colorbar_length_input",
                label = "Colorbar length",
                value = reactives$color_bar_length_val,
                min = 0.1,
                max = 1.0,
                step = 0.1
            ),
            checkboxInput(
                inputId = "legend_show_colorbar_input",
                label = "Show colorbar",
                value = isTRUE(reactives$show_colorbar_val)
            )
        ),
        size = 'm',
        easyClose = TRUE,
        footer = tagList(
            modalButton("Close")
        )
    ))

}, ignoreInit = TRUE)

observeEvent(input$legend_x_pos_input,{
    reactives$x_legend_pos_val <- input$legend_x_pos_input
})

observeEvent(input$legend_y_pos_input,{
    reactives$y_legend_pos_val <- input$legend_y_pos_input
})

# New observers for colorbar settings
observeEvent(input$legend_colorbar_orientation_input, {
    reactives$color_bar_orientation_val <- input$legend_colorbar_orientation_input
})

observeEvent(input$legend_colorbar_length_input, {
    # clamp to sensible bounds
    val <- as.numeric(input$legend_colorbar_length_input)
    if (is.na(val)) return()
    val <- max(0.1, min(1.0, val))
    reactives$color_bar_length_val <- val
})

observeEvent(input$legend_show_colorbar_input, {
    reactives$show_colorbar_val <- isTRUE(input$legend_show_colorbar_input)
})


# Axis configuration modal: show controls for x/y ticks, tick length/width, and grid toggles
observeEvent(list(input$configure_axis,input$configure_axis_fit), {
    showModal(modalDialog(
        title = "Configure axes and ticks",
        div(style = "width:300px; max-width:90vw;",
            numericInput(
                inputId = "axis_n_xticks_input",
                label = "Number of X ticks",
                value = reactives$n_xticks_val,
                min = 2,
                max = 8,
                step = 1
            ),
            numericInput(
                inputId = "axis_n_yticks_input",
                label = "Number of Y ticks",
                value = reactives$n_yticks_val,
                min = 2,
                max = 8,
                step = 1
            ),
            numericInput(
                inputId = "axis_tick_length_input",
                label = "Tick length (px)",
                value = reactives$tick_length_val,
                min = 0,
                max = 100,
                step = 1
            ),
            numericInput(
                inputId = "axis_tick_width_input",
                label = "Tick width (px)",
                value = reactives$tick_width_val,
                min = 0,
                max = 10,
                step = 1
            ),
            checkboxInput(
                inputId = "axis_show_x_grid_input",
                label = "Show X grid",
                value = isTRUE(reactives$show_grid_x_val)
            ),
            checkboxInput(
                inputId = "axis_show_y_grid_input",
                label = "Show Y grid",
                value = isTRUE(reactives$show_grid_y_val)
            )
        ),
        size = 'm',
        easyClose = TRUE,
        footer = tagList(
            modalButton("Close")
        )
    ))
}, ignoreInit = TRUE)

# Observers to update reactives when the modal inputs change
observeEvent(input$axis_n_xticks_input, {
    val <- as.integer(input$axis_n_xticks_input)
    if (is.na(val) || val < 1) return()
    val <- min(50, max(1, val))
    reactives$n_xticks_val <- val
})

observeEvent(input$axis_n_yticks_input, {
    val <- as.integer(input$axis_n_yticks_input)
    if (is.na(val) || val < 1) return()
    val <- min(50, max(1, val))
    reactives$n_yticks_val <- val
})

observeEvent(input$axis_tick_length_input, {
    val <- as.numeric(input$axis_tick_length_input)
    if (is.na(val)) return()
    val <- min(100, max(0, val))
    reactives$tick_length_val <- val
})

observeEvent(input$axis_tick_width_input, {
    val <- as.numeric(input$axis_tick_width_input)
    if (is.na(val)) return()
    val <- min(10, max(0, val))
    reactives$tick_width_val <- val
})

observeEvent(input$axis_show_x_grid_input, {
    reactives$show_grid_x_val <- isTRUE(input$axis_show_x_grid_input)
})

observeEvent(input$axis_show_y_grid_input, {
    reactives$show_grid_y_val <- isTRUE(input$axis_show_y_grid_input)
})


# Export configuration modal: file type, plot width, plot height
observeEvent(list(input$configure_export,input$configure_export_fit), {
    showModal(modalDialog(
        title = "Export options",
        div(style = "width:400px; max-width:90vw;",
            selectInput(
                inputId = "export_file_type_input",
                label = "File type",
                choices = c("PNG" = "png", "SVG" = "svg", "JPEG" = "jpeg"),
                selected = reactives$plot_type_val
            ),
            numericInput(
                inputId = "export_plot_width_input",
                label = "Plot width (px)",
                value = reactives$plot_width_val,
                min = 100,
                max = 10000,
                step = 50
            ),
            numericInput(
                inputId = "export_plot_height_input",
                label = "Plot height (px)",
                value = reactives$plot_height_val,
                min = 100,
                max = 10000,
                step = 50
            )
        ),
        size = 'm',
        easyClose = TRUE,
        footer = tagList(
            modalButton("Close")
        )
    ))
}, ignoreInit = TRUE)

# Observers to update reactives when the export modal inputs change
observeEvent(input$export_file_type_input, {
    req(input$export_file_type_input)
    reactives$plot_type_val <- input$export_file_type_input
})

observeEvent(input$export_plot_width_input, {
    val <- as.numeric(input$export_plot_width_input)
    if (is.na(val)) return()
    val <- min(10000, max(100, round(val)))
    reactives$plot_width_val <- val
})

observeEvent(input$export_plot_height_input, {
    val <- as.numeric(input$export_plot_height_input)
    if (is.na(val)) return()
    val <- min(10000, max(100, round(val)))
    reactives$plot_height_val <- val
})


# Appearance modal: marker size, font size, line width, max points
observeEvent(list(input$configure_appearance,input$configure_appearance_fit), {
    showModal(modalDialog(
        title = "Appearance",
        div(style = "width:500px; max-width:90vw;",
            sliderInput('appearance_marker_size', 'Marker size',
                value = reactives$plot_marker_size_val, min = 1, max = 20, step = 0.5),
            numericInput('appearance_font_size', 'Font size',
                value = reactives$plot_axis_size_val, min = 4, max = 60, step = 1),
            sliderInput('appearance_line_width', 'Line width',
                value = reactives$plot_line_width_val, min = 0.5, max = 10, step = 0.5),
            sliderInput('appearance_max_points', 'Max points',
                value = reactives$max_points_val, min = 100, max = 20000, step = 100)
        ),
        size = 'm',
        easyClose = TRUE,
        footer = tagList(
            modalButton('Close')
        )
    ))
}, ignoreInit = TRUE)

# Observers to sync appearance inputs into reactives
observeEvent(input$appearance_marker_size, {
    val <- as.numeric(input$appearance_marker_size)
    if (is.na(val)) return()
    val <- max(0.1, min(100, val))
    reactives$plot_marker_size_val <- val
})

observeEvent(input$appearance_font_size, {
    val <- as.numeric(input$appearance_font_size)
    if (is.na(val)) return()
    val <- max(4, min(100, round(val)))
    reactives$plot_axis_size_val <- val
})

observeEvent(input$appearance_line_width, {
    val <- as.numeric(input$appearance_line_width)
    if (is.na(val)) return()
    val <- max(0.1, min(100, val))
    reactives$plot_line_width_val <- val
})

observeEvent(input$appearance_max_points, {
    val <- as.integer(input$appearance_max_points)
    if (is.na(val)) return()
    val <- max(100, min(1e6, val))
    reactives$max_points_val <- val
})


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

# Render signal plot
output$fitted_signal_residuals <- renderPlotly({

    req(input$table1)
    req(reactives$update_plots)
    req(reactives$signal_df)
    req(reactives$signal_df_fitted)

    fig <- plot_fluo_signal_residuals(
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

# Render signal plot
output$fitted_signal_and_residuals <- renderPlotly({

    req(input$table1)
    req(reactives$update_plots)
    req(reactives$signal_df)
    req(reactives$signal_df_fitted)

    fig <- plot_fits_and_residuals(
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

