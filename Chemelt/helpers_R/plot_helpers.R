get_colors_from_numeric_values <- function(values,minVal,maxVal,useLogScale=TRUE) {

    viridis <- c("#440154","#471063","#481d6f","#472a7a",
               "#414487","#3c4f8a","#375a8c",
               "#32648e","#2a788e","#26828e",
               "#228b8d","#1f958b","#22a884",
               "#2cb17e","#3bbb75","#4ec36b",
               "#7ad151","#95d840","#b0dd2f","#cae11f",
               "#fde725")

    if (useLogScale) {

        minVal <- log10(minVal)
        maxVal <- log10(maxVal)

    }

    seq <- seq(minVal,maxVal,length.out = 21)

    if (useLogScale) {
        idx <- sapply(values,function(v) which.min(abs(log10(v) - seq)))
    } else {
        idx <- sapply(values,function(v) which.min(abs(v - seq)))
    }

  return(viridis[idx])
}

config_fig <- function(fig,filename,plot_type,plot_width,plot_height) {
    fig %>%  config(
        toImageButtonOptions = list(
        format = plot_type,
        filename = filename,
        width = plot_width * 50,
        height = plot_height * 50
        ), displaylogo = FALSE,
        modeBarButtonsToRemove = list(
            'hoverClosestCartesian',
            'hoverCompareCartesian',
            'lasso2d','select2d')
        )
}

plot_fluo_signal <- function(
    signal_df,
    plot_width=12,
    plot_height=8,
    plot_type='svg',
    axis_size=12,
    unfolding_fitted_data = NULL,
    x_legend_pos=1,
    y_legend_pos=1,
    color_bar_length=0.5,
    color_bar_orientation='h',
    show_grid_x=FALSE,
    show_grid_y=FALSE,
    y_axis_label='Signal',
    marker_size = 2,
    line_width = 2,
    max_points = 2000,
    derivative = FALSE){

    # Select at most 20 temperatures
    n_rows <- nrow(signal_df)

    extra_hover_text <- ' (M)'

    # Compute regularly spaced indices
    idx <- seq(1, n_rows, length.out = min(max_points, n_rows))
    idx <- round(idx)

    # Remove duplicates in idx vector
    idx <- unique(idx)

    # Subset the dataframe
    signal_df <- signal_df[idx, ]

    # Subset the fitted data, if provided
    if (!is.null(unfolding_fitted_data)) {
        unfolding_fitted_data <- unfolding_fitted_data[idx,]
    }

    signal_df$Denaturant <- signif(signal_df$Denaturant, 3)

    x_axis_label <- "Temperature (ºC)"

    min_x <- min(signal_df$Temperature) - 5
    max_x <- max(signal_df$Temperature) + 5

    xaxis <- list(title = x_axis_label,
        titlefont = list(size = axis_size),
        tickfont = list(size = axis_size),
        range = c(min_x, max_x),
        showgrid = show_grid_x,
        showline = TRUE,
        zeroline = FALSE,
        ticks = "outside",
        tickwidth = 2,
        ticklen = tick_length_cst)

    expand_y_factor <- 0.12
    expand_y_pos    <- 1 + expand_y_factor
    expand_y_neg    <- 1 - expand_y_factor

    min_s <- min(signal_df$Signal)
    min_s <- ifelse(min_s > 0, min_s*expand_y_neg, min_s*expand_y_pos)
    max_s <- max(signal_df$Signal)
    max_s <- ifelse(max_s > 0, max_s*expand_y_pos, max_s*expand_y_neg)

    if (derivative) y_axis_label <- paste0("1st derivative / ",y_axis_label)

    yaxis <- list(title= y_axis_label,
        titlefont = list(size = axis_size),
        tickfont = list(size = axis_size),
        range = c(min_s, max_s),
        showgrid = show_grid_y,
        showline = TRUE,
        zeroline = FALSE,
        ticks = "outside",
        tickwidth = 2,
        ticklen = tick_length_cst)

    fig <- plot_ly()

    # Use markers for signal, lines for derivative
    if (!derivative) {

        # Plot the fitting as black lines if unfolding_fitted_data is provided
        if (!is.null(unfolding_fitted_data)) {

            groups <- unique(signal_df$ID)

            # Plot one trace per group, to avoid problems with the lines ...
            for (i in 1:length(groups)) {

                group    <- groups[i]

                group_df <- unfolding_fitted_data[unfolding_fitted_data$ID == group, ]

                group_df <- group_df[order(group_df$Temperature), ]

                fig <- fig %>% add_trace(
                    data = group_df,
                    x = ~Temperature,
                    y = ~Signal,
                    color = I('black'),
                    type = "scatter",
                    mode = "lines",
                    showlegend = FALSE,
                    line=list(width=line_width))
            }

        }

        fig <- fig %>% add_trace(
            data=signal_df,
            x = ~Temperature,
            y = ~Signal,
            color = ~Denaturant,
            type = "scatter",
            mode = "markers",
            showlegend = FALSE,
            text = ~paste0(Denaturant, extra_hover_text),
            name="",
            hoverinfo = 'text+x+y',
            marker=list(size=marker_size))

    } else {

        # We need to plot one group at a time, to avoid problems with the lines

        groups <- unique(signal_df$ID)

        for (i in 1:length(groups)) {
            group    <- groups[i]

            group_df <- signal_df[signal_df$ID == group, ]
            fig <- fig %>% add_trace(
                data = group_df,
                x = ~Temperature,
                y = ~Signal,
                color = ~Denaturant,
                type = "scatter",
                mode = "lines",
                showlegend = FALSE,
                text = ~paste0(Denaturant, extra_hover_text),
                name="",
                hoverinfo = 'text+x+y',
                line=list(width=line_width))
        }

    }

    fig <- fig %>% layout(
        xaxis = xaxis,
        yaxis = yaxis,
        font="Roboto"
    )

    min_z <- min(signal_df$Denaturant)
    max_z <- max(signal_df$Denaturant)

    tickvals <- c(min_z,min_z + (max_z - min_z)/2,max_z)

    tickvals[2] <- round(tickvals[2],2)

     # Set layout and position the colorbar
    fig <- fig %>% colorbar(
        title = list(
            text="[Denaturant] (M)",
            font=list(size=axis_size-1)
            ),
        x = x_legend_pos,   # Horizontal position
        y = y_legend_pos,   # Vertical position
        xanchor = "right",  # Anchoring to the right side
        yanchor = "top",
        tickvals = tickvals,  # Ticks from max to min, rounded to two decimal places
        ticktext = tickvals,  # Use the same tick values as labels
        tickfont = list(size = axis_size-2),  # Font size of the ticks
        len = color_bar_length,  # Length of the color bar
        orientation = color_bar_orientation,
        outlinewidth = 0)

    fig <- config_fig(
        fig,
        filename="Signal_versus_temperature",
        plot_type=plot_type,
        plot_width=plot_width,
        plot_height=plot_height)

    return(fig)

}

plot_2d <- function(
    df,
    plot_width=12,
    plot_height=8,
    plot_type='svg',
    axis_size=12,
    show_grid_x=FALSE,
    show_grid_y=FALSE,
    marker_size = 2,
    x_label="Denaturant (M)",
    y_label="T<sub>m</sub> (ºC) / 1st derivative",
    filename="Tm_versus_denaturant"){

    # The first column is the y-value
    # The second column is the x-value

    # Plot the Tm versus denaturant
    fig <- plot_ly(x = df[,2], y = df[,1],
        type = "scatter", mode = "markers", showlegend = FALSE,
        marker=list(size=marker_size))

    # Set the axis labels
    xaxis <- list(title = x_label,
        titlefont = list(size = axis_size),
        tickfont = list(size = axis_size),
        showgrid = show_grid_x,
        showline = TRUE,
        zeroline = FALSE,
        ticks = "outside",
        tickwidth = 2,
        ticklen = tick_length_cst)

    yaxis <- list(title = y_label,
        titlefont = list(size = axis_size),
        tickfont = list(size = axis_size),
        showgrid = show_grid_y,
        showline = TRUE,
        zeroline = FALSE,
        ticks = "outside",
        tickwidth = 2,
        ticklen = tick_length_cst)

    fig <- fig %>% layout(
        xaxis = xaxis,
        yaxis = yaxis,
        font="Roboto")

    fig <- config_fig(
        fig,
        filename=filename,
        plot_type=plot_type,
        plot_width=plot_width,
        plot_height=plot_height)

    return(fig)

}

plot_initial_signal_versus_denaturant <- function(
    signal_df,
    plot_width=12,
    plot_height=8,
    plot_type='svg',
    axis_size=12,
    show_grid_x=FALSE,
    show_grid_y=FALSE,
    marker_size = 2){

    # For each group in signal_df, get the first signal value
    # and the corresponding denaturant value
    initial_signal_df <- signal_df %>%
        group_by(ID) %>%
        summarise(
            Initial_Signal = first(Signal),
            Denaturant     = first(Denaturant))

    initial_signal_df <- as.data.frame(initial_signal_df)[,c(2,3)]

    fig <- plot_2d(
        df = initial_signal_df,
        plot_width = plot_width,
        plot_height = plot_height,
        plot_type = plot_type,
        axis_size = axis_size,
        show_grid_x = show_grid_x,
        show_grid_y = show_grid_y,
        marker_size = marker_size,
        x_label = "Denaturant (M)",
        y_label = "Initial signal (a.u.)",
        filename = "Initial_signal_versus_denaturant")

    return(fig)

}
