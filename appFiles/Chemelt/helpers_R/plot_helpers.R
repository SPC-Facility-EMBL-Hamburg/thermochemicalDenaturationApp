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

nice_temperature_ticks_05 <- function(min_temp, max_temp, n_ticks = 6) {

  # Ideal spacing
  raw_step <- (max_temp - min_temp) / (n_ticks - 1)

  # Allowed steps (must land on 0 or 5)
  allowed_steps <- c(5, 10, 15, 20, 25, 50)

  # Pick closest allowed step
  step <- allowed_steps[which.min(abs(allowed_steps - raw_step))]

  # Center ticks over the range
  center <- (min_temp + max_temp) / 2

  start <- center - step * (n_ticks - 1) / 2
  start <- round(start / 5) * 5

  ticks <- start + step * seq(0, n_ticks - 1)

  return(ticks)
}

get_axis_ticks <- function(min_val, max_val, n_ticks = 6) {

  axis_step <- (max_val - min_val) / (n_ticks - 1)
  tickpos <- seq(min_val, max_val, by = axis_step)

  return(tickpos)
}

# Helper: format numeric tick positions using a given number of significant digits
format_axis_labels <- function(ticks, sig = 3) {
  # Use formatC with format='g' to get significant-digit formatting,
  # which will switch to scientific notation for very large/small values.
  sapply(ticks, function(x) {
    # Handle NA/NULL safely
    if (is.null(x) || is.na(x)) return(NA_character_)
    formatC(signif(x, digits = sig), format = 'g', digits = sig)
  }, USE.NAMES = FALSE)
}

config_fig <- function(fig,filename,plot_type,plot_width,plot_height) {
    fig %>%  config(
        toImageButtonOptions = list(
        format = plot_type,
        filename = filename,
        width = plot_width,
        height = plot_height
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
    show_colorbar=TRUE,
    show_grid_x=FALSE,
    show_grid_y=FALSE,
    y_axis_label='Signal',
    marker_size = 2,
    line_width = 2,
    max_points = 2000,
    n_xticks = 6,
    n_yticks = 6,
    tick_length = 8,
    tick_width = 2,
    derivative = FALSE){

    # Select at most 20 temperatures
    n_rows <- nrow(signal_df)

    extra_hover_text <- ' (M)'

    out_n <- min(max_points, n_rows)

    # Compute regularly spaced indices
    idx <- seq(1, n_rows, length.out = out_n)
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

    xticks_pos <- nice_temperature_ticks_05(min_x + 5, max_x - 5, n_ticks = n_xticks)

    xaxis <- list(title = x_axis_label,
        titlefont = list(size = axis_size),
        tickfont = list(size = axis_size),
        range = c(min_x, max_x),
        showgrid = show_grid_x,
        showline = TRUE,
        zeroline = FALSE,
        ticks = "outside",
        tickwidth = tick_width,
        ticklen = tick_length,
        tickmode = "array",
        tickvals = xticks_pos)

    expand_y_factor <- 0.1
    expand_y_pos    <- 1 + expand_y_factor
    expand_y_neg    <- 1 - expand_y_factor

    min_s <- min(signal_df$Signal)
    max_s <- max(signal_df$Signal)

    min_s <- ifelse(min_s > 0, min_s*expand_y_neg, min_s*expand_y_pos)
    max_s <- ifelse(max_s > 0, max_s*expand_y_pos, max_s*expand_y_neg)

    yticks_pos <- get_axis_ticks(min_s, max_s, n_ticks = n_yticks)

    if (derivative) y_axis_label <- paste0("1st derivative / ",y_axis_label)

    yaxis <- list(title= y_axis_label,
        titlefont = list(size = axis_size),
        tickfont = list(size = axis_size),
        range = c(min_s, max_s),
        showgrid = show_grid_y,
        showline = TRUE,
        zeroline = FALSE,
        ticks = "outside",
        tickwidth = tick_width,
        ticklen = tick_length,
        tickmode = "array",
        tickvals = yticks_pos,
        ticktext = format_axis_labels(yticks_pos, sig = 3)
    )

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

     # Set layout and position the colorbar (conditionally show/hide)
    if (show_colorbar) {
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
    } else {
        fig <- fig %>% hide_colorbar()
    }

    fig <- config_fig(
        fig,
        filename="Signal_versus_temperature",
        plot_type=plot_type,
        plot_width=plot_width,
        plot_height=plot_height)

    return(fig)

}

plot_fluo_signal_residuals <- function(
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
    show_colorbar=TRUE,
    show_grid_x=FALSE,
    show_grid_y=FALSE,
    marker_size = 2,
    line_width = 2,
    max_points = 2000,
    n_xticks = 6,
    n_yticks = 6,
    tick_length = 8,
    tick_width = 2){

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

    # Subset the fitted data
    unfolding_fitted_data <- unfolding_fitted_data[idx,]

    signal_df$Denaturant <- signif(signal_df$Denaturant, 3)

    x_axis_label <- "Temperature (ºC)"

    min_x <- min(signal_df$Temperature) - 5
    max_x <- max(signal_df$Temperature) + 5

    xticks_pos <- nice_temperature_ticks_05(min_x + 5, max_x - 5, n_ticks = n_xticks)

    xaxis <- list(title = x_axis_label,
        titlefont = list(size = axis_size),
        tickfont = list(size = axis_size),
        range = c(min_x, max_x),
        showgrid = show_grid_x,
        showline = TRUE,
        zeroline = FALSE,
        ticks = "outside",
        tickwidth = tick_width,
        ticklen = tick_length,
        tickmode = "array",
        tickvals = xticks_pos
        )

    fig <- plot_ly()

    names(unfolding_fitted_data)[names(unfolding_fitted_data) == "Signal"] <- "Signal_fit"

    # Merge signal_df and unfolding_fitted_data to compute residuals
    signal_df <- merge(signal_df, unfolding_fitted_data[,c("ID","Temperature","Denaturant","Signal_fit")],
        by=c("ID","Temperature","Denaturant"))

    signal_df$residuals <- signal_df$Signal - signal_df$Signal_fit

    fig <- fig %>% add_trace(
        data=signal_df,
        x = ~Temperature,
        y = ~residuals,
        color = ~Denaturant,
        type = "scatter",
        mode = "markers",
        showlegend = FALSE,
        text = ~paste0(Denaturant, extra_hover_text),
        name="",
        hoverinfo = 'text+x+y',
        marker=list(size=marker_size))

    expand_y_factor <- 0.12
    expand_y_pos    <- 1 + expand_y_factor
    expand_y_neg    <- 1 - expand_y_factor

    min_s <- min(signal_df$residuals)
    max_s <- max(signal_df$residuals)

    min_s <- ifelse(min_s > 0, min_s*expand_y_neg, min_s*expand_y_pos)
    max_s <- ifelse(max_s > 0, max_s*expand_y_pos, max_s*expand_y_neg)

    yticks_pos <- get_axis_ticks(min_s, max_s, n_ticks = n_yticks)

    yaxis <- list(title= "Residuals",
        titlefont = list(size = axis_size),
        tickfont = list(size = axis_size),
        range = c(min_s, max_s),
        showgrid = show_grid_y,
        showline = TRUE,
        zeroline = FALSE,
        ticks = "outside",
        tickwidth = tick_width,
        ticklen = tick_length,
        tickmode = "array",
        tickvals = yticks_pos,
        ticktext = format_axis_labels(yticks_pos, sig = 3)
    )

    fig <- fig %>% layout(
        xaxis = xaxis,
        yaxis = yaxis,
        font="Roboto",
        shapes = list(
            list(
                type = "line",
                x0 = min_x,
                x1 = max_x,
                y0 = 0,
                y1 = 0,
                line = list(
                    color = "red",
                    width = 2,
                    dash = "dash")
            )
        )
    )

    min_z <- min(signal_df$Denaturant)
    max_z <- max(signal_df$Denaturant)

    tickvals <- c(min_z,min_z + (max_z - min_z)/2,max_z)

    tickvals[2] <- round(tickvals[2],2)

     # Set layout and position the colorbar (conditionally show/hide)
    if (show_colorbar) {
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
    } else {
        fig <- fig %>% hide_colorbar()
    }

    fig <- config_fig(
        fig,
        filename="Residuals_versus_temperature",
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
    n_xticks = 6,
    n_yticks = 6,
    tick_length = 8,
    tick_width = 2,
    x_label="Denaturant (M)",
    y_label="T<sub>m</sub> (ºC) / 1st derivative",
    filename="Tm_versus_denaturant",
    y_zeroline = FALSE,               # whether to enable y-axis zeroline
    y_zeroline_color = "red",
    y_zeroline_width = 2){

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
        tickwidth = tick_width,
        ticklen = tick_length,
        tickmode = "array",
        tickvals = get_axis_ticks(min(df[,2]), max(df[,2]), n_ticks = n_xticks),
        ticktext = format_axis_labels(get_axis_ticks(min(df[,2]), max(df[,2]), n_ticks = n_xticks), sig = 3)
        )

    # Use the y_zeroline parameters to populate the yaxis zeroline properties
    yaxis <- list(title = y_label,
        titlefont = list(size = axis_size),
        tickfont = list(size = axis_size),
        showgrid = show_grid_y,
        showline = TRUE,
        zeroline = isTRUE(y_zeroline),
        zerolinecolor = y_zeroline_color,
        zerolinewidth = y_zeroline_width,
        ticks = "outside",
        tickwidth = tick_width,
        ticklen = tick_length,
        tickmode = "array",
        tickvals = get_axis_ticks(min(df[,1]), max(df[,1]), n_ticks = n_yticks),
        ticktext = format_axis_labels(get_axis_ticks(min(df[,1]), max(df[,1]), n_ticks = n_yticks), sig = 3)
        )

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
    marker_size = 2,
    n_xticks = 6,
    n_yticks = 6,
    tick_length = 8,
    tick_width = 2){

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
        n_xticks = n_xticks,
        n_yticks = n_yticks,
        tick_length = tick_length,
        tick_width = tick_width,
        x_label = "Denaturant (M)",
        y_label = "Initial signal (a.u.)",
        filename = "Initial_signal_versus_denaturant")

    return(fig)

}

plot_fits_and_residuals <- function(
    signal_df,
    plot_width = 12,
    plot_height = 8,
    plot_type = 'svg',
    axis_size = 12,
    unfolding_fitted_data,
    x_legend_pos = 1,
    y_legend_pos = 1,
    color_bar_length = 0.5,
    color_bar_orientation = 'h',
    show_colorbar = TRUE,
    show_grid_x = FALSE,
    show_grid_y = FALSE,
    y_axis_label = 'Signal',
    marker_size = 2,
    line_width = 2,
    max_points = 2000,
    n_xticks = 6,
    n_yticks = 6,
    tick_length = 8,
    tick_width = 2
) {

    n_rows <- nrow(signal_df)
    extra_hover_text <- ' (M)'

    # ----------------------------
    # Subsample points
    # ----------------------------
    idx <- seq(1, n_rows, length.out = min(max_points, n_rows))
    idx <- unique(round(idx))

    signal_df <- signal_df[idx, ]
    unfolding_fitted_data <- unfolding_fitted_data[idx, ]

    signal_df$Denaturant <- signif(signal_df$Denaturant, 3)

    # ----------------------------
    # Y axis (signal)
    # ----------------------------
    expand_y_factor <- 0.12
    min_s <- min(signal_df$Signal)
    max_s <- max(signal_df$Signal)

    min_s <- ifelse(min_s > 0, min_s * (1 - expand_y_factor), min_s * (1 + expand_y_factor))
    max_s <- ifelse(max_s > 0, max_s * (1 + expand_y_factor), max_s * (1 - expand_y_factor))

    yticks_pos <- get_axis_ticks(min_s, max_s, n_ticks = n_yticks)

    yaxis_signal <- list(
        title = y_axis_label,
        titlefont = list(size = axis_size),
        tickfont = list(size = axis_size),
        range = c(min_s, max_s),
        showgrid = show_grid_y,
        showline = TRUE,
        zeroline = FALSE,
        ticks = "outside",
        tickwidth = tick_width,
        ticklen = tick_length,
        tickmode = "array",
        tickvals = yticks_pos,
        ticktext = format_axis_labels(yticks_pos, sig = 3)
     )

    # ============================================================
    # TOP PANEL: SIGNAL + FIT
    # ============================================================
    fig_signal <- plot_ly()

    # ============================================================
    # COLORBAR tickvals
    # ============================================================
    min_z <- min(signal_df$Denaturant)
    max_z <- max(signal_df$Denaturant)

    tickvals <- c(min_z, (min_z + max_z) / 2, max_z)
    tickvals[2] <- round(tickvals[2], 2)

    fig_signal <- fig_signal %>%
      add_trace(
        data = signal_df,
        x = ~Temperature,
        y = ~Signal,
        type = "scatter",
        mode = "markers",
        marker = list(
          size = marker_size,
          color = ~Denaturant,
          colorscale = "Viridis",
          cmin = min_z,
          cmax = max_z,
          showscale = TRUE,
          colorbar = list(
            title = list(
                text = "[Denaturant] (M)",
                font = list(size = axis_size - 1)
            ),
            tickfont = list(size = axis_size - 2),  # Font size of the ticks
            len = color_bar_length,
            x = x_legend_pos,
            y = y_legend_pos,
            xanchor = "right",
            yanchor = "top",
            tickvals = tickvals,
            ticktext = tickvals,
            orientation = color_bar_orientation,
            outlinewidth = 0
          )
        ),
        showlegend = FALSE
      )


    groups <- unique(signal_df$ID)

    # ---- fitted curves ----
    for (group in groups) {

        group_df <- unfolding_fitted_data[
            unfolding_fitted_data$ID == group, ]
        group_df <- group_df[order(group_df$Temperature), ]

        fig_signal <- fig_signal %>%
            add_trace(
                inherit = FALSE,
                data = group_df,
                x = ~Temperature,
                y = ~Signal,
                type = "scatter",
                mode = "lines",
                color = I("black"),
                showlegend = FALSE,
                line = list(width = line_width)
            )
    }

    fig_signal <- fig_signal %>%
        layout(
            yaxis = yaxis_signal,
            font = "Roboto",
            uirevision = "fit"
        )

    # ============================================================
    # BOTTOM PANEL: RESIDUALS
    # ============================================================
    residual_df <- merge(
        signal_df,
        unfolding_fitted_data,
        by = c("ID", "Temperature", "Denaturant"),
        suffixes = c("_obs", "_fit")
    )

    residual_df$Residual <- residual_df$Signal_obs - residual_df$Signal_fit

    min_s <- min(residual_df$Residual)
    max_s <- max(residual_df$Residual)

    expand_y_factor_res <- 0.06
    min_s <- ifelse(min_s > 0, min_s * (1 - expand_y_factor_res), min_s * (1 + expand_y_factor_res))
    max_s <- ifelse(max_s > 0, max_s * (1 + expand_y_factor_res), max_s * (1 - expand_y_factor_res))

    yticks_pos <- get_axis_ticks(min_s, max_s, n_ticks = n_yticks)

    fig_res <- plot_ly() %>%
        add_trace(
            data = residual_df,
            x = ~Temperature,
            y = ~Residual,
            type = "scatter",
            mode = "markers",
            marker = list(
              size = marker_size,
              color = ~Denaturant,
              colorscale = "Viridis",
              cmin = min_z,
              cmax = max_z,
              showscale = FALSE   # CRITICAL
            ),
            showlegend = FALSE,
            hoverinfo = "x+y"
          ) %>%
        layout(
            yaxis = list(
                title = "Residuals",
                titlefont = list(size = axis_size),
                tickfont = list(size = axis_size),
                showgrid = show_grid_y,
                zeroline = TRUE,
                showline = TRUE,
                ticks = "outside",
                tickwidth = tick_width,
                ticklen = tick_length,
                tickmode = "array",
                tickvals = yticks_pos,
                ticktext = format_axis_labels(yticks_pos, sig = 3)
            ),
            uirevision = "fit"
        )

    # ============================================================
    # COMBINE PANELS
    # ============================================================
    fig <- subplot(
        fig_signal,
        fig_res,
        nrows = 2,
        heights = c(0.7, 0.3),
        shareX = TRUE,
        titleY = TRUE
    )

   x_axis_label <- "Temperature (ºC)"
    min_x <- min(signal_df$Temperature) - 5
    max_x <- max(signal_df$Temperature) + 5

    # ----------------------------
    # X axis
    # ----------------------------

    xticks_pos <- nice_temperature_ticks_05(min_x + 5, max_x - 5, n_ticks = n_xticks)

    xaxis <- list(
        title = x_axis_label,
        titlefont = list(size = axis_size),
        tickfont = list(size = axis_size),
        range = c(min_x, max_x),
        showgrid = show_grid_x,
        showline = TRUE,
        zeroline = FALSE,
        ticks = "outside",
        tickwidth = tick_width,
        ticklen = tick_length,
        tickmode = "array",
        tickvals = xticks_pos
    )

    # Set shared x-axis for both panels
    fig <- fig %>% layout(
        xaxis = xaxis,
        font = "Roboto"
    )

    # ============================================================
    # EXPORT / CONFIG
    # ============================================================
    fig <- config_fig(
        fig,
        filename = "Signal_and_residuals",
        plot_type = plot_type,
        plot_width = plot_width,
        plot_height = plot_height
    )

    return(fig)
}
