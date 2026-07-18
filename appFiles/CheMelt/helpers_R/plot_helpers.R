get_colors_from_numeric_values <- function(values,minVal,maxVal,useLogScale=TRUE) {

    viridis = c(
         '#440154', '#450457', '#46085c', '#460b5e', '#471063', '#471365', '#481769', '#481b6d', '#481d6f', '#482173',
         '#482475', '#482878', '#472c7a', '#472e7c', '#46327e', '#463480', '#453882', '#443a83', '#433e85', '#424186',
         '#414487', '#3f4788', '#3e4989', '#3d4d8a', '#3c508b', '#3b528b', '#39558c', '#38588c', '#375b8d', '#365d8d',
         '#34608d', '#33638d', '#32658e', '#31688e', '#306a8e', '#2e6d8e', '#2d708e', '#2c718e', '#2b748e', '#2a768e',
         '#29798e', '#287c8e', '#277e8e', '#26818e', '#26828e', '#25858e', '#24878e', '#238a8d', '#228d8d', '#218f8d',
         '#20928c', '#20938c', '#1f968b', '#1f998a', '#1e9b8a', '#1f9e89', '#1fa088', '#1fa287', '#20a486', '#22a785',
         '#24aa83', '#25ac82', '#28ae80', '#2ab07f', '#2eb37c', '#32b67a', '#35b779', '#3aba76', '#3dbc74', '#42be71',
         '#48c16e', '#4cc26c', '#52c569', '#56c667', '#5cc863', '#60ca60', '#67cc5c', '#6ece58', '#73d056', '#7ad151',
         '#7fd34e', '#86d549', '#8ed645', '#93d741', '#9bd93c', '#a0da39', '#a8db34', '#addc30', '#b5de2b', '#bddf26',
         '#c2df23', '#cae11f', '#d0e11c', '#d8e219', '#dfe318', '#e5e419', '#ece51b', '#f1e51d', '#f8e621', '#fde725'
    )

    if (useLogScale) {

        minVal <- log10(minVal)
        maxVal <- log10(maxVal)

    }

    seq <- seq(minVal,maxVal,length.out = length(viridis))

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
    plot_width = 800,
    plot_height = 600,
    plot_type = "svg",
    axis_size = 12,
    unfolding_fitted_data = NULL,
    x_legend_pos = 1,
    y_legend_pos = 1,
    color_bar_length = 0.5,
    color_bar_orientation = "h",
    show_colorbar = TRUE,
    show_grid_x = FALSE,
    show_grid_y = FALSE,
    marker_size = 2,
    line_width = 2,
    max_points = 2000,
    n_xticks = 6,
    n_yticks = 6,
    tick_length = 8,
    tick_width = 2,
    derivative = FALSE,
    baseline_df = NULL,
    temp_units_str = "°C") {

  # Select at most max_points
  n_rows <- nrow(signal_df)
  extra_hover_text <- " (M)"

  out_n <- min(max_points, n_rows)

  idx <- seq(1, n_rows, length.out = out_n)
  idx <- round(idx)
  idx <- unique(idx)

  signal_df <- signal_df[idx, ]

  if (!is.null(unfolding_fitted_data)) {
    unfolding_fitted_data <- unfolding_fitted_data[idx, ]
  }

  signal_df$Denaturant <- signif(signal_df$Denaturant, 3)

  x_axis_label <- paste0("Temperature (", temp_units_str, ")")

  min_x <- min(signal_df$Temperature) - 5
  max_x <- max(signal_df$Temperature) + 5

  xticks_pos <- nice_temperature_ticks_05(min_x + 5, max_x - 5, n_ticks = n_xticks)

  xaxis_config <- list(
    title = list(text = x_axis_label, font = list(size = axis_size)),
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

  expand_y_factor <- 0.1
  expand_y_pos <- 1 + expand_y_factor
  expand_y_neg <- 1 - expand_y_factor

  unique_labels <- unique(signal_df$Label)
  n_labels <- length(unique_labels)

  plot_list <- list()

  min_z <- min(signal_df$Denaturant)
  max_z <- max(signal_df$Denaturant)
  need_color_bar <- min_z != max_z

  global_min_s <- min(signal_df$Signal)
  global_max_s <- max(signal_df$Signal)

  for (i in seq_len(n_labels)) {
    current_label <- unique_labels[i]
    label_data <- signal_df[signal_df$Label == current_label, ]

    p <- plot_ly()

    if (!derivative) {
      if (!is.null(unfolding_fitted_data)) {
        label_fitted <- unfolding_fitted_data[unfolding_fitted_data$Label == current_label, ]
        groups <- unique(label_data$ID)

        if (!is.null(baseline_df)) {

          # Columns are - Temperature, Signal, Baseline_type, Label
          baseline_df_native <- baseline_df[baseline_df[,3] == "Native" & baseline_df[,4] == current_label, ]
          baseline_df_unfolded <- baseline_df[baseline_df[,3] == "Unfolded" & baseline_df[,4] == current_label, ]

          p <- p %>% add_trace(
            x = baseline_df_native[,1],
            y = baseline_df_native[,2],
            color = I("red"),
            type = "scatter",
            mode = "lines",
            showlegend = FALSE,
            line = list(width = line_width, dash = "dash")
          )

          p <- p %>% add_trace(
            x = baseline_df_unfolded[,1],
            y = baseline_df_unfolded[,2],
            color = I("red"),
            type = "scatter",
            mode = "lines",
            showlegend = FALSE,
            line = list(width = line_width, dash = "dash")
          )

        }

        for (j in seq_along(groups)) {
          group <- groups[j]
          group_df <- label_fitted[label_fitted$ID == group, ]
          group_df <- group_df[order(group_df$Temperature), ]

          p <- p %>% add_trace(
            data = group_df,
            x = ~Temperature,
            y = ~Signal,
            color = I("black"),
            type = "scatter",
            mode = "lines",
            showlegend = FALSE,
            line = list(width = line_width)
          )
        }
      }

      groups <- unique(label_data$ID)
      for (j in seq_along(groups)) {
        group <- groups[j]
        group_df <- label_data[label_data$ID == group, ]

        denaturant_val <- unique(group_df$Denaturant)[1]
        hex_color <- get_colors_from_numeric_values(
          denaturant_val, min_z, max_z, useLogScale = FALSE
        )

        p <- p %>% add_trace(
          data = group_df,
          x = ~Temperature,
          y = ~Signal,
          type = "scatter",
          mode = "markers",
          color = I(hex_color),
          showlegend = FALSE,
          text = ~paste0(Denaturant, extra_hover_text),
          name = "",
          hoverinfo = "text+x+y",
          marker = list(size = marker_size)
        )
      }

    } else {
      groups <- unique(label_data$ID)

      for (j in seq_along(groups)) {
        group <- groups[j]
        group_df <- label_data[label_data$ID == group, ]

        denaturant_val <- unique(group_df$Denaturant)[1]
        hex_color <- get_colors_from_numeric_values(
          denaturant_val, min_z, max_z, useLogScale = FALSE
        )

        p <- p %>% add_trace(
          data = group_df,
          x = ~Temperature,
          y = ~Signal,
          type = "scatter",
          mode = "lines",
          showlegend = FALSE,
          text = ~paste0(Denaturant, extra_hover_text),
          name = "",
          hoverinfo = "text+x+y",
          line = list(width = line_width, color = hex_color)
        )
      }
    }

    if (i == 1 && need_color_bar) {
      df <- data.frame(x = min_x, y = global_min_s, values = c(min_z, max_z))
      p <- p %>% add_trace(
        data = df,
        x = ~x,
        y = ~y,
        type = "scatter",
        mode = "markers",
        color = ~values,
        marker = list(size = 0.001),
        showlegend = FALSE
      )
    }

    plot_list[[i]] <- p
  }

  fig <- subplot(plot_list, nrows = n_labels, shareX = TRUE, titleY = TRUE, margin = 0.05)

  layout_updates <- list(
    font = list(family = "Roboto")
  )

  for (i in seq_len(n_labels)) {
    current_label <- unique_labels[i]
    label_data <- signal_df[signal_df$Label == current_label, ]

    min_s_label <- min(label_data$Signal)
    max_s_label <- max(label_data$Signal)

    min_s_label <- if (min_s_label > 0) min_s_label * expand_y_neg else min_s_label * expand_y_pos
    max_s_label <- if (max_s_label > 0) max_s_label * expand_y_pos else max_s_label * expand_y_neg

    yticks_pos_label <- get_axis_ticks(min_s_label, max_s_label, n_ticks = n_yticks)

    yaxis_config_label <- list(
      title = list(text = current_label, font = list(size = axis_size)),
      tickfont = list(size = axis_size),
      range = c(min_s_label, max_s_label),
      showgrid = show_grid_y,
      showline = TRUE,
      zeroline = FALSE,
      ticks = "outside",
      tickwidth = tick_width,
      ticklen = tick_length,
      tickmode = "array",
      tickvals = yticks_pos_label,
      ticktext = format_axis_labels(yticks_pos_label, sig = 3)
    )

    x_suffix <- if (i == 1) "" else as.character(i)
    layout_updates[[paste0("xaxis", x_suffix)]] <- xaxis_config
    layout_updates[[paste0("yaxis", x_suffix)]] <- yaxis_config_label
  }

  fig <- do.call(plotly::layout, c(list(fig), layout_updates))

  if (show_colorbar && need_color_bar) {
    tickvals <- c(min_z, min_z + (max_z - min_z) / 2, max_z)
    ticktext <- c(min_z, round(min_z + (max_z - min_z) / 2, 2), max_z)

    fig <- fig %>% colorbar(
      title = list(
        text = "[Denaturant] (M)",
        font = list(size = axis_size - 1)
      ),
      x = x_legend_pos,
      y = y_legend_pos,
      xanchor = "right",
      yanchor = "top",
      tickvals = tickvals,
      ticktext = ticktext,
      tickfont = list(size = axis_size - 2),
      len = color_bar_length,
      orientation = color_bar_orientation,
      outlinewidth = 0
    )
  } else if (need_color_bar) {
    fig <- fig %>% hide_colorbar()
  }

  fig <- config_fig(
    fig,
    filename = "Signal_versus_temperature",
    plot_type = plot_type,
    plot_width = plot_width,
    plot_height = plot_height
  )

  return(fig)
}

plot_fluo_signal_residuals <- function(
    signal_df,
    plot_width = 12,
    plot_height = 8,
    plot_type = "svg",
    axis_size = 12,
    unfolding_fitted_data = NULL,
    x_legend_pos = 1,
    y_legend_pos = 1,
    color_bar_length = 0.5,
    color_bar_orientation = "h",
    show_colorbar = TRUE,
    show_grid_x = FALSE,
    show_grid_y = FALSE,
    marker_size = 2,
    line_width = 2,
    max_points = 2000,
    n_xticks = 6,
    n_yticks = 6,
    tick_length = 8,
    tick_width = 2,
    temp_units_str = "°C") {

  if (is.null(unfolding_fitted_data)) {
    stop("`unfolding_fitted_data` must be provided to compute residuals.")
  }

  n_rows <- nrow(signal_df)
  extra_hover_text <- " (M)"

  idx <- seq(1, n_rows, length.out = min(max_points, n_rows))
  idx <- round(idx)
  idx <- unique(idx)

  signal_df <- signal_df[idx, ]
  unfolding_fitted_data <- unfolding_fitted_data[idx, ]

  signal_df$Denaturant <- signif(signal_df$Denaturant, 3)

  x_axis_label <- paste0("Temperature (", temp_units_str, ")")

  min_x <- min(signal_df$Temperature) - 5
  max_x <- max(signal_df$Temperature) + 5

  xticks_pos <- nice_temperature_ticks_05(min_x + 5, max_x - 5, n_ticks = n_xticks)

  xaxis_config <- list(
    title = list(text = x_axis_label, font = list(size = axis_size)),
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

  names(unfolding_fitted_data)[names(unfolding_fitted_data) == "Signal"] <- "Signal_fit"

  signal_df <- merge(
    signal_df,
    unfolding_fitted_data[, c("ID", "Temperature", "Denaturant", "Signal_fit", "Label")],
    by = c("ID", "Temperature", "Denaturant", "Label")
  )

  signal_df$residuals <- signal_df$Signal - signal_df$Signal_fit

  expand_y_factor <- 0.12
  expand_y_pos <- 1 + expand_y_factor
  expand_y_neg <- 1 - expand_y_factor

  unique_labels <- unique(signal_df$Label)
  n_labels <- length(unique_labels)

  plot_list <- list()

  min_z <- min(signal_df$Denaturant)
  max_z <- max(signal_df$Denaturant)
  need_color_bar <- min_z != max_z

  global_min_r <- min(signal_df$residuals)
  global_max_r <- max(signal_df$residuals)

  for (i in seq_len(n_labels)) {
    current_label <- unique_labels[i]
    label_data <- signal_df[signal_df$Label == current_label, ]

    p <- plot_ly()

    groups <- unique(label_data$ID)
    for (j in seq_along(groups)) {
      group <- groups[j]
      group_df <- label_data[label_data$ID == group, ]

      denaturant_val <- unique(group_df$Denaturant)[1]
      hex_color <- get_colors_from_numeric_values(
        denaturant_val, min_z, max_z, useLogScale = FALSE
      )

      p <- p %>% add_trace(
        data = group_df,
        x = ~Temperature,
        y = ~residuals,
        type = "scatter",
        mode = "markers",
        color = I(hex_color),
        showlegend = FALSE,
        text = ~paste0(Denaturant, extra_hover_text),
        name = "",
        hoverinfo = "text+x+y",
        marker = list(size = marker_size)
      )
    }

    if (i == 1 && need_color_bar) {
      df <- data.frame(x = min_x, y = global_min_r, values = c(min_z, max_z))
      p <- p %>% add_trace(
        data = df,
        x = ~x,
        y = ~y,
        type = "scatter",
        mode = "markers",
        color = ~values,
        marker = list(size = 0.001),
        showlegend = FALSE
      )
    }

    plot_list[[i]] <- p
  }

  fig <- subplot(plot_list, nrows = n_labels, shareX = TRUE, titleY = TRUE, margin = 0.05)

  layout_updates <- list(
    font = list(family = "Roboto")
  )

  shapes_list <- list()

  for (i in seq_len(n_labels)) {
    current_label <- unique_labels[i]
    label_data <- signal_df[signal_df$Label == current_label, ]

    min_r_label <- min(label_data$residuals)
    max_r_label <- max(label_data$residuals)

    min_r_label <- if (min_r_label > 0) min_r_label * expand_y_neg else min_r_label * expand_y_pos
    max_r_label <- if (max_r_label > 0) max_r_label * expand_y_pos else max_r_label * expand_y_neg

    yticks_pos_label <- get_axis_ticks(min_r_label, max_r_label, n_ticks = n_yticks)

    yaxis_config_label <- list(
      title = list(text = paste(current_label, "- Residuals"), font = list(size = axis_size)),
      tickfont = list(size = axis_size),
      range = c(min_r_label, max_r_label),
      showgrid = show_grid_y,
      showline = TRUE,
      zeroline = FALSE,
      ticks = "outside",
      tickwidth = tick_width,
      ticklen = tick_length,
      tickmode = "array",
      tickvals = yticks_pos_label,
      ticktext = format_axis_labels(yticks_pos_label, sig = 3)
    )

    x_axis_name <- if (i == 1) "xaxis" else paste0("xaxis", i)
    y_axis_name <- if (i == 1) "yaxis" else paste0("yaxis", i)

    layout_updates[[x_axis_name]] <- xaxis_config
    layout_updates[[y_axis_name]] <- yaxis_config_label

    y_ref <- if (i == 1) "y" else paste0("y", i)
    shapes_list[[i]] <- list(
      type = "line",
      x0 = min_x,
      x1 = max_x,
      y0 = 0,
      y1 = 0,
      yref = y_ref,
      line = list(
        color = "red",
        width = 2,
        dash = "dash"
      )
    )
  }

  layout_updates$shapes <- shapes_list

  fig <- do.call(plotly::layout, c(list(fig), layout_updates))

  if (show_colorbar && need_color_bar) {
    tickvals <- c(min_z, min_z + (max_z - min_z) / 2, max_z)
    ticktext <- c(min_z, round(min_z + (max_z - min_z) / 2, 2), max_z)

    fig <- fig %>% colorbar(
      title = list(
        text = "[Denaturant] (M)",
        font = list(size = axis_size - 1)
      ),
      x = x_legend_pos,
      y = y_legend_pos,
      xanchor = "right",
      yanchor = "top",
      tickvals = tickvals,
      ticktext = ticktext,
      tickfont = list(size = axis_size - 2),
      len = color_bar_length,
      orientation = color_bar_orientation,
      outlinewidth = 0
    )
  } else if (need_color_bar) {
    fig <- fig %>% hide_colorbar()
  }

  fig <- config_fig(
    fig,
    filename = "Residuals_versus_temperature",
    plot_type = plot_type,
    plot_width = plot_width,
    plot_height = plot_height
  )

  return(fig)
}


plot_2d <- function(
    df,
    plot_width = 12,
    plot_height = 8,
    plot_type = "svg",
    axis_size = 12,
    show_grid_x = FALSE,
    show_grid_y = FALSE,
    marker_size = 2,
    n_xticks = 6,
    n_yticks = 6,
    tick_length = 8,
    tick_width = 2,
    x_label = "Denaturant (M)",
    y_label = "T<sub>m</sub> (ºC) / 1st derivative",
    y_labels = NULL,
    filename = "Tm_versus_denaturant",
    y_zeroline = FALSE,
    y_zeroline_color = "red",
    y_zeroline_width = 2) {

    is_list <- is.list(df) && !is.data.frame(df)

    if (is_list) {
        n_plots <- length(df)
        plot_list <- list()

        if (!is.null(y_labels)) {
            plot_names <- y_labels
        } else {
            plot_names <- names(df)
            if (is.null(plot_names) || all(plot_names == "")) {
                plot_names <- sapply(seq_len(n_plots), function(i) {
                    col_name <- colnames(df[[i]])[1]
                    if (!is.null(col_name) && col_name != "") {
                        col_name
                    } else {
                        paste0(y_label, " ", i)
                    }
                })
            }
        }

        expand_y_factor <- 0.1
        expand_y_pos <- 1 + expand_y_factor
        expand_y_neg <- 1 - expand_y_factor

        all_x <- unlist(lapply(df, function(d) d[, 2]))
        min_x <- min(all_x)
        max_x <- max(all_x)
        x_span <- max_x - min_x
        x_pad <- if (x_span > 0) x_span * 0.01 else 0.01
        x_range <- c(min_x - x_pad, max_x + x_pad)
        xticks_pos <- get_axis_ticks(x_range[1], x_range[2], n_ticks = n_xticks)

        for (i in seq_len(n_plots)) {
            current_df <- df[[i]]
            current_name <- plot_names[i]

            p <- plot_ly(
                x = current_df[, 2],
                y = current_df[, 1],
                type = "scatter",
                mode = "markers",
                showlegend = FALSE,
                marker = list(size = marker_size)
            )

            plot_list[[i]] <- p
        }

        fig <- subplot(
            plot_list,
            nrows = n_plots,
            shareX = TRUE,
            titleY = TRUE,
            margin = 0.05
        )

        layout_updates <- list(
            font = list(family = "Roboto")
        )

        xaxis_config <- list(
            title = list(text = x_label, font = list(size = axis_size)),
            tickfont = list(size = axis_size),
            range = x_range,
            showgrid = show_grid_x,
            showline = TRUE,
            zeroline = FALSE,
            ticks = "outside",
            tickwidth = tick_width,
            ticklen = tick_length,
            tickmode = "array",
            tickvals = xticks_pos,
            ticktext = format_axis_labels(xticks_pos, sig = 3)
        )

        for (i in seq_len(n_plots)) {
            current_df <- df[[i]]
            current_name <- plot_names[i]

            min_y <- min(current_df[, 1])
            max_y <- max(current_df[, 1])
            min_y <- ifelse(min_y > 0, min_y * expand_y_neg, min_y * expand_y_pos)
            max_y <- ifelse(max_y > 0, max_y * expand_y_pos, max_y * expand_y_neg)
            yticks_pos <- get_axis_ticks(min_y, max_y, n_ticks = n_yticks)

            yaxis_config <- list(
                title = list(text = current_name, font = list(size = axis_size)),
                tickfont = list(size = axis_size),
                range = c(min_y, max_y),
                showgrid = show_grid_y,
                showline = TRUE,
                zeroline = isTRUE(y_zeroline),
                zerolinecolor = y_zeroline_color,
                zerolinewidth = y_zeroline_width,
                ticks = "outside",
                tickwidth = tick_width,
                ticklen = tick_length,
                tickmode = "array",
                tickvals = yticks_pos,
                ticktext = format_axis_labels(yticks_pos, sig = 3)
            )

            axis_suffix <- if (i == 1) "" else as.character(i)
            layout_updates[[paste0("xaxis", axis_suffix)]] <- xaxis_config
            layout_updates[[paste0("yaxis", axis_suffix)]] <- yaxis_config
        }

        fig <- do.call(plotly::layout, c(list(fig), layout_updates))

    } else {
        fig <- plot_ly(
            x = df[, 2],
            y = df[, 1],
            type = "scatter",
            mode = "markers",
            showlegend = FALSE,
            marker = list(size = marker_size)
        )

        min_x <- min(df[, 2])
        max_x <- max(df[, 2])
        x_span <- max_x - min_x
        x_pad <- if (x_span > 0) x_span * 0.01 else 0.01
        x_range <- c(min_x - x_pad, max_x + x_pad)
        xticks_pos <- get_axis_ticks(x_range[1], x_range[2], n_ticks = n_xticks)
        yticks_pos <- get_axis_ticks(min(df[, 1]), max(df[, 1]), n_ticks = n_yticks)

        xaxis <- list(
            title = list(text = x_label, font = list(size = axis_size)),
            tickfont = list(size = axis_size),
            showgrid = show_grid_x,
            showline = TRUE,
            zeroline = FALSE,
            ticks = "outside",
            tickwidth = tick_width,
            ticklen = tick_length,
            tickmode = "array",
            range = x_range,
            tickvals = xticks_pos,
            ticktext = format_axis_labels(xticks_pos, sig = 3)
        )

        yaxis <- list(
            title = list(text = y_label, font = list(size = axis_size)),
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
            tickvals = yticks_pos,
            ticktext = format_axis_labels(yticks_pos, sig = 3)
        )

        fig <- fig %>% layout(
            xaxis = xaxis,
            yaxis = yaxis,
            font = list(family = "Roboto")
        )
    }

    fig <- config_fig(
        fig,
        filename = filename,
        plot_type = plot_type,
        plot_width = plot_width,
        plot_height = plot_height
    )

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
    tick_width = 2) {

    # For each group in signal_df, get the first signal value
    # and the corresponding denaturant value
    initial_signal_df <- signal_df %>%
        group_by(ID, Label) %>%
        summarise(
            Initial_Signal = first(Signal),
            Denaturant     = first(Denaturant),
            .groups = 'drop')

    # Check if there are multiple labels
    unique_labels <- unique(initial_signal_df$Label)
    
    if (length(unique_labels) > 1) {
        # Multiple labels - create list of dataframes for subplots
        df_list <- lapply(unique_labels, function(label) {
            label_data <- initial_signal_df[initial_signal_df$Label == label, ]
            # Return dataframe with Initial_Signal as first column (y), Denaturant as second (x)
            as.data.frame(label_data)[,c("Initial_Signal", "Denaturant")]
        })
        
        # Create y-labels combining base text with Label values
        y_labels <- paste0("Initial signal (a.u.) - ", unique_labels)
        
        fig <- plot_2d(
            df = df_list,
            y_labels = y_labels,
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
    } else {
        # Single label - use original single plot approach
        initial_signal_df <- as.data.frame(initial_signal_df)[,c("Initial_Signal", "Denaturant")]
        
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
    }

    return(fig)

}

plot_fits_and_residuals <- function(
    signal_df,
    plot_width = 12,
    plot_height = 8,
    plot_type = "svg",
    axis_size = 12,
    unfolding_fitted_data,
    x_legend_pos = 1,
    y_legend_pos = 1,
    color_bar_length = 0.5,
    color_bar_orientation = "h",
    show_colorbar = TRUE,
    show_grid_x = FALSE,
    show_grid_y = FALSE,
    marker_size = 2,
    line_width = 2,
    max_points = 2000,
    n_xticks = 6,
    n_yticks = 6,
    tick_length = 8,
    tick_width = 2,
    temp_units_str = "°C"
) {

    n_rows <- nrow(signal_df)
    extra_hover_text <- " (M)"

    # ----------------------------
    # Subsample points
    # ----------------------------
    idx <- seq(1, n_rows, length.out = min(max_points, n_rows))
    idx <- unique(round(idx))

    signal_df <- signal_df[idx, ]
    unfolding_fitted_data <- unfolding_fitted_data[idx, ]

    signal_df$Denaturant <- signif(signal_df$Denaturant, 3)

    # ----------------------------
    # X axis configuration
    # ----------------------------
    x_axis_label <- paste0("Temperature (", temp_units_str, ")")
    min_x <- min(signal_df$Temperature) - 5
    max_x <- max(signal_df$Temperature) + 5
    xticks_pos <- nice_temperature_ticks_05(min_x + 5, max_x - 5, n_ticks = n_xticks)

    xaxis_config <- list(
        title = list(text = x_axis_label, font = list(size = axis_size)),
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

    # ----------------------------
    # Global color scale settings
    # ----------------------------
    min_z <- min(signal_df$Denaturant)
    max_z <- max(signal_df$Denaturant)
    need_color_bar <- min_z != max_z

    # ----------------------------
    # Create subplots
    # Row 1 = fits/signals
    # Row 2 = residuals
    # ----------------------------
    unique_labels <- unique(signal_df$Label)
    n_labels <- length(unique_labels)

    signal_plot_list <- list()
    residual_plot_list <- list()

    expand_y_factor_signal <- 0.12
    expand_y_factor_res <- 0.06

    for (i in seq_len(n_labels)) {
        current_label <- unique_labels[i]
        label_signal_df <- signal_df[signal_df$Label == current_label, ]
        label_fitted_df <- unfolding_fitted_data[unfolding_fitted_data$Label == current_label, ]

        # ============================================================
        # SIGNAL + FIT PLOT
        # ============================================================
        min_s <- min(label_signal_df$Signal)
        max_s <- max(label_signal_df$Signal)
        min_s <- ifelse(min_s > 0, min_s * (1 - expand_y_factor_signal), min_s * (1 + expand_y_factor_signal))
        max_s <- ifelse(max_s > 0, max_s * (1 + expand_y_factor_signal), max_s * (1 - expand_y_factor_signal))
        yticks_pos_signal <- get_axis_ticks(min_s, max_s, n_ticks = n_yticks)

        p_signal <- plot_ly()

        # Colored data points
        if (i == 1 && need_color_bar) {
            p_signal <- p_signal %>%
                add_trace(
                    data = label_signal_df,
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
                            tickfont = list(size = axis_size - 2),
                            len = color_bar_length,
                            x = x_legend_pos,
                            y = y_legend_pos,
                            xanchor = "right",
                            yanchor = "top",
                            tickvals = c(min_z, round((min_z + max_z) / 2, 2), max_z),
                            ticktext = c(min_z, round((min_z + max_z) / 2, 2), max_z),
                            orientation = color_bar_orientation,
                            outlinewidth = 0
                        )
                    ),
                    showlegend = FALSE
                )
        } else {
            groups <- unique(label_signal_df$ID)
            for (j in seq_along(groups)) {
                group <- groups[j]
                group_df <- label_signal_df[label_signal_df$ID == group, ]
                denaturant_val <- unique(group_df$Denaturant)[1]
                hex_color <- get_colors_from_numeric_values(
                    denaturant_val, min_z, max_z, useLogScale = FALSE
                )

                p_signal <- p_signal %>%
                    add_trace(
                        data = group_df,
                        x = ~Temperature,
                        y = ~Signal,
                        type = "scatter",
                        mode = "markers",
                        color = I(hex_color),
                        showlegend = FALSE,
                        marker = list(size = marker_size),
                        text = ~paste0(Denaturant, extra_hover_text),
                        hoverinfo = "text+x+y"
                    )
            }
        }

        # Fitted curves
        groups <- unique(label_signal_df$ID)
        for (group in groups) {
            group_df <- label_fitted_df[label_fitted_df$ID == group, ]
            group_df <- group_df[order(group_df$Temperature), ]

            p_signal <- p_signal %>%
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

        p_signal <- p_signal %>%
            layout(
                yaxis = list(
                    title = list(text = current_label, font = list(size = axis_size)),
                    tickfont = list(size = axis_size),
                    range = c(min_s, max_s),
                    showgrid = show_grid_y,
                    showline = TRUE,
                    zeroline = FALSE,
                    ticks = "outside",
                    tickwidth = tick_width,
                    ticklen = tick_length,
                    tickmode = "array",
                    tickvals = yticks_pos_signal,
                    ticktext = format_axis_labels(yticks_pos_signal, sig = 3)
                )
            )

        signal_plot_list[[i]] <- p_signal

        # ============================================================
        # RESIDUALS PLOT
        # ============================================================
        residual_df <- merge(
            label_signal_df,
            label_fitted_df,
            by = c("ID", "Temperature", "Denaturant", "Label"),
            suffixes = c("_obs", "_fit")
        )
        residual_df$Residual <- residual_df$Signal_obs - residual_df$Signal_fit

        min_r <- min(residual_df$Residual)
        max_r <- max(residual_df$Residual)
        min_r <- ifelse(min_r > 0, min_r * (1 - expand_y_factor_res), min_r * (1 + expand_y_factor_res))
        max_r <- ifelse(max_r > 0, max_r * (1 + expand_y_factor_res), max_r * (1 - expand_y_factor_res))
        yticks_pos_residual <- get_axis_ticks(min_r, max_r, n_ticks = n_yticks)

        p_residual <- plot_ly()

        groups <- unique(residual_df$ID)
        for (j in seq_along(groups)) {
            group <- groups[j]
            group_df <- residual_df[residual_df$ID == group, ]
            denaturant_val <- unique(group_df$Denaturant)[1]
            hex_color <- get_colors_from_numeric_values(
                denaturant_val, min_z, max_z, useLogScale = FALSE
            )

            p_residual <- p_residual %>%
                add_trace(
                    data = group_df,
                    x = ~Temperature,
                    y = ~Residual,
                    type = "scatter",
                    mode = "markers",
                    color = I(hex_color),
                    showlegend = FALSE,
                    hoverinfo = "x+y",
                    marker = list(size = marker_size)
                )
        }

        p_residual <- p_residual %>%
            layout(
                yaxis = list(
                    title = list(
                        text = paste(current_label, "- Residuals"),
                        font = list(size = axis_size)
                    ),
                    tickfont = list(size = axis_size),
                    range = c(min_r, max_r),
                    showgrid = show_grid_y,
                    showline = TRUE,
                    zeroline = FALSE,
                    ticks = "outside",
                    tickwidth = tick_width,
                    ticklen = tick_length,
                    tickmode = "array",
                    tickvals = yticks_pos_residual,
                    ticktext = format_axis_labels(yticks_pos_residual, sig = 3)
                )
            )

        residual_plot_list[[i]] <- p_residual
    }

    # ----------------------------
    # Combine panels
    # ----------------------------
    all_plots <- c(signal_plot_list, residual_plot_list)

    fig <- subplot(
        all_plots,
        nrows = 2,
        heights = c(0.7, 0.3),
        shareX = TRUE,
        titleY = TRUE,
        margin = 0.05
    )

    # ----------------------------
    # Layout updates
    # ----------------------------
    layout_updates <- list(
        font = list(family = "Roboto")
    )

    shapes_list <- list()

    for (i in seq_len(n_labels)) {
        current_label <- unique_labels[i]
        label_signal_df <- signal_df[signal_df$Label == current_label, ]
        label_fitted_df <- unfolding_fitted_data[unfolding_fitted_data$Label == current_label, ]

        residual_df <- merge(
            label_signal_df,
            label_fitted_df,
            by = c("ID", "Temperature", "Denaturant", "Label"),
            suffixes = c("_obs", "_fit")
        )
        residual_df$Residual <- residual_df$Signal_obs - residual_df$Signal_fit

        # Signal axis range
        min_s <- min(label_signal_df$Signal)
        max_s <- max(label_signal_df$Signal)
        min_s <- ifelse(min_s > 0, min_s * (1 - expand_y_factor_signal), min_s * (1 + expand_y_factor_signal))
        max_s <- ifelse(max_s > 0, max_s * (1 + expand_y_factor_signal), max_s * (1 - expand_y_factor_signal))
        yticks_pos_signal <- get_axis_ticks(min_s, max_s, n_ticks = n_yticks)

        # Residual axis range
        min_r <- min(residual_df$Residual)
        max_r <- max(residual_df$Residual)
        min_r <- ifelse(min_r > 0, min_r * (1 - expand_y_factor_res), min_r * (1 + expand_y_factor_res))
        max_r <- ifelse(max_r > 0, max_r * (1 + expand_y_factor_res), max_r * (1 - expand_y_factor_res))
        yticks_pos_residual <- get_axis_ticks(min_r, max_r, n_ticks = n_yticks)

        # Top row axes
        x_axis_name_signal <- if (i == 1) "xaxis" else paste0("xaxis", i)
        y_axis_name_signal <- if (i == 1) "yaxis" else paste0("yaxis", i)

        layout_updates[[x_axis_name_signal]] <- xaxis_config
        layout_updates[[y_axis_name_signal]] <- list(
            title = list(text = current_label, font = list(size = axis_size)),
            tickfont = list(size = axis_size),
            range = c(min_s, max_s),
            showgrid = show_grid_y,
            showline = TRUE,
            zeroline = FALSE,
            ticks = "outside",
            tickwidth = tick_width,
            ticklen = tick_length,
            tickmode = "array",
            tickvals = yticks_pos_signal,
            ticktext = format_axis_labels(yticks_pos_signal, sig = 3)
        )

        # Bottom row axes
        residual_pos <- n_labels + i
        x_axis_name_residual <- if (residual_pos == 1) "xaxis" else paste0("xaxis", residual_pos)
        y_axis_name_residual <- if (residual_pos == 1) "yaxis" else paste0("yaxis", residual_pos)

        layout_updates[[x_axis_name_residual]] <- xaxis_config
        layout_updates[[y_axis_name_residual]] <- list(
            title = list(
                text = paste(current_label, "- Residuals"),
                font = list(size = axis_size)
            ),
            tickfont = list(size = axis_size),
            range = c(min_r, max_r),
            showgrid = show_grid_y,
            showline = TRUE,
            zeroline = FALSE,
            ticks = "outside",
            tickwidth = tick_width,
            ticklen = tick_length,
            tickmode = "array",
            tickvals = yticks_pos_residual,
            ticktext = format_axis_labels(yticks_pos_residual, sig = 3)
        )

        y_ref <- if (residual_pos == 1) "y" else paste0("y", residual_pos)
        shapes_list[[i]] <- list(
            type = "line",
            x0 = min_x,
            x1 = max_x,
            y0 = 0,
            y1 = 0,
            yref = y_ref,
            line = list(
                color = "red",
                width = 2,
                dash = "dash"
            )
        )
    }

    layout_updates$shapes <- shapes_list

    fig <- do.call(plotly::layout, c(list(fig), layout_updates))

    # ----------------------------
    # Colorbar
    # ----------------------------
    if (show_colorbar && need_color_bar) {
        tickvals <- c(min_z, min_z + (max_z - min_z) / 2, max_z)
        ticktext <- c(min_z, round(min_z + (max_z - min_z) / 2, 2), max_z)

        fig <- fig %>% colorbar(
            title = list(
                text = "[Denaturant] (M)",
                font = list(size = axis_size - 1)
            ),
            x = x_legend_pos,
            y = y_legend_pos,
            xanchor = "right",
            yanchor = "top",
            tickvals = tickvals,
            ticktext = ticktext,
            tickfont = list(size = axis_size - 2),
            len = color_bar_length,
            orientation = color_bar_orientation,
            outlinewidth = 0
        )
    } else if (need_color_bar) {
        fig <- fig %>% hide_colorbar()
    }

    fig <- config_fig(
        fig,
        filename = "Signal_and_residuals",
        plot_type = plot_type,
        plot_width = plot_width,
        plot_height = plot_height
    )

    return(fig)
}
