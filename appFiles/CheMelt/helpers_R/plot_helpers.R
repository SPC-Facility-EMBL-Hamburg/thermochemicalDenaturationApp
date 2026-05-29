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
    plot_width=800,
    plot_height=600,
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
    tick_width = 2,
    derivative = FALSE){

    # Select at most max_points
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

    # Common x-axis configuration
    xaxis_config <- list(
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

    # Create subplots - one per label
    unique_labels <- unique(signal_df$Label)
    n_labels <- length(unique_labels)
    
    # Create a list to store individual plots
    plot_list <- list()
    
    # Global min/max for denaturant to ensure consistent colorbar
    min_z <- min(signal_df$Denaturant)
    max_z <- max(signal_df$Denaturant)
    need_color_bar <- min_z != max_z
    
    # Global min/max for signal (used for invisible trace positioning)
    global_min_s <- min(signal_df$Signal)
    global_max_s <- max(signal_df$Signal)
    
    for (i in 1:n_labels) {
        current_label <- unique_labels[i]
        label_data <- signal_df[signal_df$Label == current_label, ]
        
        # Create a plot for this label
        p <- plot_ly()
        
        # Use markers for signal, lines for derivative
        if (!derivative) {
            # Plot the fitting as black lines if unfolding_fitted_data is provided
            if (!is.null(unfolding_fitted_data)) {
                label_fitted <- unfolding_fitted_data[unfolding_fitted_data$Label == current_label, ]
                groups <- unique(label_data$ID)
                
                for (j in 1:length(groups)) {
                    group <- groups[j]
                    group_df <- label_fitted[label_fitted$ID == group, ]
                    group_df <- group_df[order(group_df$Temperature), ]
                    
                    p <- p %>% add_trace(
                        data = group_df,
                        x = ~Temperature,
                        y = ~Signal,
                        color = I('black'),
                        type = "scatter",
                        mode = "lines",
                        showlegend = FALSE,
                        line = list(width = line_width))
                }
            }
            
            # Add the data points with explicit colors
            # Group by ID and add one trace per group to ensure proper coloring
            groups <- unique(label_data$ID)
            for (j in 1:length(groups)) {
                group <- groups[j]
                group_df <- label_data[label_data$ID == group, ]
                
                # Get the color for this denaturant concentration
                denaturant_val <- unique(group_df$Denaturant)[1]
                hex_color <- get_colors_from_numeric_values(denaturant_val, min_z, max_z, useLogScale = FALSE)
                
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
                    hoverinfo = 'text+x+y',
                    marker = list(size = marker_size))
            }
            
        } else {
            # Derivative mode - plot lines
            groups <- unique(label_data$ID)
            
            for (j in 1:length(groups)) {
                group <- groups[j]
                group_df <- label_data[label_data$ID == group, ]
                
                # Get the color for this denaturant concentration
                denaturant_val <- unique(group_df$Denaturant)[1]
                hex_color <- get_colors_from_numeric_values(denaturant_val, min_z, max_z, useLogScale = FALSE)
                
                p <- p %>% add_trace(
                    data = group_df,
                    x = ~Temperature,
                    y = ~Signal,
                    type = "scatter",
                    mode = "lines",
                    showlegend = FALSE,
                    text = ~paste0(Denaturant, extra_hover_text),
                    name = "",
                    hoverinfo = 'text+x+y',
                    line = list(width = line_width, color = hex_color))
            }
        }
        
        # Add invisible trace for colorbar only on the first subplot
        if (i == 1 && need_color_bar) {
            df <- data.frame(x = min_x, y = global_min_s, values = c(min_z, max_z))
            p <- p %>% add_trace(
                data = df,
                x = ~x,
                y = ~y,
                type = 'scatter',
                mode = 'markers',
                color = ~values,
                marker = list(size = 0.001),
                showlegend = FALSE)
        }
        
        # Set y-axis title for this subplot using the Label value
        p <- p %>% layout(yaxis = list(title = current_label))
        
        plot_list[[i]] <- p
    }
    
    # Combine plots using subplot
    fig <- subplot(plot_list, nrows = n_labels, shareX = TRUE, titleY = TRUE, margin = 0.05)
    
    # Update layout for all subplots
    layout_updates <- list(font = "Roboto")
    
    # Set axis titles for all subplots
    for (i in 1:n_labels) {
        current_label <- unique_labels[i]
        label_data <- signal_df[signal_df$Label == current_label, ]
        
        # Calculate y-axis range for this specific subplot
        min_s_label <- min(label_data$Signal)
        max_s_label <- max(label_data$Signal)
        min_s_label <- ifelse(min_s_label > 0, min_s_label*expand_y_neg, min_s_label*expand_y_pos)
        max_s_label <- ifelse(max_s_label > 0, max_s_label*expand_y_pos, max_s_label*expand_y_neg)
        yticks_pos_label <- get_axis_ticks(min_s_label, max_s_label, n_ticks = n_yticks)
        
        # Create y-axis configuration for this subplot
        yaxis_config_label <- list(
            titlefont = list(size = axis_size),
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
        
        # X-axis configuration
        x_axis_name <- if (i == 1) "xaxis" else paste0("xaxis", i)
        layout_updates[[x_axis_name]] <- c(xaxis_config, list(title = x_axis_label))
        
        # Y-axis configuration - use the actual Label value as title
        y_axis_name <- if (i == 1) "yaxis" else paste0("yaxis", i)
        layout_updates[[y_axis_name]] <- c(yaxis_config_label, list(title = current_label))
    }
    
    fig <- fig %>% layout(layout_updates)
    
    # Configure the colorbar if needed
    if (show_colorbar && need_color_bar) {
        tickvals <- c(min_z, min_z + (max_z - min_z)/2, max_z)
        ticktext <- c(min_z, round(min_z + (max_z - min_z)/2, 2), max_z)
        
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
            outlinewidth = 0)
    } else if (need_color_bar) {
        # Hide colorbar if show_colorbar is FALSE but we still added the invisible trace
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
    y_labels=NULL,                    # optional vector of y-axis labels for subplots
    filename="Tm_versus_denaturant",
    y_zeroline = FALSE,               # whether to enable y-axis zeroline
    y_zeroline_color = "red",
    y_zeroline_width = 2){

    # Check if df is a list of dataframes
    is_list <- is.list(df) && !is.data.frame(df)
    
    if (is_list) {
        # Multiple dataframes - create subplots
        n_plots <- length(df)
        plot_list <- list()
        
        # Get names for y-axis titles
        # Priority: 1) y_labels argument, 2) list names, 3) column name of first column, 4) y_label parameter
        if (!is.null(y_labels)) {
            plot_names <- y_labels
        } else {
            plot_names <- names(df)
            if (is.null(plot_names) || all(plot_names == "")) {
                # Try to use column names from the dataframes
                plot_names <- sapply(1:n_plots, function(i) {
                    col_name <- colnames(df[[i]])[1]
                    if (!is.null(col_name) && col_name != "") {
                        return(col_name)
                    } else {
                        # Fall back to y_label with index
                        return(paste0(y_label, " ", i))
                    }
                })
            }
        }
        
        # Common x-axis configuration
        xaxis_config <- list(
            titlefont = list(size = axis_size),
            tickfont = list(size = axis_size),
            showgrid = show_grid_x,
            showline = TRUE,
            zeroline = FALSE,
            ticks = "outside",
            tickwidth = tick_width,
            ticklen = tick_length
        )
        
        expand_y_factor <- 0.1
        expand_y_pos <- 1 + expand_y_factor
        expand_y_neg <- 1 - expand_y_factor
        
        for (i in 1:n_plots) {
            current_df <- df[[i]]
            current_name <- plot_names[i]
            
            # Create individual plot
            p <- plot_ly(
                x = current_df[,2], 
                y = current_df[,1],
                type = "scatter", 
                mode = "markers", 
                showlegend = FALSE,
                marker = list(size = marker_size)
            )
            
            # Set y-axis title for this subplot
            p <- p %>% layout(yaxis = list(title = current_name))
            
            plot_list[[i]] <- p
        }
        
        # Combine plots using subplot
        fig <- subplot(plot_list, nrows = n_plots, shareX = TRUE, titleY = TRUE, margin = 0.05)
        
        # Update layout for all subplots
        layout_updates <- list(font = "Roboto")
        
        # Calculate global x-range from all dataframes
        all_x <- unlist(lapply(df, function(d) d[,2]))
        min_x <- min(all_x)
        max_x <- max(all_x)
        xticks_pos <- get_axis_ticks(min_x, max_x, n_ticks = n_xticks)
        
        # Set axis configuration for all subplots
        for (i in 1:n_plots) {
            current_df <- df[[i]]
            current_name <- plot_names[i]
            
            # Calculate y-axis range for this specific subplot
            min_y <- min(current_df[,1])
            max_y <- max(current_df[,1])
            min_y <- ifelse(min_y > 0, min_y * expand_y_neg, min_y * expand_y_pos)
            max_y <- ifelse(max_y > 0, max_y * expand_y_pos, max_y * expand_y_neg)
            yticks_pos <- get_axis_ticks(min_y, max_y, n_ticks = n_yticks)
            
            # Create y-axis configuration for this subplot
            yaxis_config <- list(
                titlefont = list(size = axis_size),
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
            
            # X-axis configuration
            x_axis_name <- if (i == 1) "xaxis" else paste0("xaxis", i)
            layout_updates[[x_axis_name]] <- c(xaxis_config, list(
                title = x_label,
                tickmode = "array",
                tickvals = xticks_pos,
                ticktext = format_axis_labels(xticks_pos, sig = 3)
            ))
            
            # Y-axis configuration
            y_axis_name <- if (i == 1) "yaxis" else paste0("yaxis", i)
            layout_updates[[y_axis_name]] <- c(yaxis_config, list(title = current_name))
        }
        
        fig <- fig %>% layout(layout_updates)
        
    } else {
        # Single dataframe - original behavior
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
    }

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
