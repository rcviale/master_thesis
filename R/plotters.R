group_line_plot <- function(.data,
                            .x,
                            .y,
                            .color,
                            .title,
                            .xlab  = NULL,
                            .ylab  = NULL,
                            .colab = NULL,
                            .path  = NA){
  # This function makes a line plot for each group (time series in this case).
  
  .title <- .data |> 
    dplyr::pull({{ .title }}) |> 
    unique()
  
  ## Pick legend order based on the (renamed) strategy/title
  .title_chr <- as.character(.title)[1]
  legend_breaks <- NULL
  if (grepl("^CS", .title_chr)) {
    legend_breaks <- c("HML", "Short", "Portfolio 2", "Portfolio 3", "Portfolio 4", "Long")
  } else if (grepl("^TS", .title_chr)) {
    legend_breaks <- c("HML", "Short", "Long")
  } else if (.title_chr %in% c("Dollar", "Dollar Carry")) {
    legend_breaks <- c("Single")
  }
  
  p <- .data |> 
    ggplot2::ggplot(ggplot2::aes(x = {{ .x }}, y = {{ .y }}, color = {{ .color}})) +  # Initialize ggplot with aesthetics mapping
    ggplot2::geom_line(linewidth = 1, na.rm = TRUE) +  # Add lines for each group with specified line width
    ggplot2::labs(
      title = ifelse(is.null(.title), paste0("Cumulative Excess Returns for ", rlang::enexpr(.y), " Portfolios"), .title),
      x     = ifelse(is.null(.xlab), paste0(rlang::enexpr(.x)), .xlab),
      y     = ifelse(is.null(.ylab), paste0(rlang::enexpr(.y)), .ylab),
      color = ifelse(is.null(.colab), paste0(rlang::enexpr(.color)), .colab)
    ) +  # Set the plot title based on the 'from' column
    ggplot2::theme_minimal() + # Apply a minimalistic theme to the plot
    ggplot2::scale_color_discrete(breaks = legend_breaks)
  
  if (is.na(.path)){
    
    # Simply print the plot adding plotly to it
    p |> 
      plotly::ggplotly() |>  # Convert ggplot object to interactive Plotly object
      print()  # Print the interactive plot
    
  } else {
    
    # Save the plot to a file
    ggplot2::ggsave(
      filename = paste0(.path, .title, ".png"),
      plot     = p,
      width    = 8,
      height   = 5,
      dpi      = 300,
      bg       = "white"
    ) 
    
  }
  
}




simple_line <- function(.data,
                        .x,
                        .y,
                        .title = NULL,
                        .xlab  = NULL,
                        .ylab  = NULL,
                        .colab = NULL,
                        .col   = "red",
                        .path  = NA){
  #   This function generates a simple line plotly with nice formatting.
  
  p <- .data |>
    ggplot2::ggplot() +  # Initialize a ggplot object with the input data
    ggplot2::geom_line(  # Add a line geometry to the plot
      ggplot2::aes(x = {{ .x }}, y = {{ .y }}),  # Map aesthetics: x and y variables
      col       = "red",     # Set line color
      linewidth = 1,         # Set line width
      na.rm     = TRUE
    ) +
    ggplot2::labs(           # Add labels: title, x-axis label, y-axis label
      title = ifelse(is.null(.title), paste0("Line Plot of ", rlang::enexpr(.y), " over ", rlang::enexpr(.x)), .title),
      x     = ifelse(is.null(.xlab), paste0(rlang::enexpr(.x)), .xlab),
      y     = ifelse(is.null(.ylab), paste0(rlang::enexpr(.y)), .ylab),
      color = ifelse(is.null(.colab), paste0(rlang::enexpr(.col)), .colab)
    ) + 
    ggplot2::theme_minimal()      # Apply a black and white theme to the plot
  
  if (is.na(.path)){
    
    # Simply print the plot adding plotly to it
    p |> 
      plotly::ggplotly() |>  # Convert ggplot object to interactive Plotly object
      print()  # Print the interactive plot
    
  } else {
    
    # Save the plot to a file
    ggplot2::ggsave(
      filename = paste0(.path, .title, ".png"),
      plot     = p,
      width    = 8,
      height   = 5,
      dpi      = 600,
      bg       = "white"
    ) 
    
  }
  
}



multiple_lines <- function(.data,
                           .x,
                           .y,
                           .col,
                           .title = NULL,
                           .xlab  = NULL,
                           .ylab  = NULL,
                           .colab = NULL,
                           .path  = NA){
  #   This function generates a line plotly with multiple lines/colors which are given in a column of the input.
  
  p <- .data |>
    ggplot2::ggplot() +  # Initialize a ggplot object with the input data
    ggplot2::geom_line(  # Add a line geometry to the plot
      ggplot2::aes(x = {{ .x }}, y = {{ .y }}, color = {{ .col }}),  # Map aesthetics: x, y variables and color
      linewidth = 1,          # Set line width
      na.rm = TRUE
    ) +
    ggplot2::labs(           # Add labels: title, x-axis label, y-axis label
      title = ifelse(is.null(.title), paste0("Line Plot of ", rlang::enexpr(.y), " over ", rlang::enexpr(.x)), .title),
      x     = ifelse(is.null(.xlab), paste0(rlang::enexpr(.x)), .xlab),
      y     = ifelse(is.null(.ylab), paste0(rlang::enexpr(.y)), .ylab),
      color = ifelse(is.null(.colab), paste0(rlang::enexpr(.col)), .colab)
    ) + 
    ggplot2::theme_minimal()      # Apply a black and white theme to the plot
  
  if (is.na(.path)){
    
    # Simply print the plot adding plotly to it
    p |> 
      plotly::ggplotly() |>  # Convert ggplot object to interactive Plotly object
      print()  # Print the interactive plot
    
  } else {
    
    # Save the plot to a file
    ggplot2::ggsave(
      filename = paste0(.path, .title, ".png"),
      plot     = p,
      width    = 8,
      height   = 5,
      dpi      = 300,
      bg       = "white"
    ) 
    
  }
  
}



comparison_heatmap <- function(.data,
                               .x,
                               .inverted = FALSE,
                               .mid      = NA,
                               .title    = NULL,
                               .xlab     = NULL,
                               .path     = NA){
  
  if (.inverted == FALSE) {
    
    palette <- c("red", "white", "blue")
    
  } else {
    
    palette <- c("blue", "white", "red")
    
  }
  
  # Desired order of x-axis
  timing_order <- c("Momentum-1m-3Y", "Momentum-1m-5Y", "Momentum-1m-10Y",
                    "Momentum-3m-3Y", "Momentum-3m-5Y", "Momentum-3m-10Y",
                    "Momentum-6m-3Y", "Momentum-6m-5Y", "Momentum-6m-10Y",
                    "Momentum-12m-3Y", "Momentum-12m-5Y", "Momentum-12m-10Y",
                    "RVar", "RVol")
  
  # Desired order of y-axis
  strategy_order <- c(
    "Naive Multifactor",
    "TS-Momentum-12m",
    "CS-Momentum-12m", 
    "TS-Momentum-6m", 
    "CS-Momentum-6m", 
    "TS-Momentum-3m",
    "CS-Momentum-3m", 
    "TS-Momentum-1m", 
    "CS-Momentum-1m", 
    "TS-Carry",
    "CS-Carry", 
    "Dollar Carry", 
    "Dollar"
  )
  
  .data <- .data |> 
    mutate(timing   = factor(timing, levels = timing_order),
           strategy = factor(strategy, levels = strategy_order))
  
  if (is.na(.mid)){
    
    .mid <- .data |> 
      dplyr::pull(dif) |> 
      mean()
    
  } 
  
  p <- .data |> 
    ggplot(aes(
      x = timing,
      y = strategy,
      fill = dif)) +
    geom_tile() +
    scale_fill_gradient2(
      low = palette[1], 
      mid = palette[2], 
      high = palette[3], 
      midpoint = .mid
    ) +
    ggplot2::labs(
      title = ifelse(is.null(.title), 
                     paste0("Heatmap of ", rlang::enexpr(.x), " differences vs. untimed counterpart"), 
                     .title),
      x     = ifelse(is.null(.xlab), paste0(rlang::enexpr(.x)), .xlab),
      y     = "Factor",
      fill  = ifelse(enexpr(.x) == "dif", "Alpha", "Change")
    ) + 
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  if (enexpr(.x) == "ann_ret" | enexpr(.x) == "dif") {
    
    p <- p + 
      geom_text(aes(label = ifelse(pval, paste0(round(dif, 2), "*"), round(dif, 2))), size = 3) 
    
  } else {
    
    p <- p + 
      geom_text(aes(label = round(dif, 2)), size = 3)
      
  }
  
  if (is.na(.path)){
    
    # Simply print the plot
    p |> 
      print()
    
  } else {
    
    filename <- ifelse(is.null(.title), rlang::enexpr(.x), .title)
    
    # Save the plot to a file
    ggplot2::ggsave(
      filename = paste0(.path, ".png"),
      plot     = p,
      width    = 8,
      height   = 5,
      dpi      = 300,
      bg       = "white"
    ) 
    
  }
  
}
