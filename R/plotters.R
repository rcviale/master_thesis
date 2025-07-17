group_line_plot <- function(.data,
                            .x,
                            .y,
                            .color,
                            .title,
                            .path = NA){
  # This function makes a line plot for each group (time series in this case).
  .title <- .data |> 
    dplyr::pull({{ .title }}) |> 
    unique()
  
  p <- .data |> 
    ggplot2::ggplot(ggplot2::aes(x = {{ .x }}, y = {{ .y }}, color = {{ .color}})) +  # Initialize ggplot with aesthetics mapping
    ggplot2::geom_line(linewidth = 1, na.rm = TRUE) +  # Add lines for each group with specified line width
    ggplot2::labs(title = .title) +  # Set the plot title based on the 'from' column
    ggplot2::theme_minimal()  # Apply a minimalistic theme to the plot
  
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
      y     = ifelse(is.null(.ylab), paste0(rlang::enexpr(.y)), .ylab)
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



multiple_lines <- function(.data,
                           .x,
                           .y,
                           .col,
                           .title = NULL,
                           .xlab  = NULL,
                           .ylab  = NULL,
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
      y     = ifelse(is.null(.ylab), paste0(rlang::enexpr(.y)), .ylab)
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
                               .title = NULL,
                               .inverted = FALSE,
                               .mid = NA,
                               .path = NA){
  
  if (.inverted == FALSE) {
    
    palette <- c("red", "white", "blue")
    
  } else {
    
    palette <- c("blue", "white", "red")
    
  }
  
  .data <- .data |> 
    pivot_longer(
      -c(strategy, {{ .x }}),
      names_to  = "timing",
      values_to = "dif"
    )
  
  # Desired order of x-axis
  timing_order <- c("mom_1_12", "mom_1_36", "mom_1_60",
                    "mom_3_12", "mom_3_36", "mom_3_60",
                    "mom_6_12", "mom_6_36", "mom_6_60",
                    "mom_12_12", "mom_12_36", "mom_12_60", 
                    "rvar", "rvol")
  
  # Desired order of y-axis
  strategy_order <- c(
    "dol", "dol_carry",
    "cs_carry", "ts_carry",
    "cs_mom1", "cs_mom3", "cs_mom6", "cs_mom12",
    "ts_mom1", "ts_mom3", "ts_mom6", "ts_mom12",
    "naive"  # optional, add any others you have
  )
  
  .data <- .data |> 
    mutate(timing   = factor(timing, levels = timing_order),
           strategy = factor(strategy, levels = strategy_order))
  
  if (is.na(.mid)){
    
    .mid <- .data |> 
      dplyr::pull(dif) |> 
      mean()
    
  } else {
    
    .mid <- 0
    
  }
  
  p <- .data |> 
    ggplot(aes(
      x = timing,
      y = strategy,
      fill = dif)) +
    geom_tile() +
    geom_text(aes(label = round(dif, 2)), size = 3) +
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
    ) + 
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  if (is.na(.path)){
    
    # Simply print the plot
    p |> 
      print()
    
  } else {
    
    filename <- ifelse(is.null(.title), rlang::enexpr(.x), .title)
    
    # Save the plot to a file
    ggplot2::ggsave(
      filename = paste0(.path, "comparison_", filename, ".png"),
      plot     = p,
      width    = 8,
      height   = 5,
      dpi      = 300,
      bg       = "white"
    ) 
    
  }
  
}
