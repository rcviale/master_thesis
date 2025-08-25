group_line_plot <- function(.data,
                            .x,
                            .y,
                            .color,
                            .title,
                            .xlab  = NULL,
                            .ylab  = NULL,
                            .colab = NULL,
                            .path  = NA){
  # This function creates a separate line plot for each group (time series) in the data.
  # Args:
  #   .data : tibble/data.frame containing data to plot
  #   .x    : column name for x-axis values
  #   .y    : column name for y-axis values
  #   .color: column name for color grouping (legend categories)
  #   .title: column name used as title or character string
  #   .xlab/.ylab/.colab: optional labels for axes and legend
  #   .path : directory path to save PNG; if NA, print interactive Plotly chart
  
  # Extract unique values from the title column (will usually be one value per call)
  .title <- .data |>
    dplyr::pull({{ .title }}) |>                  # dplyr: extract values from the specified title column
    base::unique()                                # base R: keep unique values
  
  # Pick legend order based on strategy/title name
  .title_chr <- base::as.character(.title)[1]     # convert to character (first value if vector)
  legend_breaks <- NULL
  if (base::grepl("^CS", .title_chr)) {           # if Cross-Sectional strategy
    legend_breaks <- c("HML", "Short", "Portfolio 2", "Portfolio 3", "Portfolio 4", "Long")
  } else if (base::grepl("^TS", .title_chr)) {    # if Time-Series strategy
    legend_breaks <- c("HML", "Short", "Long")
  } else if (.title_chr %in% c("Dollar", "Dollar Carry")) {  # if Dollar strategies
    legend_breaks <- c("Single")
  }
  
  # Build the ggplot object
  p <- .data |> 
    ggplot2::ggplot(ggplot2::aes(x = {{ .x }}, 
                                 y = {{ .y }}, 
                                 color = {{ .color }})) +    # map x, y, and color to given columns
    ggplot2::geom_line(linewidth = 1, na.rm = TRUE) +        # draw lines, ignore missing values
    ggplot2::labs(                                           # set plot labels
      title = ifelse(is.null(.title), 
                     paste0("Cumulative Excess Returns for ", rlang::enexpr(.y), " Portfolios"), 
                     .title),
      x     = ifelse(is.null(.xlab), paste0(rlang::enexpr(.x)), .xlab),
      y     = ifelse(is.null(.ylab), paste0(rlang::enexpr(.y)), .ylab),
      color = ifelse(is.null(.colab), paste0(rlang::enexpr(.color)), .colab)
    ) +
    ggplot2::theme_minimal() +                                 # clean minimal theme
    ggplot2::scale_color_discrete(breaks = legend_breaks)      # enforce custom legend order
  
  if (base::is.na(.path)) {
    
    # Display an interactive plot in Plotly if no save path is given
    p |> 
      plotly::ggplotly() |>                                    # convert ggplot object to interactive Plotly
      base::print()                                            # render the interactive plot
    
  } else {
    
    # Save the static ggplot to file
    ggplot2::ggsave(
      filename = paste0(.path, .title, ".png"),                 # construct file name
      plot     = p,                                             # plot object
      width    = 8,                                             # inches
      height   = 5,                                             # inches
      dpi      = 300,                                           # resolution
      bg       = "white"                                        # background color
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
      dpi      = 500,
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
  # This function produces a heatmap comparing timed vs untimed factor statistics.
  # Args:
  #   .data     : tibble/data.frame containing at least 'timing', 'strategy', 'dif' columns
  #   .x        : symbol specifying the variable of interest (e.g., ann_ret, sharpe, dif)
  #   .inverted : logical; if TRUE, reverse the color palette
  #   .mid      : numeric midpoint for diverging color scale; if NA, computed from data
  #   .title    : optional plot title
  #   .xlab     : optional x-axis label
  #   .path     : file path prefix to save plot; if NA, prints to screen
  
  # Choose color palette based on inversion flag
  if (.inverted == FALSE) {
    palette <- c("red", "white", "blue")
  } else {
    palette <- c("blue", "white", "red")
  }
  
  # Desired order of x-axis (timing signals)
  timing_order <- c("Momentum-1m-3Y", "Momentum-1m-5Y", "Momentum-1m-10Y",
                    "Momentum-3m-3Y", "Momentum-3m-5Y", "Momentum-3m-10Y",
                    "Momentum-6m-3Y", "Momentum-6m-5Y", "Momentum-6m-10Y",
                    "Momentum-12m-3Y", "Momentum-12m-5Y", "Momentum-12m-10Y",
                    "RVar", "RVol")
  
  # Desired order of y-axis (strategies)
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
  
  # Reorder factors in data
  .data <- .data |> 
    dplyr::mutate(
      timing   = base::factor(timing,   levels = timing_order),
      strategy = base::factor(strategy, levels = strategy_order)
    )
  
  # If midpoint not provided, compute as mean of differences
  if (base::is.na(.mid)){
    .mid <- .data |> 
      dplyr::pull(dif) |> 
      base::mean()
  } 
  
  # Build heatmap
  p <- .data |> 
    ggplot2::ggplot(ggplot2::aes(
      x    = timing,
      y    = strategy,
      fill = dif
    )) +
    ggplot2::geom_tile() +
    ggplot2::scale_fill_gradient2(
      low       = palette[1], 
      mid       = palette[2], 
      high      = palette[3], 
      midpoint  = .mid
    ) +
    ggplot2::labs(
      title = ifelse(
        base::is.null(.title), 
        paste0("Heatmap of ", rlang::enexpr(.x), " differences vs. untimed counterpart"), 
        .title
      ),
      x     = ifelse(base::is.null(.xlab), paste0(rlang::enexpr(.x)), .xlab),
      y     = "Factor",
      fill  = ifelse(rlang::enexpr(.x) == "dif", "Alpha", "Change")
    ) + 
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
    )
  
  # Add numeric labels to each tile
  if (rlang::enexpr(.x) == "ann_ret" | rlang::enexpr(.x) == "dif") {
    p <- p + 
      ggplot2::geom_text(
        ggplot2::aes(
          label = ifelse(pval, paste0(base::round(dif, 2), "*"), base::round(dif, 2))
        ),
        size = 3
      )
  } else {
    p <- p + 
      ggplot2::geom_text(
        ggplot2::aes(label = base::round(dif, 2)),
        size = 3
      )
  }
  
  if (base::is.na(.path)){
    # Print plot to screen
    base::print(p)
  } else {
    # Save to file
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
