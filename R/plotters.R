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
                        .col   = "red"){
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
    ggplot2::theme_bw()      # Apply a black and white theme to the plot
  
  p |>
    plotly::ggplotly() |>    # Convert the ggplot object to an interactive Plotly plot
    print()                  # Print the interactive plot
}



multiple_lines <- function(.data,
                           .x,
                           .y,
                           .col,
                           .title = NULL,
                           .xlab  = NULL,
                           .ylab  = NULL){
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
  
  p |>
    plotly::ggplotly() |>    # Convert the ggplot object to an interactive Plotly plot
    print()                  # Print the interactive plot
  
}

