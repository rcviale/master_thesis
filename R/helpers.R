lustig_returns <- function(.data,
                       .side = "side", 
                       .market = "mkt"){
  
  .data |> 
    dplyr::mutate(px = log(px)) |>
    tidyr::pivot_wider(names_from  = c("mkt", "side"),
                       names_sep   = ".",
                       values_from = px) |> 
    dplyr::arrange(date, from) |> 
    dplyr::group_by(from) |> 
    dplyr::mutate(
      # rl = dplyr::lag(fwd.bid) - spot.ask,
      # rs = -dplyr::lag(fwd.ask) + spot.bid,
      rl = fwd.bid - dplyr::lead(spot.ask),
      rs = -fwd.ask + dplyr::lead(spot.bid)
    ) |> 
    dplyr::ungroup() |> 
    tidyr::drop_na(rl, rs)
  
}



assign_portfolio <- function(.data, 
                             .variable, 
                             .n_portfolios) {
  #   Refer to https://www.tidy-finance.org/r/univariate-portfolio-sorts.html
  
  # Compute breakpoints
  breakpoints <- .data |>
    dplyr::pull({{ .variable }}) |>
    quantile(
      probs = seq(0, 1, length.out = .n_portfolios + 1),
      na.rm = TRUE,
      names = FALSE
    )
  
  # Assign portfolios
  assigned_portfolios <- .data |>
    dplyr::mutate(portfolio = findInterval(
      dplyr::pick(dplyr::everything()) |>
        dplyr::pull({{ .variable }}),
      breakpoints,
      all.inside = TRUE
    )) |>
    dplyr::pull(portfolio)
  
  # Output
  return(assigned_portfolios)
  
}



compute_portfolios <- function(.data,
                               .variable,
                               .n_portfolios){
  
  .data |> 
    tidyr::drop_na({{ .variable }}) |> 
    dplyr::group_by(date) |> 
    dplyr::mutate(
      portfolio = assign_portfolio(
        .data         = dplyr::pick(dplyr::everything()),
        .variable     = {{ .variable }},
        .n_portfolios = .n_portfolios
      ),
      portfolio = as.factor(paste0("p", portfolio))
    ) |>
    dplyr::group_by(portfolio, date) |>
    dplyr::summarize(
      ret_l = sum(rl / dplyr::n()),
      ret_s = sum(rs / dplyr::n()),
      .groups = "drop"
    )
  
}



compute_ls <- function(.data,
                       .long  = "ret_l_p3",
                       .short = "ret_s_p1",
                       .port_col = "portfolio",
                       .ret_col  = c("ret_l", "ret_s")) {
  
  .data |> 
    tidyr::pivot_wider(names_from  = {{ .port_col }},
                       values_from = {{ .ret_col }}) |>
    dplyr::mutate(ls = (1 + .data[[ .long ]]) * (1 + .data[[ .short ]]) - 1) |> 
    tidyr::pivot_longer(-date,
                        names_to  = .port_col,
                        values_to = "ex_ret")
  
}
