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



compute_signals <- function(.data){
  #   This function computes all the signals that will be used as sorting variables for the replicated factors.
  
  .data |> 
    dplyr::mutate(
      fwd_disc = fwd.bid - spot.ask, # Carry
    ) |> 
    dplyr::group_by(from) |> 
    dplyr::mutate(
      # Momentum
      mom1     = dplyr::lag(rl),
      mom3     = slider::slide_dbl(.x = rl, .f = sum, .before = 2, .complete = TRUE) |> dplyr::lag(),
      mom6     = slider::slide_dbl(.x = rl, .f = sum, .before = 5, .complete = TRUE) |> dplyr::lag(),
      mom12    = slider::slide_dbl(.x = rl, .f = sum, .before = 10, .complete = TRUE) |> dplyr::lag()
    ) |> 
    dplyr::group_by(date) |> 
    dplyr::mutate(
      avg_fd = mean(fwd_disc, na.rm = TRUE), # Dollar Carry
    ) |> 
    dplyr::ungroup()
  
}



compute_dol_carry <- function(.data){
  #   This function computes the returns for the Dollar (1/N of all currencies) strategy.
  
  .data |> 
    dplyr::summarise(
      dol_carry = ifelse(mean(avg_fd, na.rm = T) >= 0, mean(rl), mean(rs)), # I take the mean(avg_fd) to save a few 
      # lines of code. Since avg_fd is the same in every date, it's also the same as it's mean.
      .by       = date,
    ) |> 
    tidyr::pivot_longer(
      -date,
      names_to  = "strategy",
      values_to = "ret_l"
    )
  
}



compute_dol <- function(.data){
  #   This function computes returns for the Dollar Carry strategy. 
  
  .data |> 
    dplyr::summarise(
      dol = mean(rl),
      .by = date
    ) |> 
    tidyr::pivot_longer(
      -date,
      names_to  = "strategy",
      values_to = "ret_l"
    )
  
}



assign_portfolio <- function(.data, 
                             .variable, 
                             .n_portfolios) {
  #   This function computes the breakpoints for a variable/signal and sorts securities into portfolios according 
  # to this variable.
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



multiple_portfolio_sorts <- function(.data,
                                     .variable,
                                     .n_portfolios){
  
  .data |> 
    tidyr::drop_na({{ .variable }}) |> 
    dplyr::group_by(date, signal) |> 
    dplyr::mutate(
      portfolio = assign_portfolio(
        .data         = dplyr::pick(dplyr::everything()),
        .variable     = {{ .variable }},
        .n_portfolios = .n_portfolios
      ),
      portfolio = as.factor(paste0("p", portfolio))
    ) |>
    dplyr::group_by(portfolio, date, signal) |>
    dplyr::summarize(
      ret_l = sum(rl / dplyr::n()),
      ret_s = sum(rs / dplyr::n()),
      .groups = "drop"
    ) |> 
    dplyr::arrange(date, signal, portfolio)
  
}



multiple_hml <- function(.data,
                       .long  = "ret_l_p3",
                       .short = "ret_s_p1",
                       .port_col = "portfolio",
                       .ret_col  = c("ret_l", "ret_s")) {
  
  longs <- df |> 
    multiple_portfolio_sorts(.variable = var,
                             .n_portfolios = 5) |> 
    select(-ret_s) |> 
    filter(portfolio == "p5") |> 
    pivot_wider(names_from = portfolio,
                values_from = ret_l) |> 
    rename(long_p5 = p5) 
  
  
  shorts <- df |> 
    multiple_portfolio_sorts(.variable = var,
                             .n_portfolios = 5) |> 
    select(-ret_l) |> 
    filter(portfolio == "p1") |> 
    pivot_wider(names_from = portfolio,
                values_from = ret_s) |> 
    rename(short_p1 = p1) 
  
  shorts |> 
    inner_join(longs, by = join_by(date, signal)) |> 
    mutate(ret_l     = long_p5 - short_p1,
           portfolio = "hml", 
           ret_s     = NA) |> 
    select(-c(short_p1, long_p5)) |> 
    bind_rows(df |>   multiple_portfolio_sorts(.variable = var,
                                               .n_portfolios = 5) ) |> 
    arrange(date, signal, portfolio)
  
}



perf_stats <- function(.data){
  
  .data |> 
    group_by(signal, portfolio) |> 
    summarise(
      ann_ret = (exp(mean(ret_l) * 12) - 1) * 100,
      ann_vol = sd(exp(ret_l) - 1) * sqrt(12) * 100,
      .groups = "drop"
    ) |>
    dplyr::mutate(
      sharpe  = ann_ret / ann_vol,
    ) |> 
    purrr::modify_if(.p = is.numeric,
                     .f = ~round(.x, 2))
  
}