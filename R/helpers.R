lustig_returns <- function(.data,
                           .side = "side", 
                           .market = "mkt"){
  
  .data |> 
    tidyr::pivot_wider(
      names_from  = c( {{ .market }} , {{ .side }} ),
      names_sep   = ".",
      values_from = px
    ) |> 
    dplyr::arrange(date, from) |> 
    dplyr::mutate(
      date = ceiling_date(date, unit = "month") - 1
    ) |> 
    dplyr::group_by(date, from) |>
    dplyr::slice_tail(n = 1) |>
    dplyr::ungroup() |>
    purrr::modify_if(
      .p = is.numeric,
      .f = log
    ) |> 
    dplyr::group_by(from) |>
    dplyr::mutate(
      rl = fwd.bid - dplyr::lead(spot.ask),
      rs = -fwd.ask + dplyr::lead(spot.bid),
    ) |> 
    dplyr::ungroup() |> 
    tidyr::drop_na(rl, rs) # NAs can be dropped here because there are no observations where we have one and not the other.
  
}



compute_signals <- function(.data){
  #   This function computes all the signals that will be used as sorting variables for the replicated factors.
  
  .data |> 
    dplyr::mutate(
      carry = fwd.bid - spot.ask, # Carry
    ) |> 
    dplyr::group_by(from) |> 
    dplyr::mutate(
      # Momentum
      mom1     = dplyr::lag(rl),
      mom3     = slider::slide_dbl(.x = rl, .f = sum, .before = 2, .complete = TRUE) |> dplyr::lag(),
      mom6     = slider::slide_dbl(.x = rl, .f = sum, .before = 5, .complete = TRUE) |> dplyr::lag(),
      mom12    = slider::slide_dbl(.x = rl, .f = sum, .before = 11, .complete = TRUE) |> dplyr::lag()
    ) |> 
    dplyr::group_by(date) |> 
    dplyr::mutate(
      avg_fd = mean(carry, na.rm = TRUE), # Dollar Carry
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
    ) |> 
    dplyr::mutate(
      portfolio = "single"
    )
  
}



cs_logret <- function(.x){
  #   This function computes the cross sectional return for a vector, and returns it in log return.
  
  log( mean( exp(.x) - 1, na.rm = T ) + 1 )
  
}



cs_wlogret <- function(.x, .w){
  #   This function computes the cross sectional return for a vector, and returns it in log return.
  
  log( sum(.w * ( exp(.x) - 1 ), na.rm = T ) + 1 )
  
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
    ) |> 
    dplyr::mutate(
      portfolio = "single"
    )
  
}



compute_ts_factors <- function(.data){
  #   This function computes the time series factors, i.e. time series carry and time series momentum.
  
  .data <- .data |> 
    dplyr::group_by(date, signal) |> 
    dplyr::summarise(
      ret_l = if_else( rlang::is_empty( rl[var > 0] ), 
                       0, 
                       cs_logret( rl[var > 0]) ),
      ret_s = if_else( rlang::is_empty( rs[var < 0] ), 
                       0, 
                       cs_logret(rs[var < 0]) ),
      .groups = "drop"
    ) |> 
    dplyr::mutate(
      hml = ret_l + ret_s,
      strategy = paste0("ts_", signal)
    ) |> 
    dplyr::select(-signal)
  
  .hml <- .data |> 
    dplyr::select(date, strategy, ret_l = hml) |>
    dplyr::mutate(
      portfolio = "hml"
    )
  
  .long <- .data|> 
    dplyr::select(date, strategy, ret_l) |> 
    dplyr::mutate(
      portfolio = "long"
    )
  
  .data |> 
    dplyr::select(date, strategy, ret_s) |> 
    dplyr::mutate(
      portfolio = "short"
    ) |>
    dplyr::bind_rows(.hml, .long) 
  
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
                                     .n_portfolios = 5){
  
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
      ret_l = cs_logret(rl),
      ret_s = cs_logret(rs),
      # ret_l = cs_xs_simple(rl),
      # ret_s = cs_xs_simple(rs),
      .groups = "drop"
    ) |> 
    dplyr::arrange(date, signal, portfolio) |> 
    dplyr::mutate(
      strategy = paste0("cs_", signal)
    ) |> 
    dplyr::select(-signal)
  
}



multiple_hml <- function(.data,
                         .n_portfolios = 5) {
  #   This function computes the high-minus-low (HML) portfolio return for sorted portfolios and appends it to the
  # input data.
  
  high_id <- paste0("p", .n_portfolios)
  
  longs <- .data |> 
    dplyr::filter(portfolio == high_id) |>  # Filter for the highest portfolio (long leg)
    dplyr::select(-c(portfolio, ret_s)) |> 
    dplyr::rename(long = ret_l)
  
  .data |> 
    dplyr::filter(portfolio == "p1") |>  # Filter for the lowest portfolio (short leg)
    dplyr::select(-c(ret_l, portfolio)) |>  # Remove long return column
    dplyr::rename(short = ret_s) |>  # Rename the p1 column to short_p1
    dplyr::inner_join(  # Join with long returns by date and strategy
      longs, 
      dplyr::join_by(date, strategy)
    ) |> 
    dplyr::mutate(
      ret_l     = long + short,  # Compute HML return
      portfolio = as.factor("hml"),  # Label the portfolio as "hml"
      ret_s     = NA  # Set short return to NA (not applicable)
    ) |> 
    dplyr::select(-c(short, long)) |>  # Drop intermediate columns
    dplyr::bind_rows(.data) |>  # Append original data to include all portfolios
    dplyr::arrange(date, strategy, portfolio)  # Sort by date, strategy, and portfolio
  
}



rename_edge_portfolios <- function(.data){
  #   This function renames the first and last portfolios (1 and 5 if using quintile sorts) to short and long respectively.
  
  .data |> 
    dplyr::mutate(
      portfolio = dplyr::case_when(
        portfolio == "p1" ~ "short",
        portfolio == "p5" ~ "long",
        .default  =  portfolio
      )
    ) 
  
}



switch_shorts <- function(.data){
  
  .data |> 
    dplyr::mutate(ret = if_else(portfolio == "short", ret_s, ret_l)) |> 
    dplyr::select(-c(ret_l, ret_s))
  
}



compute_naive <- function(.data,
                          .portfolios = c("hml", "single")){
  
  naives <- .data |> 
    dplyr::filter(portfolio %in% c("hml", "single")) |> 
    dplyr::select(-portfolio) |> 
    dplyr::group_by(date) |> 
    dplyr::summarise(
      ret = mean(ret, na.rm = TRUE)
    ) |> 
    dplyr::mutate(
      strategy  = "naive",
      portfolio = "1/N"
    )
  
  .data |> 
    dplyr::bind_rows(naives)
  
}



organize_portfolios <- function(.data,
                                .timed = FALSE){
  #   This function arranges the merged factors tibble and keeps only columns of interest.
  
  if (.timed == TRUE){
    
    .data |> 
      dplyr::arrange(date, strategy, portfolio) |> 
      dplyr::select(date, strategy, portfolio, timing, ret)
    
  } else {
    
    .data |> 
      dplyr::arrange(date, strategy, portfolio) |> 
      dplyr::select(date, strategy, portfolio, ret)
    
  }
  
}



timed_momentum <- function(.data,
                           .mom_windows = c(1, 3, 6, 12),
                           .vol_windows = c(12, 36, 60)){
  
  .data |>
    mutate(vol_window = list(.vol_windows)) |> 
    unnest(vol_window) |> 
    group_by(strategy, portfolio, vol_window) |>
    mutate(
      vol = sqrt(12) * (
        slider::slide_dbl(
          .x = ret,
          .f = sd,
          .before = unique(vol_window) - 1,
          .complete = TRUE
        ) |> dplyr::lag()
      )
    ) |> 
    ungroup() |> 
    mutate(mom_window = list(.mom_windows)) |>
    unnest(mom_window) |> 
    group_by(strategy, portfolio, vol_window, mom_window) |>
    mutate(
      mom       = (12 / mom_window) * (
        slider::slide_dbl(
          .x        = ret,
          .f        = sum,
          .before   = unique(mom_window) - 1,
          .complete = TRUE
        ) |> dplyr::lag()
      ),
      timing    = paste("mom", mom_window, vol_window, sep = "_"),
    ) |>
    drop_na(mom, vol) |> 
    group_by(date, strategy, portfolio, timing) |> 
    mutate(
      w_mom     = max(-2, min(2, mom / vol)),
      ret       = cs_wlogret(ret, w_mom)
    ) |> 
    ungroup() |> 
    select(-c(vol_window, mom_window, mom, vol, w_mom))
  
}



timed_variance <- function(.data){
  
  .data |>
    group_by(strategy, portfolio) |> 
    mutate(
      rvar   = dplyr::lag(ret)^2, 
      cumvar = slider::slide_dbl(.x = rvar, .f = ~mean(.x, na.rm = TRUE), .before = Inf, .complete = TRUE
      )
    ) |> 
    group_by(date, strategy, portfolio) |> 
    mutate(
      w_var  = min(2, rvar / cumvar),
      ret    = cs_wlogret(ret, w_var),
      timing = "var"
    ) |> 
    drop_na(w_var) |> 
    ungroup() |> 
    select(-c(rvar, cumvar, w_var))
  
}



perf_stats <- function(.data,
                       .ret = ret){
  #   This function computes performance statistics. Input should be in log returns.
  
  .data |> 
    group_by(strategy) |> 
    summarise(
      ann_ret = (exp(mean( {{ .ret }} ) * 12) - 1) * 100,
      ann_vol = sd(exp( {{ .ret }} ) - 1) * sqrt(12) * 100,
      .groups = "drop"
    ) |>
    dplyr::mutate(
      sharpe  = ann_ret / ann_vol,
    ) |> 
    purrr::modify_if(.p = is.numeric,
                     .f = ~round(.x, 2)) |> 
    dplyr::arrange(dplyr::desc(sharpe))
  
}
