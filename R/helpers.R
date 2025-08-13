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



compute_mid_return <- function(.data){
  
  .data |> 
    dplyr::filter(mkt == "spot") |> 
    dplyr::select(-c(mkt, side)) |> 
    dplyr::summarise(
      mid_px  = mean(px, na.rm = TRUE),
      .by     = c(date, from)
    ) |> 
    dplyr::group_by(from) |> 
    dplyr::mutate(
      rm = (mid_px / dplyr::lag(mid_px) - 1)
    ) |> 
    dplyr::ungroup() |>
    dplyr::select(-mid_px) |> 
    dplyr::drop_na(rm)
  
}


cs_logret <- function(.x){
  #   This function computes the cross sectional return for a vector, and returns it in log return.
  
  log( mean( exp(.x), na.rm = T ) )
  
}



cs_wlogret <- function(.x, .w){
  #   This function computes the cross sectional return for a vector, and returns it in log return.
  
  log( sum(.w * ( expm1(.x) ), na.rm = T ) + 1 )
  
}



cs_wlogrv <- function(.x, .w){
  #   This function computes the cross sectional realized variance for a vector. Does NOT convert it back to log.
  
  sum(.w * ( expm1(.x) ), na.rm = T )^2
  
}



compute_dol_carry <- function(.data){
  #   This function computes the returns for the Dollar Carry strategy.
  
  .data |> 
    dplyr::summarise(
      ret_l = ifelse(mean(avg_fd, na.rm = T) >= 0, cs_logret(rl), cs_logret(rs)), # I take the mean(avg_fd) to 
      # save a few lines of code. Since avg_fd is the same in every date, it's also the same as it's mean.
      ret_s = ifelse(mean(avg_fd, na.rm = T) >= 0, cs_logret(rs), cs_logret(rs)),
      curs  = list(list(hi = from, lo = from)),
      .by   = date,
    ) |> 
    dplyr::mutate(
      strategy = "dol_carry",
      portfolio = "single"
    )
  
}



compute_dol <- function(.data){
  #   This function computes returns for the Dollar strategy. 
  
  .data |> 
    dplyr::summarise(
      ret_l = cs_logret(rl),
      ret_s = cs_logret(rs),
      curs  = list(list(hi = from, lo = from)),
      .by = date
    ) |> 
    dplyr::mutate(
      strategy = "dol",
      portfolio = "single"
    )
  
}



compute_ts_factors <- function(.data){
  #   This function computes the time series factors, i.e. time series carry and time series momentum.
  
  .data <- .data |> 
    dplyr::group_by(date, signal) |> 
    dplyr::summarise(
      ret_l_pos = if_else( rlang::is_empty( rl[var > 0] ), 
                           0, 
                           cs_logret( rl[var > 0]) ),
      ret_s_neg = if_else( rlang::is_empty( rs[var < 0] ), 
                           0, 
                           cs_logret( rs[var < 0]) ),
      ret_l_neg = if_else( rlang::is_empty( rl[var < 0] ), 
                           0, 
                           cs_logret( rl[var < 0]) ),
      ret_s_pos = if_else( rlang::is_empty( rs[var > 0] ), 
                           0, 
                           cs_logret( rs[var > 0]) ),
      pos_curs  = list(curs = from[ var > 0 ]),
      neg_curs  = list(curs = from[ var < 0 ]),
      .groups = "drop"
    ) |> 
    dplyr::mutate(
      hml = ret_l_pos + ret_s_neg,
      lmh = ret_l_neg + ret_s_pos,
      strategy = paste0("ts_", signal)
    ) |> 
    dplyr::select(-signal)
  
  #   Long basket returns: we can long or short this basket in the future. This basket is for currencies with 
  # positive var
  long <- .data |> 
    dplyr::select(
      date, 
      strategy, 
      ret_l = ret_l_pos, 
      ret_s = ret_s_pos, 
      curs  = pos_curs
    ) |> 
    group_by(date, strategy) |> 
    mutate(
      curs      = list(list(hi = as.vector(unlist(curs)), lo = as.vector(unlist(curs)))),
      portfolio = "long"
    )
  
  short <- .data |> 
    dplyr::select(
      date, 
      strategy, 
      ret_l = ret_l_neg, 
      ret_s = ret_s_neg, 
      curs  = neg_curs
    ) |> 
    group_by(date, strategy) |> 
    mutate(
      curs      = list(list(hi = as.vector(unlist(curs)), lo = as.vector(unlist(curs)))),
      portfolio = "short"
    )
  
  .data |> 
    dplyr::select(
      date, 
      strategy, 
      ret_l   = hml,
      ret_s   = lmh,
      curs_hi = pos_curs, # In HML = ret_l I am longing curs_hi currencies and shorting curs_lo ones
      curs_lo = neg_curs  # In LMH = ret_s I am longing curs_lo currencies and shorting curs_hi ones
    ) |>
    dplyr::mutate(
      curs = map2(.x = curs_hi, 
                  .y = curs_lo, 
                  .f = ~list(hi = .x, lo = .y)),
      portfolio = "hml"
    ) |> 
    dplyr::select(-c(curs_hi, curs_lo)) |> 
    dplyr::bind_rows(long, short) |> 
    dplyr::arrange(date, strategy, portfolio)
  
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
      curs  = list(list(hi = from, lo = from)),
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
    dplyr::select(-portfolio) |> 
    dplyr::rename(ret_l_p5 = ret_l, ret_s_p5 = ret_s, curs_p5 = curs)
  
  .data |>
    dplyr::filter(portfolio == "p1") |>  # Filter for the lowest portfolio (short leg)
    dplyr::select(-portfolio) |>  # Remove long return column
    dplyr::rename(ret_l_p1 = ret_l, ret_s_p1 = ret_s, curs_p1 = curs) |>
    dplyr::inner_join(  # Join with long returns by date and strategy
      longs,
      dplyr::join_by(date, strategy)
    ) |>
    dplyr::group_by(date, strategy) |>
    dplyr::mutate(
      ret_l     = ret_l_p5 + ret_s_p1,  # Compute HML return
      ret_s     = ret_l_p1 + ret_s_p5,  # Set short return to NA (not applicable)
      curs      = purrr::map2(.x = curs_p5,
                              .y = curs_p1,
                              .f = ~list(hi = as.vector(unlist(.x$hi)), lo = as.vector(unlist(.y$hi)))),
      portfolio = as.factor("hml"),  # Label the portfolio as "hml"
    ) |>
    dplyr::ungroup() |> 
    dplyr::select(date, strategy, portfolio, ret_l, ret_s, curs) |> 
    dplyr::bind_rows(.data) |> 
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



compute_naive <- function(.data,
                          .portfolios = c("hml", "single")){
  
  naives <- .data |> 
    dplyr::filter(portfolio %in% c("hml", "single")) |> 
    dplyr::group_by(date) |> 
    dplyr::summarise(
      ret_l = cs_logret(ret_l),
      ret_s = cs_logret(ret_s),
      curs  = list(
        list(
          hi = as.vector(unlist(purrr::map(.x = curs, .f = "hi"))),
          lo = as.vector(unlist(purrr::map(.x = curs, .f = "lo")))  
        )
      ),
      .groups = "drop"
    ) |> 
    dplyr::mutate(
      curs = map(curs, \(x) {       # work row-wise on the list column
        all_cur <- union(x$hi, x$lo)  # every currency that occurs at this date
        
        # balance = (#times in hi) − (#times in lo)
        bal <- sapply(all_cur,
                      function(c) sum(x$hi == c) - sum(x$lo == c))
        
        list(                         # wrap in *one* list so `curs3` is list-column
          hi = rep(all_cur[bal > 0],  bal[bal > 0]),   # positive balance → long side
          lo = rep(all_cur[bal < 0], -bal[bal < 0])    # negative balance → short side
        )
      })
    ) |> 
    dplyr::mutate(
      strategy  = "naive",
      portfolio = "1/K"
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
      dplyr::select(date, strategy, portfolio, ret_l, ret_s, curs)
    
  }
  
}



timed_momentum <- function(.data,
                           .mom_windows = c(1, 3, 6, 12),
                           .vol_windows = c(36, 60, 120)) {
  
  .data |>
    dplyr::mutate(vol_window = list(.vol_windows)) |>
    tidyr::unnest(vol_window) |>
    dplyr::group_by(strategy, portfolio, vol_window) |>
    dplyr::mutate(
      vol = sqrt(12) * slider::slide_dbl(
        .x        = expm1(ret_l),
        .f        = ~ stats::sd(.x, na.rm = TRUE),
        .before   = vol_window[1] - 1,
        .complete = TRUE
      ) |> dplyr::lag()
    ) |>
    dplyr::ungroup() |>
    dplyr::mutate(mom_window = list(.mom_windows)) |>
    tidyr::unnest(mom_window) |>
    dplyr::arrange(date) |>
    dplyr::group_by(strategy, portfolio, vol_window, mom_window) |>
    dplyr::mutate(
      mom = (12 / mom_window) * slider::slide_dbl(
        .x        = expm1(ret_l), 
        .f        = ~ sum(.x, na.rm = TRUE),
        .before   = mom_window[1] - 1,
        .complete = TRUE
      ) |> dplyr::lag(),
      timing = paste0("mom_", mom_window, "_", vol_window)
    ) |>
    tidyr::drop_na(mom, vol) |>
    dplyr::group_by(strategy, portfolio, timing) |>
    dplyr::mutate(weight = pmax(-2, pmin(2, mom / vol))) |>
    dplyr::ungroup() |> 
    dplyr::summarise(
      ret = cs_wlogret(
        ifelse(weight >= 0, ret_l, ret_s),
        abs(weight)
      ),
      .by = c(date, strategy, portfolio, timing)
    )
  
}



compute_rv <- function(.data) {
  
  .data |>
    summarise(
      ret = mean(rm, na.rm = TRUE), # 1/N return per day and side
      .by  = c(day, side)
    ) |> 
    summarise(
      ret_day = sum(ret), # Add long and short (in case there is a short side, otherwise it will be the same)
      .by = day
    ) |>   
    slice_tail(n = 22) |>  # Sometimes the dates for all currencies won't match, which would make RV be computed 
    # over more than 22 days. Thus, I take the only the last 22 observations, which will ignore currencies for 
    # which data was missing.
    summarise(
      rv = sum( (ret_day - mean(ret_day, na.rm = T))^2, na.rm = T)
    ) |>
    pull(rv)
  
}



timed_variance <- function(.data, .midspots){
  
  midrets <- .midspots |> 
    group_by(from) |> 
    mutate(
      month  = lubridate::ceiling_date(date, "month") - 1,  
      window = slide(.x      = date, 
                     .f      = identity, 
                     .before = 21,
                     .complete = TRUE),
    ) |>
    ungroup() |> 
    drop_na(window) |>
    filter(date == month) |> 
    select(month, from, date = window) |> 
    unnest(date) |> 
    left_join(
      .midspots, 
      join_by(date, from)
    ) |> 
    select(date = month, day = date, from, rm) |> 
    nest(midrets = -c(date, from))
  
  .data |>
    unnest_longer(
      curs, 
      indices_to = "side", 
      values_to = "from"
    ) |> 
    unnest(from) |> 
    filter(
      side != "lo" | portfolio %in% c("hml", "1/K") # Filter out the lo in portfolios where it is equal to hi
    ) |>
    left_join(
      y  = midrets,
      by = dplyr::join_by(date, from),
      relationship = "many-to-one"
    ) |> 
    unnest(midrets) |> 
    nest(data = c(day, from, side, rm)) |> 
    mutate(
      rvar = map_dbl(.x = data,
                     .f = compute_rv),
      rvol = sqrt(rvar)
    ) |> 
    pivot_longer(
      rvar:rvol,
      names_to  = "timing",
      values_to = "value"
    ) |> 
    group_by(strategy, portfolio, timing) |> 
    mutate(
      # weight =  slider::slide_dbl(.x        = value, 
      #                             .f        = ~mean(.x, na.rm = TRUE), 
      #                             .before   = Inf, 
      #                             .complete = TRUE) / value,
      weight = pmin(2,
                    value / slider::slide_dbl(.x        = value,
                                              .f        = ~mean(.x, na.rm = TRUE),
                                              .before   = Inf,
                                              .complete = TRUE)),
    ) |> 
    ungroup() |> 
    summarise(
      ret = cs_wlogret(ret_l, weight), # Since rvar is strictly nonnegative, it never takes short positions.,
      .by = c(date, strategy, portfolio, timing)
    ) 
  
}



rename_factors <- function(.data){
  
  # Original labels
  strategies <- c("cs_carry", "dol", "dol_carry", "naive", "ts_carry", 
                  "cs_mom1", "ts_mom1", "cs_mom3", "ts_mom3", 
                  "cs_mom6", "ts_mom6", "cs_mom12", "ts_mom12")
  
  # Nicer labels
  strategy_labels <- c("CS-Carry", "Dollar", "Dollar Carry", "Naive Multifactor", "TS-Carry",
                       "CS-Momentum-1m", "TS-Momentum-1m", "CS-Momentum-3m", "TS-Momentum-3m",
                       "CS-Momentum-6m", "TS-Momentum-6m", "CS-Momentum-12m", "TS-Momentum-12m")
  
  # Named vector for recoding
  label_map <- setNames(strategy_labels, strategies)
  
  .data |> 
    dplyr::mutate(
      strategy = dplyr::recode(strategy, !!!label_map)
    )
}



rename_portfolios <- function(.data){
  
  # Original labels
  original <- c("hml", "long", "short", "1/K", "single", "p2", "p3", "p4")
  
  # Nicer labels
  nicer <- c("HML", "Long", "Short", "Single", "Single", "Portfolio 2", "Portfolio 3", "Portfolio 4")
  
  # Named vector for recoding
  label_map <- setNames(nicer, original)
  
  .data |> 
    dplyr::mutate(
      portfolio = dplyr::recode(portfolio, !!!label_map)
    )
}



rename_timings <- function(.data){
  
  # Original labels
  timing <- c("mom_1_36", "mom_1_60", "mom_1_120",
              "mom_3_36", "mom_3_60", "mom_3_120",
              "mom_6_36", "mom_6_60", "mom_6_120",
              "mom_12_36", "mom_12_60", "mom_12_120",
              "rvar", "rvol")
  
  # Nicer labels
  timing_labels <-  c("Momentum-1m-3Y", "Momentum-1m-5Y", "Momentum-1m-10Y",
                      "Momentum-3m-3Y", "Momentum-3m-5Y", "Momentum-3m-10Y",
                      "Momentum-6m-3Y", "Momentum-6m-5Y", "Momentum-6m-10Y",
                      "Momentum-12m-3Y", "Momentum-12m-5Y", "Momentum-12m-10Y",
                      "RVar", "RVol")
  
  # Named vector for recoding
  label_map <- setNames(timing_labels, timing)
  
  .data |> 
    dplyr::mutate(
      timing = dplyr::recode(timing, !!!label_map)
    )
  
}



perf_stats <- function(.data,
                       .ret = ret,
                       ...){
  #   This function computes performance statistics. Input should be in log returns. Output is in simple returns.
  
  .data |> 
    group_by(...) |> 
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



compare_stats <- function(.factors, .timed, .stat, .ret = ret, .signif = 0.05){
  
  if (dplyr::enexpr(.stat) == "ann_ret") {
    
    significants <- .timed |> 
      left_join(
        .factors,
        by = dplyr::join_by(date, strategy),
        relationship = "many-to-one"
      ) |> 
      mutate(dif = ret.x - ret.y) |> 
      select(strategy, timing, dif) |> 
      summarise(
        pval = ifelse(t.test(dif)$p.value <= .signif, TRUE, FALSE),
        .by = c(strategy, timing)
      ) 
    
  }
  
  untimed_stats <- .factors |> 
    perf_stats(
      .ret = {{ .ret }},
      strategy
    ) |> 
    select(strategy, {{ .stat }})
  
  timed_stats <- .timed |> 
    perf_stats(
      .ret = {{ .ret }},
      strategy, timing
    ) |> 
    select(strategy, timing, {{ .stat }})
  
  res <- timed_stats |> 
    pivot_wider(
      names_from  = timing,
      values_from = {{ .stat }}
    ) |> 
    left_join(
      y  = untimed_stats,
      by = join_by(strategy)
    ) |> 
    select(
      strategy, {{ .stat }},
      `Momentum-1m-3Y`, `Momentum-1m-5Y`, `Momentum-1m-10Y`,
      `Momentum-3m-3Y`, `Momentum-3m-5Y`, `Momentum-3m-10Y`,
      `Momentum-6m-3Y`, `Momentum-6m-5Y`, `Momentum-6m-10Y`,
      `Momentum-12m-3Y`, `Momentum-12m-5Y`, `Momentum-12m-10Y`,
      `RVar`, `RVol`
    ) |> 
    mutate(
      across(
        .cols = -c(strategy, {{ .stat }}),
        .fns  = ~ .x - {{ .stat }}
      )
    ) |> 
    arrange(desc({{ .stat }})) |> 
    pivot_longer(
      -c(strategy, {{ .stat }}),
      names_to  = "timing",
      values_to = "dif"
    ) 
  
  if (dplyr::enexpr(.stat) == "ann_ret") {
    
    res |> 
      left_join(
        significants,
        by = join_by(strategy, timing),
        relationship = "many-to-one"
      )
     
  } else {
    
    res
    
  }
  
}



compute_alphas <- function(.factors, .timed){
  
  .timed |> 
    rename(timed_ret = ret) |> 
    left_join(
      y  = .factors,
      by = join_by(date, strategy)
    ) |> 
    mutate(
      strategy = paste(strategy, timing, sep = "."),
      # across(
      #   .cols = timed_ret:ret,
      #   .fns  = ~(exp(.x) - 1) * 100
      # )
    ) |> 
    select(-timing) |> 
    nest(data = -strategy) |> 
    mutate(
      reg = map(
        .x = data,
        .f = ~lm(timed_ret ~ ret, data = .x)
      ),
      tidy = map(
        .x = reg,
        .f = broom::tidy
      ) 
    ) |> 
    unnest(tidy) |> 
    filter(term == "(Intercept)") |> 
    select(strategy, estimate, p.value) |> 
    mutate(
      estimate = (exp(12 * estimate) - 1) * 100,
    ) |> 
    separate(
      col  = strategy,
      into = c("strategy", "timing"),
      sep  = "\\."
    ) |> 
    rename(
      pval = p.value,
      dif  = estimate
    )
  
}
