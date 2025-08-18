lustig_returns <- function(.data,
                           .side = "side", 
                           .market = "mkt"){
  # This function computes the long and short log returns (rl and rs) as in Lustig et al.
  # Args:
  #   .data   : tibble/data.frame with columns 'date', 'from', 'px', plus market and side identifiers
  #   .side   : column name (string) containing "bid"/"ask" info
  #   .market : column name (string) containing market type ("spot", "fwd", etc.)
  
  .data |> 
    tidyr::pivot_wider(                               # Reshape so each mkt/side combination becomes its own column
      names_from  = c({{ .market }}, {{ .side }}),    # Combine market type and side for column names
      names_sep   = ".",                              # Separate them with "."
      values_from = px                                # Fill with price values
    ) |> 
    dplyr::arrange(date, from) |>                     # Order rows by date and currency
    dplyr::mutate(
      date = lubridate::ceiling_date(date, unit = "month") - 1  # Set date to last day of the month
    ) |> 
    dplyr::group_by(date, from) |>                    # Group by month-end and currency
    dplyr::slice_tail(n = 1) |>                       # Keep the last observation in the month (if multiple)
    dplyr::ungroup() |> 
    purrr::modify_if(                                 # Apply log() to all numeric columns (convert to log prices)
      .p = is.numeric,
      .f = base::log
    ) |> 
    dplyr::group_by(from) |>                          # Group by currency to compute returns
    dplyr::mutate(
      rl = fwd.bid - dplyr::lead(spot.ask),           # Long return: fwd bid - next month's spot ask
      rs = -fwd.ask + dplyr::lead(spot.bid)           # Short return: -fwd ask + next month's spot bid
    ) |> 
    dplyr::ungroup() |> 
    tidyr::drop_na(rl, rs)                            # Drop rows where either rl or rs is missing
}



compute_signals <- function(.data){
  # This function computes all the signals that will be used as sorting variables for the replicated factors.
  # Signals include: Carry, Momentum (1, 3, 6, 12 months), and Dollar Carry.
  
  .data |> 
    dplyr::mutate(
      carry = fwd.bid - spot.ask   # Carry: forward bid minus spot ask
    ) |> 
    dplyr::group_by(from) |>       # Group by currency for rolling momentum calculations
    dplyr::mutate(
      # Momentum signals (lagged to avoid look-ahead bias):
      mom1  = dplyr::lag(rl),      # 1-month momentum = previous month's long return
      
      mom3  = slider::slide_dbl(   # Sum of last 3 months' long returns
        .x        = rl,
        .f        = base::sum,     # base::sum ensures no masking from other packages
        .before   = 2,             # Include 2 months before current → total 3 months
        .complete = TRUE           # Require a full window of data
      ) |> dplyr::lag(),
      
      mom6  = slider::slide_dbl(   # Sum of last 6 months' long returns
        .x        = rl,
        .f        = base::sum,
        .before   = 5,
        .complete = TRUE
      ) |> dplyr::lag(),
      
      mom12 = slider::slide_dbl(   # Sum of last 12 months' long returns
        .x        = rl,
        .f        = base::sum,
        .before   = 11,
        .complete = TRUE
      ) |> dplyr::lag()
    ) |> 
    dplyr::group_by(date) |>       # Group by date for cross-sectional calculations
    dplyr::mutate(
      avg_fd = base::mean(carry, na.rm = TRUE)  # Dollar Carry: mean carry across all currencies on a given date
    ) |> 
    dplyr::ungroup()               # Remove grouping to return a plain tibble
  
}



compute_mid_return <- function(.data){
  # This function computes monthly returns based on the mid price of spot quotes.
  # Args:
  #   .data: tibble/data.frame with columns 'mkt', 'side', 'px', 'date', 'from'
  
  .data |> 
    dplyr::filter(mkt == "spot") |>                # Keep only spot market quotes
    dplyr::select(-c(mkt, side)) |>                 # Remove market type and side columns
    dplyr::summarise(                               # Compute the average price (mid price) per currency-date
      mid_px  = base::mean(px, na.rm = TRUE),
      .by     = c(date, from)                       # Grouping directly inside summarise
    ) |> 
    dplyr::group_by(from) |>                        # Group by currency for return calculation
    dplyr::mutate(
      rm = (mid_px / dplyr::lag(mid_px) - 1)        # Compute return = current / previous - 1
    ) |> 
    dplyr::ungroup() |> 
    dplyr::select(-mid_px) |>                       # Remove mid price column (keep only returns)
    tidyr::drop_na(rm)                              # Drop missing returns
  
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
  # This function computes the returns for the Dollar Carry strategy.
  # Args:
  #   .data: tibble/data.frame containing 'avg_fd', 'rl', 'rs', 'from', 'date'
  # Logic:
  #   If average forward discount (avg_fd) is positive → go long all currencies, otherwise, go short.
  
  .data |> 
    dplyr::summarise(
      ret_l = base::ifelse(                               # Returns for longing the factor
        base::mean(avg_fd, na.rm = TRUE) >= 0,            # Same avg_fd across currencies per date
        cs_logret(rl),                                    # Long = rl when avg_fd ≥ 0
        cs_logret(rs)                                     # Long = rs when avg_fd < 0
      ),
      ret_s = base::ifelse(                               # Returns for shorting the factor
        base::mean(avg_fd, na.rm = TRUE) >= 0,
        cs_logret(rs),                                    # Short = rs when avg_fd ≥ 0
        cs_logret(rl)                                     # Short = rl when avg_fd < 0
      ),
      curs  = base::list(                                 # Store portfolio currencies
        base::list(hi = from, lo = from)                  # Both hi and lo are the same here (single portfolio)
      ),
      .by   = date                                        # Summarise per date
    ) |> 
    dplyr::mutate(
      strategy  = "dol_carry",                            # Add strategy name
      portfolio = "single"                                # Single portfolio (no sorting buckets)
    )
}



compute_dol <- function(.data){
  # This function computes returns for the Dollar strategy.
  # Args:
  #   .data: tibble/data.frame containing 'rl', 'rs', 'from', 'date'
  # Logic:
  #   Dollar strategy is the equally weighted return across all currencies.
  
  .data |> 
    dplyr::summarise(
      ret_l = cs_logret(rl),                              # Long factor return: average of rl across currencies
      ret_s = cs_logret(rs),                              # Short factor return: average of rs across currencies
      curs  = base::list(                                 # Store currencies in both hi and lo lists
        base::list(hi = from, lo = from)
      ),
      .by = date                                          # Summarise per date
    ) |> 
    dplyr::mutate(
      strategy  = "dol",                                  # Label as Dollar strategy
      portfolio = "single"                                # Single portfolio (no sorting buckets)
    )
}




compute_ts_factors <- function(.data){
  # This function computes Time-Series factors:
  #   - Time-Series Carry
  #   - Time-Series Momentum
  # For each signal, currencies are split into positive and negative var groups.
  # Returns are computed separately for these groups, and combined into HML and LMH portfolios.
  
  # Step 1: Aggregate returns by date and signal into positive and negative baskets
  .data <- .data |> 
    dplyr::group_by(date, signal) |> 
    dplyr::summarise(
      ret_l_pos = dplyr::if_else(                             # Long-leg return for positive var currencies
        rlang::is_empty(rl[var > 0]),
        0,
        cs_logret(rl[var > 0])
      ),
      ret_s_neg = dplyr::if_else(                             # Short-leg return for negative var currencies
        rlang::is_empty(rs[var < 0]),
        0,
        cs_logret(rs[var < 0])
      ),
      ret_l_neg = dplyr::if_else(                             # Long-leg return for negative var currencies
        rlang::is_empty(rl[var < 0]),
        0,
        cs_logret(rl[var < 0])
      ),
      ret_s_pos = dplyr::if_else(                             # Short-leg return for positive var currencies
        rlang::is_empty(rs[var > 0]),
        0,
        cs_logret(rs[var > 0])
      ),
      pos_curs  = base::list(curs = from[var > 0]),           # Positive var currency list
      neg_curs  = base::list(curs = from[var < 0]),           # Negative var currency list
      .groups   = "drop"
    ) |> 
    dplyr::mutate(
      hml      = ret_l_pos + ret_s_neg,                       # High-minus-Low portfolio returns
      lmh      = ret_l_neg + ret_s_pos,                       # Low-minus-High portfolio returns
      strategy = base::paste0("ts_", signal)                  # Strategy label
    ) |> 
    dplyr::select(-signal)
  
  # Step 2: Create "long" basket portfolio
  long <- .data |> 
    dplyr::select(
      date, 
      strategy, 
      ret_l = ret_l_pos, 
      ret_s = ret_s_pos, 
      curs  = pos_curs
    ) |> 
    dplyr::group_by(date, strategy) |> 
    dplyr::mutate(
      curs      = base::list(                                  # Store as list with hi and lo identical
        base::list(
          hi = base::as.vector(base::unlist(curs)), 
          lo = base::as.vector(base::unlist(curs))
        )
      ),
      portfolio = "long"
    )
  
  # Step 3: Create "short" basket portfolio
  short <- .data |> 
    dplyr::select(
      date, 
      strategy, 
      ret_l = ret_l_neg, 
      ret_s = ret_s_neg, 
      curs  = neg_curs
    ) |> 
    dplyr::group_by(date, strategy) |> 
    dplyr::mutate(
      curs      = base::list(
        base::list(
          hi = base::as.vector(base::unlist(curs)), 
          lo = base::as.vector(base::unlist(curs))
        )
      ),
      portfolio = "short"
    )
  
  # Step 4: Combine into final dataset with HML portfolio + long + short baskets
  .data |> 
    dplyr::select(
      date, 
      strategy, 
      ret_l   = hml,
      ret_s   = lmh,
      curs_hi = pos_curs, # HML long currencies
      curs_lo = neg_curs  # HML short currencies
    ) |>
    dplyr::mutate(
      curs = purrr::map2(                                     # Combine hi and lo lists into one
        .x = curs_hi, 
        .y = curs_lo, 
        .f = ~base::list(hi = .x, lo = .y)
      ),
      portfolio = "hml"
    ) |> 
    dplyr::select(-c(curs_hi, curs_lo)) |> 
    dplyr::bind_rows(long, short) |>                          # Append long and short baskets
    dplyr::arrange(date, strategy, portfolio)                 # Order by date, strategy, and portfolio type
  
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
    tidyr::drop_na({{ .variable }}) |>                 # tidyr: keep rows where the sorting variable is not NA
    dplyr::group_by(date, signal) |>                   # dplyr: sort independently within each (date, signal)
    dplyr::mutate(
      portfolio = assign_portfolio(                    # user helper: assign each obs to a portfolio bucket
        .data         = dplyr::pick(dplyr::everything()),  # dplyr: pass all current columns to helper
        .variable     = {{ .variable }},               # tidy-eval: the sorting variable (e.g., carry/momentum)
        .n_portfolios = .n_portfolios                  # number of portfolios (default 5)
      ),
      portfolio = base::as.factor(base::paste0("p", portfolio))  # label as "p1", "p2", ..., "pN"
    ) |>
    dplyr::group_by(portfolio, date, signal) |>
    dplyr::summarize(                                  # dplyr: compute portfolio-level returns per (date, signal)
      ret_l = cs_logret(rl),                           # user helper: cross-sectional log return (long leg)
      ret_s = cs_logret(rs),                           # user helper: cross-sectional log return (short leg)
      curs  = base::list(base::list(hi = from, lo = from)),  # store tickers in both legs for bookkeeping
      .groups = "drop"
    ) |> 
    dplyr::arrange(date, signal, portfolio) |>         # dplyr: tidy ordering
    dplyr::mutate(
      strategy = base::paste0("cs_", signal)           # tag as cross-sectional strategy for this signal
    ) |> 
    dplyr::select(-signal)                             # drop raw signal column (encoded in strategy label)
  
}



multiple_hml <- function(.data,
                         .n_portfolios = 5) {
  # This function computes the High-Minus-Low (HML) portfolio from sorted portfolios and appends it to the input.
  
  high_id <- base::paste0("p", .n_portfolios)                    # ID of highest portfolio bucket, e.g. "p5"
  
  longs <- .data |> 
    dplyr::filter(portfolio == high_id) |>                       # Keep highest portfolio (long leg)
    dplyr::select(-portfolio) |>                                 # Drop portfolio label (not needed for join)
    dplyr::rename(                                               # Rename columns to distinguish p5 leg
      ret_l_p5 = ret_l, 
      ret_s_p5 = ret_s, 
      curs_p5  = curs
    )
  
  .data |>
    dplyr::filter(portfolio == "p1") |>                          # Keep lowest portfolio (short leg)
    dplyr::select(-portfolio) |>                                 # Drop portfolio label (not needed for join)
    dplyr::rename(                                               # Rename columns to distinguish p1 leg
      ret_l_p1 = ret_l, 
      ret_s_p1 = ret_s, 
      curs_p1  = curs
    ) |>
    dplyr::inner_join(                                           # Join p1 leg with p5 leg by date and strategy
      longs,
      dplyr::join_by(date, strategy)
    ) |>
    dplyr::group_by(date, strategy) |>
    dplyr::mutate(
      ret_l = ret_l_p5 + ret_s_p1,                               # HML long-leg: long(p5) + short(p1)
      ret_s = ret_l_p1 + ret_s_p5,                               # LMH short-leg: long(p1) + short(p5)
      curs  = purrr::map2(                                       # Combine currency lists for bookkeeping
        .x = curs_p5,
        .y = curs_p1,
        .f = ~base::list(
          hi = base::as.vector(base::unlist(.x$hi)),             # Hi leg currencies (from p5)
          lo = base::as.vector(base::unlist(.y$hi))              # Lo leg currencies (from p1)
        )
      ),
      portfolio = base::as.factor("hml")                         # Label the constructed portfolio as "hml"
    ) |>
    dplyr::ungroup() |> 
    dplyr::select(date, strategy, portfolio, ret_l, ret_s, curs) |>  # Keep standard schema
    dplyr::bind_rows(.data) |>                                   # Append original portfolios (p1..pN)
    dplyr::arrange(date, strategy, portfolio)                    # Stable ordering
}



rename_edge_portfolios <- function(.data){
  # This function renames the extreme portfolios:
  #   - First portfolio (p1) → "short"
  #   - Last portfolio (p5, for quintiles) → "long"
  
  .data |> 
    dplyr::mutate(
      portfolio = dplyr::case_when(
        portfolio == "p1" ~ "short",   # Rename lowest-ranked portfolio
        portfolio == "p5" ~ "long",    # Rename highest-ranked portfolio
        .default          = portfolio  # Keep other portfolio labels unchanged
      )
    )
}



compute_naive <- function(.data,
                          .portfolios = c("hml", "single")){
  # This function computes the "naive" equal-weighted (1/K) factor
  # from existing HML and single portfolios, aggregating across them.
  
  naives <- .data |> 
    dplyr::filter(portfolio %in% c("hml", "single")) |>      # Keep only HML and single portfolios
    dplyr::group_by(date) |> 
    dplyr::summarise(
      ret_l = cs_logret(ret_l),                              # Equal-weighted long-leg return
      ret_s = cs_logret(ret_s),                              # Equal-weighted short-leg return
      curs  = base::list(
        base::list(
          hi = base::as.vector(base::unlist(                 # Combine all hi-side currencies across portfolios
            purrr::map(.x = curs, .f = "hi")
          )),
          lo = base::as.vector(base::unlist(                 # Combine all lo-side currencies across portfolios
            purrr::map(.x = curs, .f = "lo")
          ))
        )
      ),
      .groups = "drop"
    ) |> 
    dplyr::mutate(
      curs = purrr::map(                                     # Process each row's hi/lo list to rebalance
        curs,
        \(x) {                                               
          all_cur <- base::union(x$hi, x$lo)                 # All currencies appearing in either side
          
          # Balance = (# times in hi) − (# times in lo)
          bal <- base::sapply(
            all_cur,
            function(c) base::sum(x$hi == c) - base::sum(x$lo == c)
          )
          
          base::list(                                        # Wrap in a single list for list-column
            hi = base::rep(all_cur[bal > 0],  bal[bal > 0]), # Positive balance → long side
            lo = base::rep(all_cur[bal < 0], -bal[bal < 0])  # Negative balance → short side
          )
        }
      )
    ) |> 
    dplyr::mutate(
      strategy  = "naive",                                   # Strategy label
      portfolio = "1/K"                                      # Equal-weighted across K portfolios
    )
  
  .data |> 
    dplyr::bind_rows(naives)                                 # Append naive portfolio to original data
  
}



organize_portfolios <- function(.data,
                                .timed = FALSE){
  # This function orders the merged factor tibble and keeps only relevant columns.
  # Args:
  #   .data  : tibble/data.frame of factor returns
  #   .timed : if TRUE, keep timing-related columns; otherwise, keep long/short returns and currencies
  
  if (.timed == TRUE){
    
    .data |> 
      dplyr::arrange(date, strategy, portfolio) |>     # Sort chronologically and by strategy/portfolio
      dplyr::select(date, strategy, portfolio, timing, ret)  # Keep only timing columns
    
  } else {
    
    .data |> 
      dplyr::arrange(date, strategy, portfolio) |>     # Sort chronologically and by strategy/portfolio
      dplyr::select(date, strategy, portfolio, ret_l, ret_s, curs)  # Keep L/S returns and currency baskets
    
  }
  
}




timed_momentum <- function(.data,
                           .mom_windows = c(1, 3, 6, 12),
                           .vol_windows = c(36, 60, 120)) {
  # This function computes volatility-scaled (timed) momentum returns for different 
  # combinations of momentum lookback windows and volatility estimation windows.
  
  .data |>
    dplyr::mutate(
      vol_window = base::list(.vol_windows)                      # Store volatility windows in list column
    ) |>
    tidyr::unnest(vol_window) |>                                 # Expand each vol_window into its own row
    dplyr::group_by(strategy, portfolio, vol_window) |> 
    dplyr::mutate(
      vol = base::sqrt(12) *                                     # Annualized volatility
        slider::slide_dbl(
          .x        = base::expm1(ret_l),                        # Convert log returns to simple returns
          .f        = ~ stats::sd(.x, na.rm = TRUE),             # Rolling standard deviation
          .before   = vol_window[1] - 1,                         # Window length
          .complete = TRUE
        ) |> dplyr::lag()                                        # Lag so signal is ex-ante
    ) |> 
    dplyr::ungroup() |> 
    dplyr::mutate(
      mom_window = base::list(.mom_windows)                      # Store momentum lookback windows in list column
    ) |>
    tidyr::unnest(mom_window) |>                                 # Expand each mom_window into its own row
    dplyr::arrange(date) |> 
    dplyr::group_by(strategy, portfolio, vol_window, mom_window) |> 
    dplyr::mutate(
      mom = (12 / mom_window) *                                  # Annualized momentum
        slider::slide_dbl(
          .x        = base::expm1(ret_l),                        # Simple returns
          .f        = ~ base::sum(.x, na.rm = TRUE),             # Rolling sum of returns
          .before   = mom_window[1] - 1,                         # Window length
          .complete = TRUE
        ) |> dplyr::lag(),                                       # Lag for ex-ante signal
      timing = base::paste0("mom_", mom_window, "_", vol_window) # Timing label (e.g., mom_3_60)
    ) |> 
    tidyr::drop_na(mom, vol) |>                                  # Remove rows with missing momentum or vol
    dplyr::group_by(strategy, portfolio, timing) |> 
    dplyr::mutate(
      weight = base::pmax(-2, base::pmin(2, mom / vol))          # Vol-scaled signal, capped at ±2
    ) |> 
    dplyr::ungroup() |> 
    dplyr::summarise(
      ret = cs_wlogret(                                          # Weighted cross-sectional log return
        base::ifelse(weight >= 0, ret_l, ret_s),                 # Long when signal ≥ 0, else short
        base::abs(weight)                                        # Weight magnitude
      ),
      .by = c(date, strategy, portfolio, timing)
    )
}



compute_rv <- function(.data) {
  # This function computes realized variance (RV) over the last 22 days
  # based on average daily returns across currencies.
  
  .data |>
    dplyr::summarise(
      ret = base::mean(rm, na.rm = TRUE),                  # Mean return across currencies for each (day, side)
      .by  = c(day, side)
    ) |> 
    dplyr::summarise(
      ret_day = base::sum(ret),                            # Add long and short sides (or just long if no short)
      .by = day
    ) |>   
    dplyr::slice_tail(n = 22) |>                           # Keep only last 22 days to avoid missing-data bias
    dplyr::summarise(
      rv = base::sum(                                      # Realized variance: sum of squared deviations
        (ret_day - base::mean(ret_day, na.rm = TRUE))^2, 
        na.rm = TRUE
      )
    ) |>
    dplyr::pull(rv)                                        # Return scalar RV value
  
}



timed_variance <- function(.data, .midspots){
  # This function applies volatility/variance timing to factor portfolios
  # based on realized variance computed from mid-spot returns.
  
  # --- Step 1: Prepare rolling mid-price returns windows ---
  midrets <- .midspots |> 
    dplyr::group_by(from) |> 
    dplyr::mutate(
      month  = lubridate::ceiling_date(date, "month") - 1,        # Month-end date
      window = slider::slide(                                     # Rolling 22-day window of dates
        .x        = date, 
        .f        = base::identity, 
        .before   = 21,
        .complete = TRUE
      )
    ) |>
    dplyr::ungroup() |> 
    tidyr::drop_na(window) |>                                     # Drop incomplete windows
    dplyr::filter(date == month) |>                               # Keep only month-end observations
    dplyr::select(month, from, date = window) |> 
    tidyr::unnest(date) |>                                        # Expand each window’s dates
    dplyr::left_join(                                             # Add returns for those dates
      .midspots, 
      by = dplyr::join_by(date, from)
    ) |> 
    dplyr::select(date = month, day = date, from, rm) |> 
    tidyr::nest(midrets = -c(date, from))                         # Store per-currency return windows as list-cols
  
  # --- Step 2: Match currency baskets to midrets data ---
  .data |>
    tidyr::unnest_longer(                                         # Expand hi/lo currency baskets
      curs, 
      indices_to = "side", 
      values_to  = "from"
    ) |> 
    tidyr::unnest(from) |> 
    dplyr::filter(
      side != "lo" | portfolio %in% c("hml", "1/K")               # Keep only hi leg unless it's HML or 1/K
    ) |>
    dplyr::left_join(                                             # Attach midrets to matching currencies
      y  = midrets,
      by = dplyr::join_by(date, from),
      relationship = "many-to-one"
    ) |> 
    tidyr::unnest(midrets) |> 
    tidyr::nest(data = c(day, from, side, rm)) |>                  # Nest returns for RV computation
    dplyr::mutate(
      rvar = purrr::map_dbl(.x = data, .f = compute_rv),           # Realized variance
      rvol = base::sqrt(rvar)                                      # Realized volatility
    ) |> 
    tidyr::pivot_longer(                                           # Long format: rvar & rvol
      rvar:rvol,
      names_to  = "timing",
      values_to = "value"
    ) |> 
    dplyr::group_by(strategy, portfolio, timing) |> 
    dplyr::mutate(
      weight = base::pmin(                                         # Scale exposure by relative variance/volatility
        2,
        value / slider::slide_dbl(
          .x        = value,
          .f        = ~ base::mean(.x, na.rm = TRUE),
          .before   = Inf,
          .complete = TRUE
        )
      )
    ) |> 
    dplyr::ungroup() |> 
    dplyr::summarise(
      ret = cs_wlogret(ret_l, weight),                             # Weighted return (no shorts, since var ≥ 0)
      .by = c(date, strategy, portfolio, timing)
    )
  
}



rename_factors <- function(.data){
  # This function replaces internal factor strategy codes with human-readable labels.
  
  # Original labels (internal strategy codes)
  strategies <- c(
    "cs_carry", "dol", "dol_carry", "naive", "ts_carry", 
    "cs_mom1", "ts_mom1", "cs_mom3", "ts_mom3", 
    "cs_mom6", "ts_mom6", "cs_mom12", "ts_mom12"
  )
  
  # Human-friendly labels
  strategy_labels <- c(
    "CS-Carry", "Dollar", "Dollar Carry", "Naive Multifactor", "TS-Carry",
    "CS-Momentum-1m", "TS-Momentum-1m", "CS-Momentum-3m", "TS-Momentum-3m",
    "CS-Momentum-6m", "TS-Momentum-6m", "CS-Momentum-12m", "TS-Momentum-12m"
  )
  
  # Named vector for recoding
  label_map <- stats::setNames(strategy_labels, strategies)
  
  .data |> 
    dplyr::mutate(
      strategy = dplyr::recode(strategy, !!!label_map)  # Replace codes with nicer labels
    )
  
}



rename_portfolios <- function(.data){
  # This function replaces portfolio codes with human-readable names.
  
  # Original portfolio codes
  original <- c("hml", "long", "short", "1/K", "single", "p2", "p3", "p4")
  
  # Human-friendly labels
  nicer <- c("HML", "Long", "Short", "Single", "Single", "Portfolio 2", "Portfolio 3", "Portfolio 4")
  
  # Named vector for recoding
  label_map <- stats::setNames(nicer, original)
  
  .data |> 
    dplyr::mutate(
      portfolio = dplyr::recode(portfolio, !!!label_map)  # Replace codes with nicer labels
    )
  
}




rename_timings <- function(.data){
  # This function replaces raw timing variable codes with more readable labels.
  
  # Original timing codes
  timing <- c(
    "mom_1_36", "mom_1_60", "mom_1_120",
    "mom_3_36", "mom_3_60", "mom_3_120",
    "mom_6_36", "mom_6_60", "mom_6_120",
    "mom_12_36", "mom_12_60", "mom_12_120",
    "rvar", "rvol"
  )
  
  # Human-friendly labels
  timing_labels <-  c(
    "Momentum-1m-3Y", "Momentum-1m-5Y", "Momentum-1m-10Y",
    "Momentum-3m-3Y", "Momentum-3m-5Y", "Momentum-3m-10Y",
    "Momentum-6m-3Y", "Momentum-6m-5Y", "Momentum-6m-10Y",
    "Momentum-12m-3Y", "Momentum-12m-5Y", "Momentum-12m-10Y",
    "RVar", "RVol"
  )
  
  # Named mapping vector
  label_map <- stats::setNames(timing_labels, timing)
  
  .data |> 
    dplyr::mutate(
      timing = dplyr::recode(timing, !!!label_map)  # Replace codes with nicer labels
    )
  
}



perf_stats <- function(.data,
                       .ret = ret,
                       ...){
  # This function computes performance statistics for portfolio returns.
  # Input:  log returns
  # Output: annualized simple returns (%), volatility (%), and Sharpe ratio
  
  .data |> 
    dplyr::group_by(...) |> 
    dplyr::summarise(
      ann_ret = (base::exp(base::mean({{ .ret }}) * 12) - 1) * 100,          # Annualized return (%)
      ann_vol = stats::sd(base::exp({{ .ret }}) - 1) * base::sqrt(12) * 100, # Annualized volatility (%)
      .groups = "drop"
    ) |>
    dplyr::mutate(
      sharpe  = ann_ret / ann_vol                                           # Sharpe ratio
    ) |> 
    purrr::modify_if(
      .p = base::is.numeric,
      .f = ~ base::round(.x, 2)                                             # Round all numeric columns to 2 decimals
    ) |> 
    dplyr::arrange(dplyr::desc(sharpe))                                     # Sort by Sharpe ratio (descending)
}



compare_stats <- function(.factors, .timed, .stat, .ret = ret, .signif = 0.05){
  # Compare a chosen performance statistic (.stat) between timed and untimed factors.
  # Optionally mark statistically significant improvements when comparing annualized returns.
  
  if (dplyr::enexpr(.stat) == "ann_ret") {
    # For annualized returns, test if the mean difference (timed − untimed) is significant per (strategy, timing)
    significants <- .timed |> 
      dplyr::left_join(                                      # Join timed with untimed series on (date, strategy)
        .factors,
        by = dplyr::join_by(date, strategy),
        relationship = "many-to-one"
      ) |> 
      dplyr::mutate(dif = ret.x - ret.y) |>                  # Difference of returns at each date
      dplyr::select(strategy, timing, dif) |> 
      dplyr::summarise(                                      # One t-test per (strategy, timing)
        pval = base::ifelse(stats::t.test(dif)$p.value <= .signif, TRUE, FALSE),
        .by = c(strategy, timing)
      )
  }
  
  # Compute untimed stats per strategy
  untimed_stats <- .factors |> 
    perf_stats(
      .ret = {{ .ret }},
      strategy
    ) |> 
    dplyr::select(strategy, {{ .stat }})
  
  # Compute timed stats per (strategy, timing)
  timed_stats <- .timed |> 
    perf_stats(
      .ret = {{ .ret }},
      strategy, timing
    ) |> 
    dplyr::select(strategy, timing, {{ .stat }})
  
  # Pivot timed stats wide by timing, join untimed baseline, compute deltas, then long format
  res <- timed_stats |> 
    tidyr::pivot_wider(
      names_from  = timing,
      values_from = {{ .stat }}
    ) |> 
    dplyr::left_join(
      y  = untimed_stats,
      by = dplyr::join_by(strategy)
    ) |> 
    dplyr::select(
      strategy, {{ .stat }},
      `Momentum-1m-3Y`, `Momentum-1m-5Y`, `Momentum-1m-10Y`,
      `Momentum-3m-3Y`, `Momentum-3m-5Y`, `Momentum-3m-10Y`,
      `Momentum-6m-3Y`, `Momentum-6m-5Y`, `Momentum-6m-10Y`,
      `Momentum-12m-3Y`, `Momentum-12m-5Y`, `Momentum-12m-10Y`,
      `RVar`, `RVol`
    ) |> 
    dplyr::mutate(
      dplyr::across(
        .cols = -c(strategy, {{ .stat }}),
        .fns  = ~ .x - {{ .stat }}                         # Difference vs. untimed baseline
      )
    ) |> 
    dplyr::arrange(dplyr::desc({{ .stat }})) |> 
    tidyr::pivot_longer(
      -c(strategy, {{ .stat }}),
      names_to  = "timing",
      values_to = "dif"
    )
  
  # Attach significance flags only for ann_ret; otherwise return the differences table
  if (dplyr::enexpr(.stat) == "ann_ret") {
    res |> 
      dplyr::left_join(
        significants,
        by = dplyr::join_by(strategy, timing),
        relationship = "many-to-one"
      )
  } else {
    res
  }
  
}



compute_alphas <- function(.factors, .timed){
  # Computes alphas (intercepts) from regressions of timed vs untimed factor returns.
  # Returns: strategy, timing, annualized alpha (%), and p-value.
  
  .timed |> 
    dplyr::rename(timed_ret = ret) |>                       # Rename timed return column for clarity
    dplyr::left_join(                                       # Join timed with untimed series by (date, strategy)
      y  = .factors,
      by = dplyr::join_by(date, strategy)
    ) |> 
    dplyr::mutate(
      strategy = base::paste(strategy, timing, sep = ".")   # Encode timing in strategy label (e.g., "CS-Carry.mom_3_60")
    ) |> 
    dplyr::select(-timing) |>                               # Drop separate timing column (now in strategy label)
    tidyr::nest(data = -strategy) |>                        # One nested tibble per strategy (with its dates)
    dplyr::mutate(
      reg = purrr::map(                                     # Run OLS per strategy: timed_ret ~ ret
        .x = data,
        .f = ~ stats::lm(timed_ret ~ ret, data = .x)
      ),
      tidy = purrr::map(                                    # Tidy regression output
        .x = reg,
        .f = broom::tidy
      ) 
    ) |> 
    tidyr::unnest(tidy) |>                                  # Expand tidy results back into rows
    dplyr::filter(term == "(Intercept)") |>                 # Keep alpha (intercept) rows only
    dplyr::select(strategy, estimate, p.value) |>           # Keep alpha estimate and p-value
    dplyr::mutate(
      estimate = (base::exp(12 * estimate) - 1) * 100       # Annualize alpha (log→simple, in %)
    ) |> 
    tidyr::separate(                                        # Split back into strategy and timing columns
      col  = strategy,
      into = c("strategy", "timing"),
      sep  = "\\."
    ) |> 
    dplyr::rename(
      pval = p.value,                                       # Rename p-value column
      dif  = estimate                                       # Rename alpha estimate to 'dif' for consistency
    )
  
}
