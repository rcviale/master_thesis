raw_to_bidask <- function(.data,
                          .long_cols = c("code", "px"),
                          .sep_delim = "(",
                          .sep_into  = c("code", "side"),
                          .bid_id    = "EB)",
                          .fwd_end   = c("F", "1MFP", "1M")){
  #   This function takes the raw data from DS and formats it into a long tibble with date, code, side, px, mkt
  #   Input: Codes of time series as headers, dates in the first column. Series for bids, asks, spots and forwards.
  
  .data |> 
    tidyr::pivot_longer( # Pivot into code column for DS codes (includes suffix for bid/ask) and px for prices.
      -date,
      names_to  = .long_cols[1],
      values_to = .long_cols[2]
    ) |> 
    tidyr::separate_wider_delim( # DS provides the time series with format X(EB)/X(EO), where X is the DS code, and 
      # EB/EO are identifiers for bids and asks, respectively. Separate bid/ask identifier into new column.
      col   = code,
      delim = .sep_delim,
      names = .sep_into
    ) |> 
    dplyr::mutate(
      date = lubridate::as_date(date), # Convert from datetime to date (since I use monthly data, time is not relevant).
      side = dplyr::if_else(side == .bid_id, "bid", "ask"), # Change bid/ask identifier column.
    )
  
}


fromto_scale <- function(.data,
                         .fromto,
                         .by_col,
                         .scale_col){
  #   This function joins the quotes data with the table containing information on the from/to and the spreads scales.
  
  .data |> 
    left_join(
      y            = .fromto,
      by           = join_by( {{ .by_col }} ),
      relationship = "many-to-one" # Each row in df has to match at most 1 row in fromto.
    ) |> 
    mutate(
      px  = ifelse(!is.na( {{ .scale_col }} ), px / {{ .scale_col }}, px), # Scale spreads according to scale (outrights are NA).
      mkt = ifelse(!is.na( {{ .scale_col }} ), "spr", mkt) # Identify spreads in mkt column. After this, mkt column contains 
      # 3 identifiers: spot, spr (spread quotes) and fwd (outright quotes)
    ) |> 
    select(-c( {{ .by_col }}, {{ .scale_col }} )) # Drop name and scale column
  
}



check_basf <- function(.data){
  #   This function checks if half the observations are bids, asks, spots and forwards (including spreads).
  
  response <- .data |> 
    summarise(
      total_rows = n(),
      n_bid      = sum(side == "bid"),
      n_ask      = sum(side == "ask"),
      n_spot     = sum(mkt == "spot"),
      n_fwd_spr  = sum(mkt %in% c("fwd", "spr"))
    ) |> 
    mutate(
      side_balanced  = (n_bid == n_ask),
      mkt_balanced   = (n_spot == n_fwd_spr),
      all_balanced   = (side_balanced == TRUE) & (mkt_balanced == TRUE)
    ) |> 
    pull(all_balanced)
    

  writeLines(paste0("Half bids/asks and spots/forwards: ", response))
  
}
