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



direct_quote <- function(.data,
                         .spread = TRUE){
  #   This function takes indirect quotes and turns them into direct ones.
  
  temp <- .data |> 
    dplyr::pull(from)
  
  if (.spread == TRUE) {
    
    .data |> 
      dplyr::mutate(
        px   = -px,
        from = to,
        to   = temp,
        side = dplyr::if_else(side == "bid", "ask", "bid")
      )
    
  } else { 
    
    .data |> 
      dplyr::mutate(
        px   = 1 / px,
        from = to,
        to   = temp,
        side = dplyr::if_else(side == "bid", "ask", "bid")
      )
    
  }
  
}



keep_usdgbp <- function(.data,
                        .keep = FALSE){
  #   This function filters out the USDGBP pair or isolates it.
  
  if (.keep == FALSE){
    
    .data |> 
      filter(
        !(
          (from == "United States Dollar" & to == "United Kingdom Pound") |
            (from == "United Kingdom Pound" & to == "United States Dollar")
        )
      )
    
  } else {
    
    .data |> 
      filter(
        (from == "United States Dollar" & to == "United Kingdom Pound") |
          (from == "United Kingdom Pound" & to == "United States Dollar")
      )
    
  }
  
}



spr_to_outright <- function(.data,
                            .froms){
  #   This function converts spreads to outright quotes. Returns a tibble with the outright and spot quotes.
  
  if (.froms == "United Kingdom Pound"){
    
    # Add USD to the .froms vector
    .froms <- c(.froms, "United States Dollar")
    
    spots <-  .data |> 
      dplyr::filter(from %in% .froms & to %in% .froms & mkt == "spot") 
    
    #   Isolate the USDGBP indirect spread quotes and turn it into a direct spread.
    .data <- .data |> 
      dplyr::filter(from %in% .froms & to %in% .froms & mkt == "spr") |> 
      direct_quote(.spread = TRUE) 
    
  } else {
    
    spots <- .data |> 
      dplyr::filter(from %in% .froms & mkt == "spot")
    
  }
  
  spots_to_join <- spots |> 
    dplyr::select(-c(to, mkt),
                  spot = px)
  
  .data |> 
    dplyr::inner_join( # Join with respective spot quotes 
      spots_to_join,
      by = dplyr::join_by(date, side, from)
    ) |> 
    dplyr::mutate(
      px  = spot + px, # Compute outright forward quotes
      mkt = if_else(mkt == "spr", "fwd", mkt) # Change mkt identifier
    ) |> 
    dplyr::select(-spot) |> # Remove column
    dplyr::bind_rows(spots)
  
}



cross_to_usd <- function(.data,
                         .cross = usdgbp,
                         .to = "United Kingdom Pound"){
  #   This function converts all quotes in .from currency to USD. 
  #   Only for direct quotes (no pair with GBP is indirect, except for USDGBP, which was separately treated).
  
  .cross <- .cross |> 
    dplyr::select(-c(from, to), 
           cross = px)
  
  .data |> 
    dplyr::filter( # Filter everything that is quoted vs. GBP
      to == .to
    ) |> # C3.1
    dplyr::select(-to) |> 
    dplyr::inner_join(
      .cross,
      by = dplyr::join_by(date, side, mkt),
      relationship = "many-to-one"
    ) |>
    dplyr::mutate( # C3.2
      px = px * cross, # GBPXXX * USDGBP = USDXXX
      to = "United States Dollar"
    ) |> 
    dplyr::select(-cross)
  
}



lustig_cleaning <- function(.data){
  #   Based on large failures of covered interest rate parity, we chose to delete the following observations from 
  # our sample: 
  #   - South Africa from the end of July 1985 to the end of August 1985; 
  #   - Malaysia from the end of August 1998 to the end of June 2005; 
  #   - Indonesia from the end of December 2000 to the end of May 2007; 
  #   - Turkey from the end of October 2000 to the end of November 2001; 
  #   - United Arab Emirates from the end of June 2006 to the end of November 2006. 
  
  .data |> 
    dplyr::filter(
      # South Africa from the end of July 1985 to the end of August 1985
      !(from == "South African Rand" & date >= "1985-07-01" & date <= "1985-08-31") &
        
        # Malaysia from the end of August 1998 to the end of June 2005
        !(from == "Malaysian Ringgit" & date >= "1998-08-01" & date <= "2005-06-30") & 
        
        # Indonesia from the end of December 2000 to the end of May 2007
        !(from == "Indonesian Rupiah" & date >= "2000-12-01" & date <= "2007-05-31") & 
        
        # Turkey from the end of October 2000 to the end of November 2001
        !(from == "Turkish Lira" & date >= "2000-10-01" & date <= "2001-11-30") & 
        
        # United Arab Emirates from the end of June 2006 to the end of November 2006
        !(from == "United Arab Emirates Dirham" & date >= "2006-06-01" & date <= "2006-11-30")
    )
  
}
