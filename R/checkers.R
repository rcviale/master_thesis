check1 <- function(.data){
  #   This function checks if half the observations are bids, asks, spots and forwards (including spreads).
  
  response <- .data |> 
    rename(date = `Code`) |> 
    raw_to_bidask() |> # Function that transforms raw data to a long tibble with identifiers for bid/ask.
    fromto_scale(gen_inf, code, scale) |> # Function to join with general info tibble and scale all spreads.
    select(-c(ticker, country)) |> 
    summarise(
      n_bid      = sum(side == "bid"),
      n_ask      = sum(side == "ask"),
      n_spot     = sum(mkt == "spot"),
      n_fwd_spr  = sum(mkt %in% c("fwd", "spr"))
    ) |> 
    mutate(
      side_dif  = n_bid - n_ask,
      mkt_dif   = n_spot - n_fwd_spr
    )
  
  writeLines(paste0("Bids/asks difference: ", response |> pull(side_dif)))
  
  writeLines(paste0("Spots/forwards difference: ", response |> pull(mkt_dif)))
  
}



check2 <- function(.data){
  #   This function checks if all bids < asks.
  
  response <- .data |> 
    pivot_wider(
      names_from  = side,
      values_from = px
    ) |> 
    summarise(
      bads = sum(bid > ask, na.rm = T)
    ) |> 
    pull(bads)
  
  N <- .data |> 
    pivot_wider(
      names_from  = side,
      values_from = px
    ) |> 
    nrow()
  
  writeLines(paste0("Percentage of bids > asks: ", round(response / N * 100, 2), "%"))
  
}



check3 <- function(.data){
  #   This function checks that all currencies with GBP on one side (excluding USD) are directly quoted. 
  
  response <- .data |> 
    filter( # Filter out USDGBP, since we already dealt with that.
      !(
        (from == "United States Dollar" & to == "United Kingdom Pound") |
          (from == "United Kingdom Pound" & to == "United States Dollar")
      )
    ) |> 
    filter( # Filter everything that is quoted vs. GBP
      from == "United Kingdom Pound" | to == "United Kingdom Pound"
    ) |> 
    summarise(
      to_not_gbp = sum(to != "United Kingdom Pound")
    ) |> 
    pull(to_not_gbp)
  
  writeLines(paste0("Amount of indirect quotes with GBP: ", response))
  
}



check4 <- function(.data){
  #   This function checks that the mid (bid and ask mean) spot USDCHF cross rate on 31/01/2025 is ~0.91
  
  response <- .data |> 
    filter(from == "Swiss Franc" & to == "United States Dollar" & date == "2025-01-31") |> 
    pull(px) |> 
    mean() |> 
    round(2)
  
  writeLines(paste0("Mid spot USDCHF cross rate: ", response))
  
}
