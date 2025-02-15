raw_to_basf <- function(.data){
  
  .data |> 
    pivot_longer(
      -date,
      names_to  = "code",
      values_to = "px"
    ) |> 
    separate_wider_delim(
      col   = code,
      delim = "(",
      names = c("code", "side")
    ) |> 
    mutate(
      date = as_date(date),
      side = if_else(side == "EB)", "bid", "ask") |> as.factor(),
      mkt  = if_else(str_ends(code, "F") | str_ends(code, "1MFP") | str_ends(code, "1M"),
                     "fwd",
                     "spot") |> as.factor()
    )
  
}
