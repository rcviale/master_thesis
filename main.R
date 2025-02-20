if (!require("tidyverse")) install.packages("tidyverse"); library("tidyverse")
if (!require("frenchdata")) install.packages("frenchdata"); library("frenchdata")
if (!require("readxl")) install.packages("readxl"); library("readxl")

source("R/cleaners.R")

#   Import table with base/price currencies, spread scales, tickers and countries
gen_inf <- readxl::read_xlsx("Data/ds_data.xlsx", sheet = "fromto")

df <- readxl::read_xlsx("Data/ds_data.xlsx", sheet = "quotes", skip = 1) |> 
  rename(date = `Code`) |> 
  raw_to_bidask() |> # Function that transforms raw data to a long tibble with identifiers for bid/ask.
  fromto_scale(gen_inf, code, scale) |> # Function to join with general info tibble and scale all spreads.
  select(-c(ticker, country)) |> # These columns will only be used later, after all currencies are directly quoted.
  drop_na() # Drop missing prices observations


#   Isolate GBPSUD 1m
gb_spr <- df |> 
  filter((from == "United Kingdom Pound" & to == "United States Dollar" & mkt == "spr") |
           (from == "United States Dollar" & to == "United Kingdom Pound" & mkt == "spr")) |> 
  mutate(px   = -px,
         temp = from,
         from = to,
         to   = temp,
         side = if_else(side == "bid", "ask", "bid")) |> 
  select(-temp)

usdgbp <- df |> 
  filter((from == "United Kingdom Pound" & to == "United States Dollar" & mkt == "spot") |
           (from == "United States Dollar" & to == "United Kingdom Pound" & mkt == "spot")) |> 
  bind_rows(gb_spr) |>
  # select(-c(from, to))
  pivot_wider(names_from = c(side, mkt),
              names_sep = "_",
              values_from = px) |> 
  mutate(ask_fwd = if_else(!is.na(ask_spr), ask_spot + ask_spr, NA),
         bid_fwd = if_else(!is.na(bid_spr), bid_spot + bid_spr, NA)) |> 
  select(-ends_with("spr")) |> 
  pivot_longer(-c(date, from, to),
               names_to = c("side", "mkt"),
               names_sep = "_",
               values_to = "px")

#   
df |> 
  filter( # Filter out USDGBP, since we already dealt with that.
    !(
      (from == "United States Dollar" & to == "United Kingdom Pound") |
        (from == "United Kingdom Pound" & to == "United States Dollar")
    )
  ) |> 
  filter(
    from == "United Kingdom Pound" | to == "United Kingdom Pound"
  ) |> 
  select(-to) |> # A1: everything that is quoted vs GBP (excluding USD) is directly quoted
  pivot_wider() 
#FIXME have to invert all the quotes to USD. Since some are quoted vs GBP, have to first isolate all of those 
  mutate(
    px = if_else(mkt == "spr", -px, 1 / px)
    side =
  )
# filter(!(to %in% c("United States Dollar", "United Kingdom Pound"))) |>
pivot_wider(names_from = c(mkt, side), 
            names_sep = "_",
            values_from = px) |> 
  mutate(
    out_bid = if_else(is.na(fwd_bid), spot_bid + spr_bid, fwd_bid),
    out_ask = if_else(is.na(fwd_ask), spot_ask + spr_ask, fwd_bid)
  ) |> 
  filter(date >= "1982-01-01") |> 
  view()
mutate(
  across(
    .cols = spot_bid:fwd_ask, 
    .fns  = ~ 1 / .x
  ),
  temp = from,
  from = to, 
  to   = temp
) |> 
  select(-temp) |> 
  rename(
    spot_ask = spot_bid,
    spot_bid = spot_ask,
    fwd_ask  = fwd_bid,
    fwd_bid  = fwd_ask
  )

mutate(
  px = case_when(
    !(to %in% c("United Kingdom Pound", "United States Dollar")) & mkt == "spot" ~ px
  )
)
pivot_wider(names_from = mkt, values_from = px) |> 
  drop_na(spr) |> 
  arrange(date, from) |> view()

tickers <- readxl::read_xlsx("Data/ds_data.xlsx", sheet = "nametickers")
