if (!require("tidyverse")) install.packages("tidyverse"); library("tidyverse")
if (!require("frenchdata")) install.packages("frenchdata"); library("frenchdata")
if (!require("readxl")) install.packages("readxl"); library("readxl")

source("R/cleaners.R")

df <- readxl::read_xlsx("Data/ds_data.xlsx", sheet = "quotes", skip = 1) |> 
  rename(date = `Code`) |> 
  raw_to_basf() # Function that transforms raw data to a long tibble with identifiers for bid/ask and spot/forward.

#   When raw_to_basf pivots longer the data, all missing observations are preserved. Thus, we should have a tibble where
# half the rows are bids, half are asks, half are spots, and half are forwards. Check if these 4 conditions hold
df |> 
  summarise(
    bids  = sum(side == "bid") == nrow(df) / 2,
    asks  = sum(side == "ask") == nrow(df) / 2,
    spots = sum(mkt == "spot") == nrow(df) / 2,
    fwds  = sum(mkt == "fwd") == nrow(df) / 2
  )

#   Import table with base/price currencies of each ticker
fromto <- readxl::read_xlsx("Data/ds_data.xlsx", sheet = "fromto")

df <- df |> 
  fromto_scale(fromto, code, scale) |> 
  drop_na(px)

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

df |> 
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

#   Filter indirectly quoted currencies and convert them to direct quotes
df |> 
  filter(to == "United Kingdom Pound")
  filter(!(to %in% c("United States Dollar", "United Kingdom Pound"))) |> 
  drop_na(px) |> #FIXME have to invert all the quotes to USD. Since some are quoted vs GBP, have to first isolate all of those 
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
