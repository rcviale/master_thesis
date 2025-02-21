if (!require("tidyverse")) install.packages("tidyverse"); library("tidyverse")
if (!require("frenchdata")) install.packages("frenchdata"); library("frenchdata")
if (!require("readxl")) install.packages("readxl"); library("readxl")

source("R/cleaners.R")
source("R/checkers.R")

#   Import table with base/price currencies, spread scales, tickers and countries
gen_inf <- readxl::read_xlsx("Data/ds_data.xlsx", sheet = "fromto")

df <- readxl::read_xlsx("Data/ds_data.xlsx", sheet = "quotes", skip = 1) |> 
  rename(date = `Code`) |> 
  raw_to_bidask() |> # Function that transforms raw data to a long tibble with identifiers for bid/ask.
  fromto_scale(gen_inf, code, scale) |> # Function to join with general info tibble and scale all spreads.
  select(-c(ticker, country)) |> # These columns will only be used later, after all currencies are directly quoted.
  drop_na() # Drop missing prices observations

# C1: After these computations, but before dropping NAs, we should have 50% of observations being bids/asks/spots/forwards.
readxl::read_xlsx("Data/ds_data.xlsx", sheet = "quotes", skip = 1) |> 
  check1() # Ideal: 0

#   Isolate USDGBP 1m forward spread and quote all of it directly
gbp_spr <- df |> 
  filter((from == "United Kingdom Pound" & to == "United States Dollar" & mkt == "spr") |
           (from == "United States Dollar" & to == "United Kingdom Pound" & mkt == "spr")) |> 
  mutate(px   = -px,
         temp = from,
         from = to,
         to   = temp,
         side = if_else(side == "bid", "ask", "bid")) |> 
  select(-temp)

#   Isolate USDGBP spot and forward quotes, compute outright forward quotes
usdgbp <- df |> 
  filter((from == "United Kingdom Pound" & to == "United States Dollar" & mkt == "spot") |
           (from == "United States Dollar" & to == "United Kingdom Pound" & mkt == "spot")) |> 
  bind_rows(gbp_spr) |>
  pivot_wider(names_from = c(side, mkt),
              names_sep = "_",
              values_from = px) |> 
  mutate(ask_fwd = if_else(!is.na(ask_spr), ask_spot + ask_spr, NA),
         bid_fwd = if_else(!is.na(bid_spr), bid_spot + bid_spr, NA)) |> 
  select(-ends_with("spr")) |> 
  pivot_longer(-c(date, from, to),
               names_to = c("side", "mkt"),
               names_sep = "_",
               values_to = "px") |> 
  drop_na()

rm(gbp_spr)

#   C2: All bids < asks
usdgbp |> 
  check2() # Ideal = 0

#   Isolate all the quotes vs. GBP and compute the USDXXX cross rate
gbpxxx <- df |> 
  filter( # Filter out USDGBP, since we already dealt with that.
    !(
      (from == "United States Dollar" & to == "United Kingdom Pound") |
        (from == "United Kingdom Pound" & to == "United States Dollar")
    )
  ) |> 
  filter( # Filter everything that is quoted vs. GBP
    from == "United Kingdom Pound" | to == "United Kingdom Pound"
  ) |> # C3
  select(-to) |> 
  inner_join(usdgbp |> 
               select(-c(from, to), 
                      usdgbp = px),
             by = join_by(date, side, mkt),
             relationship = "many-to-one") |>
  mutate(
    px = px * usdgbp, # GBPXXX * USDGBP = USDXXX
    to = "United States Dollar"
  ) 

#   C3: everything that is quoted vs GBP (excluding USD) is directly quoted
df |> 
  check3() # Ideal = 0

#   C4: (sanity check) USDCHF mid spot cross rate should be ~.91 on 31/01/2025 (Yahoo! Finance value)
gbpxxx |> 
  check4() # Ideal = .91

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
