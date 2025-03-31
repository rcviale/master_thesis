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

#   Isolate USDGBP 1m forward spread and quote it directly and outright (USDGBP fwd is spread quoted originally)
gbp_spr <- df |> 
  filter((from == "United Kingdom Pound" & to == "United States Dollar" & mkt == "spr") |
           (from == "United States Dollar" & to == "United Kingdom Pound" & mkt == "spr")) |> 
  direct_quote(.spread = TRUE)

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
gbpx <- df |> 
  filter( # Filter out USDGBP, since we already dealt with that.
    !(
      (from == "United States Dollar" & to == "United Kingdom Pound") |
        (from == "United Kingdom Pound" & to == "United States Dollar")
    )
  ) |> 
  filter( # Filter everything that is quoted vs. GBP
    from == "United Kingdom Pound" | to == "United Kingdom Pound"
  ) |> # C3.1
  select(-to) |> 
  inner_join(usdgbp |> 
               select(-c(from, to), 
                      usdgbp = px),
             by = join_by(date, side, mkt),
             relationship = "many-to-one") |>
  mutate( # C3.2
    px = px * usdgbp, # GBPXXX * USDGBP = USDXXX
    to = "United States Dollar"
  ) |> 
  select(-usdgbp)

#   C3.1: everything that is quoted vs GBP (excluding USD) is directly quoted
df |> 
  check31() # Ideal = 0

#   C3.2: everything that is quoted vs GBP (excluding USD) is not spread quoted
df |> 
  check32() # Ideal = 0

#   C4: (sanity check) USDCHF mid spot cross rate should be ~.91 on 31/01/2025 (Yahoo! Finance value)
gbpx |> 
  check4() # Ideal = .91

#   Isolate all indirect quotes vs. USD and convert to direct ones. 
xusd <- df |> 
  filter( # Filter out USDGBP, since we already dealt with that.
    !(
      (from == "United States Dollar" & to == "United Kingdom Pound") |
        (from == "United Kingdom Pound" & to == "United States Dollar")
    )
  ) |> 
  filter( # Filter out everything that is quoted vs. GBP, since we already dealt with that.
    !(from == "United Kingdom Pound" | to == "United Kingdom Pound")
  ) |>
  filter( # Filter everything that is indirectly quoted vs. USD, but not GBP
    from == "United States Dollar" & to != "United Kingdom Pound"
  ) |> 
  mutate(
    px   = 1 / px, # C5
    side = if_else(side == "bid", "ask", "bid"),
    from = to,
    to   = "United States Dollar"
  )

#   C5: check that there are no spreads indirectly quoted vs. USD.
df |> 
  check5()

#   Isolate non-spread direct quotes vs. USD and join them with all the other sets created so far
usdx <- df |> 
  filter( # Filter out USDGBP, since we already dealt with that.
    !(
      (from == "United States Dollar" & to == "United Kingdom Pound") |
        (from == "United Kingdom Pound" & to == "United States Dollar")
    )
  ) |> 
  filter(to == "United States Dollar" & mkt != "spr") |> 
  bind_rows(xusd, gbpx, usdgbp)

rm(xusd, gbpx, usdgbp)

#   Isolate the names of the currencies that have spreads directly quoted vs. USD
usd_spr_crncy <- df |> 
  filter( # Filter out USDGBP, since we already dealt with that.
    !(
      (from == "United States Dollar" & to == "United Kingdom Pound") |
        (from == "United Kingdom Pound" & to == "United States Dollar")
    )
  ) |> 
  filter( # Filter every spread quoted vs. USD
    to == "United States Dollar" & mkt == "spr"
  ) |> 
  pull(from) |> 
  unique()

#   Isolate the spot rates for the currencies which have spread quotes vs. USD
usdx_spr_spots <- usdx |> 
  filter(from %in% usd_spr_crncy) |> 
  select(-c(to, mkt),
         spot = px)

rm(usd_spr_crncy)

#   Isolate the spreads directly quoted vs. USD, join with spot rates and compute outright prices
df |> 
  filter( # Filter out USDGBP, since we already dealt with that.
    !(
      (from == "United States Dollar" & to == "United Kingdom Pound") |
        (from == "United Kingdom Pound" & to == "United States Dollar")
    )
  ) |> 
  filter( # Filter every spread quoted vs. USD
    to == "United States Dollar" & mkt == "spr"
  ) |> 
  inner_join( # Join with respective spot quotes 
    usdx_spr_spots,
    by = join_by(date, side, from)
  ) |> 
  mutate(
    px  = spot + px, # Compute outright forward quotes
    mkt = if_else(mkt == "spr", "fwd", mkt) # Change mkt identifier
  ) |> 
  select(-spot) |> # Remove column
  bind_rows(usdx) |> # Combine with the other subsets
  write_rds("Data/all_outright.rds") # Save as rds

#   Lustig et al. cleaning
read_rds("Data/all_outright.rds") |> 
  lustig_cleaning()
