if (!require("tidyverse")) install.packages("tidyverse"); library("tidyverse")
if (!require("frenchdata")) install.packages("frenchdata"); library("frenchdata")
if (!require("readxl")) install.packages("readxl"); library("readxl")

source("R/cleaners.R")
source("R/checkers.R")
source("R/helpers.R")

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

#   Isolate USDGBP spot and forward quotes, compute outright forward quotes
usdgbp <- df |> 
  spr_to_outright(.froms = "United Kingdom Pound")

rm(gbp_spr)

#   C2: All bids < asks
usdgbp |> 
  check2() # Ideal = 0

#   Isolate all the quotes vs. GBP and compute the USDXXX cross rate
gbpx <- df |> 
  keep_usdgbp() |>  # Filter out USDGBP, since we already dealt with that.
  cross_to_usd()

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
  keep_usdgbp() |>  # Filter out USDGBP, since we already dealt with that.
  filter( # Filter everything that is indirectly quoted vs. USD
    from == "United States Dollar"
  ) |> 
  direct_quote(.spread = FALSE)

#   C5: check that there are no spreads indirectly quoted vs. USD.
df |> 
  check5()

#   Isolate non-spread direct quotes vs. USD and join them with all the other sets created so far
usdx <- df |> 
  keep_usdgbp() |>  # Filter out USDGBP, since we already dealt with that.
  filter(to == "United States Dollar" & mkt != "spr") |> 
  bind_rows(xusd, gbpx, usdgbp)

rm(xusd, gbpx, usdgbp)

#   Isolate the names of the currencies that have spreads directly quoted vs. USD
usd_spr_crncy <- df |> 
  keep_usdgbp() |>  # Filter out USDGBP, since we already dealt with that.
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
  keep_usdgbp() |>  # Filter out USDGBP, since we already dealt with that.
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
  bind_rows(usdx) |> # Combine with the other subset
  select(-to) |> # Remove to column, as everything is to USD now
  write_rds("Data/all_outright.rds") # Save as .rds

#   Lustig et al. cleaning and returns computation
read_rds("Data/all_outright.rds") |>  
  lustig_cleaning() |> 
  lustig_returns() |> 
  write_rds("Data/returns.rds")

rm(list = ls())

# 
df <- read_rds("Data/returns.rds") |>
  # rl_t = the return I get in month t+1 for going long in month t
  # fwd_disc_t = the carry I observe in month t and expect to get in month t+1
  # => fwd_disc is the expectation, rl is what realized
  mutate(
    fwd_disc = fwd.bid - spot.ask, # Carry
  ) |> 
  group_by(from) |> 
  mutate(
    # Momentum
    mom1     = lag(rl),
    mom3     = slider::slide_dbl(.x = rl, .f = sum, .before = 2, .complete = TRUE) |> lag(),
    mom6     = slider::slide_dbl(.x = rl, .f = sum, .before = 5, .complete = TRUE) |> lag(),
    mom12    = slider::slide_dbl(.x = rl, .f = sum, .before = 10, .complete = TRUE) |> lag()
  ) |> 
  group_by(date) |> 
  mutate(
    avg_fd = mean(fwd_disc), # Dollar Carry
    N      = n()
  ) 

df

p <- df |>
  ggplot(aes(x = date, y = N)) + 
  geom_line()

plotly::ggplotly(p)
