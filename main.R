source("R/startup.R")

source("R/cleaners.R")
source("R/checkers.R")

##### Data Processing & Cleaning #####

#   Import table with base/price currencies, spread scales, tickers and countries
gen_inf <- readxl::read_xlsx("Data/ds_data.xlsx", sheet = "fromto")

df <- readxl::read_xlsx("Data/ds_data.xlsx", sheet = "quotes", skip = 1) |> 
  rename(date = `Code`) |> 
  raw_to_bidask() |> # Function that transforms raw data to a long tibble with identifiers for bid/ask.
  fromto_scale(gen_inf, code, scale) |> # Function to join with general info tibble and scale all spreads.
  select(-c(ticker, country, eudate)) |> # These columns will only be used later, after all currencies are directly quoted.
  drop_na() # Drop missing prices observations

# C1: After these computations, but before dropping NAs, we should have 50% of observations being bids/asks/spots/forwards.
readxl::read_xlsx("Data/ds_data.xlsx", sheet = "quotes", skip = 1) |> 
  check1() # Ideal: 0

#   Isolate USDGBP spot and forward quotes, compute outright forward quotes
usdgbp <- df |> 
  spr_to_outright(.froms = "United Kingdom Pound")

rm(gbp_spr)

#   Isolate all the quotes vs. GBP and compute the USDXXX cross rate
gbpx <- df |> 
  drop_usdgbp() |>  # Filter out USDGBP, since we already dealt with that.
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
  drop_usdgbp() |>  # Filter out USDGBP, since we already dealt with that.
  filter( # Filter everything that is indirectly quoted vs. USD
    from == "United States Dollar"
  ) |> 
  direct_quote(.spread = FALSE)

#   C5: check that there are no spreads indirectly quoted vs. USD.
df |> 
  check5()

#   Isolate the names of the currencies that have spreads directly quoted vs. USD
usd_spr_crncy <- df |> 
  drop_usdgbp() |>  # Filter out USDGBP, since we already dealt with that.
  filter( # Filter every spread quoted vs. USD
    to == "United States Dollar" & mkt == "spr"
  ) |> 
  pull(from) |> 
  unique()

#   Convert all spreads quoted vs. USD to outright quotes.
usdx_spr <- df |> 
  drop_usdgbp() |>  # Filter out USDGBP, since we already dealt with that.
  spr_to_outright(.froms = usd_spr_crncy, 
                  .new_spots = gbpx) # New spots argument has the spot cross rates for the currencies which were 
# originally quoted vs. GBP.

#   Some currencies had spots quoted vs. GBP. To avoid double entries, we filter those out, as they were included in
# the usdx_spr tibble through the .new_spots argument.
gbpx <- gbpx |> 
  filter(!(from %in% usd_spr_crncy))

#   Get tibble with the dates in which Eurozone countries joined the monetary union.
eudates <- gen_inf |> 
  get_eu_dates()

#   Join all tibbles, keep "good" bid/ask quotes, clean as in Lustig et al. and save RDS.
df |> 
  drop_usdgbp() |> # Filter out USDGBP, since we already dealt with that.
  filter(to == "United States Dollar" & mkt != "spr" & !(from %in% usd_spr_crncy)) |> # Filter all direct quotes vs.
  # USD which are not spreads and are not in the spread-quoted currencies list (i.e, they are quoted directly and as
  # outright).
  bind_rows(xusd, usdgbp, usdx_spr, gbpx) |> 
  select(-to) |> # Remove to column, as everything is to USD now
  arrange(date, from) |> 
  drop_bad_bidask() |> # Drop all observations where bid > ask
  lustig_cleaning(eudates) |> # Implement Lustig et al. cleaning procedures 
  write_rds("Data/all_outright.rds") # Save as .rds

#   C2: All bids < asks
read_rds("Data/all_outright.rds") |>  
  check2() # Ideal = 0

rm(list = ls())

##### Returns and Signals #####
source("R/startup.R")
source("R/helpers.R")
source("R/plotters.R")

#   This chunk will plot the time series for each currency (group). This is to check if any of the time series behaves
# strangely, and if so, it's easy to see which market/side does so.
# read_rds("Data/all_outright.rds") |>  
#   mutate(label = paste(mkt, side)) |> 
#   group_split(from) |> 
#   walk(group_line_plot)

#   Compute returns and signals
df <- read_rds("Data/all_outright.rds") |>  
  lustig_returns() |> 
  # rl_t (rs_t) = the return I get in month t+1 for going long (short) in month t
  # fwd_disc_t = the carry I observe in month t and expect to get in month t+1
  # => fwd_disc is the expectation, rl is what realized
  compute_signals()# |> 
# select(-starts_with(c("spot.", "fwd.")))

#   Simple plot for amount of currencies over time
df |> 
  group_by(date) |> 
  mutate(N = n()) |> 
  simple_line(.x = date,
              .y = N)
#   From the plot, we can see N<=3 (i.e. insufficient) until 1990-04-30. Thus, we will start from 1990-05-31.
df <- df |> 
  filter(date >= "1990-05-31")

#   Compute Dollar Carry strategy
factors <- df |> 
  drop_na(fwd_disc) |>  # There are 5 currencies which have forward quotes starting before spot quotes, which makes it
  # so that there's no forward discount in the first observation. Here we are dropping these observations because since
  # these NA's didn't enter the fwd_disc (and consequently avg_fd) calculation, their returns should not be considered 
  # in the Dollar Carry strategy.
  compute_dol_carry()

#   Add Dollar strategy
factors <- df |> 
  compute_dol() |> 
  bind_rows(factors) |> 
  arrange(date, strategy)

#   Drop unecessary columns, drop NA's, pivot longer and keep only dates where N>=20.
df <- df |> 
  select(-starts_with(c("spot.", "fwd.")), -avg_fd) |> 
  pivot_longer(
    -c(date, from, rl, rs),
    names_to  = "signal",
    values_to = "var"
  ) |> 
  drop_na(var) |> 
  group_by(date, signal) |> 
  filter(n() >= 20) |> 
  ungroup() |> 
  arrange(date, signal) 

#   Compute portfolio sorts
df |> 
  multiple_portfolio_sorts(.variable = var,
                           .n_portfolios = 5) |>
  multiple_long_short()
  group_by(signal, portfolio) |> 
  mutate(
    cum_long  = exp(cumsum(ret_l)) - 1,
    cum_short = exp(cumsum(ret_s)) - 1
  ) |> 
  ungroup() |> 
  group_split(signal) |> 
  walk(.f = ~group_line_plot(.data = .x,
                             .x = date,
                             .y = cum_short,
                             .color = portfolio,
                             .title = signal))

