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
tickers <- readxl::read_xlsx("Data/ds_data.xlsx", sheet = "nametickers")

df |> 
  left_join(
    y            = fromto,
    by           = join_by(code),
    relationship = "many-to-one" # Each row in df has to match at most 1 row in fromto
  ) |> 
  mutate(px = ifelse(!is.na(scale), px / scale, px)) |> 
  select(-c(code, scale)) # Drop name and scale column
