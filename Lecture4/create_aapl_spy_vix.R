library(tidyquant)
library(tidyverse)
library(here)

aapl_spy_vix <- tq_get(
  c("AAPL", "SPY", "^VIX"),
  from = "2023-01-01",
  to   = Sys.Date()
) %>%
  select(date, symbol, adjusted) %>%
  rename(
    ticker = symbol,
    adj_close = adjusted
  )

write_csv(aapl_spy_vix, here::here('Lecture4', 'data', 'aapl_spy_vix.csv'))
