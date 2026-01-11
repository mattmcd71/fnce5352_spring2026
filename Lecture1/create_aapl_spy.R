library(tidyquant)
library(tidyverse)
library(here)

aapl_spy <- tq_get(
  c("AAPL", "SPY"),
  from = "2023-01-01",
  to   = Sys.Date()
) %>%
  select(date, symbol, adjusted) %>%
  rename(
    ticker = symbol,
    adj_close = adjusted
  )

write_csv(aapl_spy, here::here('Lecture1', 'aapl_spy.csv'))
