# libraries
library(tidyquant)
library(httr)
library(readxl)

# self-made tq functions
tq_diff <- function(df, n = 1) {
  df %>% 
    group_by(symbol) %>% 
    mutate(
      price_lag = lag(price, n),
      price = price - price_lag
    ) %>% 
    ungroup() %>% 
    select(-price_lag)
}

tq_ma <- function(df, n = 3) {
  df %>% 
    group_by(symbol) %>% 
    tidyquant::tq_mutate(
      select = price,
      mutate_fun = SMA,
      n = n
    ) %>% 
    ungroup() %>% 
    rename(
      foo = price,
      price = SMA
    ) %>% 
    select(-foo)
}

tq_gr <- function(df, n = 12, annualize = 1) {
  df %>% 
    group_by(symbol) %>% 
    mutate(
      price_lag = lag(price, n),
      price = (price / price_lag)^annualize * 100 - 100
    ) %>% 
    ungroup() %>% 
    select(-price_lag)
}

price_sa <- function(df, frequency) {
  df %>%   
    mutate(
      price = price %>% 
        ts(start = c(lubridate::year(date[1]), lubridate::month(date[1])),
           frequency = frequency),
      price = price %>% 
        seasonal::seas() %>% 
        seasonal::final() %>% 
        as.numeric()
    )
}

tq_sa <- function(df, frequency = 12) {
  df %>% 
    group_by(symbol) %>% 
    nest() %>% 
    mutate(
      data = map(data, price_sa, frequency = frequency)
    ) %>% 
    unnest(data) %>% 
    ungroup()
}
