library(tidyquant)
library(tidyverse)
library(fpp3)

emp_raw <- tq_get(
  c("PAYNSA"), 
  get = "economic.data", 
  from = "1976-01-01"
)

emp <- emp_raw |>
  mutate(Month = yearmonth(data), employment = price)
|>
  select(month, employment)