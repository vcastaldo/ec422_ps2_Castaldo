library(tidyquant)
library(tidyverse)

# unemp_raw <- tq_get(
#   c("UNRATE", "ORUR", "CAUR", "TXUR", "NYUR"),
#   get  = "economic.data",
#   from = "1976-01-01"
# )
# 
# head(unemp_raw)
# 
# dir.create("data", showWarnings = FALSE)
# 
# write_rds(unemp_raw, "data/unemp_raw.rds")

unemp_raw = read_rds("data/unemp_raw.rds")
tail(unemp_raw)



unemp <- unemp_raw |>
  rename(unemployment_rate = price) |>
  mutate(
    region = case_when(
      symbol == "UNRATE" ~ "United States",
      symbol == "ORUR"   ~ "Oregon",
      symbol == "CAUR"   ~ "California",
      symbol == "TXUR"   ~ "Texas",
      symbol == "NYUR"   ~ "New York"
    )
  )

unemp <- unemp |>
  arrange(region, date) |>
  group_by(region) |>
  mutate(
    unemployment_rate = replace(
      unemployment_rate,
      date == as.Date("2025-10-01"),
      unemployment_rate[date == as.Date("2025-09-01")]
    )
  ) |>
  ungroup()

unemp <- unemp |>
  arrange(region, date) |>
  group_by(region) |>
  mutate(unemployment_rate_change = unemployment_rate - lag(unemployment_rate)) |>
  ungroup()
head(unemp)

unemp_summary <- unemp |>
  group_by(region) |>
  summarize(
    mean_rate = mean(unemployment_rate, na.rm = TRUE),
    min_rate  = min(unemployment_rate,  na.rm = TRUE),
    max_rate  = max(unemployment_rate,  na.rm = TRUE)
  )

unemp_recent <- unemp |>
  filter(date >= as.Date("2015-01-01"))

unemp_recent_summary <- unemp_recent |>
  group_by(region) |>
  summarize(
    mean_rate = mean(unemployment_rate, na.rm = TRUE),
    min_rate  = min(unemployment_rate,  na.rm = TRUE),
    max_rate  = max(unemployment_rate,  na.rm = TRUE)
  )

unemp_summary
unemp_recent_summary

# 4.3 Find the Extremes
unemp_top_increases <- unemp |>
  slice_max(unemployment_rate_change, n = 5)

unemp_top_decreases <- unemp |>
  slice_min(unemployment_rate_change, n = 5)

unemp_top_increases
unemp_top_decreases

# 4.4 Find the Extremes Excluding 2020
unemp_no_covid <- unemp |>
  filter(year(date) != 2020)

unemp_top_increases_no_covid <- unemp_no_covid |>
  slice_max(unemployment_rate_change, n = 5)

unemp_top_decreases_no_covid <- unemp_no_covid |>
  slice_min(unemployment_rate_change, n = 5)

unemp_top_increases_no_covid
unemp_top_decreases_no_covid


unemp_no_covid <- unemp |>
  filter(year(date) != 2020)

unemp_top_increases_no_covid <- unemp_no_covid |>
  slice_max(unemployment_rate_change, n = 5)

unemp_top_decreases_no_covid <- unemp_no_covid |>
  slice_min(unemployment_rate_change, n = 5)

unemp_top_increases_no_covid
unemp_top_decreases_no_covid

print(unemp_top_decreases_no_covid, width = Inf)


# 5.1 Line plot
ggplot(unemp, aes(x = date, y = unemployment_rate, color = region)) +
  geom_line() +
  labs(
    title = "Monthly Unemployment Rate by Region, 1976–2026",
    x = "Date",
    y = "Unemployment Rate (%)",
    color = "Region"
  ) +
  theme_minimal()

# 5.2 Faceted line plots
ggplot(unemp, aes(x = date, y = unemployment_rate, color = region)) +
  geom_line() +
  facet_wrap(~ region) +
  labs(
    title = "Monthly Unemployment Rate by Region, 1976–2026",
    x = "Date",
    y = "Unemployment Rate (%)"
  ) +
  theme_minimal() +
  theme(legend.position = "none")


# 5.3 Bar chart of mean unemployment by region
ggplot(unemp_summary,
       aes(x = fct_reorder(region, mean_rate), y = mean_rate, fill = region)) +
  geom_col() +
  labs(
    title = "Average Unemployment Rate by Region, 1976–2026",
    x = "Region",
    y = "Mean Unemployment Rate (%)"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

# 5.4 Oregon vs. U.S. unemployment gap
or_us_gap <- unemp |>
  filter(region %in% c("Oregon", "United States")) |>
  select(date, region, unemployment_rate) |>
  pivot_wider(names_from = region, values_from = unemployment_rate) |>
  mutate(gap = Oregon - `United States`)

ggplot(or_us_gap, aes(x = date, y = gap)) +
  geom_line(color = "darkgreen") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    title = "Oregon Unemployment Rate Minus U.S. Rate, 1976–2026",
    x = "Date",
    y = "Oregon − U.S. (percentage points)"
  ) +
  theme_minimal()

I plotted the gap between Oregons and the US unemployment rates to see whether 
the Oregon Labor market moves with the nation. Oregons unemployment tends to 
rise above the national rate during recessions.
