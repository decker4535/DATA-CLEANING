
library(dplyr)
library(stringr)

houston_zip_count <- df %>%
  filter(str_detect(zip_name, regex("Houston", ignore_case = TRUE))) %>%
  summarise(
    n_houston_zip_codes = n_distinct(postal_code)
  )

houston_zip_count

library(dplyr)
library(stringr)
library(tidyr)

houston <- df %>%
  filter(str_detect(zip_name, regex("Houston", ignore_case = TRUE))) %>%
  mutate(
    month_date_yyyymm = as.integer(month_date_yyyymm),
    year = month_date_yyyymm %/% 100,
    median_listing_price = as.numeric(median_listing_price)
  ) %>%
  filter(!is.na(median_listing_price))

zip_changes <- houston %>%
  mutate(
    period = case_when(
      year <= 2016 ~ "pre",
      year >= 2019 ~ "post",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(period)) %>%
  group_by(postal_code, zip_name, period) %>%
  summarise(
    avg_price = mean(median_listing_price, na.rm = TRUE),
    n_months = n(),
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from = period,
    values_from = avg_price
  ) %>%
  mutate(
    price_change = post - pre,
    pct_change = (post - pre) / pre
  ) %>%
  filter(!is.na(price_change))

top_gainers <- zip_changes %>%
  arrange(desc(price_change)) %>%
  slice_head(n = 10)

top_gainers

top_losers <- zip_changes %>%
  arrange(price_change) %>%
  slice_head(n = 10)

top_losers

library(dplyr)
library(stringr)
library(ggplot2)

houston_2017 <- df %>%
  filter(str_detect(zip_name, regex("Houston", ignore_case = TRUE))) %>%
  mutate(
    month_date_yyyymm = as.integer(month_date_yyyymm),
    year = month_date_yyyymm %/% 100,
    month = month_date_yyyymm %% 100,
    median_listing_price = as.numeric(median_listing_price)
  ) %>%
  filter(
    year == 2017,
    !is.na(median_listing_price)
  )

zip_2017_change <- houston_2017 %>%
  group_by(postal_code) %>%
  summarise(
    jan_price = median_listing_price[month == 1][1],
    dec_price = median_listing_price[month == 12][1],
    price_change_2017 = dec_price - jan_price,
    .groups = "drop"
  ) %>%
  filter(!is.na(price_change_2017))


ggplot(zip_2017_change,
       aes(x = reorder(postal_code, price_change_2017),
           y = price_change_2017)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Change in Median Listing Price by ZIP Code (2017)",
    x = "ZIP Code",
    y = "Price Change ($)"
  )


#Over time prices 2017
library(dplyr)
library(stringr)
library(ggplot2)

houston_2017 <- df %>%
  filter(str_detect(zip_name, regex("Houston", ignore_case = TRUE))) %>%
  mutate(
    month_date_yyyymm = as.integer(month_date_yyyymm),
    year  = month_date_yyyymm %/% 100,
    month = month_date_yyyymm %% 100,
    median_listing_price = as.numeric(median_listing_price)
  ) %>%
  filter(
    year == 2017,
    !is.na(median_listing_price)
  )

ggplot(houston_2017,
       aes(x = month, y = median_listing_price)) +
  geom_boxplot(group = month) +
  scale_x_continuous(breaks = 1:12) +
  labs(
    title = "Distribution of Median Listing Prices Across Houston ZIP Codes (2017)",
    x = "Month",
    y = "Median Listing Price ($)"
  )

ggplot(houston_2017,
       aes(x = month,
           y = median_listing_price,
           group = postal_code)) +
  geom_line(alpha = 0.4) +
  scale_x_continuous(breaks = 1:12) +
  labs(
    title = "Monthly Median Listing Prices by ZIP Code (Houston, 2017)",
    x = "Month",
    y = "Median Listing Price ($)"
  )

ggplot(houston_2017,
       aes(x = month, y = median_listing_price)) +
  geom_smooth(stat = "summary", fun = median, se = FALSE) +
  scale_x_continuous(breaks = 1:12) +
  labs(
    title = "Median Trend Across Houston ZIP Codes (2017)",
    x = "Month",
    y = "Median Listing Price ($)"
  )
