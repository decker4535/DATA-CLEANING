library(dplyr)
library(lubridate)
library(stringr)

fema_raw <- read.csv(
  "IndividualAssistanceMultipleLossFloodProperties.csv",
  stringsAsFactors = FALSE
)

fema_harvey <- fema_raw %>%
  filter(disasterNumber == 4332) %>%
  mutate(
    declarationDate = as.Date(declarationDate),
    month_date_yyyymm = year(declarationDate) * 100 + month(declarationDate),
    damagedZipCode = as.character(damagedZipCode)
  ) %>%
  filter(!is.na(damagedZipCode))
fema_zip_month <- fema_harvey %>%
  group_by(damagedZipCode, month_date_yyyymm) %>%
  summarise(
    fema_claim_count = n(),
    .groups = "drop"
  ) %>%
  rename(postal_code = damagedZipCode)

df <- read.csv("Metro_mean_sale_price_uc_sfr_month.csv", stringsAsFactors = FALSE)

housing <- df %>%
  filter(str_detect(zip_name, regex("Houston", ignore_case = TRUE))) %>%
  mutate(
    month_date_yyyymm = as.integer(month_date_yyyymm),
    postal_code = as.character(postal_code),
    median_listing_price = as.numeric(median_listing_price)
  ) %>%
  filter(!is.na(median_listing_price))

analysis_df <- housing %>%
  left_join(
    fema_zip_month,
    by = c("postal_code", "month_date_yyyymm")
  ) %>%
  mutate(
    fema_claim_count = ifelse(is.na(fema_claim_count), 0, fema_claim_count)
  )

model_ols <- lm(
  median_listing_price ~ fema_claim_count,
  data = analysis_df
)

summary(model_ols)
model_zip_fe <- lm(
  median_listing_price ~ fema_claim_count + factor(postal_code),
  data = analysis_df
)

summary(model_zip_fe)
