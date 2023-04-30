library(here)
library(dplyr)

builds <- read.csv(here::here("data", "crypto_mining", "builds.csv"))
purchases <- read.csv(here::here("data", "crypto_mining", "purchases.csv"))
internet_expenses <- read.csv(here::here("data", "crypto_mining", "internet_expenses.csv"))
electricity_expenses <- read.csv(here::here("data", "crypto_mining", "electricity_expenses.csv"))
amazon_transactions <- read.csv(here::here("data", "macu", "amazon_transactions.csv"))
newegg_transactions <- read.csv(here::here("data", "macu", "newegg_transactions.csv"))
homedepot_transactions <- read.csv(here::here("data", "macu", "homedepot_transactions.csv"))

koinly_2021_income_report <- read.csv(here::here("data", "koinly", "koinly_2021_income_report.csv"))
koinly_2021_expense_report <- read.csv(here::here("data", "koinly", "koinly_2021_expense_report.csv"))

koinly_2021_income_report_summarized <- koinly_2021_income_report %>%
  dplyr::group_by(type) %>%
  dplyr::summarise( amount_usd=sum(value_usd) )

koinly_2021_expense_report_summarized <- koinly_2021_expense_report %>%
  dplyr::group_by(description) %>%
  dplyr::summarise( amount_usd=sum(value_usd) )


koinly_2022_expense_report <- read.csv(here::here("data", "koinly", "koinly_2022_expense_report.csv"))

koinly_2022_expense_report_summarized <- koinly_2022_expense_report %>%
  dplyr::group_by(description) %>%
  dplyr::summarise( amount_usd=sum(value_usd) )


wattage_by_tower <- builds %>%
  dplyr::group_by(tower) %>%
  dplyr::summarise(total_wattage=sum(watts))

# totals and differences of yearly billed amounts
yearly_electricity_expenses <- electricity_expenses %>%
                                dplyr::group_by( year ) %>%
                                dplyr::summarise( yearly_cost = sum(amount) ) %>%
                                dplyr::arrange( year ) %>%
                                dplyr::ungroup( ) %>%
                                dplyr::mutate( change = yearly_cost - dplyr::lag(yearly_cost) )

number_of_months_collected_for_2020 <- 8
avg_2020_monthly_electricity_expense <- sum( dplyr::filter(electricity_expenses, year == 2020)$amount ) / number_of_months_collected_for_2020
estimated_2020_expected_electricity_expense <- avg_2020_monthly_electricity_expense*12
estimated_2022_electricity_expense <- sum( dplyr::filter(electricity_expenses, year == 2022)$amount ) - estimated_2020_expected_electricity_expense


internet_2021_expenses <- sum( dplyr::filter(internet_expenses, year == 2021)$amount )

internet_2022_expenses <- sum( dplyr::filter(internet_expenses, year == 2022)$amount )

total_amazon_transactions <- sum(amazon_transactions["Amount"])
total_newegg_transactions <- sum(newegg_transactions["Amount"])
total_homedepot_transactions <- sum(homedepot_transactions["Amount"])

total_mining_cost <- sum(purchases["amount_usd"])
resale_value <- sum(purchases["amount_usd"])/2

# TODO: add the following calculation
# days_until_roi <- (total_mining_cost-resale_value-profit)/daily_profit

