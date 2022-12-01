library(here)
library(dplyr)

income <- read.csv(here::here("data", "income.csv"))
expenses <- read.csv(here::here("data", "expenses.csv"))
assets <- read.csv(here::here("data", "assets.csv"))
debts <- read.csv(here::here("data", "debts.csv"))

total_assets <- sum(assets["usd_equivalent"])
total_debts <- sum(debts["usd_equivalent"])
net_worth <- total_assets - total_debts

monthly_assets_compound_interest <- sum( assets["usd_equivalent"] * assets["apy"]/(12*100) )
monthly_debts_compound_interest <- sum( debts["usd_equivalent"] * debts["apy"]/(12*100) )
net_monthly_compound_interest <- monthly_assets_compound_interest - monthly_debts_compound_interest

monthly_cash_back <- sum(expenses["usd_equivalent"]*expenses["cash_back_percentage"]/100)
total_monthly_income <- sum(income["usd_equivalent"]) + monthly_cash_back 
total_monthly_expenses <- sum(expenses["usd_equivalent"]) + sum(debts["payment"])
total_monthly_ach_required_expenses <- sum(dplyr::filter(expenses, expenses["ach_required"] == 1)["usd_equivalent"]) + sum(debts["payment"])
net_monthly_income <- total_monthly_income - total_monthly_expenses

monthly_speculative_return <- sum( assets["usd_equivalent"] * assets["speculative_return"]/(12*100) )
monthly_passive_income <- sum(dplyr::filter(income, income["passive_ind"] == 1)["usd_equivalent"]) + monthly_assets_compound_interest + monthly_cash_back
monthly_speculative_passive_income <- monthly_passive_income + monthly_speculative_return
speculative_passive_monthly_change <- monthly_speculative_passive_income - total_monthly_expenses 
passive_monthly_change <- monthly_passive_income - total_monthly_expenses 
 

get_assets_by_type <- function(assets) {
  assets %>%
    dplyr::group_by(type) %>%
    dplyr::summarize( total_asset=sum(usd_equivalent) )
}

get_debts_by_type <- function(debts) {
  debts %>%
    dplyr::group_by(type) %>%
    dplyr::summarize( total_debt=sum(usd_equivalent) )
}

get_totals_by_type <- function(assets, debts) {
  totals_by_type <- merge(get_assets_by_type(assets), get_debts_by_type(debts), by="type", all=TRUE) %>%
      replace(is.na(.), 0) %>%
      dplyr::mutate( total_overall=total_asset-total_debt )
}

calculate_contributions <- function(assests_contribution_percentage, crypto_contribution_percentage, amount) {
    # either contribute to assests or just save
    assets_contribution <- assests_contribution_percentage/100 * amount
    # assets contribution split
    crypto_contribution <- crypto_contribution_percentage/100 * assets_contribution
    stocks_contribution <- ( (100-crypto_contribution_percentage)/100 ) * assets_contribution
    monthly_save <- ( (100-assests_contribution_percentage)/100 ) * amount
    return(list("crypto"=crypto_contribution, "stock"=stocks_contribution, "usd"=monthly_save))
}

calculate_airdrop_assets_contributions <- function(airdrop_amount, crypto_contribution_percentage, assets) {
    contributions <- calculate_contributions(100, crypto_contribution_percentage, airdrop_amount)
    
    crypto_contribution_amount <<- contributions$crypto / nrow( dplyr::filter(assets, assets["contribute_ind"] & assets["crypto_ind"] ) )
    crypto_contribution <- assets["crypto_ind"] * assets["contribute_ind"] * crypto_contribution_amount 
    
    stock_contribution_amount <<- contributions$stock / nrow( dplyr::filter(assets, assets["contribute_ind"] & assets["stock_ind"]) )
    stock_contribution <- assets["stock_ind"] * assets["contribute_ind"] * stock_contribution_amount 
    
    df <- assets %>%
      dplyr::select(id) %>%
      dplyr::mutate(contribution=crypto_contribution+stock_contribution)
    return(df)
}
 
calculate_wealth_projection <- function(assets, debts, years, 
                                        assests_contribution_percentage, crypto_contribution_percentage, 
                                        net_monthly_income, airdrop_amount, net_worth, label) {
    compounding_assets <- assets["usd_equivalent"]
    compounding_debts <- debts["usd_equivalent"]
    assets_apy <- assets["apy"]
    debts_apy <- debts["apy"]
    assets_speculative_return <- assets["speculative_return"]
    monthly_auto_contribution <- assets["monthly_auto_contribution"]
    
    airdrop_contributions <- calculate_airdrop_assets_contributions(airdrop_amount, crypto_contribution_percentage, assets)
    compounding_assets <- compounding_assets+airdrop_contributions["contribution"] 
    
    contributions <- calculate_contributions(assests_contribution_percentage, crypto_contribution_percentage, net_monthly_income)
    
    crypto_monthly_contribution_amount <<- contributions$crypto / nrow( dplyr::filter(assets, assets["contribute_ind"] & assets["crypto_ind"] ) )
    crypto_monthly_contribution <- assets["crypto_ind"] * assets["contribute_ind"] *crypto_monthly_contribution_amount 
    
    stocks_monthly_contribution_amount <<- contributions$stock / nrow( dplyr::filter(assets, assets["contribute_ind"] & assets["stock_ind"]) )
    stocks_monthly_contribution <- assets["stock_ind"] * assets["contribute_ind"] * stocks_monthly_contribution_amount 
    
    savings_monthly_contribution_amount <- contributions$usd / nrow( dplyr::filter(assets, assets["savings_ind"] == 1 ) )
    savings_monthly_contribution <- assets["savings_ind"] * savings_monthly_contribution_amount    
    
    debts_monthly_deduction <- debts["payment"] 
    
    # start with a base of net worth
    net_worth <- c(net_worth)
    for ( i in 1:(12*years) ) {
      # compounding assets calculation
      prev_compounding_assets <- compounding_assets
      compounding_assets <- ( compounding_assets 
                              + compounding_assets*assets_apy/(12*100) 
                              + compounding_assets*assets_speculative_return/(12*100) 
                              + crypto_monthly_contribution 
                              + stocks_monthly_contribution 
                              + savings_monthly_contribution
                              + monthly_auto_contribution )
      
      # compounding debts calculation
      prev_compounding_debts <- compounding_debts
      compounding_debts <- ( compounding_debts 
                             + compounding_debts*debts_apy/(12*100) 
                             - debts_monthly_deduction )
      
      compounding_debts_greater_than_zero <- compounding_debts > 0
      compounding_debts <- compounding_debts*compounding_debts_greater_than_zero
      debt_is_zero <- compounding_debts == 0
      left_over_debt_deductions <- sum( debt_is_zero*debts_monthly_deduction )
      
      net_worth_change <- ( sum(compounding_assets-prev_compounding_assets) 
                            - sum(compounding_debts-prev_compounding_debts) 
                            + left_over_debt_deductions )
      
      net_worth <- c(net_worth, net_worth[[i]]+net_worth_change) 
    }
    month <- 1:(12*years+1)
    
    wealth_projection <- data.frame(net_worth, month)
    assets["usd_equivalent"] <- compounding_assets
    debts["usd_equivalent"] <- compounding_debts
    
    wealth_projection["label"] <- label
    assets["label"] <- label
    debts["label"] <- label
    
    return( list("wealth_projection"=wealth_projection, "assets"=assets, "debts"=debts) )
}
