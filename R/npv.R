# Querstions & Notes
# ----------------------------------------------------------------------------
# is this the correct discount rate?
# ----------------------------------------------------------------------------
npv <- function(yield_outcomes,r = 0.12 , given_yield = 1  , production_cost = 3800  , almond_profit = 2.50  , ton_to_pound = 2000 , start_year = 1989) {
  # mutate to add column for yield
  # ------------------------------
  avg_yield <- yeild_outcomes %>%
    mutate(est_yield = yeild+given_yield) %>% # anomally -> yield
    mutate(profit = (est_yield*ton_to_pound*almond_profit)-(production_cost)) %>% # annual profit
    mutate(time = year-start_year) %>% # add time column
    mutate(npv = (profit)/(1+r)**time) # calculate npv by year
  # sum annual npv to get total npv
  # -------------------------------
  npv <- dollar(sum(avg_yield$npv))
  # return desired output
  # ---------------------
  return(npv)
}