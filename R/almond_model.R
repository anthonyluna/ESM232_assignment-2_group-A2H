#' 


almond_model <- function(clim_data, k1=-0.015 , k2=-0.0046 , k3=-0.07 , k4= 0.0043, intercept = 0.28 ) {
  ## Error checking
  ## You can use the library checkmate to
  ## make this easier, or just do it with ifelse statements
  
  # check clim_data dimensions
  # check clim_data column names
  # check clim_data column datatypes
  # check coefficients and intercept value
  
  ## End error checking
  
  df_summarized <- clim_data %>% 
    select(year,month,day,tmin_c,precip) %>% 
    group_by(year,month) %>% 
    summarize(avg_tmin_c=mean(tmin_c),tot_precip = sum(precip)) %>% 
    ungroup()
  
  df_temp <- df_summarized %>% 
    filter(month==2) %>% 
    select(-tot_precip,-month)
  
  df_precip <- df_summarized %>% 
    filter(month==1) %>% 
    select(-avg_tmin_c,-month)
  
  df_out <- full_join(df_temp,df_precip) %>% 
    mutate(yeild = k1*avg_tmin_c+k2*(avg_tmin_c^2)+k3*tot_precip+k4*(tot_precip^2)+intercept) %>% 
    select(year,yeild)
  
  return(df_out)
  
  
}