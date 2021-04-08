#' Almond Model
#'
#' This function computes the yield of almonds
#’ based on monthly minimum temperature in February and 
#' accumulated precipitation in January (Lobell 2006)
#' @param k1 coefficient for Febuary Tmin changes 
#' @param k2 quadratic term coefficient for Febuary Tmin changes
#' @param k3 coefficient for January Precipitation changes
#' @param k4 quadratic term coefficient for January Precipitation changes
#' @author Anthony Luna, Chen Xing, Atefeh Mohseni
#' @examples almond_model()
#' @return Almonds yield anomaly (ton acre-1)

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
  
  # df_out <- full_join(df_temp,df_precip) %>% 
  #   mutate(yeild = k1*avg_tmin_c+k2*(avg_tmin_c^2)+k3*tot_precip+k4*(tot_precip^2)+intercept) %>% 
  #   select(year,yeild)
  df_out = k1*df_temp$avg_tmin_c+k2*(df_temp$avg_tmin_c^2)+k3*df_precip$tot_precip+k4*(df_precip$tot_precip^2)+intercept
  
  return(df_out)
  
  
}