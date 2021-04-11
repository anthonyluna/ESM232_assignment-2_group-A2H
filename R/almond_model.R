#' 


almond_model <- function(clim_data, k1=-0.015 , k2=-0.0046 , k3=-0.07 , k4= 0.0043, intercept = 0.28 ) {
  ## Error checking
  ## You can use the library checkmate to
  ## make this easier, or just do it with ifelse statements
  
  # check clim_data dimensions
  no_col <- max(count.fields(clim_data, sep = " "))
  if (no_col < 10)
  {
    print("The input climate data file should have 10 columns")
  } else {
    # check clim_data column names
    file_header <- read.table(clim_data, header=TRUE, sep = " ", dec = ".")
    file_headers = colnames(file_header)
    
    expected_header = list("row_number", "D", "day", "month", "year", "wy", "tmax_c", "tmin_c", "precip", "wyd")
    
    check_res <- all.equal(file_headers, expected_headers)
    
    if (check_res) {
      # check clim_data column datatypes
      summary <- summary.default(file_header)
      numeric_types = length(summary[which(summary=="numeric")])
      
      if (numeric_types < 10){
        print("There should be at least 10 numeric data type in the input file!")
      }
    }
    else{
      print("The format of the input file header is not as expected!")
    }
  }
  
  
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