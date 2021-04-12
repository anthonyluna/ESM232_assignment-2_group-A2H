#' Almond Model
#'
#' This function computes the annual yield of almonds in California
#’ based on monthly minimum temperature in February and 
#' accumulated precipitation in January (Lobell et al. 2006)
#' @param clim_data file path to climate data that includes fields for row_number, D, day, month, year, wy, tmax_C, tmin_c, precip, wyd
#' @param k1 coefficient for February minimum temperature changes 
#' @param k2 quadratic term coefficient for February minimum temperature changes
#' @param k3 coefficient for January Precipitation changes
#' @param k4 quadratic term coefficient for January Precipitation changes
#' @param intercept the intercept of the fitted line
#' @author Anthony Luna, Chen Xing, Atefeh Mohseni
#' @examples almond_model(["1" 1991-06-01 1 6 1991 1992 21.232 14.234 1.56 1])
#' @return Almonds yield anomaly (ton acre-1)
#' @references
#' Lobell D B, Field C B, Cahill K N, et al. Impacts of future climate change on 
#' California perennial crop yields: Model projections with climate and crop 
#' uncertainties[J]. Agricultural and Forest Meteorology, 2006, 141(2-4): 208-218.

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
    file_header <- read.delim(clim_data, header=TRUE, sep = " ", dec = ".")
    file_headers = colnames(file_header)
    
    expected_header =c("row_number", "D", "day", "month", "year", "wy", "tmax_c", "tmin_c", "precip", "wyd")
    
    check_res <- isTRUE(all.equal(file_headers, expected_header))
    
    if (check_res) {
      # check clim_data column datatypes
      summary <- summary.default(file_header)
      numeric_types = length(summary[which(summary=="numeric")])
      date_types = length(summary[which(summary=="character")])
      
      if (numeric_types != 9 | date_types!= 1){
        print("There should have 9 numeric data type and 1 date type in the input file!")
      }
    }
    else{
      print("The format of the input file header is not as expected!")
    }
  }
  
  clim_data <- read_delim(file = clim_data, delim=" ")
  
  # check coefficients and intercept value
  
  ## End error checking
  
  # Turn the daily minimum temperature into monthly data through monthly average
  # and calculate monthly accumulated precipitation from daily station
  # precipitation data
  
  df_summarized <- clim_data %>% 
    select(year,month,day,tmin_c,precip) %>% 
    group_by(year,month) %>% 
    summarize(avg_tmin_c=mean(tmin_c),tot_precip = sum(precip)) %>% 
    ungroup()
  
  # Extract the February minimum temperature from the data summary
  
  df_temp <- df_summarized %>% 
    filter(month==2) %>% 
    select(-tot_precip,-month)
  
  # Extract the January precipitation from the data summary
  
  df_precip <- df_summarized %>% 
    filter(month==1) %>% 
    select(-avg_tmin_c,-month)
  
  # Calculate the almond annual yield from the statistical relationship:
  # Y = k1*(Tn,2) + k2*(Tn,2)^2 + k3*(P1) + k4*(P1)^2 + intercept
  # Y: almond yield anomaly (ton/acre); Tn,2: February minimum temperature (C); 
  # P1: January precipitation (mm)
  
  df_out <- full_join(df_temp,df_precip) %>% 
    mutate(yeild = k1*avg_tmin_c+k2*(avg_tmin_c^2)+k3*tot_precip+k4*(tot_precip^2)+intercept) %>% 
    mutate(yeild_temp = k1*avg_tmin_c+k2*(avg_tmin_c^2)) %>% 
    mutate(yeild_precip = k3*tot_precip+k4*(tot_precip^2)) %>% 
    mutate(intercept = intercept)
  
  # return the yield to the main function
  
  return(df_out)
  
  
}