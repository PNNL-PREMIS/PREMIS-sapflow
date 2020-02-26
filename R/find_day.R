## Function to match days with similar weather conditions
## Created 1/20/2020 for Rs-Js analysis | Stephanie Pennington

similar_days <- function(doy, climate_data, constraints) {
  # Find all days in a window (of size `window`) around `doy` for
  # which the air temperature is within `tair_delta` of the tair on `doy`
  
  # doy: Day of year, an integer
  # climate_data: A data frame with at least `doy` and `tair` (degC) columns
  # constraints: A vector with at least one climate variable giving maximum difference
  
  # Returns a vector of doys, integers matching these conditions
  
  # Note this function will need to handle edge cases, when doy is close
  # to the beginning or end of the time series
  
  #needs daily values of climate data
  
  if(any(duplicated(climate_data$DOY))) {
    stop()
  }
  
  filter(climate_data, DOY == doy) %>% 
    select(names(constraints)) -> test
  
  filter(climate_data, Tair <= test$Tair + constraints[["Tair"]], Tair >= test$Tair - constraints[["Tair"]], 
         DOY <= test$DOY + constraints[["DOY"]], DOY >= test$DOY - constraints[["DOY"]]) -> x
  
  filter(x, DOY != test$DOY)
  
}


# Plan
# 1. similar_days() with test data on GitHub
# 2. compute mean PAR by day, select sunny days
# 3. find similar days for each sunny day
# 4. select cloudy days from those similar days
# 5. now we have sunny-cloudy pairs (maybe >1 cloudy per sunny) to analyze

wd <- data.frame(DOY = c(1,2,3,4,4,5), Tair = c(2, 2.1, 2.2, -1, 2.1, 1.5))
constraints <- c(4, 7)
names(constraints) <- c("Tair", "DOY")

similar_days(1, wd, constraints)   # returns 2

matching_weather_days(1, wd, tair_delta = 0.01, day_delta = 1)   # returns c()
matching_weather_days(3, wd, tair_delta = 1, day_delta = 2)   # returns c(1, 2, 5)

#let's try an example with multiple DOY matches
constraints <- c(0.01, 2)
names(constraints) <- c("Tair", "DOY")
similar_days(4, wd, constraints_2)


