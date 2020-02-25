## Function to match days with similar weather conditions
## Created 1/20/2020 for Rs-Js analysis | Stephanie Pennington

matching_weather_days <- function(doy, weather_data, tair_delta, day_delta) {
  # Find all days in a window (of size `window`) around `doy` for
  # which the air temperature is within `tair_delta` of the tair on `doy`
  
  # doy: Day of year, an integer
  # weather_data: A data frame with at least `doy` and `tair` (degC) columns
  # tair_delta: A double giving maximum tair difference
  # day_delta: A integer giving maximum doy difference
  
  # Returns a vector of doys, integers matching these conditions
  
  # Note this function will need to handle edge cases, when doy is close
  # to the beginning or end of the time series
  tair <- weather_data$Tair[which(weather_data$DOY == doy)] 
  
  filter(weather_data, Tair <= tair[i] + tair_delta, Tair >= tair[i] - tair_delta, 
         DOY <= doy + day_delta[i], DOY >= doy[i] - day_delta) -> x
  
  subset(x, DOY != doy)
  
}


wd <- data.frame(DOY = 1:6, Tair = c(2, 2.1, 2.2, -1, 2.1, 1.5))

matching_weather_days(1, wd, tair_delta = 1, day_delta = 1)   # returns 2
matching_weather_days(1, wd, tair_delta = 0.01, day_delta = 1)   # returns c()
matching_weather_days(3, wd, tair_delta = 1, day_delta = 2)   # returns c(1, 2, 5)


