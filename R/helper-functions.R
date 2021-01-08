## Helper Functions for Rs-F Analysis 
## Created January 8, 2021 | Stephanie Pennington

# Function to track the data size as we process it
obs_count_data <- tibble()
step_count <- 0
log_obs <- function(d, step_name) {
  # `d` is a data frame
  # `step_name` is a character description of what's done in the step
  step_count <<- step_count + 1
  d %>% 
    group_by(Tree) %>% 
    summarise(step = step_count,
              step_name = step_name,
              n = n(),
              .groups = "drop") %>% 
    bind_rows(obs_count_data) ->> obs_count_data
  d
}

# Function to compute the correlation of Rs and Js at a designated `hour_range`
compute_lag_cor <- function(rs, js, hour_range) {
  # `rs` and `js` are numeric vectors with the same length
  # hour_range is a numeric vector of length 2 that contains the start and end hour to lag
  hours <- min(hour_range):max(hour_range)
  result <- tibble(Hour = hours, Cor = NA)
  
  if(sum(!is.na(rs)) < 6 | sum(!is.na(js)) < 6) {
    return(result)  # just NA
  }
  
  for(i in seq_along(hours)) {
    js_lag <- lead(js, n = hours[i])
    result$Cor[i] <- cor(rs, js_lag, use = "na.or.complete")
  }
  return(result)
}

# Function to salculate sapflux from K
calc_sapflux <- function(sapflow_data, sapwood_area) {
  sapflow_data %>% 
    left_join(sapwood_area, by = "Tree") %>% 
    # Then, we need to transform K to Js (sapflux density) through 
    # a conversion from Granier, A. (1987)
    mutate(Js = 119 * 10^(-6) * K^(1.231)) %>%
    # Then sapflux density to sapflux (F, cm^3/s)
    mutate(F = Js * sw_area_cm2)
}

## Function to match days with similar weather conditions
## Created 1/20/2020 for Rs-Js analysis | Stephanie Pennington

similar_days <- function(day_of_year, climate_data, lookahead, constraints) {
  # Find all days in a window (of size `window`) around `doy` for
  # which the air temperature is within `tair_delta` of the tair on `doy`
  
  # doy: Day of year, an integer
  # climate_data: A data frame with at least `DOY` and `Tair` (degC) columns
  # lookahead: A positive integer with number of days forward to look
  # constraints: A vector with at least one climate variable giving maximum difference
  
  # Returns a vector of DOYs, integers matching these conditions
  
  # Note this function will need to handle edge cases, when doy is close
  # to the beginning or end of the time series
  
  if(sum(constraints < 0) > 0) {
    stop("Constrains must be positive")
  }
  
  if(!day_of_year %in% climate_data$DOY) {
    stop("Day of year not found in dataset")
  }
  
  if(any(duplicated(climate_data$DOY))) {
    stop("Duplicate DOY found")
  }
  
  if(lookahead <= 0) {
    stop("Day window must be positive")
  }
  
  # Setup
  which(climate_data$DOY == day_of_year) -> day_row
  cd <- filter(climate_data, DOY <= day_of_year + lookahead, DOY >= day_of_year)
  which(cd$DOY == day_of_year) -> cd_day_row
  message("Starting with ", nrow(cd), " rows\n")
  
  # Loop
  for(i in seq_along(constraints)) {
    con_name <- names(constraints)[i]
    con_value <- constraints[i]
    message("Constraint ", i, " is named ", con_name, " and has value ", con_value, "\n")
    # We need to unlist() to isolate single value here
    diffs <- unlist(cd[ , con_name]) - unlist(cd[cd_day_row, con_name])
    message("Differences are ", diffs, "\n")
    cd <- cd[abs(diffs) <= con_value, ]
    message("We now have ", nrow(cd), " rows\n")
  }
  
  filter(cd, DOY != day_of_year) %>% pull(DOY)
  
}

# Plan
# 1. similar_days() with test data on GitHub
# 2. compute mean PAR by day, select sunny days
# 3. find similar days for each sunny day
# 4. select cloudy days from those similar days
# 5. now we have sunny-cloudy pairs (maybe >1 cloudy per sunny) to analyze

# -------- TEST CODE --------
library(testthat)
library(tibble)
clim <- tibble(DOY = 101:105,
               x = c(1, 1.5, 0.75, 1, 50),
               y = c(10, -5, 8, 8, 11))

# Day of year not found in climate - should error
expect_error(similar_days(clim, day_of_year = 50, 1, vector()))

# Lookahead value should always be positive
expect_error(similar_days(clim, day_of_year = 101, lookahead = -1, vector()), 
             regexp = "window must be positive")

# No DOY duplicates in climate data
expect_error(similar_days(rbind(clim, clim), day_of_year = 103, lookahead = 5, vector()),
             regexp = "Duplicate DOY found")

# No constraints - should return all days
x <- similar_days(101, clim, lookahead = nrow(clim), constraints = vector())
expect_equal(x, 102:105)

# Basic constraint
con <- c("x" = 1)  # this allows x +/- 1 maximum
x <- similar_days(101, clim, lookahead = nrow(clim), constraints = con)
expect_equal(x, c(102, 103, 104))

# Same constraint, smaller lookahead window
x <- similar_days(101, clim, lookahead = 2, constraints = con)
expect_equal(x, c(102, 103))

# Multiple constraints
con <- c("x" = 1, "y" = 2)
x <- similar_days(clim, day_of_year = 101, lookahead = nrow(clim), constraints = con)
expect_equal(x, c(103, 104))

# Constrains must be positive
con <- c("x" = 1, "y" = -2)
expect_error(similar_days(clim, day_of_year = 104, lookahead = 2, constraints = con),
             regexp = "Constrains must be positive")
