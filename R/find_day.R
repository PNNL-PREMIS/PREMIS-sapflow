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
  cat("Starting with", nrow(cd), "rows\n")
  
  # Loop
  for(i in seq_along(constraints)) {
    con_name <- names(constraints)[i]
    con_value <- constraints[i]
    cat("Constraint ", i, " is named ", con_name, " and has value ", con_value, "\n")
    # We need to unlist() to isolate single value here
    diffs <- unlist(cd[ , con_name]) - unlist(cd[cd_day_row, con_name])
    cat("Differences are", diffs, "\n")
    cd <- cd[abs(diffs) <= con_value, ]
    cat("We now have", nrow(cd), "rows\n")
  }
  
  filter(cd, DOY != day_of_year) %>% pull(DOY)
  
}

# SP to do
# - make sure understand similar_days() completely
# - constraints should be positive. Have function check and write a test for it below
# - filter daily for 'sunny days' - for now let's call that the top 1/3 of values
# - run similar_days() for those sunny DOYs, getting a list (?) of similar DOYs
# - in those similar days, look for 'cloudy days' - let's call the bottom 1/3 of PAR values
# - that's the final comparison days

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
