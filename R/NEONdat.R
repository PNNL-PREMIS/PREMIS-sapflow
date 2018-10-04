# Script to process NEON tower weather data products
# Stephanie Pennington | Created October 4, 2018

library(tidyr)
library(lubridate)
library(dplyr)
library(readr)

# Load data - downloaded via https://www.neonscience.org/data/neon-data-portal 

read_NEON_dir <- function(path) {
  files <<- list.files(path, pattern = ".csv", full.names = TRUE)
  list <- list()
  for (i in files) {
    list[[i]] <- read_csv(i)
  }
  bind_rows(list)
}

PAR <- read_NEON_dir("../../../../Desktop/NEON/PAR/")
RH <- read_NEON_dir("../../../../Desktop/NEON/RH/")
precip <- read_NEON_dir("../../../../Desktop/NEON/precip/")
