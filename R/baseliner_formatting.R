# Script to process sapflow and PAR/VPD data for use in Baseliner
# Stephanie Pennington | Created May 16, 2019

# Load packages
library(readr)
library(lubridate)
library(tibble)
library(tidyr)
library(dplyr)

# Load necessary data
cat("Loading data...")

# Sapflow data
control <- read_csv("../SERC/sapflow_data/raw_data/CR1000-CONTROL_Table1_20190829.dat", 
                    col_names = c("Timestamp", "Record", "C1", "C2", "C3", "C4", "C5", "C6", "C7", "C8"), skip = 4)
  control$Timestamp <- ymd_hms(control$Timestamp)

salt <- read_csv("../SERC/sapflow_data/raw_data/CR1000-SEAWATER_Table1_20190829.dat", 
                 col_names = c("Timestamp", "Record", "S1", "S2", "S3", "S4", "S5", "S6", "S7", "S8"), skip = 4) 
  salt$Timestamp <- ymd_hms(salt$Timestamp)

fresh <- read_csv("../SERC/sapflow_data/raw_data/CR1000-FRESH_Table1_20190829.dat", 
                  col_names = c("Timestamp", "Record", "F1", "F2", "F3", "F4", "F5", "F6", "F7", "F8"), skip = 4)
  fresh$Timestamp <- ymd_hms(fresh$Timestamp)

shore <- read_csv("../SERC/sapflow_data/raw_data/CR1000-LINE_Table1_20190829.dat", 
                  col_names = c("Timestamp", "Record", "L1", "L2", "L3", "L4", "L5", "L6"), skip = 4) 
  shore$Timestamp <- ymd_hms(shore$Timestamp)

# Met data
list.files("../SERC/met_data/PAR/", full.names = TRUE) %>% 
  lapply(read_csv) %>% 
  bind_rows() %>% 
  mutate(Timestamp = ymd_hms(endDateTime), PAR = PARMean) %>% 
  select(Timestamp, PAR) -> PAR

list.files("../SERC/met_data/TRH/", full.names = TRUE) %>% 
  lapply(read_csv) %>% 
  bind_rows() %>% 
  mutate(Timestamp = ymd_hms(endDateTime), RH = RHMean, Temp = tempRHMean) %>% 
  select(Timestamp, RH, Temp) -> TRH

left_join(PAR, TRH) %>% 
  mutate(es = (0.6108 *exp((17.27 * Temp)/(273.3 + Temp))), VPD_kPa = ((100 - RH) * es)/100) -> wx_data

# Create 30 minute averages
# cat("Creating 30 minute averages...")
# averaged <- SERC_met %>% 
#   group_by(Date = cut(date.time, breaks = "30 min"), variable) %>% 
#   summarize(mean = mean(value, na.rm = TRUE), date.time = mean(date.time), n = n()) 
# averaged$Timestamp <- ymd_hms(averaged$Date)
# 
# averaged <- averaged %>%
#   separate(Date, into = c("Date", "time"), sep = " ") %>%
#   separate(time, into = c("h", "m"), sep = ":")
# averaged$time <- paste0(hour(averaged$date.time), averaged$m)
# 
# # Pull out data needed for Baseliner
# cat("Gathering necessary weather data...")
# met_wx <- tibble(Timestamp = averaged$Timestamp[which(averaged$variable == "PAR")],
#                  date = averaged$Date[which(averaged$variable == "PAR")],
#                  time = averaged$time[which(averaged$variable == "PAR")],
#                  PAR = averaged$mean[which(averaged$variable == "PAR")] * 0.327,
#                  es = (6.11 *10^((7.5 * averaged$mean[which(averaged$variable == "Temp")])/(273.3 +  averaged$mean[which(averaged$variable == "Temp")])))/10,
#                  VPD = ((100 - averaged$mean[which(averaged$variable == "RH")]) * es)/100)

# Truncate based on available weather data
cat("Combining into single dataframe...")

# Control
cat("Formatting control data...")
control.bl <- right_join(met_wx, control, by = "Timestamp")
t.series.c <-tibble(Timestamp = seq(from=as.POSIXct(first(control.bl$Timestamp)),
                                    to=as.POSIXct(last(control.bl$Timestamp)),
                                    by="30 min"))
control.bl <- left_join(t.series.c, control.bl, by = "Timestamp")
  control.bl$plot.ID <- rep(2, nrow(control.bl))
  control.bl$year <- year(control.bl$Timestamp)
  control.bl$yday <- yday(control.bl$Timestamp)
  control.bl <- control.bl %>%
    separate(Timestamp, into = c("date", "time"), sep = " ") %>%
    separate(time, into = c("h", "m"), sep = ":")
  control.bl$time <- paste0(control.bl$h, control.bl$m)
  control.bl[is.na(control.bl)] <- NaN
  control.bl <- control.bl %>% 
    select(plot.ID, year, yday, time, VPD, PAR, C1, C2, C3, C4, C5, C6, C7, C8)

# Fresh
cat("Formatting fresh data...")
fresh.bl <- right_join(met_wx, fresh, by = "Timestamp")
t.series.f <-tibble(Timestamp = seq(from=as.POSIXct(first(fresh.bl$Timestamp)),
                                    to=as.POSIXct(last(fresh.bl$Timestamp)),
                                    by="30 min"))
fresh.bl <- left_join(t.series.f, fresh.bl, by = "Timestamp")
  fresh.bl$plot.ID <- rep(1, nrow(fresh.bl))
  fresh.bl$year <- year(fresh.bl$Timestamp)
  fresh.bl$yday <- yday(fresh.bl$Timestamp)
  fresh.bl <- fresh.bl %>%
    separate(Timestamp, into = c("date", "time"), sep = " ") %>%
    separate(time, into = c("h", "m"), sep = ":")
  fresh.bl$time <- paste0(fresh.bl$h, fresh.bl$m)
  fresh.bl[is.na(fresh.bl)] <- NaN
  fresh.bl <- fresh.bl %>% 
    select(plot.ID, year, yday, time, VPD, PAR, F1, F2, F3, F4, F5, F6, F7, F8)

# Salt
cat("Formating salt data...")
salt.bl <- right_join(met_wx, salt, by = "Timestamp")
t.series.s <-tibble(Timestamp = seq(from=as.POSIXct(first(salt.bl$Timestamp)),
                                    to=as.POSIXct(last(salt.bl$Timestamp)),
                                    by="30 min"))
salt.bl <- left_join(t.series.s, salt.bl, by = "Timestamp")
  salt.bl$plot.ID <- rep(4, nrow(salt.bl))
  salt.bl$year <- year(salt.bl$Timestamp)
  salt.bl$yday <- yday(salt.bl$Timestamp)
  salt.bl <- salt.bl %>%
    separate(Timestamp, into = c("date", "time"), sep = " ") %>%
    separate(time, into = c("h", "m"), sep = ":")
  salt.bl$time <- paste0(salt.bl$h, salt.bl$m)
  salt.bl[is.na(salt.bl)] <- NaN
  salt.bl <- salt.bl %>% select(plot.ID, year, yday, time, VPD, PAR, S1, S2, S3, S4, S5, S6, S7, S8)

# Shore
cat("Formatting shoreline data...")
shore.bl <- right_join(met_wx, shore, by = "Timestamp")
t.series.sh <-tibble(Timestamp = seq(from=as.POSIXct(first(shore.bl$Timestamp)),
                                     to=as.POSIXct(last(shore.bl$Timestamp)),
                                     by="30 min"))
shore.bl <- left_join(t.series.sh, shore.bl, by = "Timestamp")
  shore.bl$plot.ID <- rep(3, nrow(shore.bl))
  shore.bl$year <- year(shore.bl$Timestamp)
  shore.bl$yday <- yday(shore.bl$Timestamp)
  shore.bl <- shore.bl %>%
    separate(Timestamp, into = c("date", "time"), sep = " ") %>%
    separate(time, into = c("h", "m"), sep = ":")
  shore.bl$time <- paste0(shore.bl$h, shore.bl$m)
  shore.bl[is.na(shore.bl)] <- NaN
  shore.bl <- shore.bl %>% select(plot.ID, year, yday, time, VPD, PAR, Sh1, Sh2, Sh3, Sh4, Sh5, Sh6)

# Save data as csv
cat("Saving dataframes...")
write_csv(control.bl, "../SERC/baseliner_data/control_bl.csv", col_names = FALSE)
write_csv(fresh.bl, "../SERC/baseliner_data/fresh_bl.csv", col_names = FALSE)
write_csv(salt.bl, "../SERC/baseliner_data/salt_bl.csv", col_names = FALSE)
write_csv(shore.bl, "../SERC/baseliner_data/shore_bl.csv", col_names = FALSE)

# The data should now be in a readable format for Baseliner to open
