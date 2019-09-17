# Script to process sapflow and PAR/VPD data for use in Baseliner
# Stephanie Pennington | Created August 27, 2019

# Load packages
library(readr)
library(lubridate)
library(tibble)
library(tidyr)
library(dplyr)

# BC files are tab delim
west <- read_tsv("../BC/CR1000 BC WEST_Table1.dat",
                 col_names = c("Timestamp", "Record", "W1", "W2", "W3", "W4", "W5", "W6", "W7", "W8"), 
                 skip = 4)
west$Timestamp <- ymd_hm(west$Timestamp)

east <- read_tsv("../BC/CR1000 BC EAST_Table1.dat",
                 col_names = c("Timestamp", "Record", "E1", "E2", "E3", "E4", "E5", "E6", "E7", "E8"), 
                 skip = 4)
east$Timestamp <- ymd_hm(east$Timestamp)

# met data
met_BC <- read_csv("../BC/BC13_MET_June_24_2019.csv", skip = 1)
met_BC$Timestamp <- mdy_hms(met_BC$'Date Time, GMT-07:00')

# pull out met data needed for Baseliner
met_time <- met_BC %>%
  separate(Timestamp, into = c("Date", "time"), sep = " ") %>%
  separate(time, into = c("h", "m"), sep = ":")
met_time$time <- paste0(met_time$h, met_time$m)
met_time$Timestamp <- paste0(met_time$Date, " ", met_time$h,":", met_time$m, ":00")

wx_BC <- tibble(Timestamp = ymd_hms(met_time$Timestamp),
                 Date = met_time$Date,
                 Time = met_time$time,
                 PAR = met_time$`PAR, µmol/m²/s (LGR S/N: 20418939, SEN S/N: 20419083)` * 0.327,
                 es = (6.11 *10^((7.5 * met_time$`Temp, °C (LGR S/N: 20418939, SEN S/N: 20411961)`)/(273.3 +  met_time$`Temp, °C (LGR S/N: 20418939, SEN S/N: 20411961)`)))/10,
                 VPD = ((100 - met_time$`RH, % (LGR S/N: 20418939, SEN S/N: 20411961)`) * es)/100)

east.bl <- left_join(east, wx_BC, by = "Timestamp")
t.series.east <-tibble(Timestamp = seq(from=as.POSIXct(first(east.bl$Timestamp)),
                                       to=as.POSIXct(last(east.bl$Timestamp)),
                                       by="30 min"))
east.bl[is.na(east.bl)] <- NaN
east.bl <- left_join(t.series.east, east.bl)
east.bl <- tibble(Plot.ID = rep(1, nrow(east.bl)),
                                Year = year(east.bl$Timestamp),
                                DOY = yday(east.bl$Timestamp),
                                Time = east.bl$Time,
                                VPD = east.bl$VPD,
                                PAR = east.bl$PAR,
                                E1 = east.bl$E1,
                                E2 = east.bl$E2,
                                E3 = east.bl$E3,
                                E4 = east.bl$E4,
                                E5 = east.bl$E5,
                                E6 = east.bl$E6,
                                E7 = east.bl$E7,
                                E8 = east.bl$E8)

west.bl <- left_join(west, wx_BC, by = "Timestamp")
t.series.west <-tibble(Timestamp = seq(from=as.POSIXct(first(west.bl$Timestamp)),
                                       to=as.POSIXct(last(west.bl$Timestamp)),
                                       by="30 min"))

west.bl <- left_join(t.series.west, west.bl)
west.bl[is.na(west.bl)] <- NaN
west.bl <- tibble(Plot.ID = rep(2, nrow(west.bl)),
                  Year = year(west.bl$Timestamp),
                  DOY = yday(west.bl$Timestamp),
                  Time = west.bl$Time,
                  VPD = west.bl$VPD,
                  PAR = west.bl$PAR,
                  W1 = west.bl$W1,
                  W2 = west.bl$W2,
                  W3 = west.bl$W3,
                  W4 = west.bl$W4,
                  W5 = west.bl$W5,
                  W6 = west.bl$W6,
                  W7 = west.bl$W7,
                  W8 = west.bl$W8)

write_csv(east.bl, "../BC/east_baseliner.csv", col_names = FALSE)
write_csv(west.bl, "../BC/west_baseliner.csv", col_names = FALSE)
