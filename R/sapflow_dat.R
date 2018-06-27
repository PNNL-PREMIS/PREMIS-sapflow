# Script to reformat sapflux data for use with Baseliner program
# Stephanie Pennington | Created May 2018

library(readr)
library(lubridate)
library(tibble)

# Load data
control <- read_csv("../sapflow_data/CR1000-CONTROL_Table1.csv")
salt <- read_csv("../sapflow_data/CR1000-SALT_Table1.csv")
fresh <- read_csv("../sapflow_data/CR1000-FRESH_Table1.csv")
shore <- read_csv("../sapflow_data/CR1000-SHORE_Table1.csv")



control_base <- tibble(
                       Year = year(control$TIMESTAMP), Day = day(control$TIMESTAMP),
                       Time = time(control$TIMESTAMP), VPD = NaN,
                       PAR = NaN, Sensor1 = control$`DiffVolt_Avg(1)`,
                       Sensor2 = control$`DiffVolt_Avg(2)`, Sensor3 = control$`DiffVolt_Avg(3)`,
                       Sensor4 = control$`DiffVolt_Avg(4)`, Sensor5 = control$`DiffVolt_Avg(5)`,
                       Sensor6 = control$`DiffVolt_Avg(6)`, Sensor7 = control$`DiffVolt_Avg(7)`,
                       Sensor8 = control$`DiffVolt_Avg(8)`)

write_csv(control_base, "../sapflow_data/control_base.csv", col_names = FALSE)
