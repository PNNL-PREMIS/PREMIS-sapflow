# Script to reformat sapflux data for use with Baseliner program
# Stephanie Pennington | Created May 2018

library(readr)
library(lubridate)
library(tibble)
library(tidyr)
library(dplyr)
library(ggplot2)
library(reshape2)

# Load data and prepare data to be combined into one dataframe
cat("Loading data...")
control <- read_csv("../sapflow_data/CR1000-CONTROL_Table1.csv", 
                    col_names = c("Timestamp", "Record", "C1", "C2", "C3", "C4", "C5", "C6", "C7", "C8"))
control$Timestamp <- mdy_hm(control$Timestamp)

salt <- read_csv("../sapflow_data/CR1000-SALT_Table1.csv", 
                 col_names = c("Timestamp", "Record", "S1", "S2", "S3", "S4", "S5", "S6", "S7", "S8")) 
salt$Timestamp <- mdy_hm(salt$Timestamp)

fresh <- read_csv("../sapflow_data/CR1000-FRESH_Table1.csv", 
                  col_names = c("Timestamp", "Record", "F1", "F2", "F3", "F4", "F5", "F6", "F7", "F8"))
fresh$Timestamp <- mdy_hm(fresh$Timestamp)

shore <- read_csv("../sapflow_data/CR1000-SHORE_Table1.csv", 
                  col_names = c("Timestamp", "Record", "Sh1", "Sh2", "Sh3", "Sh4", "Sh5", "Sh6")) 
shore$Timestamp <- mdy_hm(shore$Timestamp)

load("../SERC_met_data_for_Charlotte_&_Stephanie.Rdata")


# Create 30 minute averages
cat("Creating 30 minute averages...")
averaged <- SERC_met %>% 
  group_by(Date = cut(date.time, breaks = "30 min"), variable) %>% 
  summarize(mean = mean(value, na.rm = TRUE), date.time = mean(date.time), n = n()) 
averaged$Timestamp <- ymd_hms(averaged$Date)

averaged <- averaged %>%
  separate(Date, into = c("Date", "time"), sep = " ") %>%
  separate(time, into = c("h", "m"), sep = ":")
averaged$time <- paste0(hour(averaged$date.time), averaged$m)


# Pull out data needed for Baseliner
cat("Gathering necessary weather data...")
met_wx <- tibble(Timestamp = averaged$Timestamp[which(averaged$variable == "PAR")],
                 date = averaged$Date[which(averaged$variable == "PAR")],
                 time = averaged$time[which(averaged$variable == "PAR")],
                 PAR = averaged$mean[which(averaged$variable == "PAR")] * 0.327,
                 es = (6.11 *10^((7.5 * averaged$mean[which(averaged$variable == "Temp")])/(273.3 +  averaged$mean[which(averaged$variable == "Temp")])))/10,
                 VPD = ((100 - averaged$mean[which(averaged$variable == "RH")]) * es)/100)



# Truncate based on available weather data
cat("Combining into single dataframe...")

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

# Save data for baseliner
cat("Saving dataframes...")
write_csv(control.bl, "../baseliner_data/control_bl.csv", col_names = FALSE)
write_csv(fresh.bl, "../baseliner_data/fresh_bl.csv", col_names = FALSE)
write_csv(salt.bl, "../baseliner_data/salt_bl.csv", col_names = FALSE)
write_csv(shore.bl, "../baseliner_data/shore_bl.csv", col_names = FALSE)

# Plot using ggplot and facet by Tree
cat("Generating plots...")

control_p <- melt(control, id.vars = c("Timestamp", "Record"), measure.vars = c("C1", "C2", "C3", "C4", "C5", "C6", "C7", "C8"))
colnames(control_p) <-c("Timestamp", "Record", "Tree", "Voltage")

salt_p <- melt(id.vars = c("Timestamp", "Record"), measure.vars = c("S1", "S2", "S3", "S4", "S5", "S6", "S7", "S8"))
colnames(salt_p) <-c("Timestamp", "Record", "Tree", "Voltage")

fresh_p <- melt(id.vars = c("Timestamp", "Record"), measure.vars = c("F1", "F2", "F3", "F4", "F5", "F6", "F7", "F8"))
colnames(fresh_p) <-c("Timestamp", "Record", "Tree", "Voltage")

shore_p <- melt(id.vars = c("Timestamp", "Record"), measure.vars = c("Sh1", "Sh2", "Sh3", "Sh4", "Sh5", "Sh6"))
colnames(shore_p) <-c("Timestamp", "Record", "Tree", "Voltage")

# ----- Quick plot for BC data ----- #

east <- read_csv("../BC/CR1000-EAST_2019_4_18_Table1.dat", skip = 4, 
                 col_names = c("Timestamp", "Record", "E1", "E2", "E3", "E4", "E5", "E6", "E7", "E8")) %>% 
  melt(id.vars = c("Timestamp", "Record"), measure.vars = c("E1", "E2", "E3", "E4", "E5", "E6", "E7", "E8"))

west <- read_csv("../BC/CR1000-WEST_2019_4_18_Table1.dat", skip = 4, 
                 col_names = c("Timestamp", "Record", "W1", "W2", "W3", "W4", "W5", "W6", "W7", "W8")) %>% 
  melt(id.vars = c("Timestamp", "Record"), measure.vars = c("W1", "W2", "W3", "W4", "W5", "W6", "W7", "W8"))

control_plot <- ggplot(data = control_p, aes(x = Timestamp, y = Voltage, group = Tree)) +
  geom_line() +
  facet_wrap(~Tree, ncol = 2) +
  ylim(0,1) +
  ggtitle("Control")
print(control_plot)
ggsave("../control.png")

fresh_plot <- ggplot(data = fresh, aes(x = Timestamp, y = Voltage, group = Tree)) +
  geom_line() +
  facet_wrap(~Tree, ncol = 2) +
  ylim(0,1) +
  ggtitle("Fresh")
ggsave("../fresh.png")

salt_plot <- ggplot(data = salt, aes(x = Timestamp, y = Voltage, group = Tree)) +
  geom_line() +
  facet_wrap(~Tree, ncol = 2) +
  ylim(0,1) +
  ggtitle("Salt")
ggsave("../salt.png")

shore_plot <- ggplot(data = shore, aes(x = Timestamp, y = Voltage, group = Tree)) +
  geom_line() +
  facet_wrap(~Tree, ncol = 2) + 
  ylim(0,1) +
  ggtitle("Shore")
ggsave("../shore.png")

