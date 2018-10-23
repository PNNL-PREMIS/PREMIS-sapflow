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
                    col_names = c("Timestamp", "Record", "C1", "C2", "C3", "C4", "C5", "C6", "C7", "C8")) %>%
  melt(id.vars = c("Timestamp", "Record"), measure.vars = c("C1", "C2", "C3", "C4", "C5", "C6", "C7", "C8"))
colnames(control) <-c("Timestamp", "Record", "Tree", "Voltage")
control$Timestamp <- mdy_hm(control$Timestamp)

salt <- read_csv("../sapflow_data/CR1000-SALT_Table1.csv", 
                 col_names = c("Timestamp", "Record", "S1", "S2", "S3", "S4", "S5", "S6", "S7", "S8")) %>% 
  melt(id.vars = c("Timestamp", "Record"), measure.vars = c("S1", "S2", "S3", "S4", "S5", "S6", "S7", "S8"))
colnames(salt) <-c("Timestamp", "Record", "Tree", "Voltage")
salt$Timestamp <- mdy_hm(salt$Timestamp)

fresh <- read_csv("../sapflow_data/CR1000-FRESH_Table1.csv", 
                  col_names = c("Timestamp", "Record", "F1", "F2", "F3", "F4", "F5", "F6", "F7", "F8")) %>% 
  melt(id.vars = c("Timestamp", "Record"), measure.vars = c("F1", "F2", "F3", "F4", "F5", "F6", "F7", "F8"))
colnames(fresh) <-c("Timestamp", "Record", "Tree", "Voltage")
fresh$Timestamp <- mdy_hm(fresh$Timestamp)

shore <- read_csv("../sapflow_data/CR1000-SHORE_Table1.csv", 
                  col_names = c("Timestamp", "Record", "Sh1", "Sh2", "Sh3", "Sh4", "Sh5", "Sh6")) %>% 
  melt(id.vars = c("Timestamp", "Record"), measure.vars = c("Sh1", "Sh2", "Sh3", "Sh4", "Sh5", "Sh6"))
colnames(shore) <-c("Timestamp", "Record", "Tree", "Voltage")
shore$Timestamp <- mdy_hm(shore$Timestamp)

load("C:/Users/penn529/Desktop/SERC_met_data_for_Charlotte_&_Stephanie.Rdata")
averaged <- SERC_met %>% 
  group_by(Date = cut(date.time, breaks = "30 min"), variable) %>% 
  summarize(mean = mean(value, na.rm = TRUE), date.time = mean(date.time), n = n())
met_wx <- tibble(date.time = averaged$Date[which(averaged$variable == "PAR")],
                 PAR = averaged$mean[which(averaged$variable == "PAR")] * 0.327,
                 es = (6.11 *10^((7.5 * averaged$mean[which(averaged$variable == "Temp")])/(273.3 +  averaged$mean[which(averaged$variable == "Temp")])))/10,
                 VPD = ((100 - averaged$mean[which(averaged$variable == "RH")]) * es)/100)

# Combine into one dataframe
cat("Combining into single dataframe...")
combine <- bind_rows(control, fresh, salt, shore)
baselinerDat <- tibble(year = year(averaged$Date), 
                       VPD = met_wx$VPD,
                       PAR = met_wx$PAR,
                       sapflux = combine)


# Plot using ggplot and facet by Tree
cat("Generating plots...")
control_plot <- ggplot(data = control, aes(x = Timestamp, y = Voltage, group = Tree)) +
  geom_line() +
  facet_wrap(~Tree, ncol = 2) +
  ylim(0,1) +
  ggtitle("Control")
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

