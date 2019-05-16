# Script to plot raw sapflow data
# Stephanie Pennington | Created May 2018, updated May 2019 for clarity

library(readr)
library(lubridate)
library(tibble)
library(tidyr)
library(dplyr)
library(ggplot2)
library(reshape2)

# Load data
cat("Loading data...")
control <- read_csv("../SERC/sapflow_data/CR1000-CONTROL_Table1_20190510.dat", 
                    col_names = c("Timestamp", "Record", "C1", "C2", "C3", "C4", "C5", "C6", "C7", "C8"), skip = 4)
control$Timestamp <- ymd_hms(control$Timestamp)

salt <- read_csv("../SERC/sapflow_data/CR1000-SALT_Table1_20190510.dat", 
                 col_names = c("Timestamp", "Record", "S1", "S2", "S3", "S4", "S5", "S6", "S7", "S8"), skip = 4) 
salt$Timestamp <- ymd_hms(salt$Timestamp)

fresh <- read_csv("../SERC/sapflow_data/CR1000-FRESH_Table1_20190510.dat", 
                  col_names = c("Timestamp", "Record", "F1", "F2", "F3", "F4", "F5", "F6", "F7", "F8"), skip = 4)
fresh$Timestamp <- ymd_hms(fresh$Timestamp)

shore <- read_csv("../SERC/sapflow_data/CR1000-SHORE_Table1_20190510.dat", 
                  col_names = c("Timestamp", "Record", "Sh1", "Sh2", "Sh3", "Sh4", "Sh5", "Sh6"), skip = 4) 
shore$Timestamp <- ymd_hms(shore$Timestamp)


# Put data into long format to be easily plotted in ggplot
cat("Formatting data for ggplot...")

control_p <- melt(control, id.vars = c("Timestamp", "Record"), measure.vars = c("C1", "C2", "C3", "C4", "C5", "C6", "C7", "C8"))
colnames(control_p) <-c("Timestamp", "Record", "Tree", "Voltage")

salt_p <- melt(salt, id.vars = c("Timestamp", "Record"), measure.vars = c("S1", "S2", "S3", "S4", "S5", "S6", "S7", "S8"))
colnames(salt_p) <-c("Timestamp", "Record", "Tree", "Voltage")

fresh_p <- melt(fresh, id.vars = c("Timestamp", "Record"), measure.vars = c("F1", "F2", "F3", "F4", "F5", "F6", "F7", "F8"))
colnames(fresh_p) <-c("Timestamp", "Record", "Tree", "Voltage")

shore_p <- melt(shore, id.vars = c("Timestamp", "Record"), measure.vars = c("Sh1", "Sh2", "Sh3", "Sh4", "Sh5", "Sh6"))
colnames(shore_p) <-c("Timestamp", "Record", "Tree", "Voltage")

# Plot using ggplot and facet by Tree
cat("Generating plots...")

ggplot(data = control_p, aes(x = Timestamp, y = Voltage, group = Tree)) +
  geom_line() +
  facet_wrap(~Tree, ncol = 2) +
  ylim(0,1) +
  ggtitle("Control")
print(control_plot)
ggsave("../control.png")

ggplot(data = fresh_p, aes(x = Timestamp, y = Voltage, group = Tree)) +
  geom_line() +
  facet_wrap(~Tree, ncol = 2) +
  ylim(0,1) +
  ggtitle("Fresh")
ggsave("../fresh.png")

ggplot(data = salt_p, aes(x = Timestamp, y = Voltage, group = Tree)) +
  geom_line() +
  facet_wrap(~Tree, ncol = 2) +
  ylim(0,1) +
  ggtitle("Salt")
ggsave("../salt.png")

ggplot(data = shore_p, aes(x = Timestamp, y = Voltage, group = Tree)) +
  geom_line() +
  facet_wrap(~Tree, ncol = 2) + 
  ylim(0,1) +
  ggtitle("Shore")
ggsave("../shore.png")

# ----- Quick plot for BC data ----- #

east <- read_csv("../BC/CR1000-EAST_2019_4_18_Table1.dat", skip = 4, 
                 col_names = c("Timestamp", "Record", "E1", "E2", "E3", "E4", "E5", "E6", "E7", "E8")) %>% 
  melt(id.vars = c("Timestamp", "Record"), measure.vars = c("E1", "E2", "E3", "E4", "E5", "E6", "E7", "E8"))

west <- read_csv("../BC/CR1000-WEST_2019_4_18_Table1.dat", skip = 4, 
                 col_names = c("Timestamp", "Record", "W1", "W2", "W3", "W4", "W5", "W6", "W7", "W8")) %>% 
  melt(id.vars = c("Timestamp", "Record"), measure.vars = c("W1", "W2", "W3", "W4", "W5", "W6", "W7", "W8"))

ggplot(data = east, aes(x = Timestamp, y = Voltage, group = Tree)) +
  geom_line() +
  facet_wrap(~Tree, ncol = 2) +
  ylim(0,1) +
  ggtitle("BC-East")

ggplot(data = west, aes(x = Timestamp, y = Voltage, group = Tree)) +
  geom_line() +
  facet_wrap(~Tree, ncol = 2) + 
  ylim(0,1) +
  ggtitle("BC-West")

