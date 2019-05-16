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
colnames(east) <-c("Timestamp", "Record", "Tree", "Voltage")

west <- read_csv("../BC/CR1000-WEST_2019_4_18_Table1.dat", skip = 4, 
                 col_names = c("Timestamp", "Record", "W1", "W2", "W3", "W4", "W5", "W6", "W7", "W8")) %>% 
  melt(id.vars = c("Timestamp", "Record"), measure.vars = c("W1", "W2", "W3", "W4", "W5", "W6", "W7", "W8"))
colnames(west) <-c("Timestamp", "Record", "Tree", "Voltage")

# ----- This plots all East trees in one plot -----
ggplot(data = east, aes(x = Timestamp, y = Voltage, group = Tree)) +
  geom_line() +
  facet_wrap(~Tree, ncol = 2) +
  ylim(0,1) +
  ggtitle("BC-East")

## ----- Plotting an individual tree -----

# This plots one individual tree (in this example, we are plotting E1)
ggplot(data = filter(east, Tree == "E1"), aes(x = Timestamp, y = Voltage)) +
  geom_line() +
  facet_wrap(~Tree, ncol = 2) +
  ggtitle("East 1")
# As you can see from the plot, something went wrong in April with the data, if you would like to only view certain
# voltage data, you can change the y-axis limits using "ylim(0,1) +". For example...
ggplot(data = filter(east, Tree == "E1"), aes(x = Timestamp, y = Voltage)) +
  geom_line() +
  ylim(0,1) + # adjust the y limits here
  facet_wrap(~Tree, ncol = 2) +
  ggtitle("East 1")
# You can adjust the y limits as you want based on the range in voltage for each tree

# ----- This plots all West trees in one plot -----
ggplot(data = west, aes(x = Timestamp, y = Voltage, group = Tree)) +
  geom_line() +
  facet_wrap(~Tree, ncol = 2) + 
  ylim(0,1) +
  ggtitle("BC-West")

