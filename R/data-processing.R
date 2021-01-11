## Processing code used for sapflow-Rs.Rmd analysis
## Created January 8, 2021 | Stephanie Pennington

# Load in the raw data

# inventory
inventory <- read_csv("../../PREMIS-stormsurge/inventory/ss-inventory.csv",
                      col_types = "ccddccccdcccc")

# rs chamber ports
ports_lt <- read_csv("../../PREMIS-ghg/design/LT_portcodes.csv", col_types = "dc")

# species codes
species_codes <- read_csv("../../PREMIS-ghg/inventory_data/species_codes.csv",
                          col_types = "ccc")

# K (sapflow) data from Baseliner
read_csv("../SERC/baseliner_data/control_Kest_yr.csv", 
         col_names = c("Year", "DOY","Time","VPD", "PAR", paste0("C", 1:8)),
         col_types = strrep("d", 13)) %>% 
  mutate(Date = as_date(DOY - 1, origin = "2018-01-01")) %>% 
  separate(Date, c("Y", "Month", "Day"), sep = "-") %>% 
  separate(Time, c("Hour", "Min"), sep = -2) -> sapflow_raw

#SP needs to clarify what this does
sapflow_raw$Hour[sapflow_raw$Hour == ""] <- 0

# Baseliner outputs the data in an amiguous format, we want to reshape
sapflow_raw %>% 
  mutate(Timestamp = mdy_hm(paste(Month, Day, Year, Hour, Min), tz = "EST")) %>%
  select(-Y, -Year, - Day, - Month) %>% 
  gather("Tree", "K", C1:C8) %>% 
  log_obs("sapflow_raw gather") %>% 
  left_join(inventory, by = c("Tree" = "Sapflux")) %>% 
  log_obs("join with inventory") %>% 
  select(Timestamp, PAR, K, Tree, Species_code, Plot) -> sapflow_k_long

# continuous Rs data
read_csv("../SERC/con_licor_data_20191120.csv",
         col_types = "dTcddddddddddc") %>%
  mutate(Timestamp = force_tz(Timestamp, tz = "EST")) %>% 
  left_join(ports_lt, by = "Port") %>% 
  left_join(inventory, by = c("Tree" = "Sapflux"))-> rs_long

# precipitation data
read_csv("../SERC/met_data/SECPRE_30min.csv",
         col_types = "ccddTTddddT") %>% 
  mutate(Timestamp = ymd_hms(endDateTime, tz = "UTC")) %>% 
  rename(Precip = secPrecipBulk) %>% 
  select(Timestamp, Precip) -> precip

# air temperature and relative humidity
read_csv("../SERC/met_data/RH_30min.csv", 
         col_types = paste0("ccccTT", strrep("d", 24), "T")) %>% 
  filter(horizontalPosition == "000", verticalPosition == "060") %>% 
  mutate(Timestamp = ymd_hms(endDateTime, tz = "UTC")) %>% 
  rename(RH = RHMean, Tair = tempRHMean) %>% 
  select(Timestamp, RH, Tair) -> temprh

# PAR
read_csv("../SERC/met_data/PARPAR_30min.csv",
         col_types = paste0("ccccTT", strrep("d", 16), "T")) %>% 
  # filter for a vertical position above the canopy
  filter(verticalPosition == "050") %>% 
  mutate(Timestamp = ymd_hms(endDateTime, tz = "UTC")) %>% 
  select(Timestamp, PAR = PARMean) %>%
  left_join(precip, by = "Timestamp") %>% 
  left_join(temprh, by = "Timestamp") %>% 
  # At this point convert everything to EST for the rest of the analysis
  # Note this doesn't change the times themselves, just associated time zone
  mutate(Timestamp = with_tz(Timestamp, tzone = "EST"),
         Timestamp = round_date(Timestamp, unit = "hour")) %>% 
  group_by(Timestamp) %>% 
  summarise(PAR = mean(PAR), 
            Precip = mean(Precip), 
            RH = mean(RH),
            Tair = mean(Tair),
            .groups = "drop") %>% 
  mutate(DOY = yday(Timestamp), 
         Date = date(Timestamp),
         es = (6.11 *10^((7.5 * Tair)/(273.3 + Tair)))/10,
         VPD = ((100 - RH) * es)/100) %>% 
  select(-es) -> wx_dat_no_dpar

# Compute daytime PAR as a new column
sunlight_times <- getSunlightTimes(wx_dat_no_dpar$Date,
                                   lat = 38.9, lon = -77, tz = "EST", 
                                   keep = c("sunrise", "sunset"))
wx_dat_no_dpar$sunrise <- sunlight_times$sunrise
wx_dat_no_dpar$sunset <- sunlight_times$sunset
wx_dat_no_dpar$Daytime <- with(wx_dat_no_dpar, Timestamp >= sunrise, Timestamp <= sunset)

wx_dat_no_dpar %>% 
  filter(Daytime) %>%   # daytime only
  group_by(DOY) %>% 
  summarise(Daytime_PAR = mean(PAR, na.rm = TRUE),
            .groups = "drop") %>% 
  # we've computed mean *daytime* PAR; now merge back in
  right_join(wx_dat_no_dpar, by = "DOY") %>% 
  # compute daytime PAR groups - cloudy, medium, sunny
  group_by(month(Timestamp)) %>% 
  mutate(PAR_group = as.factor(ntile(Daytime_PAR, 3))) %>% 
  ungroup() %>% 
  select(-`month(Timestamp)`) ->  # no longer needed
  wx_dat