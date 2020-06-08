####################################################################
# Author: Dan Reed (Daniel.Reed@dfo-mpo.gc.ca)
# Purpose: Assemble Scotian Shelf data set
# Date: Tue Aug 06 11:01:25 2019
####################################################################

# Load libraries
library(data.table)
library(dplyr)
library(readr)
library(lubridate)

###################################
# ZOOPLANKTON DATA
###################################

# Species to include in the data set
species <- c("Calanus finmarchicus", "Calanus glacialis", "Calanus hyperboreus", 
             "Metridia longa", "Metridia lucens", "Metridia", "Temora", "Microcalanus",
             "Oithona", "Oithona similis", "Oithona atlantica", "Paracalanus",
             "Pseudocalanus", "Centropages typicus", "Centropages", "Scolecithricella minor",
             "Larvacea", "Gastropoda", "Bivalvia", "Euphausiacea")

# Load zooplankton data and create new data table for assembled data 
# dt_assem <- fread("../data/ScotianShelf_log_Abundance_Annual_Anomalies.csv") 
dt_assem <- fread("../data/ScotianShelf_log_Abundance_Annual_Anomalies_20200512.csv") 

# Discard unneeded columns 
dt_assem <- dt_assem[, c('station', 'year', species), with = FALSE]

# List linking line to region
line2region <- c(HL = "Central Scotian Shelf",
                 BBL = "Western Scotian Shelf",
                 LL = "Eastern Scotian Shelf",
                 CSL = "Cabot Strait")

# Add region to data table based on transect
dt_assem[, region := line2region[sub("[0-9]$", "", station)]]

###################################
# PHYSICAL DATA
###################################

# Load river data
# rivers <- fread("../data/StLawrenceAtQuebecCity.dat")
rivers <- fread("../../Steele/Updated Data/St. Lawrence River Flux/quebec_runoff_1955_2019_Bourgault_estimate.csv")

# Calculate annual mean and add yearRename columns
rivers <- rivers[, .(Mean = rowMeans(.SD)/1000), .(year = seq(1955, 2019))]

# Rename columns
colnames(rivers) <- c("year", "St. Lawrence river flux")

# Join river data 
dt_assem <- dt_assem[rivers, , on = "year"]

# Load bottom temperature data
# bottom_temp <- fread("../data/AZMP_BottomTemp_Gaps_Filled.csv")[, c("Year", "4V", "4W", "4X"), with = FALSE]
bottom_temp <- read_delim("../../Steele/Updated Data/AZMP_BottomTemp.dat",
           comment = "#", col_names = F, delim = " ", trim_ws = T)[, c(1, 11,12,13)]
names(bottom_temp) <- c("year", "Bottom temp 4V", "Bottom temp 4W", "Bottom temp 4X")
bottom_temp <- setDT(bottom_temp)
  
# Join bottom temperatures 
dt_assem <- bottom_temp[dt_assem, , on = "year"]

# Load North Atlantic Oscillation (NAO) data
# NAO <- fread("../data/AZMP_CIL_ICE_NAO_Redux.dat", skip = "1895")[, c(1, 12)]
NAO <- read_delim("../../Steele/Updated Data/AZMP_CIL_ICE_NAO.dat",
                  comment = "#", col_names = F, delim = " ", trim_ws = T)[, c(1, 12)]
names(NAO) <- c("year", "NAO")
NAO <- setDT(NAO)

# Add NAO data
dt_assem <- NAO[dt_assem, , on = "year"]

# Load sea-surface temperature data
# SST <- fread("../data/AZMP_SST.dat", skip = "1985")[, c(1, 10:13)]
SST <- read_delim("../../Steele/Updated Data/AZMP_SST_Seasonal.dat",
                  comment = "#", col_names = F, delim = " ", trim_ws = T)[, c(1, 14:17)]
names(SST) <- c("year", "SST 4V", "SST 4W", "SST 4XSS", "SST 4eGoM + BoF")
SST <- setDT(SST)

# Add SST data joining by both year
dt_assem <- SST[dt_assem, , on = "year"]

# Load Atlantic Multidecadal Oscillation (AMO) data
# AMO <- fread("../data/amo.csv")
AMO <- read_delim("../../Steele/Updated Data/amon.us.data.txt",
                  comment = "#", col_names = F, delim = " ",
                  skip = 1, n_max = 72, trim_ws = T)
AMO <- setDT(AMO)

# Calculate annual means
AMO <- AMO[, .(AMO = rowMeans(.SD)), .SDcols = c(2:13), .(year = seq(1948, 2019))]

# Join AMO data
dt_assem <- AMO[dt_assem, , on = "year"]

# Load Cold Intermediate Layer (CIL) data
# CIL <- fread("../data/AZMP_CIL_ICE_NAO_Redux.dat", skip = "1985")[, c(1,9)]
CIL <- read_delim("../../Steele/Updated Data/AZMP_CIL_ICE_NAO.dat",
                  comment = "#", col_names = F, delim = " ", trim_ws = T)[, c(1, 9)]
names(CIL) <- c("year", "CIL volume SS")

# Rename missing values
CIL <- CIL %>%
  mutate(`CIL volume SS` = ifelse(`CIL volume SS` == -99, NA, `CIL volume SS`))

CIL <- setDT(CIL)

# Add CIL data 
dt_assem <- CIL[dt_assem, , on = "year"]

###################################
# PHENOLOGY DATA
###################################
# Load SST warming data
df_loaded <- load("../../Steele/Updated Data/SST_Warming_20200320.RData")

# Convert all loaded data frames to data tables
lapply(df_loaded, function(x) setDT(get(x)))

# Add SST warming for each region
dt_assem <- df_norm_anomaly_annual_l[dt_assem, , on = c("region", "year")]

# Rename column
setnames(dt_assem, old = "value", new = "SST warming")

# Remove data frames that were loaded - later loads use the same names
rm(list = df_loaded)

# Load bloom data
df_loaded <- load("../../Steele/Updated Data/Bloom_Metrics_20200320.RData")

# Convert all loaded data frames to data tables
lapply(df_loaded, function(x) setDT(get(x)))

# Filter to start of bloom
start <- df_norm_anomaly_annual_l[variable == "start", .(region, year, value)]

# Change name
setnames(start, old = "value", new = "Bloom start")

# Add to assembled data table
dt_assem <- start[dt_assem, , on  = c("region", "year")]

# Filter to duration of bloom
duration <- df_norm_anomaly_annual_l[variable == "duration", .(region, year, value)]

# Change name
setnames(duration, old = "value", new = "Bloom duration")

# Add to assembled data table
dt_assem <- duration[dt_assem, , on  = c("region", "year")]

# Remove data frames that were loaded - later loads use the same names
rm(list = df_loaded)

# Load ice volume data
# ice_volume <- fread("../../Fish/data/AZMP_CIL_ICE_NAO_Redux.dat", skip = "1985")[, c(1,11)]
ice_volume <- read_delim("../../Steele/Updated Data/AZMP_CIL_ICE_NAO.dat",
                         comment = "#", col_names = F, delim = " ", trim_ws = T)[, c(1, 11)]
names(ice_volume) <- c("year", "Ice volume GSL+SS")
ice_volume <- ice_volume %>%
  mutate(`Ice volume GSL+SS` = ifelse(`Ice volume GSL+SS` == -99, NA, `Ice volume GSL+SS`))

ice_volume <- setDT(ice_volume) 

# Add ice volume data 
dt_assem <- ice_volume[dt_assem, , on = "year"]

# Load ice timing data
df_loaded <- load("../../Steele/Updated Data/Ice_Timing_20200330.RData")

# Convert all loaded data frames to data tables
lapply(df_loaded, function(x) setDT(get(x)))

# Filter to final day of GSL + SS ice
last_day <- df_norm_anomaly_mean_annual_l[region == "GSL_SS" & variable == "day_last", .(year, value)]

# Change name
setnames(last_day, old = "value", new = "Ice end day GSL+SS")

# Add last day of ice
dt_assem <- last_day[dt_assem, , on = "year"]

# Remove data frames that were loaded - later loads use the same names
rm(list = df_loaded)

# load Cfin/Zoo peaks
df_loaded <- load("../output/Zoo_Peaks.RData")

# Convert all loaded data frames to data tables
lapply(df_loaded, function(x) setDT(get(x)))

# Filter to Cfin total peak
Cfin_max <- Cfin_total_peaks_HL2 %>%
  dplyr::rename(`Cfin max yearday HL2`=peak)

# Turn into standardised anomalies
Cfin_max[, `Cfin max yearday HL2` := as.vector(scale(`Cfin max yearday HL2`))]

# Add year-day of maximum Calinus finmarchicus
dt_assem <- Cfin_max[dt_assem, , on = "year"]

# Filter to Cfin I-III peak
CfinI_III_max <- Cfin_I_III_peaks_HL2 %>%
  dplyr::rename(`Cfin I+II+III max yearday HL2`=peak)

# Turn into standardised anomalies
CfinI_III_max[, `Cfin I+II+III max yearday HL2` := as.vector(scale(`Cfin I+II+III max yearday HL2`))]

# Add maximum in early life stages
dt_assem <- CfinI_III_max[dt_assem, , , on = "year"]

# Filter to Zoo biomass peak
zoo_max <- Z_biomass_peaks_HL2 %>%
  dplyr::rename(`Biomass max yearday HL2`=peak)

# Turn into standardised anomalies
zoo_max[, `Biomass max yearday HL2` := as.vector(scale(`Biomass max yearday HL2`))]

# Add maximum in early life stages
dt_assem <- zoo_max[dt_assem, , on = "year"]

# Load ice timing data
# df_loaded <- load("../../Fish/data/Ice_AreaVolume_20171010.RData")
dt_ice_area <- read_delim("../../Steele/Updated Data/IceAreaRegions.GEC.dat",
                          comment = "#", col_names = F, delim = " ", trim_ws = T)[, c(2,3,4)]
names(dt_ice_area) <- c("date", "GSL", "SS")

# Rename missing values
dt_ice_area <- dt_ice_area %>%
  mutate(GSL=ifelse(GSL==-99, NA, GSL)) %>%
  mutate(SS=ifelse(SS==-99, NA, SS)) %>%
  mutate(year=year(date)) %>%
  mutate(month=month(date))

# Calculate monthly ice areas for SS & GSL *together*
dt_ice_area <- dt_ice_area %>%
  mutate(total = GSL+SS) %>%
  filter(month < 4) %>%
  group_by(year) %>%
  summarise(mean_winter_area = mean(total, na.rm=T)) %>%
  ungroup()
  
# Convert into anomalies
dt_ice_area <- setDT(dt_ice_area)
dt_ice_area <- dt_ice_area[, .(year, `Ice area GSL+SS` = as.vector(scale(mean_winter_area)))]

# Add ice area to data table
dt_assem <- dt_ice_area[dt_assem, , on = "year"]

# Read Excel sheet
strat <- read_delim("../data/weightedAnomalyData.csv",
                    comment = "#", col_names = T, delim = ",", trim_ws = T)

# filter columns
strat <- strat %>%
  select(Year, sigmaThetaGradientweightedAnomaly) %>%
  rename(year=Year, Stratification=sigmaThetaGradientweightedAnomaly)

# Convert to data table
strat <- setDT(strat)

# Add stratification
dt_assem <- strat[dt_assem, , on = "year"]

write.csv(dt_assem, file = "../output/HL2_assembled_BC.csv", row.names = FALSE)
