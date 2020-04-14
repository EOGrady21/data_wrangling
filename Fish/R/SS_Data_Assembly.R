####################################################################
# Author: Dan Reed (Daniel.Reed@dfo-mpo.gc.ca)
# Purpose: Assemble Scotian Shelf data set
# Date: Tue Aug 06 11:01:25 2019
####################################################################

# Load libraries
library(data.table)

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
dt_assem <- fread("../data/ScotianShelf_log_Abundance_Annual_Anomalies.csv") 

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
rivers <- fread("../data/StLawrenceAtQuebecCity.dat")

# Rename columns
colnames(rivers) <- c("year", "St. Lawrence river flux")

# Join river data 
dt_assem <- dt_assem[rivers, , on = "year"]

# Load bottom temperature data
bottom_temp <- fread("../data/AZMP_BottomTemp_Gaps_Filled.csv")[, c("Year", "4V", "4W", "4X"), with = FALSE]

# Rename columns
colnames(bottom_temp) <- c("year", "Bottom temp 4V", "Bottom temp 4W", "Bottom temp 4X")

# Join bottom temperatures 
dt_assem <- bottom_temp[dt_assem, , on = "year"]

# Load North Atlantic Oscillation (NAO) data
NAO <- fread("../data/AZMP_CIL_ICE_NAO_Redux.dat", skip = "1895")[, c(1, 12)]

# Rename columns
colnames(NAO) <- c("year", "NAO")

# Add NAO data
dt_assem <- dt_assem[NAO, on = "year"]

# Load sea-surface temperature data
SST <- fread("../data/AZMP_SST.dat", skip = "1985")[, c(1, 10:13)]

# Rename columns
colnames(SST) <- c("year", "SST 4V", "SST 4W", "SST 4XSS", "SST 4eGoM + BoF")

# Add SST data joining by both year
dt_assem <- SST[dt_assem, , on = "year"]

# Load Atlantic Multidecadal Oscillation (AMO) data
AMO <- fread("../data/amo.csv")

# Calculate annual means
AMO <- AMO[year < 2017, .(year, AMO = rowMeans(.SD)), .SDcols = names(AMO)[-1]]

# Join AMO data
dt_assem <- AMO[dt_assem, , on = "year"]

# Load Cold Intermediate Layer (CIL) data
CIL <- fread("../data/AZMP_CIL_ICE_NAO_Redux.dat", skip = "1985")[, c(1,9)]

# Rename columns
colnames(CIL) <- c("year", "CIL volume SS")

# Rename missing values
CIL[CIL == -99] <- NA

# Add CIL data 
dt_assem <- CIL[dt_assem, , on = "year"]

###################################
# PHENOLOGY DATA
###################################
# Load SST warming data
df_loaded <- load("../data/SST_Warming_20171005.RData")

# Convert all loaded data frames to data tables
lapply(df_loaded, function(x) setDT(get(x)))

# Add SST warming for each region
dt_assem <- df_norm_anomaly_annual_l[dt_assem, , on = c("region", "year")]

# Rename column
setnames(dt_assem, old = "value", new = "SST warming")

# Remove data frames that were loaded - later loads use the same names
rm(list = df_loaded)

# Load bloom data
df_loaded <- load("../data/Bloom_Metrics_20171005.RData")

# Convert all loaded data frames to data tables
lapply(df_loaded, function(x) setDT(get(x)))

# Filter to start of bloom
start <- df_norm_anomaly_annual_l[variable == "start", .(region, year, value)]

# Change name
setnames(start, old = "value", new = "Bloom start")

# Add to assembled data table
dt_assem <- start[dt_assem, on  = c("region", "year")]

# Filter to duration of bloom
duration <- df_norm_anomaly_annual_l[variable == "duration", .(region, year, value)]

# Change name
setnames(duration, old = "value", new = "Bloom duration")

# Add to assembled data table
dt_assem <- duration[dt_assem, on  = c("region", "year")]

# Remove data frames that were loaded - later loads use the same names
rm(list = df_loaded)

# Load ice volume data
ice_volume <- fread("../data/AZMP_CIL_ICE_NAO_Redux.dat", skip = "1985")[, c(1,11)]

# Rename columns
colnames(ice_volume) <- c("year", "Ice volume GSL+SS")

# Rename missing values
ice_volume[ice_volume == -99] <- NA 

# Add ice volume data 
dt_assem <- ice_volume[dt_assem, , on = "year"]

# Load ice timing data
df_loaded <- load("../data/Ice_Timing_20171010.RData")

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

# Load C. finmarchicus maximum data
Cfin_max <- fread("../data/Cfin_total_peaks_HL2.csv")

# Change column name to something more descriptive
colnames(Cfin_max) <- c("year", "Cfin max yearday HL2")

# Turn into standardised anomalies
Cfin_max[, `Cfin max yearday HL2` := as.vector(scale(`Cfin max yearday HL2`))]

# Add year-day of maximum Calinus finmarchicus
dt_assem <- Cfin_max[dt_assem, , on = "year"]

# Load C. finmarchicus I+II+III maximum data
CfinI_III_max <- fread("../data/Cfin_I_III_peaks_HL2.csv")

# Change column name to something more descriptive
colnames(CfinI_III_max) <- c("year", "Cfin I+II+III max yearday HL2")

# Turn into standardised anomalies
CfinI_III_max[, `Cfin I+II+III max yearday HL2` := as.vector(scale(`Cfin I+II+III max yearday HL2`))]

# Add maximum in early life stages
dt_assem <- CfinI_III_max[dt_assem, , on = "year"]

# Load zooplankton biomass maximum data
zoo_max <- fread("../data/Z_biomass_peaks_HL2.csv")

# Change column name to something more descriptive
colnames(zoo_max) <- c("year", "Biomass max yearday HL2")

# Convert to standardised anomaly
zoo_max[, `Biomass max yearday HL2` := as.vector(scale(`Biomass max yearday HL2`))]

# Add zooplankton biomass
dt_assem <- zoo_max[dt_assem, , on = "year"]

# Load ice timing data
df_loaded <- load("../data/Ice_AreaVolume_20171010.RData")

# Convert all loaded data frames to data tables
lapply(df_loaded, function(x) setDT(get(x)))

# Calculate monthly ice areas for SS & GSL *together*
dt_ice_area <- df_data_filtered_l[region != "NLS" & month < 4 & variable == "area", 
                                  .(monthly_area = sum(value)), by = c("year", "month")]

# Calculate mean winter ice area
dt_ice_area <- dt_ice_area[, .(mean_winter_area = mean(monthly_area)), by = "year"]

# Convert into anomalies
dt_ice_area <- dt_ice_area[, .(year, `Ice area GSL+SS` = as.vector(scale(mean_winter_area)))]

# Add ice area to data table
dt_assem <- dt_ice_area[dt_assem, , on = "year"]

# Read Excel sheet
strat <- readxl::read_xlsx("../data/Weighted Annual Mean Anomaly Gradient Areas 4-23.xlsx", 
                           sheet = "Density",
                           skip = 2, col_names = FALSE) 

# Convert to data table
setDT(strat)

# Drop unneeded columns and NA rows
strat <- na.omit(strat[,  (names(strat)[setdiff(1:ncol(strat), c(1, 3))]) := NULL])

# Change names
names(strat) <- c("year", "Stratification")

# Add stratification
dt_assem <- strat[dt_assem, , on = "year"]

write.csv(dt_assem, file = "../output/HL2_assembled.csv", row.names = FALSE)
