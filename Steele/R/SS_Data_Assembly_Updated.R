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
dt_assem <- fread("../Updated Data/PL_Transects_log_Abundance_Annual_Anomalies_20200331.csv") 

# Discard unneeded columns 
dt_assem <- dt_assem[, c('transect', 'year', species), with = FALSE]

# Rename transect to station
colnames(dt_assem) = c('station', 'year', species)

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

############## 1. St. Lawrence River Flux ##############

# Load river data
rivers <- fread("../Updated Data/St. Lawrence River Flux/quebec_runoff_1955_2019_Bourgault_estimate.csv")

# Mean across months
rivers = rowMeans(rivers)

# Add year, replace with mean
rivers = cbind(seq(1955,2019,1), rivers)

# Rename columns
colnames(rivers) <- c("year", "St. Lawrence river flux")

# Divide river flux by 1000 to match magnitude of previous data
rivers[,2] = rivers[,2]/1000

# Set as data table
rivers = data.table(rivers)

# Join river data 
dt_assem <- dt_assem[rivers, , on = "year"]

############## 2. Bottom Temperature ##############

# Load bottom temperature data
bottom_temp <- fread("../Updated Data/AZMP_BottomTemp.dat", skip = '1970')[, c(1,11:13)]

# Rename columns
colnames(bottom_temp) <- c("year", "Bottom temp 4V", "Bottom temp 4W", "Bottom temp 4X")

# Filling 2018 gaps in 4V and 4W via interpolation based on temp change in 4X
range = bottom_temp[year == 2017, `Bottom temp 4X`] - bottom_temp[year == 2019, `Bottom temp 4X`]
middle = bottom_temp[year == 2017, `Bottom temp 4X`] - bottom_temp[year == 2018, `Bottom temp 4X`]
prop = middle/range
range = bottom_temp[year == 2017, `Bottom temp 4V`] - bottom_temp[year == 2019, `Bottom temp 4V`]
sub = range*prop
bottom_temp[year == 2018, 2] = bottom_temp[year == 2017, `Bottom temp 4V`] - sub
range = bottom_temp[year == 2017, `Bottom temp 4W`] - bottom_temp[year == 2019, `Bottom temp 4W`]
sub = range*prop
bottom_temp[year == 2018, 3] = bottom_temp[year == 2017, `Bottom temp 4W`] - sub
rm(range,middle,prop,sub)

# Join bottom temperatures 
dt_assem <- bottom_temp[dt_assem, , on = "year"]

############## 3. NAO ##############

# Load North Atlantic Oscillation (NAO) data
NAO <- fread("../Updated Data/AZMP_CIL_ICE_NAO.dat", skip = "1951")[, c(1, 12)]

# Rename columns
colnames(NAO) <- c("year", "NAO")

# Add NAO data
dt_assem <- dt_assem[NAO, on = "year"]

############## 4. SST ##############

# Load sea-surface temperature data
SST <- fread("../Updated Data/AZMP_SST_Seasonal.dat", skip = "1983")[, c(1, 13:17)]

# Rename columns
colnames(SST) <- c("year", "SST 4Vn", "SST 4Vs", "SST 4W", "SST 4XSS", "SST 4eGoM + BoF")

# Add SST data joining by both year
dt_assem <- SST[dt_assem, , on = "year"]

############## 5. AMO ##############

# Load Atlantic Multidecadal Oscillation (AMO) data
AMO <- fread("../Updated Data/amon.us.data.txt")

# fix column names
colnames(AMO) = c('year', 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)

# Calculate annual means
AMO <- AMO[year < 2020, .(year, AMO = rowMeans(AMO[year < 2020, -1]))]

# Join AMO data
dt_assem <- AMO[dt_assem, , on = "year"]

############## 6. CIL ##############

# Load Cold Intermediate Layer (CIL) data
CIL <- fread("../Updated Data/AZMP_CIL_ICE_NAO.dat", skip = "1972")[, c(1,9)]

# Rename columns
colnames(CIL) <- c("year", "CIL volume SS")

# Rename missing values
CIL[CIL == -99] <- NA

# Add CIL data 
dt_assem <- CIL[dt_assem, , on = "year"]

############## 7. Ice Volume ##############

# Load ice volume data
ice_volume <- fread("../Updated Data/AZMP_CIL_ICE_NAO.dat", skip = "1969")[, c(1,11)]

# Rename columns
colnames(ice_volume) <- c("year", "Ice volume GSL+SS")

# Rename missing values
ice_volume[ice_volume == -99] <- NA 

# Add ice volume data 
dt_assem <- ice_volume[dt_assem, , on = "year"]

############## 8. Stratification ##############

# Load stratification data
strat = fread('../Updated Data/AZMP_FixedStations_Integrated.dat', skip = '1960')[, c(1,9)]

# Set -99s to NAs
strat[strat == -99] <- NA

# Rename columns
colnames(strat) = c('year', 'Stratification')

# Remove NA rows
strat = strat[!(is.na(Stratification)),]

# Convert to anomalies
strat$Stratification = strat$Stratification - mean(strat$Stratification)

# Convert to normalized anomalies
strat$Stratification = (strat$Stratification - mean(strat$Stratification)) / sd(strat$Stratification)

# Add stratification
dt_assem <- strat[dt_assem, , on = "year"]

###################################
# PHENOLOGY DATA
###################################

############## 1. SST Warming ##############

# Load SST warming data
df_loaded <- load("../Updated Data/SST_Warming_20200320.RData")

# Convert all loaded data frames to data tables
lapply(df_loaded, function(x) setDT(get(x)))

# Add SST warming for each region
dt_assem <- df_norm_anomaly_annual_l[dt_assem, , on = c("region", "year")]

# Rename column
setnames(dt_assem, old = "value", new = "SST warming")

# Remove data frames that were loaded - later loads use the same names
rm(list = df_loaded)

############## 2. Phytoplankton Bloom Metrics ##############

# Load bloom data
df_loaded <- load("../Updated Data/Bloom_Metrics_20200320.RData")

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

############## 3. Ice Timing ##############

# Load ice timing data
df_loaded <- load("../Updated Data/Ice_Timing_20200330.RData")

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

############## 4. C. finmarchicus Max Abundance ##############

# Load maximums data
df_loaded = load("../Updated Data/Zooplankton_Peaks_04012020.RData")

# Convert all loaded data frames to data tables
lapply(df_loaded, function(x) setDT(get(x)))

# Reduce to necessary columns
Cfin_max = df_peak_Cfin_total[,2:3]

# Change column name to something more descriptive
colnames(Cfin_max) <- c("year", "Cfin max yearday HL2")

# Turn into standardised anomalies
Cfin_max[, `Cfin max yearday HL2` := as.vector(scale(`Cfin max yearday HL2`))]

# Add year-day of maximum Calinus finmarchicus
dt_assem <- Cfin_max[dt_assem, , on = "year"]

############## 5. C. finmarchicus I+II+III Max Abundance ##############

# Reduce to necessary columns
CfinI_III_max = df_peak_Cfin_ItoIII[,2:3]

# Change column name to something more descriptive
colnames(CfinI_III_max) <- c("year", "Cfin I+II+III max yearday HL2")

# Turn into standardised anomalies
CfinI_III_max[, `Cfin I+II+III max yearday HL2` := as.vector(scale(`Cfin I+II+III max yearday HL2`))]

# Add maximum in early life stages
dt_assem <- CfinI_III_max[dt_assem, , on = "year"]

############## 6. Zooplankton Max Abundance ##############

# Reduce to necessary columns
zoo_max = df_peak_zooplankton[,2:3]

# Change column name to something more descriptive
colnames(zoo_max) <- c("year", "Zooplankton max yearday HL2")

# Convert to standardised anomaly
zoo_max[, `Zooplankton max yearday HL2` := as.vector(scale(`Zooplankton max yearday HL2`))]

# Add zooplankton abundance peak timing
dt_assem <- zoo_max[dt_assem, , on = "year"]

# Remove data frames that were loaded - later loads use the same names
rm(list = df_loaded)

############## 7. Ice Area ##############

# Load ice area data
df_loaded <- load("../Updated Data/Ice_Area_20200331.RData")

# Convert all loaded data frames to data tables
lapply(df_loaded, function(x) setDT(get(x)))

# Retrieve normalized anomalies for GSL+SS combined
dt_ice_area <- df_norm_anomaly_winter_l[region == 'GSL_SS', ]

# Drop region
dt_ice_area = data.table(dt_ice_area[,year], dt_ice_area[,value])

# Adjust column names
colnames(dt_ice_area) = c('year', 'Ice area GSL+SS')

# Add ice area to data table
dt_assem <- dt_ice_area[dt_assem, , on = "year"]


###################################
# EXPORT DATA
###################################

write.csv(dt_assem, file = "../output/HL2_assembled_updated.csv", row.names = FALSE)
