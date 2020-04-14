####################################################################
# Author: Dan Reed (Daniel.Reed@dfo-mpo.gc.ca)
# Purpose: Assemble Newfoundland data set
# Date: Tue Aug 06 11:01:25 2019
####################################################################

# Load libraries
library(data.table)

###################################
# ZOOPLANKTON DATA
###################################

# Load zooplankton data and create new data frame for assembled data 
dt_assem <- fread("../output/NL_zooplankton.csv") 

# Transects to use
transects <- c("BB", "FC", "SI", "SEGB")

# Filter to include only relevant transects
dt_assem <- dt_assem[transect %in% transects]

###################################
# PHYSICAL DATA
###################################

# Load North Atlantic Oscillation (NAO) data
physical <- fread("../data/AZMP_CIL_ICE_NAO_Redux.dat", skip = "1895")[, c(1,12,2:5)]

# Rename columns
colnames(physical) <- c("year", "NAO", "CIL volume Seal Island", 
                        "CIL volume White Bay", "CIL volume Bonavista", "CIL volumeFlemish Cap")

# Replace -99s with NAs
physical[physical == -99] <- NA

# Add physical data to master data table 
dt_assem <- dt_assem[physical, , on = "year"]

# Load Atlantic Multidecadal Oscillation (AMO) data
AMO <- fread("../data/amo.csv")

# Calculate annual means
AMO <- AMO[year < 2017, .(year, AMO = rowMeans(.SD)), .SDcols = names(AMO)[-1]]

# Join AMO data
dt_assem <- AMO[dt_assem, , on = "year"]

# Load sea-surface temperature data
SST <- fread("../data/AZMP_SST.dat", skip = "1985")[, c(1, 2:7)]

# Rename columns
colnames(SST) <- c("year", "SST 2J", "SST 3K", "SST 3L", "SST 3N", "SST 3O", "SST 3P")

# Add SST data joining by both year
dt_assem <- SST[dt_assem, , on = "year"]

# Load deep temperature data
bottom_temp <- fread("../data/AZMP_BottomTemp.dat", skip = "1964")[, c(1, 2:6)]

# Rename columns
colnames(bottom_temp) <- c("year", "Deep temp 3Ps", "Deep temp 3LNO spring", 
                           "Deep temp 2J", "Deep temp 3K", "Deep temp 3LNO fall")

# Add to data table
dt_assem <- bottom_temp[dt_assem, , on = "year"]

# Load ice volume data
ice_volume <- fread("../data/AZMP_CIL_ICE_NAO_Redux.dat", skip = "1985")[, c(1,10)]

# Rename columns
colnames(ice_volume) <- c("year", "Ice volume NL")

# Rename missing values
ice_volume[ice_volume == -99] <- NA 

# Add ice volume data 
dt_assem <- ice_volume[dt_assem, , on = "year"]

###################################
# PHENOLOGY DATA
###################################

# Load SST warming data
df_loaded <- load("../data/SST_Warming_20171005.RData")

# Convert all loaded data frames to data tables
lapply(df_loaded, function(x) setDT(get(x)))

df_anomaly_annual_w <- df_anomaly_annual_w[, .(year, `St.Anthony Basin`, `Northeast Nfld Shelf`, 
                                               `Hybernia`, `Flemish Pass`, `Southeast Shoal`)]

# Rename columns 
colnames(df_anomaly_annual_w)[-1] <- paste("SST warming", colnames(df_anomaly_annual_w)[-1], sep = " ")

# Add to data frame
dt_assem <- df_anomaly_annual_w[dt_assem, on = "year"]

# Remove data frames that were loaded - later loads use the same names
rm(list = df_loaded)

# Load bloom data
df_loaded <- load("../data/Bloom_Metrics_20171005.RData")

#Convert all loaded data frames to data tables
lapply(df_loaded, function(x) setDT(get(x)))

# Regions to include
regions <- c("St. Anthony Basin", "Northeast Nfld Shelf", "Hibernia", "Flemish Pass", "Southeast Shoal")

# Filter to relevant variables and regions
dt_bloom <- df_anomaly_annual_l[region %in% regions & variable %in% c("start", "duration")]

# Update variable naems
dt_bloom[variable == "start", variable := "Bloom start"]
dt_bloom[variable == "duration", variable := "Bloom duration"]

# Combine region name and variable name into new variable name
dt_bloom <- dt_bloom[, .(year, value, variable = paste(variable, region))]

# From long to wide
dt_bloom <- dcast(dt_bloom, year ~ ...)

# Add to data table
dt_assem <- dt_bloom[dt_assem, , on = "year"]

# Remove data frames that were loaded - later loads use the same names
rm(list = df_loaded)

# Load ice timing data
df_loaded <- load("../data/Ice_Timing_20171120.RData")

# Convert all loaded data frames to data tables
lapply(df_loaded, function(x) setDT(get(x)))

# Filter to final day of GSL + SS ice
last_day <- df_norm_anomaly_mean_annual_l[region == "NLS" & variable == "day_last", .(year, value)]

# Change name
setnames(last_day, old = "value", new = "Ice end day NL")

# Add last day of ice
dt_assem <- last_day[dt_assem, , on = "year"]

# Remove data frames that were loaded - later loads use the same names
rm(list = df_loaded)

# Load ice timing data
df_loaded <- load("../data/Ice_AreaVolume_20171010.RData")

# Convert all loaded data frames to data tables
lapply(df_loaded, function(x) setDT(get(x)))

# Filter to relevant region and select columns
dt_ice_area <- df_norm_anomaly_winter_w[region == "NLS", .(year, `Ice area NL` = area)]

# Add ice area to data table
dt_assem <- dt_ice_area[dt_assem, , on = "year"]

# Remove data frames that were loaded - later loads use the same names
rm(list = df_loaded)

# Load C. finmarchicus maximum data
Cfin_max <- fread("../data/Cfin_total_peaks_S27.csv")

# Change column name to something more descriptive
colnames(Cfin_max) <- c("year", "Cfin max yearday S27")

# Add to data table
dt_assem <- Cfin_max[dt_assem, , on = "year"]

# Load C. finmarchicus I+II+III maximum data
CfinI_III_max <- fread("../data/Cfin_I_III_peaks_S27.csv")

# Change column name to something more descriptive
colnames(CfinI_III_max) <- c("year", "Cfin I+II+III max yearday S27")

# Add to data table
dt_assem <- CfinI_III_max[dt_assem, , on = "year"]

# Load C. hyperboreus maximum data
Chyp_max <- fread("../data/Chyp_total_peaks_S27.csv")

# Change column name to something more descriptive
colnames(Chyp_max) <- c("year", "Chyp max yearday S27")

# Add to data table
dt_assem <- Chyp_max[dt_assem, , on = "year"]

# Load C. hyperboreus I+II+III maximum data
ChypI_III_max <- fread("../data/Chyp_I_III_peaks_S27.csv")

# Change column name to something more descriptive
colnames(ChypI_III_max) <- c("year", "Chyp I+II+III max yearday S27")

# Add to data table
dt_assem <- ChypI_III_max[dt_assem, , on = "year"]

# Load zooplankton biomass maximum data
biomass_max <- fread("../data/Z_biomass_peaks_S27.csv")

# Change column name to something more descriptive
colnames(biomass_max) <- c("year", "Biomass max yearday S27")

# Add to data table
dt_assem <- biomass_max[dt_assem, , on = "year"]

# Weed out rows were *all* values are NA (besides the year)
dt_assem <- dt_assem[apply(dt_assem[, -"year"], MAR = 1, function(x) !all(is.na(x))),]

write.csv(dt_assem, file = "../output/NL_assembled.csv", row.names = FALSE)
