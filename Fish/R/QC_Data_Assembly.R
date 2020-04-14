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
dt_assem <- fread("../output/QC_zooplankton_anomalies.csv")

#Function for adding region names
region_name <- function(trans){
   dplyr::case_when(
     trans %in% c("RIM", "TSI", "TASO", "TESL") ~ "Northwest GSL",
     trans %in% c("TBB", "TCEN") ~ "Northeast GSL",
     trans %in% c("TIDM", "SHED") ~ "Magdalen Shallows",
     trans == "TDC" ~ "Cabot Strait", 
     TRUE ~ NA_character_
     )
}

# Add region name
dt_assem[, region := region_name(transect)]

###################################
# PHYSICAL DATA
###################################
# Load river data
rivers <- fread("../data/StLawrenceAtQuebecCity.dat")

# Rename columns
colnames(rivers) <- c("year", "St. Lawrence river flux")

# Join river data 
dt_assem <- dt_assem[rivers, , on = "year"]

# Load North Atlantic Oscillation (NAO) data
NAO <- fread("../data/AZMP_CIL_ICE_NAO_Redux.dat", skip = "1895")[, c(1, 12)]

# Rename columns
colnames(NAO) <- c("year", "NAO")

# Add NAO data
dt_assem <- dt_assem[NAO, on = "year"]

# Load Atlantic Multidecadal Oscillation (AMO) data
AMO <- fread("../data/amo.csv")

# Calculate annual means
AMO <- AMO[year < 2017, .(year, AMO = rowMeans(.SD)), .SDcols = names(AMO)[-1]]

# Join AMO data
dt_assem <- AMO[dt_assem, , on = "year"]

# Load data
physical <- fread("../data/ACCASP_Phys_GSL_1971-2016.csv")

# Adjust column names
colnames(physical) <- gsub("_T300", " deep temperature", colnames(physical))
colnames(physical) <- gsub("(_SST.*$)", " SST", colnames(physical))

# Subset data
physical[, setdiff(1:ncol(physical), grep("Year|SST|deep", names(physical))) := NULL]

# Join data to main data frame
dt_assem <- physical[dt_assem, , on = c(Year = "year")]
setnames(dt_assem, old = "Year", new = "year")

# Load Cold Intermediate Layer (CIL) data
CIL <- fread("../data/AZMP_CIL_ICE_NAO_Redux.dat", skip = "1985")[, c(1,7)]

# Rename columns
colnames(CIL) <- c("year", "CIL index GSL")

# Rename missing values
CIL[CIL == -99] <- NA

# Add CIL data 
dt_assem <- CIL[dt_assem, , on = "year"]

# Load ice volume data
ice_volume <- fread("../data/AZMP_CIL_ICE_NAO_Redux.dat", skip = "1985")[, c(1,11)]

# Rename columns
colnames(ice_volume) <- c("year", "Ice volume GSL+SS")

# Rename missing values
ice_volume[ice_volume == -99] <- NA 

# Add ice volume data 
dt_assem <- ice_volume[dt_assem, , on = "year"]

# Load SST warming data
df_loaded <- load("../data/SST_Warming_20171005.RData")

# Convert all loaded data frames to data tables
lapply(df_loaded, function(x) setDT(get(x)))

# Filter to relevant regions
df_norm_anomaly_annual_l <- df_norm_anomaly_annual_l[region %in% c("Northeast GSL", 
                                                                   "Northwest GSL", 
                                                                   "Magdalen Shallows", 
                                                                   "Cabot Strait")]

# Add SST warming for each region
dt_assem <- df_norm_anomaly_annual_l[dt_assem, , on = c("region", "year")]

# Rename column
setnames(dt_assem, old = "value", new = "SST warming")

# Remove data frames that were loaded - later loads use the same names
rm(list = df_loaded)

# Load bloom data
bloom <- fread("../data/Bloom Stats GSL compilation 1998-2016.csv")

# Correct data types
cols <- c("Bloom start", "Bloom duration")
bloom[,  (cols) := lapply(.SD, as.numeric), .SDcols = cols]

# Add to data table
dt_assem <- bloom[dt_assem, , on = c(Year = "year", Region = "region")]

# Rename column
setnames(dt_assem, old = c("Year", "Region"), new = c("year", "region"))

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

# Load C. finmarchicus phenology data
Cfin <- fread("../data/ACCASP_Riki_Zoo_pheno_1992-2016.csv")

# Change column name to something more descriptive
colnames(Cfin) <- c("year", 
                    "Cfin I+II+III G1 max yearday Riki", 
                    "Cfin I+II+III G2 max yearday Riki", 
                    "Cfin I+II+III G ratio max yearday Riki")

# Add to data table
dt_assem <- Cfin[dt_assem, , on = "year"]

# Write to file
fwrite(dt_assem, file = "../output/QC_assembled.csv")

