########################################################################
# Author: Reid Steele (Reid.Steele@dfo-mpo.gc.ca)                      #
# Purpose: Database and clean raw SST data files for all regions       #
# Date: Wed March 11, 2020                                             #
########################################################################

# Libraries
library(data.table)

# Load in SST data -> This must be edited if adding/removing regions
cs = fread('../Data/SST/Cabot_Strait_sst.stat')
css = fread('../Data/SST/Central_Scotian_Shelf_sst.stat')
ess = fread('../Data/SST/Eastern_Scotian_Shelf_sst.stat')
fp = fread('../Data/SST/Flemish_Pass_sst.stat')
gb = fread('../Data/SST/Georges_Bank_sst.stat')
gspb = fread('../Data/SST/Green-St._Pierre_Bank_sst.stat')
hb = fread('../Data/SST/Hamilton_Bank_sst.stat')
h = fread('../Data/SST/Hybernia_sst.stat')
ls = fread('../Data/SST/Lurcher_Shoal_sst.stat')
ms = fread('../Data/SST/Magdalen_Shallows_sst.stat')
negsl = fread('../Data/SST/Northeast_GSL_sst.stat')
nenfs = fread('../Data/SST/Northeast_Nfld_Shelf_sst.stat')
nwgsl = fread('../Data/SST/Northwest_GSL_sst.stat')
ses = fread('../Data/SST/Southeast_Shoal_sst.stat')
stab = fread('../Data/SST/St.Anthony_Basin_sst.stat')
wb = fread('../Data/SST/Western_Bank_sst.stat')
wss = fread('../Data/SST/Western_Scotian_Shelf_sst.stat')

# Combine regions -> This must be edited if adding/removing regions
sst = rbind(cs,css,ess,fp,gb,gspb,hb,h,ls,ms,negsl,nenfs,nwgsl,ses,stab,wb,wss)

# Set aside raw SST data
sstraw = sst
sstraw = sstraw[,c('date-id', 'mean_sst', 'region')]

# Select used data
sst = sst[,c('date-id', 'date-id', 'mean_sst', 'region')]

# Set column names
colnames(sst) = c('year', 'month', 'mean_sst', 'region')

# Swap to data frame temporarily
sst = as.data.frame(sst)

# Differentiate month and year
months = c('jana', 'janb', 'feba', 'febb', 'mara', 'marb', 'apra', 'aprb', 'maya', 'mayb', 'juna', 'junb' , 'jula', 'julb', 'auga', 'augb', 'sepa', 'sepb', 'octa', 'octb', 'nova', 'novb', 'deca', 'decb')

for(i in months){
  sst[,1] = gsub(i, '', sst[,1])
}

for(i in as.character(seq(1997, 2020, 1))){
  sst[,2] = gsub(i, '', sst[,2])
}

# Swap back to data table
sst = data.table(sst)

# Cleaning
hist(sst$mean_sst)
sst = sst[mean_sst > -900,]
hist(sst$mean_sst)

############################################################
###################### 1. SST Warming ######################
############################################################

# Separate into March, April, and May
sstw = sst[month == 'mara' | month == 'marb' | month == 'apra' | month == 'aprb' | month == 'maya' | month == 'mayb',]

# Regress
warming = NULL
for(i in unique(sstw$year)){ # Loop through years
  for(j in unique(sstw$region)){ # Loop through regions
      filt = sstw[year == i & region == j,] # Filter to each year and region combination
      col1 = j # first column -> region
      col2 = i # second column -> year
      col3 = nrow(filt) # third column -> n
      decmonth = seq(0,2.5,0.5) # decimal month values, x in regression
    chkmonth = c('mara','marb','apra','aprb','maya','mayb') # month names
    for(k in 1:6){ # loop through filt
      if(!(chkmonth[k] %in% filt$month)){decmonth = decmonth[-k]} # check for missing data, remove appropriate decmonth value if data is missing
    }
      regr = lm(mean_sst ~ decmonth, data = filt) # regress SST vs Decimal Month
      col4 = regr$coefficients[2] # fourth column -> regression slope
      vec = data.table(col1,col2,col3,col4) # assemble columns for each row
      warming = rbind(warming, vec) # combine rows
  }
}

# Rename rows and columns
rownames(warming) = NULL
colnames(warming) = c('region', 'year', 'n', 'value')



######## Climatology ########

# Load in SST climatology data -> This must be edited if adding/removing regions
cs.c = fread('../Data/SST/Cabot_Strait_noaa-sst-clim.stat')
css.c = fread('../Data/SST/Central_Scotian_Shelf_noaa-sst-clim.stat')
ess.c = fread('../Data/SST/Eastern_Scotian_Shelf_noaa-sst-clim.stat')
fp.c = fread('../Data/SST/Flemish_Pass_noaa-sst-clim.stat')
gb.c = fread('../Data/SST/Georges_Bank_noaa-sst-clim.stat')
gspb.c = fread('../Data/SST/Green-St._Pierre_Bank_noaa-sst-clim.stat')
hb.c = fread('../Data/SST/Hamilton_Bank_noaa-sst-clim.stat')
h.c = fread('../Data/SST/Hybernia_noaa-sst-clim.stat')
ls.c = fread('../Data/SST/Lurcher_Shoal_noaa-sst-clim.stat')
ms.c = fread('../Data/SST/Magdalen_Shallows_noaa-sst-clim.stat')
negsl.c = fread('../Data/SST/Northeast_GSL_noaa-sst-clim.stat')
nenfs.c = fread('../Data/SST/Northeast_Nfld_Shelf_noaa-sst-clim.stat')
nwgsl.c = fread('../Data/SST/Northwest_GSL_noaa-sst-clim.stat')
ses.c = fread('../Data/SST/Southeast_Shoal_noaa-sst-clim.stat')
stab.c = fread('../Data/SST/St.Anthony_Basin_noaa-sst-clim.stat')
wb.c = fread('../Data/SST/Western_Bank_noaa-sst-clim.stat')
wss.c = fread('../Data/SST/Western_Scotian_Shelf_noaa-sst-clim.stat')

# Combine regions -> This must be edited if adding/removing regions
clim = rbind(cs.c,css.c,ess.c,fp.c,gb.c,gspb.c,hb.c,h.c,ls.c,ms.c,negsl.c,nenfs.c,nwgsl.c,ses.c,stab.c,wb.c,wss.c)

# Select used data
clim = clim[,c('date-id', 'mean_sst', 'region')]

# Set column names
colnames(clim) = c('month', 'mean_sst', 'region')

# Separate into March, April, and May
climw = clim[month == 'mara' | month == 'marb' | month == 'apra' | month == 'aprb' | month == 'maya' | month == 'mayb',]

# Regress
warming.clim = NULL
for(j in unique(climw$region)){ # Loop through regions
  filt = climw[region == j,] # Filter to each year and region combination
  col1 = j # first column -> region
  decmonth = seq(0,2.5,0.5) # decimal month values, x in regression
  regr = lm(mean_sst ~ decmonth, data = filt) # regress SST vs Decimal Month
  col2 = regr$coefficients[2] # fourth column -> regression slope
  vec = data.table(col1,col2) # assemble columns for each row
  warming.clim = rbind(warming.clim, vec) # combine rows
}

# Rename rows and columns
rownames(warming.clim) = NULL
colnames(warming.clim) = c('region', 'mean')



######## Anomaly ########

# Calculate Anomaly
anomaly = NULL
for(j in unique(warming$region)){ # Loop through rows
  filt = warming[region == j,] # Filter by region
  for(i in 1:nrow(filt)){ # Loop through regions
    filt[i,4] = filt[i,4] - warming.clim[region == j, 2] # Calculate anomaly -> row - climatological mean
  }
  anomaly = rbind(anomaly, filt) # compile calculated anomalies
}

# Remove n to make anomaly uniform to previous data
anomaly = anomaly[,-3]



######## Normalized Anomaly ########

# Calculate regional means and standard deviations
norm_anomaly = anomaly
mean_anomaly = NULL; sd_anomaly = NULL; regions = NULL
for(j in unique(anomaly$region)){ # Loop through regions
  filt = anomaly[region == j,] # Filter by region
  regions = c(regions, j) # Vectorize regions
  mean_anomaly = c(mean_anomaly, mean(filt$value)) # Calculate regional means
  sd_anomaly = c(sd_anomaly, sd(filt$value))   # Calculate regional standard deviations
}

# Calculate normalized anomalies -> (anomaly - mean) / standard deviation
for(i in 1:length(regions)){ # Loop through regions
  norm_anomaly[region == regions[i],]$value = 
    (norm_anomaly[region == regions[i],]$value - mean_anomaly[i])/sd_anomaly[i] # Calculate normalized anomaly
}


######## Create _w files and liken to old data ########

# change sstraw colnames to match original data
colnames(sstraw) = c('date.id', 'value', 'region')

# gsub '_' for ' ' in regions as in original data
sstraw$region = gsub('_', ' ', sstraw$region)
warming$region = gsub('_', ' ', warming$region)
warming.clim$region = gsub('_', ' ', warming.clim$region)
anomaly$region = gsub('_', ' ', anomaly$region)
norm_anomaly$region = gsub('_', ' ', norm_anomaly$region)
regions = gsub('_', ' ', regions)

# Create data_filtered_w file from warming
warming_w = as.numeric(unique(warming$year)) # Year column
for(j in unique(regions)){ # Loop through regions
  col = warming[region == j, value] # Filter data by region
  warming_w = data.table(warming_w, col) # Make each region its own column
}
colnames(warming_w) = c('year', regions) # Set column names to year and region

# Create anomaly_w file from anomaly
anomaly_w = as.numeric(unique(anomaly$year)) # Year column
for(j in unique(regions)){ # Loop through regions
  col = anomaly[region == j, value] # Filter data by region
  anomaly_w = data.table(anomaly_w, col) # Make each region its own column
}
colnames(anomaly_w) = c('year', regions) # Set column names to year and region

# Create norm_anomaly_w file from norm_anomaly
norm_anomaly_w = as.numeric(unique(norm_anomaly$year)) # Year column
for(j in unique(regions)){ # Loop through regions
  col = norm_anomaly[region == j, value] # Filter data by region
  norm_anomaly_w = data.table(norm_anomaly_w, col) # Make each region its own column
}
colnames(norm_anomaly_w) = c('year', regions) # Set column names to year and region

# Set year to integer
warming$year = as.integer(warming$year)
warming_w$year = as.integer(warming_w$year)
anomaly$year = as.integer(anomaly$year)
anomaly_w$year = as.integer(anomaly_w$year)
norm_anomaly$year = as.integer(norm_anomaly$year)
norm_anomaly_w$year = as.integer(norm_anomaly_w$year)

# Create final files for export
 # look into if extra year (1998) matters
df_data_raw_l = as.data.frame(sstraw)
df_climatology_l = as_tibble(warming.clim)
df_data_filtered_l = as_tibble(warming)
df_data_filtered_w = as_tibble(warming_w)
df_anomaly_annual_l = as_tibble(anomaly)
df_anomaly_annual_w = as_tibble(anomaly_w)
df_norm_anomaly_annual_l = as_tibble(norm_anomaly)
df_norm_anomaly_annual_w = as_tibble(norm_anomaly_w)

# Export
save(df_data_raw_l, df_climatology_l, df_data_filtered_l, df_data_filtered_w, 
                    df_anomaly_annual_l, df_anomaly_annual_w, df_norm_anomaly_annual_l, df_norm_anomaly_annual_w,
                    file = '../Updated Data/SST_Warming_20190318.RData')

# Clean up workspace
rm(list=ls())
