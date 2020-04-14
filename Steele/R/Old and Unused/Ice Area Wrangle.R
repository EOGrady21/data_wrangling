
## required packages
library(dplyr)
library(tidyr)
library(data.table)

##--------------------------------------------------------------------------------------------
## organize raw data

## load in data
df_data_raw_w = fread('../Updated Data/IceAreaRegions.GEC.dat', fill = TRUE)

## remove extra rows
df_data_raw_w = df_data_raw_w[,1:6]

## add column names
colnames(df_data_raw_w) = c('decimal year', 'date', 'GSL', 'SS', 'LabS', 'NLS')

## replace -99s with NAs
df_data_raw_w[df_data_raw_w == -99] <- NA

##---------------------------------------------------------------------------------------------
## database to years and months
df_data_filtered_w = df_data_raw_w

## remove decimal year, convert to data frame
df_data_filtered_w = data.frame(df_data_filtered_w[,-1])

## isolate month and day
df_data_filtered_w = tidyr::separate(df_data_filtered_w, 'date', c('year', 'month', 'day'), '-', convert = TRUE)

## add GSL + SS column
df_data_filtered_w = data.frame(df_data_filtered_w, df_data_filtered_w$GSL + df_data_filtered_w$SS)

## fix column names
colnames(df_data_filtered_w) = c(colnames(df_data_filtered_w)[1:7], 'GSL_SS')

##---------------------------------------------------------------------------------------------
## reshape data to long format
df_data_filtered_l <- df_data_filtered_w %>%
  dplyr::select(., year, month, day, GSL, SS, LabS, NLS, GSL_SS) %>%
  tidyr::pivot_longer(., c(GSL, SS, LabS, NLS, GSL_SS), names_to = 'region', values_to = 'ice_area') %>%
  dplyr::filter(., !is.na('ice_area'))

##---------------------------------------------------------------------------------------------
## calculate mean monthly ice data
df_data_mean_monthly_l = NULL

for(k in unique(df_data_filtered_l$region)){ # loop over regions
  reg.filt = dplyr::filter(df_data_filtered_l, region == k) # filter by region

  for(i in unique(reg.filt$year)){ # loop over years
    year.filt = dplyr::filter(reg.filt, year == i) # filter by year
    
    for(j in 1:12){                       # loop over months
      month.filt = dplyr::filter(year.filt, month == j) # filter by month
      
      mean.filt = mean(na.omit(month.filt$ice_area)) # calculate monthly mean by region and year
      sd.filt = sd(na.omit(month.filt$ice_area)) # calculate monthly sd by region and year
      n = length(na.omit(month.filt$ice_area))
      
      add = data.frame(i, j, k, mean.filt, sd.filt, n) # generate row
      colnames(add) = c('year', 'month', 'region', 'mean_ice_area', 'sd_ice_area', 'n') # rename row columns
      df_data_mean_monthly_l = rbind(df_data_mean_monthly_l, add) # add row
      
    }
    
  }
  
}

## clean up workspace
rm(reg.filt, year.filt, month.filt, mean.filt, sd.filt, add, n)

##---------------------------------------------------------------------------------------------
## Winter only
df_data_winter_w = dplyr::filter(df_data_filtered_w, month < 4)

##---------------------------------------------------------------------------------------------
## reshape data to long format
df_data_winter_l <- df_data_winter_w %>%
  dplyr::select(., year, month, day, GSL, SS, LabS, NLS, GSL_SS) %>%
  tidyr::pivot_longer(., c(GSL, SS, LabS, NLS, GSL_SS), names_to = 'region', values_to = 'ice_area') %>%
  dplyr::filter(., !is.na('ice_area'))

##---------------------------------------------------------------------------------------------
## calculate mean winter ice data
df_data_mean_winter_l = NULL

for(k in unique(df_data_winter_l$region)){ # loop over regions
  reg.filt = dplyr::filter(df_data_winter_l, region == k) # filter by region
  
  for(i in unique(reg.filt$year)){ # loop over years
    year.filt = dplyr::filter(reg.filt, year == i) # filter by year
      
      mean.filt = mean(na.omit(year.filt$ice_area)) # calculate monthly mean by region and year
      sd.filt = sd(na.omit(year.filt$ice_area)) # calculate monthly sd by region and year
      n = length(na.omit(year.filt$ice_area))
      
      add = data.frame(i, k, mean.filt, sd.filt, n) # generate row
      colnames(add) = c('year', 'region', 'mean_ice_area', 'sd_ice_area', 'n') # rename row columns
      df_data_mean_winter_l = rbind(df_data_mean_winter_l, add) # add row
      
  }
    
}
  

## clean up workspace
rm(reg.filt, year.filt, mean.filt, sd.filt, add, n)


##--------------------------------------------------------------------------------------------
## save to RData file
ofile <- "../Updated Data/Ice_Area_20200331.RData"
save(file=ofile, list=c("df_data_raw_w",
                        "df_data_filtered_w", "df_data_filtered_l",
                        "df_data_winter_w", "df_data_winter_l",
                        "df_data_mean_monthly_l", "df_data_mean_winter_l"))