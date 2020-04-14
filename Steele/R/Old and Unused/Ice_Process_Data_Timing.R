
## required package
library(dplyr)
library(tidyr)

## source custom functions
source("~/Projects/Utils/R/azmp/Station_Name_Lookup.R")

##--------------------------------------------------------------------------------------------
## read ice data from csv file
ifile <- "~/Projects/ICES_6IZPS_PP/data/Ice/Annual_Climatology.csv"
df_data_raw_w <- read.table(ifile, header=FALSE, sep=",", stringsAsFactors=FALSE, na.strings="")

## reassign column names
names(df_data_raw_w) <- c("year","lat","lon","day_first","day_last","charts","duration")

##--------------------------------------------------------------------------------------------
## filter individual grid points for which duration>0 in at least one year
target_loc <- df_data_raw_w %>%
  dplyr::filter(., year>=1999 & year<=2016) %>%
  dplyr::filter(., lon>=-70 & lon<=-43 & lat>=42 & lat<=52.5) %>%
  tidyr::unite(., loc_id, lat, lon, sep="_", remove=FALSE) %>%
  dplyr::mutate(., ice_flag=duration>0) %>%
  dplyr::group_by(., loc_id) %>%
  dplyr::summarize(., n=sum(ice_flag)) %>%
  dplyr::ungroup(.) %>%
  dplyr::filter(., n>0) %>%
  .$loc_id
df_data_filtered_w <- df_data_raw_w %>%
  dplyr::filter(., year>=1999 & year<=2016) %>%
  tidyr::unite(., loc_id, lat, lon, sep="_", remove=FALSE) %>%
  dplyr::filter(., loc_id %in% target_loc)
rm(target_loc)

## add region name variable
# df_data_filtered_w <- df_data_filtered_w %>%
#   dplyr::mutate(., region=Station_Name_Lookup(lon, lat,
#                                               name_lookup_file="~/Projects/ICES_6IZPS_PP/data/polygons/ICES_6IZPS_PP_Ice_Polygons_names.csv",
#                                               coord_lookup_file="~/Projects/ICES_6IZPS_PP/data/polygons/ICES_6IZPS_PP_Ice_Polygons_coordinates.csv")) %>%
#   dplyr::filter(., region %in% c("GSL", "SS", "NLS"))
df_data_filtered_w <- df_data_filtered_w %>%
  dplyr::mutate(., region=Station_Name_Lookup(lon, lat,
                                           name_lookup_file="~/Projects/ICES_6IZPS_PP/data/polygons/ICES_6IZPS_PP_Ice_Polygons_names_Modified.csv",
                                             coord_lookup_file="~/Projects/ICES_6IZPS_PP/data/polygons/ICES_6IZPS_PP_Ice_Polygons_coordinates_Modified.csv")) %>%
  dplyr::filter(., region %in% c("GSL_SS", "NLS"))

## reshape data to long format
df_data_filtered_l <- df_data_filtered_w %>%
  dplyr::select(., region, year, loc_id, lat, lon, day_first, day_last, duration) %>%
  tidyr::gather(., variable, value, day_first, day_last, duration)

##--------------------------------------------------------------------------------------------
## climatology at each grid point
df_climatology_grid_l <- df_data_filtered_l %>%
  dplyr::filter(., year>=1999 & year<=2015) %>%
  dplyr::group_by(., variable, loc_id) %>%
  dplyr::summarize(., mean=mean(value, na.rm=TRUE)) %>%
  dplyr::ungroup(.)

## mean climatology over region
df_climatology_mean_l <- df_data_filtered_l %>%
  dplyr::filter(., year>=1999 & year<=2015) %>%
  dplyr::group_by(., region, variable) %>%
  dplyr::summarize(., mean=mean(value, na.rm=TRUE)) %>%
  dplyr::ungroup(.)

## reshape to wide format
df_climatology_mean_w <- df_climatology_mean_l %>%
  tidyr::spread(., variable, mean) %>%
  dplyr::arrange(., region)

##--------------------------------------------------------------------------------------------
## anomalies at each grid point
df_anomaly_grid_l <- dplyr::left_join(df_data_filtered_l,
                                         df_climatology_grid_l,
                                         by=c("loc_id", "variable")) %>%
  dplyr::mutate(., value=value-mean) %>%
  dplyr::select(., region, year, lat, lon, variable, value)

## mean annual anomaly within each region
df_anomaly_mean_annual_l <- df_anomaly_grid_l %>%
  dplyr::group_by(., region, variable, year) %>%
  dplyr::summarize(., value=mean(value, na.rm=TRUE)) %>%
  dplyr::ungroup(.)

## reshape to wide format
df_anomaly_mean_annual_w <- df_anomaly_mean_annual_l %>%
  tidyr::spread(., variable, value) %>%
  dplyr::arrange(., region, year)

##--------------------------------------------------------------------------------------------
## normalized mean anomalies
df_norm_anomaly_mean_annual_l <- df_anomaly_mean_annual_l %>%
  dplyr::filter(., year>=1999 & year<=2015) %>%
  dplyr::group_by(., region, variable) %>%
  dplyr::summarize(., mean=mean(value, na.rm=TRUE), sd=sd(value, na.rm=TRUE)) %>%
  dplyr::ungroup(.)
df_norm_anomaly_mean_annual_l <- dplyr::left_join(df_anomaly_mean_annual_l,
                                              df_norm_anomaly_mean_annual_l,
                                              by=c("region", "variable")) %>%
  dplyr::mutate(., value=(value-mean)/sd) %>%
  dplyr::select(., region, variable, year, value)

## reshape to wide format
df_norm_anomaly_mean_annual_w <- df_norm_anomaly_mean_annual_l %>%
  tidyr::spread(., variable, value) %>%
  dplyr::arrange(., region, year)

##--------------------------------------------------------------------------------------------
## data frequency - i.e. number of gridpoints in given polygon at given value of each variable 
df_data_frequency_l <- df_data_filtered_l %>%
  dplyr::filter(., !is.na(value)) %>%
  dplyr::group_by(., region, year, variable, value) %>%
  dplyr::summarize(., n=n()) %>%
  dplyr::ungroup(.)

## data frequency as fraction
df_data_frequency_l <- dplyr::left_join(df_data_frequency_l,
                                      df_data_frequency_l %>%
                                        dplyr::group_by(., region, year, variable) %>%
                                        dplyr::summarize(., n_total=sum(n)),
                                      by=c("region", "year", "variable")) %>%
  dplyr::mutate(., p=n/n_total)

##--------------------------------------------------------------------------------------------
## output to csv files
# raw data
ofile <- "~/Projects/ICES_6IZPS_PP/outputs/Ice_Timing/Ice_Timing_Data_Raw_20171120.csv"
write.table(df_data_raw_w %>%
              dplyr::select(., -charts) %>%
              dplyr::arrange(., year, lat, lon),
            file=ofile, append=FALSE, quote=TRUE, sep=",",
            eol="\n", na="NA", dec=".", row.names=FALSE, col.names=TRUE)

# filtered data
ofile <- "~/Projects/ICES_6IZPS_PP/outputs/Ice_Timing/Ice_Timing_Data_Filtered_20171120.csv"
write.table(df_data_filtered_w %>%
              dplyr::select(., region, year, lat, lon, day_first, day_last, duration) %>%
              dplyr::arrange(., region, year, lat, lon),
            file=ofile, append=FALSE, quote=TRUE, sep=",",
            eol="\n", na="NA", dec=".", row.names=FALSE, col.names=TRUE)

# climatology
ofile <- "~/Projects/ICES_6IZPS_PP/outputs/Ice_Timing/Ice_Timing_Climatology_20171120.csv"
write.table(df_climatology_mean_w,
            file=ofile, append=FALSE, quote=TRUE, sep=",",
            eol="\n", na="NA", dec=".", row.names=FALSE, col.names=TRUE)

# anomalies
ofile <- "~/Projects/ICES_6IZPS_PP/outputs/Ice_Timing/Ice_Timing_Anomalies_Annual_20171120.csv"
write.table(df_anomaly_mean_annual_w,
            file=ofile, append=FALSE, quote=TRUE, sep=",",
            eol="\n", na="NA", dec=".", row.names=FALSE, col.names=TRUE)

# normalized anomalies
ofile <- "~/Projects/ICES_6IZPS_PP/outputs/Ice_Timing/Ice_Timing_Normalized_Anomalies_Annual_20171120.csv"
write.table(df_norm_anomaly_mean_annual_w,
            file=ofile, append=FALSE, quote=TRUE, sep=",",
            eol="\n", na="NA", dec=".", row.names=FALSE, col.names=TRUE)

##--------------------------------------------------------------------------------------------
## save to RData file
ofile <- "~/Projects/ICES_6IZPS_PP/outputs/Ice_Timing/Ice_Timing_20171120.RData"
save(file=ofile, list=c("df_data_raw_w",
                        "df_data_filtered_w", "df_data_filtered_l",
                        "df_climatology_grid_l", "df_climatology_mean_w", "df_climatology_mean_l",
                        "df_anomaly_grid_l", "df_anomaly_mean_annual_w", "df_anomaly_mean_annual_l",
                        "df_norm_anomaly_mean_annual_w","df_norm_anomaly_mean_annual_l",
                        "df_data_frequency_l"))
