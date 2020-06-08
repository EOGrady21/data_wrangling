
# required package
library(dplyr)
library(tidyr)

##--------------------------------------------------------------------------------------------
## declare empty data frame
df_data_raw_l <- data.frame()

# read data
input_file <- list.files(path="~/Projects/ICES_6IZPS_PP/data/SST/", pattern="*_sst.stat", full.names=TRUE)

# loop over files
for (i_file in seq(1, length(input_file), by=1)) {
  
  # load modis data
  tmp <- read.table(file=input_file[i_file], header=TRUE, sep="", stringsAsFactors=FALSE)
  # keep only desired variables
  tmp <- tmp %>% dplyr::select(., date.id, mean_sst, region.name)
  # rename variable
  tmp <- tmp %>% dplyr::rename(., value=mean_sst) %>%
    dplyr::rename(., region=region.name) %>%
    dplyr::mutate(., region=gsub("_", " ", region))
  # check for missing chl data
  tmp <- tmp %>% dplyr::mutate(., value=ifelse(value==-999, NA, value))
  
  # append data to dataframe
  df_data_raw_l <- rbind(df_data_raw_l, tmp)
  
  # clean up
  rm(tmp)
}  

##--------------------------------------------------------------------------------------------
# calculate dates
week_str <- c("a","b")
month_dec <- c(0.25,0.75)
df_data_filtered_l <- df_data_raw_l %>%
  tidyr::separate(., date.id, c("year","month","week"), c(4,7), remove=FALSE, convert=TRUE) %>%
  dplyr::mutate(., month=match(month, tolower(month.abb)), week=match(week, week_str)) %>%
  #  dplyr::mutate(., month_decimal=month+month_dec[.$week]) %>%
  dplyr::arrange(., region, year, month, week)

## filter data
df_data_filtered_l <- df_data_filtered_l %>%
  dplyr::filter(., year>=1999 & year<=2015)

##--------------------------------------------------------------------------------------------
## calculate annual means for each region
df_mean_annual_l <- df_data_filtered_l %>%
  dplyr::group_by(., region, year) %>%
  dplyr::summarise(., value=mean(value, na.rm=TRUE)) %>%
  ungroup(.) %>%
  dplyr::arrange(., region, year)

## reshape to wide format
df_mean_annual_w <- df_mean_annual_l %>%
  tidyr::spread(., region, value) %>%
  dplyr::arrange(., year)

## calculate summer means for each region
df_mean_summer_l <- df_data_filtered_l %>%
  dplyr::filter(., month>=6 & month<=8) %>%
  dplyr::group_by(., region, year) %>%
  dplyr::summarise(., value=mean(value, na.rm=TRUE)) %>%
  ungroup(.) %>%
  dplyr::arrange(., region, year)

## reshape to wide format
df_mean_summer_w <- df_mean_summer_l %>%
  tidyr::spread(., region, value) %>%
  dplyr::arrange(., year)

##--------------------------------------------------------------------------------------------
## bimonthly climatology
df_climatology_bimonthly_l <- df_data_filtered_l %>%
  group_by(., region, month, week) %>%
  summarise(., mean=mean(value, na.rm=TRUE), sd=sd(value, na.rm=TRUE)) %>%
  ungroup(.)

##--------------------------------------------------------------------------------------------
## calculate bimonthly de-seasonalized anomalies
df_anomaly_bimonthly_l <- dplyr::left_join(df_data_filtered_l,
                                          df_climatology_bimonthly_l,
                                          by=c("region","month", "week")) %>%
  dplyr::mutate(., value=(value-mean)/sd) %>%   # de-seasonalized anomaly
  dplyr::select(., region, year, month, week, value)

## composite anomalies for GSL, SS, NLS
# GSL
df_anomaly_bimonthly_l <- rbind(df_anomaly_bimonthly_l,
                               df_anomaly_bimonthly_l %>%
                                 dplyr::filter(., region %in% c("Northwest GSL", "Northeast GSL", "Magdalen Shallows", "Cabot Strait")) %>%
                                 dplyr::group_by(., year, month, week) %>%
                                 dplyr::summarize(., value=mean(value, na.rm=TRUE)) %>%
                                 dplyr::ungroup(.) %>%
                                 dplyr::mutate(., region="GSL"))
# SS
df_anomaly_bimonthly_l <- rbind(df_anomaly_bimonthly_l,
                               df_anomaly_bimonthly_l %>%
                                 dplyr::filter(., region %in% c("Eastern Scotian Shelf", "Central Scotian Shelf", "Western Scotian Shelf")) %>%
                                 dplyr::group_by(., year, month, week) %>%
                                 dplyr::summarize(., value=mean(value, na.rm=TRUE)) %>%
                                 dplyr::ungroup(.) %>%
                                 dplyr::mutate(., region="SS"))
# NLS
df_anomaly_bimonthly_l <- rbind(df_anomaly_bimonthly_l,
                               df_anomaly_bimonthly_l %>%
                                 dplyr::filter(., region %in% c("St. Anthony Basin", "Northeast Nfld Shelf", "Flemish Pass",
                                                                "Hibernia", "Southeast Shoal", "Green-St. Pierre Bank")) %>%
                                 dplyr::group_by(., year, month, week) %>%
                                 dplyr::summarize(., value=mean(value, na.rm=TRUE)) %>%
                                 dplyr::ungroup(.) %>%
                                 dplyr::mutate(., region="NLS"))

## reshape to wide format
df_anomaly_bimonthly_w <- df_anomaly_bimonthly_l %>%
  tidyr::spread(., region, value) %>%
  dplyr::arrange(., year)

## summer anomalies
df_anomaly_summer_l <- df_anomaly_bimonthly_l %>%
  dplyr::filter(., month>=6 & month<=8) %>%
  dplyr::group_by(., region, year) %>%
  dplyr::summarise(., value=mean(value, na.rm=TRUE)) %>%
  ungroup(.) %>%
  dplyr::arrange(., region, year)

## reshape to wide format
df_anomaly_summer_w <- df_anomaly_summer_l %>%
  tidyr::spread(., region, value) %>%
  dplyr::arrange(., year)

## annual anomalies
df_anomaly_annual_l <- df_anomaly_bimonthly_l %>%
  dplyr::group_by(., region, year) %>%
  dplyr::summarise(., value=mean(value, na.rm=TRUE)) %>%
  ungroup(.) %>%
  dplyr::arrange(., region, year)

## reshape to wide format
df_anomaly_annual_w <- df_anomaly_annual_l %>%
  tidyr::spread(., region, value) %>%
  dplyr::arrange(., year)

##--------------------------------------------------------------------------------------------
## calculate summer annual normalized anomalies
df_norm_anomaly_summer_l <- df_anomaly_summer_l %>%
  dplyr::group_by(., region) %>%
  dplyr::summarize(., mean=mean(value, na.rm=TRUE), sd=sd(value, na.rm=TRUE)) %>%
  dplyr::ungroup(.)
df_norm_anomaly_summer_l <- dplyr::left_join(df_anomaly_summer_l,
                                             df_norm_anomaly_summer_l,
                                             by=c("region")) %>%
  dplyr::mutate(., value=(value-mean)/sd) %>%
  dplyr::select(., region, year, value)

## reshape to wide format
df_norm_anomaly_summer_w <- df_norm_anomaly_summer_l %>%
  tidyr::spread(., region, value) %>%
  dplyr::arrange(., year)

## calculate annual normalized anomalies
df_norm_anomaly_annual_l <- df_anomaly_annual_l %>%
  dplyr::group_by(., region) %>%
  dplyr::summarize(., mean=mean(value, na.rm=TRUE), sd=sd(value, na.rm=TRUE)) %>%
  dplyr::ungroup(.)
df_norm_anomaly_annual_l <- dplyr::left_join(df_anomaly_annual_l,
                                             df_norm_anomaly_annual_l,
                                             by=c("region")) %>%
  dplyr::mutate(., value=(value-mean)/sd) %>%
  dplyr::select(., region, year, value)

## reshape to wide format
df_norm_anomaly_annual_w <- df_norm_anomaly_annual_l %>%
  tidyr::spread(., region, value) %>%
  dplyr::arrange(., year)


##--------------------------------------------------------------------------------------------
## print to csv file
# raw data
ofile <- "~/Projects/ICES_6IZPS_PP/outputs/SST/SST_Data_Raw.csv"
write.table(df_data_raw_l %>%
              tidyr::spread(., region, value) %>%
              dplyr::arrange(., date.id),
            file=ofile, append=FALSE, quote=TRUE, sep=",",
            eol="\n", na="NA", dec=".", row.names=FALSE, col.names=TRUE)

# filtered data
ofile <- "~/Projects/ICES_6IZPS_PP/outputs/SST/SST_Data_Filtered.csv"
write.table(df_data_filtered_l %>%
              tidyr::spread(., region, value) %>%
              dplyr::arrange(., year, month, week),
            file=ofile, append=FALSE, quote=TRUE, sep=",",
            eol="\n", na="NA", dec=".", row.names=FALSE, col.names=TRUE)

# annual means
ofile <- "~/Projects/ICES_6IZPS_PP/outputs/SST/SST_Means_Annual.csv"
write.table(df_mean_annual_w, file=ofile, append=FALSE, quote=TRUE, sep=",",
            eol="\n", na="NA", dec=".", row.names=FALSE,
            col.names=TRUE)

# summer means
ofile <- "~/Projects/ICES_6IZPS_PP/outputs/SST/SST_Means_Summer.csv"
write.table(df_mean_summer_w, file=ofile, append=FALSE, quote=TRUE, sep=",",
            eol="\n", na="NA", dec=".", row.names=FALSE,
            col.names=TRUE)

## bimonthly climatology
ofile <- "~/Projects/ICES_6IZPS_PP/outputs/SST/SST_Climatology_bimonthly.csv"
cat("Mean", file=ofile, sep="\n")
write.table(df_climatology_bimonthly_l %>%
              dplyr::select(., region, month, week, mean) %>%
              tidyr::spread(., region, mean),
            file=ofile, append=TRUE, quote=TRUE, sep=",",
            eol="\n", na="NA", dec=".", row.names=FALSE, col.names=TRUE)
cat(file=ofile, append=TRUE, sep="\n")
cat("Standard deviation", file=ofile, append=TRUE, sep="\n")
write.table(df_climatology_bimonthly_l %>%
              dplyr::select(., region, month, week, sd) %>%
              tidyr::spread(., region, sd),
            file=ofile, append=TRUE, quote=TRUE, sep=",",
            eol="\n", na="NA", dec=".", row.names=FALSE, col.names=TRUE)

# bi-weekly anomalies
ofile <- "~/Projects/ICES_6IZPS_PP/outputs/SST/SST_Anomalies_bimonthly.csv"
write.table(df_anomaly_bimonthly_w, file=ofile, append=FALSE, quote=TRUE, sep=",",
            eol="\n", na="NA", dec=".", row.names=FALSE,
            col.names=TRUE)

# summer anomalies
ofile <- "~/Projects/ICES_6IZPS_PP/outputs/SST/SST_Anomalies_Summer.csv"
write.table(df_anomaly_summer_w, file=ofile, append=FALSE, quote=TRUE, sep=",",
            eol="\n", na="NA", dec=".", row.names=FALSE,
            col.names=TRUE)

# annual anomalies
ofile <- "~/Projects/ICES_6IZPS_PP/outputs/SST/SST_Anomalies_Annual.csv"
write.table(df_anomaly_annual_w, file=ofile, append=FALSE, quote=TRUE, sep=",",
            eol="\n", na="NA", dec=".", row.names=FALSE,
            col.names=TRUE)

## summer normalized anomalies
ofile <- "~/Projects/ICES_6IZPS_PP/outputs/SST/SST_Normalized_Anomalies_Summer.csv"
write.table(df_norm_anomaly_summer_w, file=ofile, append=FALSE, quote=TRUE, sep=",",
            eol="\n", na="NA", dec=".", row.names=FALSE,
            col.names=TRUE)

## annual normalized anomalies
ofile <- "~/Projects/ICES_6IZPS_PP/outputs/SST/SST_Normalized_Anomalies_Annual.csv"
write.table(df_norm_anomaly_annual_w, file=ofile, append=FALSE, quote=TRUE, sep=",",
            eol="\n", na="NA", dec=".", row.names=FALSE,
            col.names=TRUE)

##--------------------------------------------------------------------------------------------
## save to RData file
ofile <- "~/Projects/ICES_6IZPS_PP/outputs/SST/SST.RData"
save(file=ofile, list=c("df_data_raw_l", "df_data_filtered_l",
                        "df_mean_annual_l", "df_mean_annual_w", "df_mean_summer_l", "df_mean_summer_w",
                        "df_climatology_bimonthly_l",
                        "df_anomaly_bimonthly_l", "df_anomaly_bimonthly_w",
                        "df_anomaly_annual_l", "df_anomaly_annual_w", "df_anomaly_summer_l", "df_anomaly_summer_w",
                        "df_norm_anomaly_annual_l", "df_norm_anomaly_annual_w", "df_norm_anomaly_summer_l", "df_norm_anomaly_summer_w"))
