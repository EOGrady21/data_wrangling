
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
  dplyr::mutate(., month_decimal=month+month_dec[.$week]) %>%
  dplyr::arrange(., region, year, month, week)

## filter data
df_data_filtered_l <- df_data_filtered_l %>%
  dplyr::filter(., year>=1999 & year<=2015)

## calculate rate of warming (degC/month)
df_data_filtered_l <- df_data_filtered_l %>%
  dplyr::filter(., month %in% c(3,4,5)) %>%
  dplyr::filter(., !is.na(value)) %>%
  dplyr::select(., region, year, month_decimal, value) %>%
  dplyr::group_by(., region, year) %>%
  dplyr::summarize(., n=n(), value=(n*sum(month_decimal*value, na.rm=TRUE)-sum(month_decimal, na.rm=TRUE)*sum(value, na.rm=TRUE))/(n*sum(month_decimal^2, na.rm=TRUE)-sum(month_decimal, na.rm=TRUE)^2)) %>%
  dplyr::ungroup(.)

## reshape to wide format
df_data_filtered_w <- df_data_filtered_l %>%
  dplyr::select(., region, year, value) %>%
  tidyr::spread(., region, value) %>%
  dplyr::arrange(., year)

##--------------------------------------------------------------------------------------------
## climatology
df_climatology_l <- df_data_filtered_l %>%
  group_by(., region) %>%
  summarise(., mean=mean(value, na.rm=TRUE)) %>%
  ungroup(.)

##--------------------------------------------------------------------------------------------
## calculate annual anomalies
df_anomaly_annual_l <- dplyr::left_join(df_data_filtered_l,
                                        df_climatology_l,
                                        by=c("region")) %>%
  dplyr::mutate(., value=value-mean) %>%
  dplyr::select(., region, year, value)

## composite anomalies for GSL, SS, NLS
# GSL
df_anomaly_annual_l <- rbind(df_anomaly_annual_l,
                             df_anomaly_annual_l %>%
                               dplyr::filter(., region %in% c("Northwest GSL", "Northeast GSL", "Magdalen Shallows", "Cabot Strait")) %>%
                               dplyr::group_by(., year) %>%
                               dplyr::summarize(., value=mean(value, na.rm=TRUE)) %>%
                               dplyr::ungroup(.) %>%
                               dplyr::mutate(., region="GSL"))
# SS
df_anomaly_annual_l <- rbind(df_anomaly_annual_l,
                             df_anomaly_annual_l %>%
                               dplyr::filter(., region %in% c("Eastern Scotian Shelf", "Central Scotian Shelf", "Western Scotian Shelf")) %>%
                               dplyr::group_by(., year) %>%
                               dplyr::summarize(., value=mean(value, na.rm=TRUE)) %>%
                               dplyr::ungroup(.) %>%
                               dplyr::mutate(., region="SS"))
# NLS
df_anomaly_annual_l <- rbind(df_anomaly_annual_l,
                             df_anomaly_annual_l %>%
                               dplyr::filter(., region %in% c("St. Anthony Basin", "Northeast Nfld Shelf", "Flemish Pass",
                                                              "Hibernia", "Southeast Shoal", "Green-St. Pierre Bank")) %>%
                               dplyr::group_by(., year) %>%
                               dplyr::summarize(., value=mean(value, na.rm=TRUE)) %>%
                               dplyr::ungroup(.) %>%
                               dplyr::mutate(., region="NLS"))

## reshape to wide format
df_anomaly_annual_w <- df_anomaly_annual_l %>%
  tidyr::spread(., region, value) %>%
  dplyr::arrange(., year)

##--------------------------------------------------------------------------------------------
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
ofile <- "~/Projects/ICES_6IZPS_PP/outputs/SST_Warming/SST_Warming_Data_Raw.csv"
write.table(df_data_raw_l %>%
              tidyr::spread(., region, value) %>%
              dplyr::arrange(., date.id),
            file=ofile, append=FALSE, quote=TRUE, sep=",",
            eol="\n", na="NA", dec=".", row.names=FALSE, col.names=TRUE)

# filtered data
ofile <- "~/Projects/ICES_6IZPS_PP/outputs/SST_Warming/SST_Warming_Data_Filtered.csv"
write.table(df_data_filtered_w,
            file=ofile, append=FALSE, quote=TRUE, sep=",",
            eol="\n", na="NA", dec=".", row.names=FALSE, col.names=TRUE)

## climatology
ofile <- "~/Projects/ICES_6IZPS_PP/outputs/SST_Warming/SST_Warming_Climatology.csv"
write.table(df_climatology_l,
            file=ofile, append=FALSE, quote=TRUE, sep=",",
            eol="\n", na="NA", dec=".", row.names=FALSE, col.names=TRUE)

# annual anomalies
ofile <- "~/Projects/ICES_6IZPS_PP/outputs/SST_Warming/SST_Warming_Anomalies_Annual.csv"
write.table(df_anomaly_annual_w, file=ofile, append=FALSE, quote=TRUE, sep=",",
            eol="\n", na="NA", dec=".", row.names=FALSE,
            col.names=TRUE)

## annual normalized anomalies
ofile <- "~/Projects/ICES_6IZPS_PP/outputs/SST_Warming/SST_Warming_Normalized_Anomalies_Annual.csv"
write.table(df_norm_anomaly_annual_w, file=ofile, append=FALSE, quote=TRUE, sep=",",
            eol="\n", na="NA", dec=".", row.names=FALSE,
            col.names=TRUE)

##--------------------------------------------------------------------------------------------
## save to RData file
ofile <- "~/Projects/ICES_6IZPS_PP/outputs/SST_Warming/SST_Warming.RData"
save(file=ofile, list=c("df_data_raw_l", "df_data_filtered_l", "df_data_filtered_w",
                        "df_climatology_l",
                        "df_anomaly_annual_l", "df_anomaly_annual_w",
                        "df_norm_anomaly_annual_l", "df_norm_anomaly_annual_w"))
