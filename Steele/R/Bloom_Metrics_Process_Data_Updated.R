
# required package
library(tidyr)
library(dplyr)
library(xlsx)

##--------------------------------------------------------------------------------------------
## declare empty data frame
df_data_raw_w <- data.frame(region=character(0), year=numeric(0), start=numeric(0), amplitude=numeric(0), duration=numeric(0), magnitude=numeric(0), label=character(0))

## read seawifs data from csv file
tmp = read.csv('../data/PP/1998-2007-seawifs-spring-bloom.csv'); tmp = tmp[-1,]
names(tmp) <- c("region", "year", "start", "amplitude", "duration", "magnitude")

## add to empty data frame
df_data_raw_w <- rbind(df_data_raw_w,
                       tmp %>% dplyr::mutate(., label="seawifs"))

## read modis data from csv file
tmp = read.csv('../data/PP/2003-2017-modis-spring-bloom.csv'); tmp = tmp[-1,]
names(tmp) <- c("region", "year", "start", "amplitude", "duration", "magnitude")

## add to empty data frame
df_data_raw_w <- rbind(df_data_raw_w,
                       tmp %>% dplyr::mutate(., label="modis"))

## read 2015 data from excel sheet
tmp = read.csv('../data/PP/2012-2019-viirs-spring-bloom.csv'); tmp = tmp[-1,]
names(tmp) <- c("region", "year", "start", "amplitude", "duration", "magnitude")

## add to empty data frame
df_data_raw_w <- rbind(df_data_raw_w,
                       tmp %>% dplyr::mutate(., label="viirs"))
rm(tmp)

##--------------------------------------------------------------------------------------------
## fix up region names
df_data_filtered_w <- df_data_raw_w %>%
  dplyr::mutate(., region=trimws(region)) %>%  # remove trailing spaces
  dplyr::mutate(., region=gsub(" ", "_", region))  # replace white spaces
df_data_filtered_w$region[df_data_filtered_w$region=="Bras_dOr"] <- "Bras_d'Or"
df_data_filtered_w$region[df_data_filtered_w$region=="Northeast_Gulf_of_St._Lawrence"] <- "Northeast_GSL"
df_data_filtered_w$region[df_data_filtered_w$region=="Northwest_Gulf_of_St._Lawrence"] <- "Northwest_GSL"
df_data_filtered_w$region[df_data_filtered_w$region=="St.Anthony_Basin"] <- "St._Anthony_Basin"
df_data_filtered_w <- df_data_filtered_w %>%
  dplyr::mutate(., region=gsub("_", " ", region))

# make year and values numeric
df_data_filtered_w$year = as.numeric(levels(df_data_filtered_w$year))[df_data_filtered_w$year]
df_data_filtered_w$start = as.numeric(levels(df_data_filtered_w$start))[df_data_filtered_w$start]
df_data_filtered_w$duration = as.numeric(levels(df_data_filtered_w$duration))[df_data_filtered_w$duration]
df_data_filtered_w$magnitude = as.numeric(levels(df_data_filtered_w$magnitude))[df_data_filtered_w$magnitude]
df_data_filtered_w$amplitude = as.numeric(levels(df_data_filtered_w$amplitude))[df_data_filtered_w$amplitude]

# substitution for Eastern Scotian Shelf, Western Bank and Central Scotian Shelf - 2008
#df_data_filtered_w <- rbind(df_data_filtered_w,
#                           df_data_filtered_w %>% filter(., region %in% c("Central Scotian Shelf","Western Bank","Eastern Scotian Shelf")
#                                                          & year==2008 & label=="modis") %>%
#                              dplyr::mutate(., label="seawifs"))

# check if below is needed
# # substitution for Flemish Pass - 2010
# i_modis <- df_data_filtered_w$region=="Flemish Pass" & df_data_filtered_w$year==2010 & df_data_filtered_w$label=="modis"
# i_seawifs <- df_data_filtered_w$region=="Flemish Pass" & df_data_filtered_w$year==2010 & df_data_filtered_w$label=="seawifs"
# df_data_filtered_w[i_modis, 3:6] <- df_data_filtered_w[i_seawifs, 3:6]

## filter for sensor
df_data_filtered_w <- df_data_filtered_w %>%
  dplyr::filter(., (year>=1998 & year<=2007 & label=="seawifs") |
                  (year>=2008 & year<=2011 & label=="modis") |
                  (year>=2012 & label=="viirs")) %>%
  dplyr::arrange(., region, year)

## reshape data to long format
df_data_filtered_l <- df_data_filtered_w %>%
  dplyr::select(., region, year, start, amplitude, duration, magnitude) %>%
  tidyr::gather(., variable, value, start, amplitude, duration, magnitude) %>%
  dplyr::filter(., !is.na(value))

##--------------------------------------------------------------------------------------------
# calculate climatology
df_climatology_l <- df_data_filtered_l %>%
  dplyr::group_by(., region, variable) %>%
  dplyr::summarize(., mean=mean(value, na.rm=TRUE)) %>%
  dplyr::ungroup(.)

## reshape to wide format
df_climatology_w <- df_climatology_l %>%
  tidyr::spread(., variable, mean) %>%
  dplyr::arrange(., region)

##--------------------------------------------------------------------------------------------
## calculate anomalies
df_anomaly_annual_l <- dplyr::left_join(df_data_filtered_l,
                                        df_climatology_l,
                                        by=c("region", "variable")) %>%
  dplyr::mutate(., value=(value-mean)) %>%
  dplyr::select(., -mean)

## composite anomalies for GSL, SS, NLS
# GSL
df_anomaly_annual_l <- rbind(df_anomaly_annual_l,
                             df_anomaly_annual_l %>%
                               dplyr::filter(., region %in% c("Northwest GSL", "Northeast GSL", "Magdalen Shallows", "Cabot Strait")) %>%
                               dplyr::group_by(., year, variable) %>%
                               dplyr::summarize(., value=mean(value, na.rm=TRUE)) %>%
                               dplyr::ungroup(.) %>%
                               dplyr::mutate(., region="GSL"))
# SS
df_anomaly_annual_l <- rbind(df_anomaly_annual_l,
                             df_anomaly_annual_l %>%
                               dplyr::filter(., region %in% c("Eastern Scotian Shelf", "Central Scotian Shelf", "Western Scotian Shelf")) %>%
                               dplyr::group_by(., year, variable) %>%
                               dplyr::summarize(., value=mean(value, na.rm=TRUE)) %>%
                               dplyr::ungroup(.) %>%
                               dplyr::mutate(., region="SS"))
# NLS
df_anomaly_annual_l <- rbind(df_anomaly_annual_l,
                             df_anomaly_annual_l %>%
                               dplyr::filter(., region %in% c("St. Anthony Basin", "Northeast Nfld Shelf", "Flemish Pass",
                                                              "Hibernia", "Southeast Shoal", "Green-St. Pierre Bank")) %>%
                               dplyr::group_by(., year, variable) %>%
                               dplyr::summarize(., value=mean(value, na.rm=TRUE)) %>%
                               dplyr::ungroup(.) %>%
                               dplyr::mutate(., region="NLS"))

## reshape to wide format
df_anomaly_annual_w <- df_anomaly_annual_l %>%
  tidyr::spread(., variable, value) %>%
  dplyr::arrange(., region, year)

##--------------------------------------------------------------------------------------------
## calculate annual normalized anomalies
df_norm_anomaly_annual_l <- df_anomaly_annual_l %>%
  dplyr::group_by(., region, variable) %>%
  dplyr::summarize(., mean=mean(value, na.rm=TRUE), sd=sd(value, na.rm=TRUE)) %>%
  dplyr::ungroup(.)
df_norm_anomaly_annual_l <- dplyr::left_join(df_anomaly_annual_l,
                                             df_norm_anomaly_annual_l,
                                             by=c("region", "variable")) %>%
  dplyr::mutate(., value=(value-mean)/sd) %>%
  dplyr::select(., region, variable, year, value)

## reshape to wide format
df_norm_anomaly_annual_w <- df_norm_anomaly_annual_l %>%
  tidyr::spread(., variable, value) %>%
  dplyr::arrange(., region, year)

##--------------------------------------------------------------------------------------------
## print to csv file
# raw data
ofile <- "~/Projects/ICES_6IZPS_PP/outputs/Bloom_Metrics/Bloom_Metrics_Data_Raw.csv"
write.table(df_data_raw_w %>%
              dplyr::arrange(., region, year),
            file=ofile, append=FALSE, quote=TRUE, sep=",",
            eol="\n", na="NA", dec=".", row.names=FALSE, col.names=TRUE)

# filtered data
ofile <- "~/Projects/ICES_6IZPS_PP/outputs/Bloom_Metrics/Bloom_Metrics_Data_Filtered.csv"
write.table(df_data_filtered_w %>%
              dplyr::arrange(., region, year),
            file=ofile, append=FALSE, quote=TRUE, sep=",",
            eol="\n", na="NA", dec=".", row.names=FALSE, col.names=TRUE)

## climatology
ofile <- "~/Projects/ICES_6IZPS_PP/outputs/Bloom_Metrics/Bloom_Metrics_Climatology.csv"
write.table(df_climatology_w, file=ofile, append=FALSE, quote=TRUE, sep=",",
            eol="\n", na="NA", dec=".", row.names=FALSE,
            col.names=TRUE)

## annual anomalies
ofile <- "~/Projects/ICES_6IZPS_PP/outputs/Bloom_Metrics/Bloom_Metrics_Anomalies_Annual.csv"
write.table(df_anomaly_annual_w, file=ofile, append=FALSE, quote=TRUE, sep=",",
            eol="\n", na="NA", dec=".", row.names=FALSE,
            col.names=TRUE)

## annual normalized anomalies
ofile <- "~/Projects/ICES_6IZPS_PP/outputs/Bloom_Metrics/Bloom_Metrics_Normalized_Anomalies_Annual.csv"
write.table(df_norm_anomaly_annual_w, file=ofile, append=FALSE, quote=TRUE, sep=",",
            eol="\n", na="NA", dec=".", row.names=FALSE,
            col.names=TRUE)

##--------------------------------------------------------------------------------------------
## save to RData file
ofile <- "../Updated Data/Bloom_Metrics_20200320.RData"
save(file=ofile, list=c("df_data_raw_w",
                        "df_data_filtered_w", "df_data_filtered_l",
                        "df_climatology_w", "df_climatology_l",
                        "df_anomaly_annual_w", "df_anomaly_annual_l",
                        "df_norm_anomaly_annual_w", "df_norm_anomaly_annual_l"))
