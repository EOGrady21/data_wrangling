
## required package
library(dplyr)
library(tidyr)
library(xlsx)

##--------------------------------------------------------------------------------------------
## declare empty data frame
df_data_raw_w <- data.frame(year=numeric(0), month=numeric(0), area=numeric(0), extent=numeric(0), volume=numeric(0), region=character(0))

## read ice data from excel file
# GSL data
gc()
ifile <- "~/Projects/ICES_6IZPS_PP/data/Ice/area_duration.xls"
tmp <- read.xlsx2(ifile, sheetName="GSL Monthly Means", as.data.frame=TRUE,
                  header=FALSE, startRow=2, stringsAsFactors=FALSE,
                  colIndex=c(1, 2, 4, 5, 6),
                  colClasses=rep("numeric", 5))
names(tmp) <- c("year", "month", "area", "extent", "volume")
df_data_raw_w <- rbind(df_data_raw_w,
                       tmp %>% dplyr::mutate(., region="GSL"))

# Scotian Shelf data
gc()
tmp <- read.xlsx2(ifile, sheetName="SS Monthly Means", as.data.frame=TRUE,
                  header=FALSE, startRow=2, stringsAsFactors=FALSE,
                  colIndex=c(1, 2, 4, 5, 6),
                  colClasses=rep("numeric", 5))
names(tmp) <- c("year", "month", "area", "extent", "volume")
df_data_raw_w <- rbind(df_data_raw_w,
                       tmp %>% dplyr::mutate(., region="SS"))

# Newfoundland & Labrador data
gc()
tmp <- read.xlsx2(ifile, sheetName="NfldLab Monthly Means", as.data.frame=TRUE,
                  header=FALSE, startRow=2, stringsAsFactors=FALSE,
                  colIndex=c(1, 2, 4, 5, 8),
                  colClasses=rep("numeric", 5))
names(tmp) <- c("year", "month", "area", "extent", "volume")
df_data_raw_w <- rbind(df_data_raw_w,
                       tmp %>% dplyr::mutate(., region="NLS"))
rm(tmp)

##--------------------------------------------------------------------------------------------
## filter data
df_data_filtered_w <- df_data_raw_w %>%
  dplyr::filter(., year>=1999 & year<=2015)

## reshape data to long format
df_data_filtered_l <- df_data_filtered_w %>%
  dplyr::select(., region, year, month, area, extent, volume) %>%
  tidyr::gather(., variable, value, area, extent, volume)

##--------------------------------------------------------------------------------------------
## calculate annual winter [J-F-M] means for GSL and SS; [D-J-F-M-A-M-J] for NL
df_mean_winter_l <- rbind(df_data_filtered_l %>%                     # GSL & SS
                            dplyr::filter(., region!="NL") %>%
                            dplyr::filter(., month>=1 & month<=3) %>%
                            dplyr::select(., -month) %>%
                            dplyr::group_by(., region, year, variable) %>%
                            dplyr::summarise(., value=mean(value, na.rm=TRUE)) %>%
                            ungroup(.),
                          df_data_filtered_l %>%                      # NL
                            dplyr::filter(., region=="NL") %>%
                            dplyr::mutate(., year=ifelse(month==12, year+1, year), month=ifelse(month==12, 0, month)) %>%
                            dplyr::filter(., month>=0 & month<=6) %>%
                            dplyr::select(., -month) %>%
                            dplyr::group_by(., region, year, variable) %>%
                            dplyr::summarise(., value=mean(value, na.rm=TRUE)) %>%
                            ungroup(.)) %>%
  dplyr::arrange(., region, variable, year)

## reshape to wide format
df_mean_winter_w <- df_mean_winter_l %>%
  tidyr::spread(., variable, value) %>%
  dplyr::arrange(., region, year)

##--------------------------------------------------------------------------------------------
## calculate climatology - winter means
df_climatology_winter_l <- df_mean_winter_l %>%
  dplyr::group_by(., region, variable) %>%
  dplyr::summarise(., mean=mean(value, na.rm=TRUE)) %>%
  dplyr::ungroup(.)

## reshape to wide format
df_climatology_winter_w <- df_climatology_winter_l %>%
  tidyr::spread(., variable, mean) %>%
  dplyr::arrange(., region)

##--------------------------------------------------------------------------------------------
## calculate winter annual anomalies
df_anomaly_winter_l <- dplyr::left_join(df_mean_winter_l,
                                        df_climatology_winter_l,
                                        by=c("region", "variable")) %>%
  dplyr::mutate(., value=value-mean) %>%
  dplyr::select(., region, year, variable, value)

## reshape to wide format
df_anomaly_winter_w <- df_anomaly_winter_l %>%
  tidyr::spread(., variable, value) %>%
  dplyr::arrange(., region, year)

##--------------------------------------------------------------------------------------------
## calculate winter annual normalized anomalies
df_norm_anomaly_winter_l <- df_anomaly_winter_l %>%
  dplyr::group_by(., region, variable) %>%
  dplyr::summarize(., mean=mean(value, na.rm=TRUE), sd=sd(value, na.rm=TRUE)) %>%
  dplyr::ungroup(.)
df_norm_anomaly_winter_l <- dplyr::left_join(df_anomaly_winter_l,
                                             df_norm_anomaly_winter_l,
                                             by=c("region", "variable")) %>%
  dplyr::mutate(., value=(value-mean)/sd) %>%
  dplyr::select(., region, variable, year, value)

## reshape to wide format
df_norm_anomaly_winter_w <- df_norm_anomaly_winter_l %>%
  tidyr::spread(., variable, value) %>%
  dplyr::arrange(., region, year)

##--------------------------------------------------------------------------------------------
## print to csv file
# raw data
ofile <- "~/Projects/ICES_6IZPS_PP/outputs/Ice_AreaVolume/Ice_AreaVolume_Data_Raw.csv"
write.table(df_data_raw_w %>%
              dplyr::select(., region, year, month, area, extent, volume) %>%
              dplyr::arrange(., region, year, month),
            file=ofile, append=FALSE, quote=TRUE, sep=",",
            eol="\n", na="NA", dec=".", row.names=FALSE, col.names=TRUE)

# filtered data
ofile <- "~/Projects/ICES_6IZPS_PP/outputs/Ice_AreaVolume/Ice_AreaVolume_Data_Filtered.csv"
write.table(df_data_filtered_w %>%
              dplyr::select(., region, year, month, area, extent, volume) %>%
              dplyr::arrange(., region, year, month),
            file=ofile, append=FALSE, quote=TRUE, sep=",",
            eol="\n", na="NA", dec=".", row.names=FALSE, col.names=TRUE)

# winter means
ofile <- "~/Projects/ICES_6IZPS_PP/outputs/Ice_AreaVolume/Ice_AreaVolume_Means_Winter.csv"
write.table(df_mean_winter_w, file=ofile, append=FALSE, quote=TRUE, sep=",",
            eol="\n", na="NA", dec=".", row.names=FALSE,
            col.names=TRUE)

## winter climatology
ofile <- "~/Projects/ICES_6IZPS_PP/outputs/Ice_AreaVolume/Ice_AreaVolume_Climatology_Winter.csv"
write.table(df_climatology_winter_w, file=ofile, append=FALSE, quote=TRUE, sep=",",
            eol="\n", na="NA", dec=".", row.names=FALSE,
            col.names=TRUE)

## winter anomalies
ofile <- "~/Projects/ICES_6IZPS_PP/outputs/Ice_AreaVolume/Ice_AreaVolume_Anomalies_Winter.csv"
write.table(df_anomaly_winter_w, file=ofile, append=FALSE, quote=TRUE, sep=",",
            eol="\n", na="NA", dec=".", row.names=FALSE,
            col.names=TRUE)

## winter normalized anomalies
ofile <- "~/Projects/ICES_6IZPS_PP/outputs/Ice_AreaVolume/Ice_AreaVolume_Normalized_Anomalies_Winter.csv"
write.table(df_norm_anomaly_winter_w, file=ofile, append=FALSE, quote=TRUE, sep=",",
            eol="\n", na="NA", dec=".", row.names=FALSE,
            col.names=TRUE)

##--------------------------------------------------------------------------------------------
## save to RData file
ofile <- "~/Projects/ICES_6IZPS_PP/outputs/Ice_AreaVolume/Ice_AreaVolume.RData"
save(file=ofile, list=c("df_data_raw_w",
                        "df_data_filtered_w", "df_data_filtered_l",
                        "df_mean_winter_w", "df_mean_winter_l",
                        "df_climatology_winter_w", "df_climatology_winter_l",
                        "df_anomaly_winter_w", "df_anomaly_winter_l",
                        "df_norm_anomaly_winter_w", "df_norm_anomaly_winter_l"))
