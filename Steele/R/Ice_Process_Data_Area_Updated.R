
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

##--------------------------------------------------------------------------------------------
## calculate annual winter [J-F-M] means for GSL and SS; [D-J-F-M-A-M-J] for NL
df_mean_winter_l <- rbind(df_data_filtered_l %>%                     # GSL & SS
                            dplyr::filter(., region!="NL") %>%
                            dplyr::filter(., month>=1 & month<=3) %>%
                            dplyr::select(., -month) %>%
                            dplyr::group_by(., region, year) %>%
                            dplyr::summarise(., SD = sd(ice_area, na.rm=TRUE), 
                                             ice_area=mean(ice_area, na.rm=TRUE), 
                                             n = n()) %>%
                            ungroup(.),
                          df_data_filtered_l %>%                      # NL
                            dplyr::filter(., region=="NL") %>%
                            dplyr::mutate(., year=ifelse(month==12, year+1, year), month=ifelse(month==12, 0, month)) %>%
                            dplyr::filter(., month>=0 & month<=6) %>%
                            dplyr::select(., -month) %>%
                            dplyr::group_by(., region, year) %>%
                            dplyr::summarise(., SD = sd(ice_area, na.rm=TRUE), 
                                             ice_area=mean(ice_area, na.rm=TRUE), 
                                             n = n()) %>%
                            ungroup(.)) %>%
  dplyr::arrange(., region, year)

## rearrange columns
df_mean_winter_l = df_mean_winter_l[,c(1,2,4,3,5)]

##--------------------------------------------------------------------------------------------
## calculate climatology - winter means
df_climatology_winter_l <- df_mean_winter_l %>%
  dplyr::group_by(., region) %>%
  dplyr::summarise(., mean=mean(ice_area, na.rm=TRUE)) %>%
  dplyr::ungroup(.)

##--------------------------------------------------------------------------------------------
## calculate winter annual anomalies
df_anomaly_winter_l <- dplyr::left_join(df_mean_winter_l,
                                        df_climatology_winter_l,
                                        by=c("region")) %>%
  dplyr::mutate(., anomaly=ice_area-mean) %>%
  dplyr::select(., region, year, anomaly)

## reshape to wide format
df_anomaly_winter_w <- df_anomaly_winter_l %>%
  tidyr::pivot_wider(., names_from = region, values_from = anomaly) %>%
  dplyr::arrange(., year, GSL, SS, NLS, LabS, GSL_SS)

##--------------------------------------------------------------------------------------------
## calculate winter annual normalized anomalies
df_norm_anomaly_winter_l <- df_anomaly_winter_l %>%
  dplyr::group_by(., region) %>%
  dplyr::summarize(., mean=mean(anomaly, na.rm=TRUE), sd=sd(anomaly, na.rm=TRUE)) %>%
  dplyr::ungroup(.)
df_norm_anomaly_winter_l <- dplyr::left_join(df_anomaly_winter_l,
                                             df_norm_anomaly_winter_l,
                                             by=c("region")) %>%
  dplyr::mutate(., value=(anomaly-mean)/sd) %>%
  dplyr::select(., region, year, value)

## reshape to wide format
df_norm_anomaly_winter_w <- df_norm_anomaly_winter_l %>%
  tidyr::pivot_wider(., names_from = region, values_from = value) %>%
  dplyr::arrange(., year, GSL, SS, NLS, LabS, GSL_SS)

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
ofile <- "../Updated Data/Ice_Area_20200331.RData"
save(file=ofile, list=c("df_data_raw_w",
                        "df_data_filtered_w", "df_data_filtered_l",
                        "df_mean_winter_l",
                        "df_climatology_winter_l",
                        "df_anomaly_winter_w", "df_anomaly_winter_l",
                        "df_norm_anomaly_winter_w", "df_norm_anomaly_winter_l"))
