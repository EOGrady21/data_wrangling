
## required package
library(dplyr)
library(tidyr)

##--------------------------------------------------------------------------------------------
## read stratification index (SI) data from dat file
ifile <- "~/Projects/ICES_6IZPS_PP/data/Physical_Metrics/AZMP_FixedStations_Integrated.dat"
df_data_raw_w <- read.table(ifile, header=FALSE, sep="", stringsAsFactors=FALSE, na.strings="",
                            comment.char="#") %>%
  dplyr::select(., c(1,5,9,17,21))

## reassign column names
names(df_data_raw_w) <- c("year","S27","HL2","SHE","RIM")

##--------------------------------------------------------------------------------------------
## reshape and tidy up the data
df_data_filtered_l <- df_data_raw_w %>%
  tidyr::gather(., "region", "value", 2:5) %>%              # long format
  dplyr::filter(., value> -99) %>%                          # eliminate missing values
  dplyr::filter(., year>=1999 & year<=2015) %>%             # year range
  dplyr::arrange(., region, year)

##--------------------------------------------------------------------------------------------
## calculate climatology
df_climatology_l <- df_data_filtered_l %>%
  dplyr::select(., region, value) %>%
  dplyr::group_by(., region) %>%
  dplyr::summarise(., mean=mean(value, na.rm=TRUE))

##--------------------------------------------------------------------------------------------
## calculate annual anomalies
df_anomaly_annual_l <- dplyr::left_join(df_data_filtered_l,
                                        df_climatology_l,
                                        by=c("region")) %>%
  dplyr::mutate(., value=(value-mean)) %>%
  dplyr::select(., -mean) %>%
  dplyr::arrange(., region, year)

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
  dplyr::select(., region, year, value) %>%
dplyr::arrange(., region, year)

## reshape to wide format
df_norm_anomaly_annual_w <- df_norm_anomaly_annual_l %>%
  tidyr::spread(., region, value) %>%
  dplyr::arrange(., year)

##--------------------------------------------------------------------------------------------
## print to csv file
# raw data
ofile <- "~/Projects/ICES_6IZPS_PP/outputs/Stratification/SI_Data_Raw.csv"
write.table(df_data_raw_w,
            file=ofile, append=FALSE, quote=TRUE, sep=",",
            eol="\n", na="NA", dec=".", row.names=FALSE, col.names=TRUE)

# filtered data
ofile <- "~/Projects/ICES_6IZPS_PP/outputs/Stratification/SI_Data_Filtered.csv"
write.table(df_data_filtered_l %>%
              tidyr::spread(., region, value) %>%
              dplyr::arrange(., year),
            file=ofile, append=FALSE, quote=TRUE, sep=",",
            eol="\n", na="NA", dec=".", row.names=FALSE, col.names=TRUE)

## monthly climatology
ofile <- "~/Projects/ICES_6IZPS_PP/outputs/Stratification/SI_Climatology.csv"
write.table(df_climatology_l, file=ofile, append=FALSE, quote=TRUE, sep=",",
            eol="\n", na="NA", dec=".", row.names=FALSE,
            col.names=TRUE)

## annual anomalies
ofile <- "~/Projects/ICES_6IZPS_PP/outputs/Stratification/SI_Anomalies_Annual.csv"
write.table(df_anomaly_annual_w, file=ofile, append=FALSE, quote=TRUE, sep=",",
            eol="\n", na="NA", dec=".", row.names=FALSE,
            col.names=TRUE)

## annual normalized anomalies
ofile <- "~/Projects/ICES_6IZPS_PP/outputs/Stratification/SI_Normalized_Anomalies_Annual.csv"
write.table(df_norm_anomaly_annual_w, file=ofile, append=FALSE, quote=TRUE, sep=",",
            eol="\n", na="NA", dec=".", row.names=FALSE,
            col.names=TRUE)

##--------------------------------------------------------------------------------------------
## save to RData file
ofile <- "~/Projects/ICES_6IZPS_PP/outputs/Stratification/SI.RData"
save(file=ofile, list=c("df_data_raw_w", "df_data_filtered_l",
                        "df_climatology_l",
                        "df_anomaly_annual_l", "df_anomaly_annual_w",
                        "df_norm_anomaly_annual_l", "df_norm_anomaly_annual_w"))
