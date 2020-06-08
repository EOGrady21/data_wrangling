
## required package
library(dplyr)
library(tidyr)

##--------------------------------------------------------------------------------------------
## read CIL data from dat file
ifile <- "~/Projects/ICES_6IZPS_PP/data/Physical_Metrics/AZMP_CIL_ICE_NAO.dat"
df_data_raw_w <- read.table(ifile, header=FALSE, sep="", stringsAsFactors=FALSE, na.strings="",
                            comment.char="#") %>%
  dplyr::select(., c(1,2,3,4,5,8,9))

## reassign column names
names(df_data_raw_w) <- c("year","Seal Island","White Bay","Bonavista","Flemish Cap","GSL","SS")

##--------------------------------------------------------------------------------------------
## reshape and tidy up the data
df_data_filtered_l <- df_data_raw_w %>%
  tidyr::gather(., "region", "value", 2:7) %>%              # long format
  dplyr::filter(., value> -99) %>%                          # eliminate missing values
  dplyr::filter(., year>=1999 & year<=2015) %>%             # year range
  dplyr::arrange(., year) %>%                               # reorder rows
  dplyr::mutate(., variable=ifelse(region %in% c("Seal Island","White Bay","Bonavista","Flemish Cap"), "CIL area", "CIL volume"))

##--------------------------------------------------------------------------------------------
## calculate climatology
df_climatology_l <- df_data_filtered_l %>%
  dplyr::select(., region, variable, value) %>%
  dplyr::group_by(., region, variable) %>%
  dplyr::summarise(., mean=mean(value, na.rm=TRUE))

## reshape to wide format
df_climatology_w <- df_climatology_l %>%
  tidyr::spread(., variable, mean) %>%
  dplyr::arrange(., region)

##--------------------------------------------------------------------------------------------
## calculate annual anomalies
df_anomaly_annual_l <- dplyr::left_join(df_data_filtered_l,
                                        df_climatology_l,
                                        by=c("region", "variable")) %>%
  dplyr::mutate(., value=(value-mean)) %>%
  dplyr::select(., -mean)

## composite anomalies forNLS
df_anomaly_annual_l <- rbind(df_anomaly_annual_l,
                             df_anomaly_annual_l %>%
                               dplyr::filter(., region %in% c("Seal Island","White Bay","Bonavista","Flemish Cap")) %>%
                               dplyr::group_by(., year, variable) %>%
                               dplyr::summarize(., value=mean(value, na.rm=TRUE)) %>%
                               dplyr::ungroup(.) %>%
                               dplyr::mutate(., region="NLS"))

# arrange
df_anomaly_annual_l <- df_anomaly_annual_l %>%
  dplyr::arrange(., variable, region, year)

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
  dplyr::select(., region, variable, year, value) %>%
  dplyr::arrange(., variable, region, year)

## reshape to wide format
df_norm_anomaly_annual_w <- df_norm_anomaly_annual_l %>%
  tidyr::spread(., variable, value) %>%
  dplyr::arrange(., region, year)

##--------------------------------------------------------------------------------------------
## print to csv file
# raw data
ofile <- "~/Projects/ICES_6IZPS_PP/outputs/CIL/CIL_Data_Raw.csv"
write.table(df_data_raw_w,
            file=ofile, append=FALSE, quote=TRUE, sep=",",
            eol="\n", na="NA", dec=".", row.names=FALSE, col.names=TRUE)

# filtered data
ofile <- "~/Projects/ICES_6IZPS_PP/outputs/CIL/CIL_Data_Filtered.csv"
write.table(df_data_filtered_l %>%
              tidyr::spread(., variable, value) %>%
              dplyr::arrange(., region, year),
            file=ofile, append=FALSE, quote=TRUE, sep=",",
            eol="\n", na="NA", dec=".", row.names=FALSE, col.names=TRUE)

## monthly climatology
ofile <- "~/Projects/ICES_6IZPS_PP/outputs/CIL/CIL_Climatology.csv"
write.table(df_climatology_w, file=ofile, append=FALSE, quote=TRUE, sep=",",
            eol="\n", na="NA", dec=".", row.names=FALSE,
            col.names=TRUE)

## annual anomalies
ofile <- "~/Projects/ICES_6IZPS_PP/outputs/CIL/CIL_Anomalies_Annual.csv"
write.table(df_anomaly_annual_w, file=ofile, append=FALSE, quote=TRUE, sep=",",
            eol="\n", na="NA", dec=".", row.names=FALSE,
            col.names=TRUE)

## annual normalized anomalies
ofile <- "~/Projects/ICES_6IZPS_PP/outputs/CIL/CIL_Normalized_Anomalies_Annual.csv"
write.table(df_norm_anomaly_annual_w, file=ofile, append=FALSE, quote=TRUE, sep=",",
            eol="\n", na="NA", dec=".", row.names=FALSE,
            col.names=TRUE)

##--------------------------------------------------------------------------------------------
## save to RData file
ofile <- "~/Projects/ICES_6IZPS_PP/outputs/CIL/CIL.RData"
save(file=ofile, list=c("df_data_raw_w", "df_data_filtered_l",
                        "df_climatology_l", "df_climatology_w",
                        "df_anomaly_annual_l", "df_anomaly_annual_w",
                        "df_norm_anomaly_annual_l", "df_norm_anomaly_annual_w"))
