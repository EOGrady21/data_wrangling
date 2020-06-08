
## required package
library(dplyr)
library(tidyr)

##--------------------------------------------------------------------------------------------
## read monthly NAO data from dat file
ifile <- "~/Projects/ICES_6IZPS_PP/data/NAO/NAO_Monthly.dat"
df_data_raw_w <- read.table(ifile, header=FALSE, sep="", stringsAsFactors=FALSE, na.strings="")

## reassign column names
names(df_data_raw_w) <- c("year","Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")

##--------------------------------------------------------------------------------------------
## reshape and tidy up the data
df_data_filtered_l <- df_data_raw_w %>%
  tidyr::gather(., "month", "value", 2:13) %>%              # long format
  dplyr::filter(., value> -99) %>%                          # eliminate missing values
  dplyr::mutate(., month=match(month, month.abb)) %>%       # convert month abb to month number
  dplyr::arrange(., year, month) %>%                        # reorder rows
  dplyr::filter(., year>=1998 & year<=2015)                 # year range (note: using Dec 1998 for year 1999 winter values)

##--------------------------------------------------------------------------------------------
## calculate annual means
df_mean_annual_l <- df_data_filtered_l %>%
  dplyr::filter(., year>=1999 & year<=2015) %>%
  dplyr::group_by(., year) %>%
  dplyr::summarise(., value=mean(value, na.rm=TRUE))

##--------------------------------------------------------------------------------------------
## calculate winter [D-J-F] means
df_mean_winter_l <- df_data_filtered_l %>%
  dplyr::mutate(., year=ifelse(month==12, year+1, year), month=ifelse(month==12, 0, month)) %>%
  dplyr::filter(., month>=0 & month<=2) %>%
  dplyr::filter(., year>=1999 & year<=2015) %>%
  dplyr::group_by(., year) %>%
  dplyr::summarise(., value=mean(value, na.rm=TRUE))

##--------------------------------------------------------------------------------------------
## calculate monthly climatology
df_climatology_month_l <- df_data_filtered_l %>%
  dplyr::filter(., year>=1999 & year<=2015) %>%
  dplyr::select(., month, value) %>%
  dplyr::group_by(., month) %>%
  dplyr::summarise(., mean=mean(value, na.rm=TRUE), sd=sd(value, na.rm=TRUE))  # monthly stats

##--------------------------------------------------------------------------------------------
## calculate de-seasonalized monthly anomalies
df_anomaly_month_l <- dplyr::left_join(df_data_filtered_l,
                                     df_climatology_month_l,
                                     by="month") %>%
  dplyr::mutate(., value=(value-mean)/sd) %>%
  dplyr::select(., year, month, value)

## reshape to wide format
df_anomaly_month_w <- df_anomaly_month_l %>%
  tidyr::spread(., month, value) %>%
  dplyr::arrange(., year)

##--------------------------------------------------------------------------------------------
## calculate annual anomalies
df_anomaly_annual_l <- df_anomaly_month_l %>%
  dplyr::filter(., year>=1999 & year<=2015) %>%
  dplyr::group_by(., year) %>%
  dplyr::summarise(., value=mean(value, na.rm=TRUE)) %>%
  dplyr::arrange(., year)

##--------------------------------------------------------------------------------------------
## calculate winter [D-J-F] anomalies
df_anomaly_winter_l <- df_anomaly_month_l %>%
  dplyr::mutate(., year=ifelse(month==12, year+1, year), month=ifelse(month==12, 0, month)) %>%
  dplyr::filter(., month>=0 & month<=2) %>%
  dplyr::filter(., year>=1999 & year<=2015) %>%
  dplyr::group_by(., year) %>%
  dplyr::summarise(., value=mean(value, na.rm=TRUE)) %>%
  dplyr::arrange(., year)

##--------------------------------------------------------------------------------------------
## calculate annual normalized anomalies
df_norm_anomaly_annual_l <- df_anomaly_annual_l %>%
  dplyr::mutate(., mean=mean(value, na.rm=TRUE), sd=sd(value, na.rm=TRUE)) %>%
  dplyr::mutate(., value=(value-mean)/sd) %>%
  dplyr::select(., year, value)

## calculate winter normalized anomalies
df_norm_anomaly_winter_l <- df_anomaly_winter_l %>%
  dplyr::mutate(., mean=mean(value, na.rm=TRUE), sd=sd(value, na.rm=TRUE)) %>%
  dplyr::mutate(., value=(value-mean)/sd) %>%
  dplyr::select(., year, value)

##--------------------------------------------------------------------------------------------
## print to csv file
# raw data
ofile <- "~/Projects/ICES_6IZPS_PP/outputs/NAO/NAO_Data_Raw.csv"
write.table(df_data_raw_w,
            file=ofile, append=FALSE, quote=TRUE, sep=",",
            eol="\n", na="NA", dec=".", row.names=FALSE, col.names=TRUE)

# filtered data
ofile <- "~/Projects/ICES_6IZPS_PP/outputs/NAO/NAO_Data_Filtered.csv"
write.table(df_data_filtered_l %>%
              tidyr::spread(., month, value) %>%
              dplyr::arrange(., year),
            file=ofile, append=FALSE, quote=TRUE, sep=",",
            eol="\n", na="NA", dec=".", row.names=FALSE, col.names=TRUE)

## annual means
ofile <- "~/Projects/ICES_6IZPS_PP/outputs/NAO/NAO_Means_Annual.csv"
write.table(df_mean_annual_l, file=ofile, append=FALSE, quote=TRUE, sep=",",
            eol="\n", na="NA", dec=".", row.names=FALSE,
            col.names=TRUE)

## annual winter [D-J-F] means
ofile <- "~/Projects/ICES_6IZPS_PP/outputs/NAO/NAO_Means_Winter.csv"
write.table(df_mean_winter_l, file=ofile, append=FALSE, quote=TRUE, sep=",",
            eol="\n", na="NA", dec=".", row.names=FALSE,
            col.names=TRUE)

## monthly climatology
ofile <- "~/Projects/ICES_6IZPS_PP/outputs/NAO/NAO_Climatology_Month.csv"
write.table(df_climatology_month_l, file=ofile, append=FALSE, quote=TRUE, sep=",",
            eol="\n", na="NA", dec=".", row.names=FALSE,
            col.names=TRUE)

## monthly anomalies
ofile <- "~/Projects/ICES_6IZPS_PP/outputs/NAO/NAO_Anomalies_Month.csv"
write.table(df_anomaly_month_w, file=ofile, append=FALSE, quote=TRUE, sep=",",
            eol="\n", na="NA", dec=".", row.names=FALSE,
            col.names=TRUE)

## annual anomalies
ofile <- "~/Projects/ICES_6IZPS_PP/outputs/NAO/NAO_Anomalies_Annual.csv"
write.table(df_anomaly_annual_l, file=ofile, append=FALSE, quote=TRUE, sep=",",
            eol="\n", na="NA", dec=".", row.names=FALSE,
            col.names=TRUE)

## winter anomalies
ofile <- "~/Projects/ICES_6IZPS_PP/outputs/NAO/NAO_Anomalies_Winter.csv"
write.table(df_anomaly_winter_l, file=ofile, append=FALSE, quote=TRUE, sep=",",
            eol="\n", na="NA", dec=".", row.names=FALSE,
            col.names=TRUE)

## annual normalized anomalies
ofile <- "~/Projects/ICES_6IZPS_PP/outputs/NAO/NAO_Normalized_Anomalies_Annual.csv"
write.table(df_norm_anomaly_annual_l, file=ofile, append=FALSE, quote=TRUE, sep=",",
            eol="\n", na="NA", dec=".", row.names=FALSE,
            col.names=TRUE)

## winter normalized anomalies
ofile <- "~/Projects/ICES_6IZPS_PP/outputs/NAO/NAO_Normalized_Anomalies_Winter.csv"
write.table(df_norm_anomaly_winter_l, file=ofile, append=FALSE, quote=TRUE, sep=",",
            eol="\n", na="NA", dec=".", row.names=FALSE,
            col.names=TRUE)

##--------------------------------------------------------------------------------------------
## save to RData file
ofile <- "~/Projects/ICES_6IZPS_PP/outputs/NAO/NAO.RData"
save(file=ofile, list=c("df_data_raw_w", "df_data_filtered_l",
                        "df_mean_annual_l", "df_mean_winter_l",
                        "df_climatology_month_l",
                        "df_anomaly_month_l", "df_anomaly_month_w",
                        "df_anomaly_annual_l", "df_anomaly_winter_l",
                        "df_norm_anomaly_annual_l", "df_norm_anomaly_winter_l"))
