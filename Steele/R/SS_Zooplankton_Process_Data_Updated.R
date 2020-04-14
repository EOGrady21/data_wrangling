
## required packages
library(dplyr)
library(tidyr)
library(data.table)

## load in data
cfin = fread('../Updated Data/PL_HL2_Abundance_Grouped_Cfin_20200401.csv')
zoop = fread('../Updated Data/PL_HL2_Abundance_Grouped_20200331.csv')

## calculate C finmarchicus I+II+III
cfin = cbind(cfin[,1:14], rowSums(cfin[,c('Calanus finmarchicus I', 'Calanus finmarchicus II', 'Calanus finmarchicus III')]))
colnames(cfin) = c(colnames(cfin)[-length(colnames(cfin))], 'Calanus finmarchicus I-III')

## transfer yearday and Cfin I-III to zoop
cfin$day == zoop$day # confirm rows are equIIIalent
cfin = cfin[,c("yearday", "Calanus finmarchicus I-III")]
df_data_raw_w = cbind(cfin, zoop)
rm(cfin,zoop)

## rearrange columns
df_data_raw_w = df_data_raw_w[,c(3:9, 1, 10:17, 2, 18:64)]

## calculate total zooplankton
totzoo = rowSums(df_data_raw_w[,8:16, 18:64]) # skip C. fin I-III, is included in C. fin

## add to data table
df_data_raw_w = cbind(df_data_raw_w[,1:64], totzoo)
colnames(df_data_raw_w) = c(colnames(df_data_raw_w)[-length(colnames(df_data_raw_w))], 'Total Zooplankton')
rm(totzoo)


##-----------------------------------------------------------------------------------------------
## Find Peaks

## total zooplankton
df_peak_zooplankton = dplyr::group_by(df_data_raw_w, year) %>%
                      dplyr::filter(., `Total Zooplankton` == max(`Total Zooplankton`)) %>%
                      dplyr::select(., station, year, yearday, `Total Zooplankton`)

## total C. finmarchicus
df_peak_Cfin_total = dplyr::group_by(df_data_raw_w, year) %>%
                     dplyr::filter(., `Calanus finmarchicus` == max(`Calanus finmarchicus`)) %>%
                     dplyr::select(., station, year, yearday, `Calanus finmarchicus`)

## C. finmarchicus I-III
df_peak_Cfin_ItoIII = dplyr::group_by(df_data_raw_w, year) %>%
                     dplyr::filter(., `Calanus finmarchicus I-III` == max(`Calanus finmarchicus I-III`)) %>%
                     dplyr::select(., station, year, yearday, `Calanus finmarchicus I-III`)

##-----------------------------------------------------------------------------------------------
## Calculate Climatologies
df_climatology_w <- cbind(df_peak_zooplankton[,c(1, 3:4)], df_peak_Cfin_total[,3:4], df_peak_Cfin_ItoIII[,3:4])
colnames(df_climatology_w) = c('station', 'yearday_total_zooplankton', 'total_zooplankton',
                               'yearday_Cfin_total', 'Cfin_total',
                               'yearday_Cfin_ItoIII', 'Cfin_ItoIII')
df_climatology_w <- cbind(summarise(df_climatology_w, yearday_total_zooplankton=mean(yearday_total_zooplankton, na.rm=TRUE)),
                          summarise(df_climatology_w, total_zooplankton=mean(total_zooplankton, na.rm=TRUE)),
                          summarise(df_climatology_w, yearday_Cfin_total=mean(yearday_Cfin_total, na.rm=TRUE)),
                          summarise(df_climatology_w, Cfin_total=mean(Cfin_total, na.rm=TRUE)),
                          summarise(df_climatology_w, yearday_Cfin_ItoIII=mean(yearday_Cfin_ItoIII, na.rm=TRUE)),
                          summarise(df_climatology_w, Cfin_ItoIII=mean(Cfin_ItoIII, na.rm=TRUE)))

##--------------------------------------------------------------------------------------------
## calculate annual anomalies
df_anomaly_peak_zooplankton <- df_peak_zooplankton %>%
  dplyr::mutate(., yearday_anomaly=yearday-df_climatology_w$yearday_total_zooplankton) %>%
  dplyr::mutate(., zooplankton_anomaly=`Total Zooplankton`-df_climatology_w$total_zooplankton) %>%
  dplyr::select(., station, year, yearday_anomaly, zooplankton_anomaly)

##--------------------------------------------------------------------------------------------
## save to RData file
ofile <- "../Updated Data/Zooplankton_Peaks_04012020.RData"
save(file=ofile, list=c("df_data_raw_w",
                        'df_peak_zooplankton',
                        'df_peak_Cfin_total',
                        'df_peak_Cfin_ItoIII'))
