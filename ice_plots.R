## compare Ice metrics
## compare SS+GSL to GSL values to determine if it is worth combining regions

# E. Chisholm
# June 11 2020

library(ggplot2)
library(dplyr)
library(reshape2)

# find data

alldatfiles <- list.files('Fish/data/')
icedatfiles <- grep(pattern = 'ice', x = alldatfiles, value = TRUE, ignore.case = TRUE)

# load data

load(file.path('Fish/data', icedatfiles[2]))

ice_area_volume <- df_data_filtered_w

# calculate ice area metrics

ice_area_ssgsl <- ice_area_volume %>%
  select(year, month, area, region) %>%
  filter(., region %in% c('SS', 'GSL')) %>%
  mutate(ym = paste0(year, month)) 

ice_area_gsl <- ice_area_volume %>%
  select(year, month, area, region) %>%
  filter(., region %in% c( 'GSL')) %>%
  mutate(ym = paste0(year, month)) 

# monthly mean values for SS+GSL
m_icearea_ssgsl <- ice_area_ssgsl %>%
  group_by(., year, month) %>%
  summarise(monthly_c = sum(area))
  
# annual mean values for SS+GSL
a_icearea_ssgsl <- ice_area_ssgsl %>%
  group_by(., year) %>%
  summarise(annual_c = sum(area))
  
# monthly mean values for GSL
m_icearea_gsl <- ice_area_gsl %>%
  group_by(., ym) %>%
  summarise(monthly_gsl = sum(area))

# annual mean values for GSL
a_icearea_gsl <- ice_area_gsl %>%
  group_by(., year) %>%
  summarise(annual_gsl = sum(area))

# merge data frames

a_icearea <- merge(a_icearea_gsl, a_icearea_ssgsl, by= 'year')
a_area_melt <- melt(a_icearea, id.vars = 'year')

m_icearea <- merge(m_icearea_gsl, m_icearea_ssgsl, by= 'ym')
m_area_melt <- melt(m_icearea, id.vars = 'ym')

# plot ice area metrics
ggplot(data = a_area_melt, aes(x = year, y = value, colour = variable))+
  geom_point() +
  geom_line() +
  labs(y = 'Ice Area', colour = 'Region')

# ggplot(data = m_area_melt, aes(x = ym, y = value, colour = variable))+
#   geom_point() +
#   geom_line() +
#   labs(y = 'Ice Area', x = 'year month', colour = 'Region')


# calculate ice volume metrics

ice_vol_ssgsl <- ice_area_volume %>%
  select(year, month, volume, region) %>%
  filter(., region %in% c('SS', 'GSL')) %>%
  mutate(ym = paste0(year, month)) 

ice_vol_gsl <- ice_area_volume %>%
  select(year, month, volume, region) %>%
  filter(., region %in% c( 'GSL')) %>%
  mutate(ym = paste0(year, month)) 

# monthly mean values for SS+GSL
m_icevol_ssgsl <- ice_vol_ssgsl %>%
  group_by(., year, month) %>%
  summarise(monthly_c = sum(volume))

# annual mean values for SS+GSL
a_icevol_ssgsl <- ice_vol_ssgsl %>%
  group_by(., year) %>%
  summarise(annual_c = sum(volume))

# monthly mean values for GSL
m_icevol_gsl <- ice_vol_gsl %>%
  group_by(., ym) %>%
  summarise(monthly_gsl = sum(volume))

# annual mean values for GSL
a_icevol_gsl <- ice_vol_gsl %>%
  group_by(., year) %>%
  summarise(annual_gsl = sum(volume))

# merge data frames

a_icevol <- merge(a_icevol_gsl, a_icevol_ssgsl, by= 'year')
a_vol_melt <- melt(a_icevol, id.vars = 'year')

# plot ice area metrics
ggplot(data = a_vol_melt, aes(x = year, y = value, colour = variable))+
  geom_point() +
  geom_line() +
  labs(y = 'Ice Volume', colour = 'Region')


# load ice timing data
# load(file.path('Fish/data', icedatfiles[4]))
# ice timing data is already combined into GSL+SS
  

# load older version of data
load('D:/data_wrangling_docs/Lab Book/data/For assembly/Ice_Timing.RData')

ice_timing <- df_data_filtered_w

# calculate final day metrics


ice_final_ssgsl <- ice_timing %>%
  select(year, loc_id, lat, lon, day_last, region) %>%
  filter(., region %in% c('SS', 'GSL'))

a_icefinal_ssgsl <- ice_final_ssgsl %>%
  group_by(year) %>%
  summarize(annual_ssgsl = max(day_last, na.rm = TRUE))

ice_final_gsl <- ice_timing %>%
  select(year, loc_id, lat, lon, day_last, region) %>%
  filter(., region %in% c( 'GSL'))

a_icefinal_gsl <- ice_final_gsl %>%
  group_by(year) %>%
  summarize(annual_gsl = max(day_last, na.rm = TRUE))

# merge data frames

a_icefinal <- merge(a_icefinal_gsl, a_icefinal_ssgsl)
a_icefinal_melt <- melt(a_icefinal, id.vars = 'year')


# plot
ggplot(data = a_icefinal_melt, aes(x = year, y = value, shape = variable, colour = variable), alpha = 0.001)+
  geom_point() +
  geom_line() +
  labs(y = 'Final Ice Day', colour = 'Region')





