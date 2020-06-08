
##--------------------------------------------------------------------------------------------
# load required packages
library(tidyverse)

##--------------------------------------------------------------------------------------------
# load data - HL2 total Cfin abundance data
load("~/Projects/fish_tmp/output/PL_HL2_Cfin_20200416.RData")

# run loess fits and generate predictions for each year
newdata <- data.frame(yearday = seq(1, 365))
df_pred <- df_abundance_grouped_l %>%
  dplyr::filter(., variable=="Calanus finmarchicus") %>%
  dplyr::mutate(., log_value = log10(value+1)) %>%
  dplyr::group_by(., year) %>%
  tidyr::nest(.) %>%
  dplyr::mutate(mod = purrr::map(.x = data, .f= ~ loess(data = ., formula="log_value ~ yearday", span=0.7))) %>%
  dplyr::mutate(pred = purrr::map(.x = mod, .f= ~ predict(., newdata))) %>%
  dplyr::select(year, pred) %>%
  tidyr::unnest(., cols=pred) %>%
  dplyr::ungroup(.) %>%
  cbind(., newdata=newdata)

# find day of maximum
Cfin_total_peaks_HL2 <- df_pred %>%
  dplyr::group_by(., year) %>%
  dplyr::do(peak=which.max(.$pred)) %>%
  tidyr::unnest(., cols=peak) %>%
  dplyr::ungroup(.)


##--------------------------------------------------------------------------------------------
# load data - HL2 total biomass data
load("~/Projects/fish_tmp/output/PL_HL2_Biomass_20200416.RData")

# run loess fits and generate predictions for each year
newdata <- data.frame(yearday = seq(1, 365))
df_pred <- df_biomass_grouped_l %>%
  dplyr::filter(., variable=="ww2_T") %>%
  dplyr::mutate(., log_value = log10(value+1)) %>%
  dplyr::group_by(., year) %>%
  tidyr::nest(.) %>%
  dplyr::mutate(mod = purrr::map(.x = data, .f= ~ loess(data = ., formula="log_value ~ yearday", span=0.7))) %>%
  dplyr::mutate(pred = purrr::map(.x = mod, .f= ~ predict(., newdata))) %>%
  dplyr::select(year, pred) %>%
  tidyr::unnest(., cols=pred) %>%
  dplyr::ungroup(.) %>%
  cbind(., newdata=newdata)

# find day of maximum
Z_biomass_peaks_HL2 <- df_pred %>%
  dplyr::group_by(., year) %>%
  dplyr::do(peak=which.max(.$pred)) %>%
  tidyr::unnest(., cols=peak) %>%
  dplyr::ungroup(.)


##--------------------------------------------------------------------------------------------
# load data - HL2 staged Cfin abundance data
load("~/Projects/fish_tmp/output/PL_HL2_Cfin_20200416.RData")

# calculate relative abundances
df <- dplyr::left_join(df_abundance_grouped_l %>%
                         dplyr::filter(., variable != "Calanus finmarchicus"),
                       df_abundance_grouped_l %>%
                         dplyr::filter(., variable=="Calanus finmarchicus") %>%
                         dplyr::select(., sample_id, value) %>%
                         dplyr::rename(., total=value),
                       by="sample_id") %>%
  dplyr::mutate(., value=value/total) %>%
  dplyr::select(., -total)

Cfin_I_III_peaks_HL2 <- tibble(year=numeric(), yearday=numeric())
for(i_year in c(seq(2000, 2006), seq(2008,2019))){
  df_w <- filter(df, year==i_year) %>%
    select(., yearday, variable, value) %>%
    tidyr::spread(., variable, value) %>%
    arrange(., yearday)
  
  res <- prcomp(x=df_w %>% select(., -yearday), scale. = TRUE, center = TRUE)
  res <- bind_cols(df_w %>% select(., yearday), as_tibble(res$x))
  
  mod1 <- loess(formula="PC1 ~ yearday", data=res, span=0.7)
  
  pred <- predict(mod1, data.frame(yearday=seq(1,365)))
  
  Cfin_I_III_peaks_HL2 <- bind_rows(Cfin_I_III_peaks_HL2,
                                    tibble(year=i_year, yearday=which.max(pred)[[1]]))
}
Cfin_I_III_peaks_HL2 <- Cfin_I_III_peaks_HL2 %>%
  dplyr::rename(peak=yearday)

##--------------------------------------------------------------------------------------------
# save results
output_file <- "~/Projects/fish_tmp/output/Zoo_Peaks.RData"
save(file=output_file, list=c("Cfin_I_III_peaks_HL2",
                              "Cfin_total_peaks_HL2",
                              "Z_biomass_peaks_HL2"))



##--------------------------------------------------------------------------------------------
## extra code

# df <- df_abundance_grouped_l %>%
#   filter(., variable=="Calanus finmarchicus") %>%
#   filter(., year==2016) %>%
#   mutate(., log_value=log10(value+1))
# 
# mod1 <- loess(formula="log_value ~ yearday", data=df, span=0.7)
# 
# pred <- predict(mod1, data.frame(yearday=seq(1,365)))
# 
# which.max(pred)


# df_mod <- df_abundance_grouped_l %>%
#   filter(., variable=="Calanus finmarchicus") %>%
#   mutate(., log_value=log10(value+1)) %>%
#   group_by(year) %>% 
#   do(mod=loess(data=., formula="log_value ~ yearday", span=0.7))
