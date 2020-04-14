####################################################################
# Author: Dan Reed (Daniel.Reed@dfo-mpo.gc.ca)
# Purpose: Combine herring fish data with set data
# Date: 11/12/17
####################################################################

##### Libraries #####
library(data.table)
library(dplyr)
library(magrittr)

##### Load data sets #####
sets <- fread("../data/Herring_Sets.csv", skip = 7)[YEAR >= 1999]
fish <- fread("../data/Herring_Fish.csv", skip = 9)[YEAR >= 1999]

##### ABUNDANCE #####
# Calculate weights
weights <- sets %>% 
  group_by(YEAR) %>% 
  distinct(STRAT, .keep_all = TRUE) %>% 
  mutate(WEIGHT_AREA = AREA/sum(AREA)) %>%  
  ungroup() %>% 
  select(YEAR, STRAT, WEIGHT_AREA)

# Calculate annual mean for each strata
# Join area weights
# Calculate weighted mean
# Calculate stratified mean
abundance <- sets %>%
  group_by(YEAR, STRAT) %>%
  summarise(STRAT_MEAN = mean(DISTNO)) %>% 
  left_join(weights) %>% 
  group_by(YEAR) %>% 
  summarise(STRAT_MEAN_ABUND = sum(STRAT_MEAN * WEIGHT_AREA)) 

# Write to file
abundance %>% write.csv("../output/Herring_SS_abundance.csv", row.names = FALSE)

##### CONDITION #####
# Calculate Fulton's coefficient
fish %<>% 
  mutate(K = 100*(WT/LEN_CM^3))

# CONDITION: RELATIVE WEIGHT
# Calculate standard weight for every length of fish
# Filter to those size classes with same means
# Filter to specific maturity stages
# Remove area variable
# Calculate annual mean for each strata
# Join area weights
# Calculate stratified mean
condition <- fish %>%
  group_by(LEN_CM) %>% 
  mutate(MEAN_WT_AT_LEN = mean(WT, na.rm = TRUE)) %>% 
  ungroup() %>% 
  filter(LEN_CM == 28, MAT %in% 3:5) %>%
  select(-AREA) %>%
  group_by(YEAR, STRATA) %>%
  summarise(ANNUAL_MEAN = mean(WT/MEAN_WT_AT_LEN, na.rm = TRUE)) %>%
  left_join(weights, by = c("STRATA" = "STRAT", "YEAR")) %>%
  group_by(YEAR) %>%
  summarise(STRAT_MEAN_K = sum(ANNUAL_MEAN * WEIGHT_AREA, na.rm = TRUE))

# Write to file
condition %>% write.csv("../output/Herring_SS_condition.csv", row.names = FALSE)