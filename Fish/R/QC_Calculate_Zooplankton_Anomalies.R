####################################################################
# Author: Dan Reed (Daniel.Reed@dfo-mpo.gc.ca)
# Purpose: Calculate zooplankton anomalies for QC stations
# Date: 13/12/17
####################################################################

##### LIBRARIES #####
library(readxl)
library(dplyr)
library(magrittr)
library(emmeans)
library(rlang)

##### LOAD DATA #####
# Load zooplankton data and create new data frame for assembled data 
quebec <- read_xlsx("../data/Quebec_zoo_indices_1999_2016.xlsx", sheet = "Quebec_all") 

# Load full Rimouski data set
riki <- read_xlsx("../data/Quebec_zoo_indices_1999_2016.xlsx", sheet = "Riki_all") 

##### DATA WRANGLING #####
# Remove Rimouski station, as it is only a partial data set
quebec %<>% 
  filter(region != "Rimouski") %>% 
  select(-contains("prop"), -contains("Zooplankton"), -transect)

# Remove proportion variables for Calanus species
riki %<>% 
  select(-contains("prop")) %>% 
  na.omit()

# Common columns between data sets
common <- intersect(colnames(quebec), colnames(riki))

# Pare down data sets
quebec %<>% select(common)
riki %<>% select(common)

# Correct data types in riki
riki[common] <- lapply(common, function(x) match.fun(paste0("as.", class(quebec[[x]])))(riki[[x]]))

# Bind data sets together
zoo <- bind_rows(riki, quebec)

# Remove Bivalvia, which was not collected for the early years of the Riki station
zoo %<>% select(-Bivalvia)

# Remove rows with NAs
zoo  %<>% na.omit()

# Combine life stages for Calanus spp.
zoo$`C. finmarchicus` <- zoo %>% select(contains("C.fin")) %>% rowSums()
zoo$`C. hyperboreus` <- zoo %>% select(contains("C.hyp")) %>% rowSums()
zoo$`C. glacialis` <- zoo %>% select(contains("C.gla")) %>% rowSums()

zoo %<>% select(-contains("CV", ignore.case = FALSE), -contains("CI", ignore.case = FALSE)) 

# Convert month to season (following season definitions of original file)
month2season <- function(m){
  ifelse(m %in% c(1:3), "winter",
         ifelse(m %in% c(4:6), "spring",
                ifelse(m %in% c(7:9), "summer",
                       ifelse(m %in% c(10:12), "autumn", NA))))
}
  
# Add seasons & transects; convert station to factor
zoo  %<>% mutate(season = as.factor(month2season(month)), 
                 transect = as.factor(gsub("[[:digit:]]","", station)),
                 station = as.factor(station),
                 year = as.factor(year))

##### CALCULATE ANOMALIES #####
# Strip digits off station names to get transect names
transects <- unique(zoo$transect)

# Assuming all taxa begin with a captial letter & other variables don't
taxa <- colnames(zoo)[grep("^[A-Z]", colnames(zoo))]

# Calculate LS annual means
lsannualmeans <- function(tra, tax){
  # Basic formula
  lm_model <- as.formula(density ~ year)
  
  # If there is more than one season
  if(dim(unique(zoo %>% filter(transect == tra) %>% select(season)))[1] > 1) lm_model <- update(lm_model, ~ . + season)
  
  # If there is more than one station
  if(dim(unique(zoo %>% filter(transect == tra) %>% select(station)))[1] > 1) lm_model <- update(lm_model, ~ . + station)
  
  # Linear model fit; filter data to transect and rename taxa column as "density"
  lm_fit <- lm(lm_model, 
               data = zoo %>% filter(transect == tra) %>% mutate(density = log10(1 + !!sym(tax))))

  # Calculate annual mean using least-square means
  summary(lsmeans(lm_fit, "year")) %>% 
    mutate(transect = tra) %>%
    select(year, transect, lsmean) %>% 
    rename(!!tax := lsmean) 
}

# Combine data frames for each species into one
combine_df <- function(mylist) Reduce(function(x, y) full_join(x, y, by = c("year", "transect")), mylist)

# For all transects and all species
data<- do.call(rbind, lapply(transects, function(y)combine_df(lapply(taxa, function(x)lsannualmeans(y, x)))))

# Calculate anomalies
data %<>% 
  group_by(transect) %>% 
  mutate_at(vars(-year, -transect),funs(scale)) %>% 
  ungroup

##### WRITE TO FILE #####
data %>% write.csv("../output/QC_zooplankton_anomalies.csv", row.names = FALSE)
