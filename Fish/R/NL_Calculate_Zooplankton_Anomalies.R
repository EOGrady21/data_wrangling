##########################################################
# AUTHOR: Dan Reed (Daniel.Reed@dfo-mpo.gc.ca)
# DATE: 15/11/17
# PURPOSE: Calculate zooplankton anomalies for NL stations
##########################################################

# Requisite libraries
library(emmeans)
library(dplyr)
library(readxl)
library(magrittr)
library(tidyr)

# Load data - file from Pierre Pepin via email to Catherine Johnson 30/10/17
data <- read_xlsx("../data/zplk_survey.xlsx")
S27 <- read_xlsx("../data/zplk_S27.xlsx")

# Bind S27 data to transects, add transect name
data %<>%
  bind_rows(S27) %>%
  mutate(sname = ifelse(is.na(sname), "STwentySeven", sname))

# Determine season for S27
data %<>%
  mutate(season = ifelse(sname == "STwentySeven", months(as.Date(doy, paste(year, "-01-01", sep = ""))), season))

# Subset to relevant data
data %<>% select(-c(3:4, 6:12, 45))

# Empty cells are read as NAs; convert to zeroes
data[is.na(data)] <- 0

# Sum Calanus life stages into a total and then remove individual life stages
data %<>% 
  mutate(CALFIN = rowSums(select(., starts_with("CALFIN")), na.rm = TRUE),
         CALGLA = rowSums(select(., starts_with("CALGLA")), na.rm = TRUE),
         CALHYP = rowSums(select(., starts_with("CALHYP")), na.rm = TRUE)) %>% 
  select(-contains("CALFINC"), -contains("CALGLAC"), -contains("CALHYPC"))

# Convert season and year to a factor
data %<>% 
  mutate(season = as.factor(season), 
         year = as.factor(year),
         sname = as.factor(sname))

# Get transect names
tran.names <- unique(gsub("[[:digit:]]", "", data$sname))

# Get species names
spec.names <- colnames(data)[4:ncol(data)]

# Log transform zooplankton abundances data
data %<>% 
  mutate_at(vars(4:ncol(data)), funs(log10(. + 1)))

# Calculate least-squares means
calc.lsmeans <- function(zoo, transect){
  # Filtered dataset for given transect
  df.trans <- data %>% filter(grepl(transect, sname)) %>% mutate(density = log10(1 + !!sym(zoo)))
  
  # Basic linear model
  lm_model <- as.formula(density ~ year)
  
  # If there is more than one season
  if(length(unique(df.trans$season))>1)lm_model <- update(lm_model, ~ . + season)
  
  # If there is more than one station
  if(length(unique(df.trans$sname))>1)lm_model <- update(lm_model, ~ . + sname)

  # Fit the model  
  lm_fit <- lm(lm_model, data = df.trans)
  
  # Calculate least-square means for each year
  df <- setNames(summary(lsmeans(lm_fit, "year")) %>% select(year, lsmean), c("year", "value"))
  
  # Append transect name and variable name, convert year to numeric
  df %<>% mutate(year = as.numeric(as.character(year)), transect = transect, variable = zoo)
  
  # Calculate anomaly
  if(sd(df$value)) return(df %<>% mutate(value = scale(value)))

  # If standard deviation is zero return NAs
  return(df %<>% mutate(value = NA))
}

# For all species and all transects, bind results into a single data frame
zoo <- do.call(rbind, lapply(spec.names, function(x) do.call(rbind, lapply(tran.names, function(y)calc.lsmeans(x, y)))))

# From long to wide format, change station identifier of S27
zoo %<>% 
  spread(variable, value) %>% 
  arrange(transect) %>% 
  mutate(transect = replace(transect, transect == "STwentySeven", "S27"))

# Write data to file
write.csv(zoo, file = "../output/NL_zooplankton.csv", row.names = FALSE)

