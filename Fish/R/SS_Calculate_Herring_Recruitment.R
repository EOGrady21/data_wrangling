####################################################################
# Author: Dan Reed (Daniel.Reed@dfo-mpo.gc.ca)
# Purpose: Calculate recruitment metric for SS herring
# Date: 20/12/17
####################################################################

##### Libraries #####
library(readxl)
library(data.table)

# Load data
german_data <- read_xlsx("../data/1999-2016 German_Scots_Trinity Acoustic data.xlsx", 
                           skip = 1, sheet = "German")
setDT(german_data)

# Rename columns
colnames(german_data) <- c("year", "station", "date", 
                           "biomass", "biomass_se", 
                           "annual_sum", "annual_sum_se",
                           "adj_biomass_turnover", "adj_biomass_turnover_se",
                           "adj_annual_sum", "adj_annualsum_se")

# Get time series
german_data <- na.omit(german_data[, .(year, adj_annual_sum)])

# Load data
scots_data <- read_xlsx("../data/1999-2016 German_Scots_Trinity Acoustic data.xlsx", 
                         skip = 1, sheet = "Scots")
setDT(scots_data)

# Rename columns
colnames(scots_data) <- c("year", "station", "date", 
                           "biomass", "biomass_se", 
                           "annual_sum", "annual_sum_se",
                           "adj_biomass_turnover", "adj_biomass_turnover_se",
                           "adj_annual_sum", "adj_annualsum_se")

# Get time series
scots_data <- na.omit(scots_data[, .(year, adj_annual_sum)])

# Join two data sets
acoustics <- merge(german_data, scots_data, by = "year", all = TRUE)

# Calculate total biomass
acoustics[, total := rowSums(.SD), .SDcols = names(acoustics)[-1]]

# Estimate recruitment and turn into anomaly
acoustics <- acoustics[, .(year, recruitment = scale(total - shift(total)))]

# Write data to file
fwrite(acoustics, "../output/Herring_SS_recruitment.csv", na = NA)