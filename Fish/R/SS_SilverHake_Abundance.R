####################################################################
# Author: Dan Reed (Daniel.Reed@dfo-mpo.gc.ca)
# Purpose: GAMs fit, ranked to SS silver hake abundance
# Date: Thu Aug 08 08:42:37 2019
####################################################################

# Load necessary libraries
library(data.table)
library(ACCASPR)

# Load principal component data
PC_data <- fread("../output/HL_PCs.csv")

# Get column headers
header <- scan("../data/SummarySilverHakeAbundBiomCond.csv", 
               nlines = 2, what = character(), sep = ",")

# Create column names by combining first and second rows
header <- paste(header[1:(length(header)/2)], 
                header[(1+length(header)/2):length(header)])

# Load data
hake <- fread("../data/SummarySilverHakeAbundBiomCond.csv", 
              skip = 2, header = FALSE)

# Names columns (& strip leading spaces)
colnames(hake) <- sub("^ ", "", header)
colnames(hake)[c(9,11,14,16)] <- c("Recruitment biomass se", "Recruitment abundance se", 
                                   "Adult biomass se", "Adult abundance se")
colnames(hake) <- gsub(" ", "_", colnames(hake))
colnames(hake) <- gsub("\\(", "", colnames(hake))
colnames(hake) <- gsub("\\)", "", colnames(hake))

# Join data sets by year
hake <- PC_data[hake, , on = "year"]

# Get name of predictors
predictors <- colnames(hake)[grep("PC",colnames(hake))]

# Run all GAMs: predictors, response variables, # knots, max # terms, data
res_gam <- run_gams(predictors, "Adult_abundance", 3, 3, hake)

# Filter those with excessive concurvity
res_gam_filtered <- filter_gams(res_gam, 0.5)

# Get top 5 GAMs ranked by AIC
top_models <- rank_gams(res_gam_filtered, 5)

# Print table of top models for total abundance and record results
tabulate_gams(top_models)

# Plot effect figures
pdf("../output/SS_silver_hake_abundance_effect.pdf", width = 14, height = 10)
lapply(top_models, 
       FUN = plot_gam_effect, 
       ylabel = expression("Effect on abundance"))
dev.off()

# Plot time series figures
pdf("../output/SS_silver_hake_abundance_ts.pdf", width = 12, height = 10)
lapply(top_models, 
       FUN = plot_gam_ts, 
       data = hake, 
       response = expression("Adult abundance (millions)"))
dev.off()

# Plot residual figures
pdf("../output/SS_silver_hake_abundance_res.pdf", width = 14, height = 10)
lapply(top_models, 
       FUN = examine_gam_res, 
       data = hake)
dev.off()
