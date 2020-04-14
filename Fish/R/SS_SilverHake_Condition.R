####################################################################
# Author: Dan Reed (Daniel.Reed@dfo-mpo.gc.ca)
# Purpose: GAMs fit, ranked to SS silver hake condition
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
predictors <- c(colnames(hake)[grep("PC",colnames(hake))], "TOTAL_Biomass")

# Run all GAMs: predictors, response variables, # knots, max # terms, data
res_gam_25 <- run_gams(predictors, "condition_wtat25cm", 3, 3, hake)

# Filter those with excessive concurvity
res_gam_filtered_25 <- filter_gams(res_gam_25, 0.5)

# Get top 5 GAMs ranked by AIC
top_models_25 <- rank_gams(res_gam_filtered_25, 5)

# Print table of top models for weight at 25 cm 
tabulate_gams(top_models_25)

# Plot effect figures
pdf("../output/SS_silver_hake_condition_effect.pdf", width = 14, height = 10)
lapply(top_models_25, 
       FUN = plot_gam_effect, 
       ylabel = expression("Effect on weight at 25 cm"))
dev.off()

# Plot time series figures
pdf("../output/SS_silver_hake_condition_ts.pdf", width = 12, height = 10)
lapply(top_models_25, 
       FUN = plot_gam_ts, 
       data = hake, 
       response = "Weight at 25 cm (g)")
dev.off()

# Plot residual figures
pdf("../output/SS_silver_hake_condition_res.pdf", width = 14, height = 10)
lapply(top_models_25, 
       FUN = examine_gam_res, 
       data = hake)
dev.off()
