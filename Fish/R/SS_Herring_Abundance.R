####################################################################
# Author: Dan Reed (Daniel.Reed@dfo-mpo.gc.ca)
# Purpose: GAMs fit to SS herring
# Date: Thu Aug 08 13:21:04 2019
####################################################################

# Load necessary libraries
library(data.table)
library(ACCASPR)

# Load principal component data
PC_data <- fread("../output/HL_PCs.csv")

# Get fish data
herring <- fread("../output/Herring_SS_abundance.csv")

# Join data sets by year
herring <- PC_data[herring, , on = c(year = "YEAR")]

# Get name of predictors
predictors <- colnames(herring)[grep("PC",colnames(herring))]

# Run all GAMs: predictors, response variables, # knots, max # terms, data
res_gam <- run_gams(predictors, "STRAT_MEAN_ABUND", 3, 3, herring)

# Filter those with excessive concurvity
res_gam_filtered <- filter_gams(res_gam, 0.5)

# Get top 5 GAMs ranked by AIC
top_models <- rank_gams(res_gam_filtered, 5)

# Print table of top models for total abundance and record results
tabulate_gams(top_models)

## Partial residual plots
pdf("../output/SS_herring_abundance_effect.pdf", width = 14, height = 10)
lapply(top_models, 
       FUN = plot_gam_effect, 
       ylabel = expression("Effect on abundance"))
dev.off()

## Time series
pdf("../output/SS_herring_abundance_ts.pdf", width = 12, height = 10)
lapply(top_models, 
       FUN = plot_gam_ts, 
       data = herring, 
       response = expression("Abundance"))
dev.off()

pdf("../output/SS_herring_abundance_res.pdf", width = 14, height = 10)
lapply(top_models, 
       FUN = examine_gam_res, 
       data = herring)
dev.off()