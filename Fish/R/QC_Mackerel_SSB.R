####################################################################
# Author: Dan Reed (Daniel.Reed@dfo-mpo.gc.ca)
# Purpose: GAMs fit, ranked to GSL mackerel SSB
# Date: Thu Aug 08 08:42:37 2019
####################################################################

# Load necessary libraries
library(data.table)
library(ACCASPR)

# Load principal component data
PC_data <- fread("../output/QC_PCs.csv")

# Load mackerel abundance data
mackerel <- fread("../data/maq_ssb.csv")

# Join data sets by year
mackerel <- PC_data[mackerel, , on = "year"]

# Get name of predictors
predictors <- colnames(mackerel)[grep("PC",colnames(mackerel))]

# Run all GAMs: predictors, response variables, # knots, max # terms, data
res_gam <- run_gams(predictors, "ssb", 3, 3, na.omit(mackerel))

# Filter those with excessive concurvity
res_gam_filtered <- filter_gams(res_gam, 0.5)

# Get top 5 GAMs ranked by AIC
top_models <- rank_gams(res_gam_filtered, 5)

# Print table of top models for total abundance and record results
tabulate_gams(top_models)

## Partial residual plots
pdf("../output/QC_mackerel_SSB_effect.pdf", width = 14, height = 10)
lapply(top_models, 
       FUN = plot_gam_effect, 
       ylabel = expression("Effect on SSB"))
dev.off()

## Time series
pdf("../output/QC_mackerel_SSB_ts.pdf", width = 12, height = 10)
lapply(top_models, 
       FUN = plot_gam_ts, 
       data = mackerel, 
       response = "SSB")
dev.off()

## Diagnostics
pdf("../output/QC_mackerel_SSB_res.pdf", width = 14, height = 10)
lapply(top_models, 
       FUN = examine_gam_res, 
       data = mackerel)
dev.off()