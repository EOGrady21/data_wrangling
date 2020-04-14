####################################################################
# Author: Dan Reed (Daniel.Reed@dfo-mpo.gc.ca)
# Purpose: GAMs fit, ranked to GSL mackerel condition
# Date: Thu Aug 08 08:42:37 2019
####################################################################

# Load necessary libraries
library(data.table)
library(ACCASPR)

# Load principal component data
PC_data <- fread("../output/QC_PCs.csv")

# Load mackerel abundance data
ssb <- fread("../data/maq_ssb.csv")

# Load mackerel condition data
mackerel <- fread("../data/maq_cond_k.csv")

# Filter to June, drop month, and rename 'year'
mackerel <- mackerel[MONTH == 6, .(year = YEAR, cond_k)]

# Add SSB & PCs
mackerel <- ssb[mackerel, , on = "year"]
mackerel <- na.omit(PC_data[mackerel, , on = "year"])

# Get name of predictors
predictors <- c(colnames(mackerel)[grep("PC",colnames(mackerel))], "ssb")

# Run all GAMs: predictors, response variables, # knots, max # terms, data
res_gam <- run_gams(predictors, "cond_k", 3, 3, mackerel)

# Filter those with excessive concurvity
res_gam_filtered <- filter_gams(res_gam, 0.5)

# Get top 5 GAMs ranked by AIC
top_models <- rank_gams(res_gam_filtered, 5)

# Tabulate results
tabulate_gams(top_models)

## Partial residual plots
pdf("../output/QC_mackerel_Condition_effect.pdf", width = 14, height = 10)
lapply(top_models, 
       FUN = plot_gam_effect, 
       ylabel = expression("Effect on K"))
dev.off()

## Time series
pdf("../output/QC_mackerel_Condition_ts.pdf", width = 12, height = 10)
lapply(top_models, 
       FUN = plot_gam_ts, 
       data = mackerel, 
       response = "K")
dev.off()

## Diagnostics
pdf("../output/QC_mackerel_Condition_res.pdf", width = 14, height = 10)
lapply(top_models, 
       FUN = examine_gam_res, 
       data = mackerel)
dev.off()