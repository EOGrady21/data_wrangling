####################################################################
# Author: Dan Reed (Daniel.Reed@dfo-mpo.gc.ca)
# Purpose: GAMs fit, ranked to GSL herring SSB
# Date: Thu Aug 08 08:42:37 2019
####################################################################

# Load necessary libraries
library(data.table)
library(ACCASPR)

# Load principal component data
PC_data <- fread("../output/QC_PCs.csv")

# Load data
herring <- setDT(read_xlsx("../data/4T-4R_herring_SSB-R.xlsx"))

# Join data sets by year
herring <- PC_data[herring, , on = c(year = "Year")]

###################################
# AUTUMN
###################################
# Get name of predictors
predictors_A <- colnames(herring)[grep("PC",colnames(herring))]

# Run all GAMs: predictors, response variables, # knots, max # terms, data
res_gam_A <- run_gams(predictors_A, "Her_4R_Fall_SSB", 3, 3, 
                      na.omit(herring[, grep("PC|Her_4R_Fall_SSB", names(herring)), with = FALSE]))

# Filter those with excessive concurvity
res_gam_filtered_A <- filter_gams(res_gam_A, 0.5)

# Get top 5 GAMs ranked by AIC
top_models_A <- rank_gams(res_gam_filtered_A, 5)

# Tabulate results
tabulate_gams(top_models_A)

## Partial residual plots
pdf("../output/QC_herring_4R_autumn_SSB_effect.pdf", width = 14, height = 10)
lapply(top_models_A, 
       FUN = plot_gam_effect, 
       ylabel = expression("Effect on SSB"))
dev.off()

## Time series
pdf("../output/QC_herring_4R_autumn_SSB_ts.pdf", width = 12, height = 10)
lapply(top_models_A, 
       FUN = plot_gam_ts, 
       data = herring, 
       response = "SSB")
dev.off()

## Diagnostics
pdf("../output/QC_herring_4R_autumn_SSB_res.pdf", width = 14, height = 10)
lapply(top_models_A, 
       FUN = examine_gam_res, 
       data = herring)
dev.off()

###################################
# SPRING
###################################
# Get name of predictors
predictors_S <- colnames(herring)[grep("PC",colnames(herring))]

# Run all GAMs: predictors, response variables, # knots, max # terms, data
res_gam_S <- run_gams(predictors_S, "Her_4R_Spr_SSB", 3, 3, 
                      na.omit(herring[, grep("PC|Her_4R_Spr_SSB", names(herring)), with = FALSE]))

# Filter those with excessive concurvity
res_gam_filtered_S <- filter_gams(res_gam_S, 0.5)

# Get top 5 GAMs ranked by AIC
top_models_S <- rank_gams(res_gam_filtered_S, 5)

# Tabulate results
tabulate_gams(top_models_S)

## Partial residual plots
pdf("../output/QC_herring_4R_spring_SSB_effect.pdf", width = 14, height = 10)
lapply(top_models_S, 
       FUN = plot_gam_effect, 
       ylabel = expression("Effect on SSB"))
dev.off()

## Time series
pdf("../output/QC_herring_4R_spring_SSB_ts.pdf", width = 12, height = 10)
lapply(top_models_S, 
       FUN = plot_gam_ts, 
       data = herring, 
       response = "SSB")
dev.off()

## Diagnostics
pdf("../output/QC_herring_4R_spring_SSB_res.pdf", width = 14, height = 10)
lapply(top_models_S, 
       FUN = examine_gam_res, 
       data = herring)
dev.off()