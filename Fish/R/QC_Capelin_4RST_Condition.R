####################################################################
# Author: Dan Reed (Daniel.Reed@dfo-mpo.gc.ca)
# Purpose: GAMs fit, ranked to GSL mackerel recruitment
# Date: Thu Aug 08 08:42:37 2019
####################################################################

# Load necessary libraries
library(data.table)
library(ACCASPR)

# Load principal component data
PC_data <- fread("../output/QC_PCs.csv")

# Load data
capelin <- fread("../data/4RST_Capelin_K.tsv")

# Adjust column names
colnames(capelin) <- c("year", "K_female", "K_male")

# Join data sets by year 
capelin <- PC_data[capelin, , on = "year"]

# Get name of predictors
predictors <- colnames(capelin)[grep("PC", colnames(capelin))]

###################################
# MALE
###################################
# Run all GAMs: predictors, response variables, # knots, max # terms, data
res_gam_m <- run_gams(predictors, "K_male", 3, 3, 
                      na.omit(capelin[, grep("PC|K_male", names(capelin)), with = FALSE]))

# Filter those with excessive concurvity
res_gam_filtered_m <- filter_gams(res_gam_m, 0.5)

# Get top 5 GAMs ranked by AIC
top_models_m <- rank_gams(res_gam_filtered_m, 5)

# Tabulate results
tabulate_gams(top_models_m)

## Partial residual plots
pdf("../output/QC_capelin_male_Condition_effect.pdf", width = 14, height = 10)
lapply(top_models_m, 
       FUN = plot_gam_effect, 
       ylabel = expression("Effect on K"))
dev.off()

## Time series
pdf("../output/QC_capelin_male_Condition_ts.pdf", width = 12, height = 10)
lapply(top_models_m, 
       FUN = plot_gam_ts, 
       data = capelin, 
       response = "K")
dev.off()

## Diagnostics
pdf("../output/QC_capeline_male_Condition_res.pdf", width = 14, height = 10)
lapply(top_models_m, 
       FUN = examine_gam_res, 
       data = capelin)
dev.off()

###################################
# FEMALE
###################################
# Run all GAMs: predictors, response variables, # knots, max # terms, data
res_gam_f <- run_gams(predictors, "K_female", 3, 3, 
                      na.omit(capelin[, grep("PC|K_female", names(capelin)), with = FALSE]))

# Filter those with excessive concurvity
res_gam_filtered_f <- filter_gams(res_gam_f, 0.5)

# Get top 5 GAMs ranked by AIC
top_models_f <- rank_gams(res_gam_filtered_f, 5)

# Tabulate results
tabulate_gams(top_models_f)

## Partial residual plots
pdf("../output/QC_capelin_female_Condition_effect.pdf", width = 14, height = 10)
lapply(top_models_f, 
       FUN = plot_gam_effect, 
       ylabel = expression("Effect on K"))
dev.off()

## Time series
pdf("../output/QC_capelin_female_Condition_ts.pdf", width = 12, height = 10)
lapply(top_models_f, 
       FUN = plot_gam_ts, 
       data = capelin, 
       response = "K")
dev.off()

## Diagnostics
pdf("../output/QC_capeline_female_Condition_res.pdf", width = 14, height = 10)
lapply(top_models_f, 
       FUN = examine_gam_res, 
       data = capelin)
dev.off()
