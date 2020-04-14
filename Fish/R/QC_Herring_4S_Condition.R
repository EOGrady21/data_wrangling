####################################################################
# Author: Dan Reed (Daniel.Reed@dfo-mpo.gc.ca)
# Purpose: GAMs fit, ranked to GSL herring condition
# Date: Thu Aug 08 08:42:37 2019
####################################################################

# Load necessary libraries
library(data.table)
library(ACCASPR)

# Load principal component data
PC_data <- fread("../output/QC_PCs.csv")

# Load data
herring <- fread("../data/4S_herring_K_stats.txt")

# Adjust column names
colnames(herring) <- c("year", "K_autumn", "K_spring", "n_autumn", "n_spring")

# Join data sets by year
herring <- PC_data[herring, , on = "year"]

# Get name of predictors
predictors <- colnames(herring)[grep("PC",colnames(herring))]

###################################
# AUTUMN
###################################
# Run all GAMs: predictors, response variables, # knots, max # terms, data
res_gam_A <- run_gams(predictors, "K_autumn", 3, 3, 
                      na.omit(herring[, (-grep("spring", names(herring))), with = FALSE]))

# Filter those with excessive concurvity
res_gam_filtered_A <- filter_gams(res_gam_A, 0.5)

# Get top 5 GAMs ranked by AIC
top_models_A <- rank_gams(res_gam_filtered_A, 5)

# Tabulate results
tabulate_gams(top_models_A)

## Partial residual plots
pdf("../output/QC_herring_4S_autumn_condition_effect.pdf", width = 14, height = 10)
lapply(top_models_A, 
       FUN = plot_gam_effect, 
       ylabel = expression("Effect on K"))
dev.off()

## Time series
pdf("../output/QC_herring_4S_autumn_condition_ts.pdf", width = 12, height = 10)
lapply(top_models_A, 
       FUN = plot_gam_ts, 
       data = herring, 
       response = "K")
dev.off()

## Diagnostics
pdf("../output/QC_herring_4S_autumn_condition_res.pdf", width = 14, height = 10)
lapply(top_models_A, 
       FUN = examine_gam_res, 
       data = herring)
dev.off()

###################################
# SPRING
###################################
# Run all GAMs: predictors, response variables, # knots, max # terms, data
res_gam_S <- run_gams(predictors, "K_spring", 3, 3, 
                      na.omit(herring[, (-grep("autumn", names(herring))), with = FALSE]))

# Filter those with excessive concurvity
res_gam_filtered_S <- filter_gams(res_gam_S, 0.5)

# Get top 5 GAMs ranked by AIC
top_models_S <- rank_gams(res_gam_filtered_S, 5)

# Tabulate results
tabulate_gams(top_models_S)

## Partial residual plots
pdf("../output/QC_herring_4S_spring_condition_effect.pdf", width = 14, height = 10)
lapply(top_models_S, 
       FUN = plot_gam_effect, 
       ylabel = expression("Effect on K"))
dev.off()

## Time series
pdf("../output/QC_herring_4S_spring_condition_ts.pdf", width = 12, height = 10)
lapply(top_models_S, 
       FUN = plot_gam_ts, 
       data = herring, 
       response = "K")
dev.off()

## Diagnostics
pdf("../output/QC_herring_4S_spring_condition_res.pdf", width = 14, height = 10)
lapply(top_models_S, 
       FUN = examine_gam_res, 
       data = herring)
dev.off()