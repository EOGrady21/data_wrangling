####################################################################
# Author: Dan Reed (Daniel.Reed@dfo-mpo.gc.ca)
# Purpose: PCA for Gulf of St. Lawrence
# Date: Wed Aug 07 11:56:00 2019
####################################################################

# Necessary libraries
library(ggplot2)
library(data.table)
library(dplyr)

# Load data tab delimited file
dt <- fread("../output/QC_assembled.csv")

# Principal component analysis for given set of variables
PCA <- function(data){
  # Remove incomplete rows & and duplicate observations
  dt <- na.omit(unique(data))
  
  # Perform PCA for given months, station and variables
  res <- prcomp(x = na.omit(dt[, -"year"]), scale. = TRUE, center = TRUE)
  
  # Add time to the scores for plotting purposes
  res$x <- data.table(year = dt$year, res$x)
  
  return(res)
}

##################################
# ZOOPLANKTON
###################################
# Columns to use
z_col <- paste(which(names(dt) %in% c("Metridia longa", "C. glacialis")), collapse = ":")
z_col <- c(which(names(dt) == "year"), eval(parse(text = z_col)))

# Perform PCA
zoo_pca <- PCA(dt[, z_col, with = FALSE])

###################################
# PHENOLOGY
###################################
# Columns to use
e_col <- which(names(dt) %in% c("SST warming", "Ice end day GSL+SS", "Ice area GSL+SS",
                                "Cfin I+II+III G1 max yearday Riki", "Cfin I+II+III G2 max yearday Riki",
                                "Cfin I+II+III G ratio max yearday Riki", 
                                "Bloom start", "Bloom duration"))
e_col <- c(which(names(dt) == "year"), rev(e_col))

# Perform PCA
phe_pca <- PCA(dt[, e_col, with = FALSE])

###################################
# PHYSICAL
###################################
y_col <- which(names(dt) %in% c("St. Lawrence river flux", 
                                "sle deep temperature", "wGSL deep temperature", "eGSL deep temperature",
                                "sle SST", "wGSL SST", "sGSL SST", "eGSL SST",
                                "NAO", "AMO", "CIL index GSL", "Ice volume GSL+SS"))
y_col <- c(which(names(dt) == "year"), y_col)

# Perform PCA
phy_pca <- PCA(dt[, y_col, with = FALSE])


###################################
# PLOTTING FUNCTIONS
###################################

# Plot loadings of first three PCs for each PCA
plot_loadings <- function(PCA_list){
  # List of plots
  p <- list()
  
  # Labels for y axis
  y_labels <- c("Physical", "Zooplankton", "Phenology")
  
  # Reordering data frame
  var_order <- list(phy = c(11, 7:9, 3, 4, 6, 5, 12, 10, 2, 1), 
                    zoo = c(12:14, 6, 1:5, 7:11), 
                    phe = c(1, 4:5, 8:6, 3:2))
  var_order <- lapply(var_order, rev)
  
  # Loop through PCAs 
  for(i in 1:length(PCA_list)){
    # Get current PCA
    PCA <- PCA_list[[i]]
    
    # Extract loadings from PCA
    loadings <- data.table(PC_var = rownames(PCA$rotation[var_order[[i]], ]), 
                           PCA$rotation[var_order[[i]], ])
    
    # Melt for the bar chart
    m_loadings <- melt(loadings, id = "PC_var")
    m_loadings[, label := ifelse(variable == "PC1", PC_var, "")]
    m_loadings[, PC_var := factor(PC_var, loadings$PC_var)]
    
    # Add line types
    m_loadings$line <- rep_len(c("dotted", "solid"), nrow(m_loadings))
    
    # Data frame for labels
    df_lab <- data.table(variable = c("PC1", "PC2", "PC3"), 
                         label = paste("(", letters[(i-1)*3+1:3], ")", sep = "")) 
    
    # Plot results
    p[[i]] <- ggplot(m_loadings %>% filter(variable %in% c("PC1", "PC2", "PC3")))
    p[[i]] <- p[[i]] + geom_text(aes(x = PC_var, y =-1.2, label = label), hjust = 0, vjust = 0.25, size = 1.75)
    p[[i]] <- p[[i]] + geom_vline(aes(xintercept = as.numeric(PC_var), linetype = line), 
                                  colour = "darkgray", alpha = 0.3, size = 0.5)
    p[[i]] <- p[[i]] + geom_text(aes(x = PC_var, y =-1.2, label = label), hjust = 0, vjust = 0.25, size = 1.75)
    p[[i]] <- p[[i]] + geom_bar(aes(x = PC_var, y = value), stat = "identity", fill = "salmon")
    p[[i]] <- p[[i]] + facet_wrap(~ variable) + coord_flip()
    p[[i]] <- p[[i]] + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                             panel.background = element_blank(),
                             axis.line = element_line(colour = "black", size = 1),
                             axis.text = element_text(size = 9, color = "black"),
                             axis.title = element_text(size = 10, color = "black"),
                             axis.ticks = element_line(size = 0.5, color = "black"),
                             text = element_text(family = "serif"),
                             axis.text.y=element_blank(),
                             panel.spacing = unit(0, "lines"),
                             strip.background = element_rect(fill = "transparent"),
                             strip.text = element_text(size = 8),
                             legend.position = "none",
                             plot.margin = unit(c(0, 0.5, -0.4, 0.25), "cm"))
    p[[i]] <- p[[i]] + xlab(y_labels[i]) + ylab("") 
    p[[i]] <- p[[i]] + scale_y_continuous(breaks = c(-0.5, 0, 0.5), limits = c(-1.2,1))
    if(i!=1)p[[i]] <- p[[i]] + theme(strip.text = element_blank())
    p[[i]] <- p[[i]] + geom_text(data = df_lab, aes(x = Inf, y = Inf, label = label), 
                                 size = 4, hjust = 1, vjust = 1, family = "serif")
  }
  
  # Create a grid of plots
  mygrid <- cowplot::plot_grid(plotlist = p, ncol=1, align="hv")
  
  # Plot figures
  print(mygrid)
  
  # Save figure
  ggsave("../output/QC_loadings.tiff", mygrid, dpi = 300, units = "cm", width = 16, height = 16, compression = "lzw")
}

# Plot time series of first three PCs for each PCA
plot_scores <- function(PCA_list){
  # List of plots
  p <- list()
  
  # Labels for y axis
  y_labels <- c("\nPhysical", "PC scores\nZooplankton", "\nPhenology")
  
  # Labels for panels
  panel_labels <- NULL
  
  # Get range of longest time series
  year_range <- range(lapply(PCA_list, FUN = function(x) range(x$x$year)))
  year_range <- data.table(year = eval(parse(text = paste(year_range, collapse = ":"))))
  
  # Extend all PCAs to longest range filling with NAs
  PCA_ext_list <- lapply(PCA_list, 
                         function(x){
                           x$x <- merge(year_range, x$x, all = TRUE)
                           return(x)
                         }
  )
  
  # Labels for the panels 
  panel_lab <- paste("(", letters[1:9], ")", sep = "")
  
  # Loop through PCAs 
  for(i in 1:length(PCA_ext_list)){
    # Get current PCA
    PCA <- PCA_ext_list[[i]]
    
    # Get proportion of variance for axis label
    panel_labels <- formatC(100 * summary(PCA)$importance[2, 1:3], format = "f", digits = 1)
    
    # Plot PC scores over time
    p[[1 + (i-1)*3]] <- ggplot(PCA$x %>% group_by(year) %>% summarise_each(funs(mean(., na.rm = TRUE)))) 
    p[[1 + (i-1)*3]] <- p[[1 + (i-1)*3]] + geom_line(aes_string(x = "year", y = "PC1"), lwd = 0.5, col = "grey45")
    p[[1 + (i-1)*3]] <- p[[1 + (i-1)*3]] + annotate("text", x = 1989, y = min(na.omit(PCA$x) %>% 
                                                                                group_by(year) %>% 
                                                                                summarise_each(funs(mean)) %>% 
                                                                                select(PC1)) + 
                                                      0.05 * diff(range(na.omit(PCA$x) %>% 
                                                                          group_by(year) %>% 
                                                                          summarise_each(funs(mean)) %>% 
                                                                          select(PC1))), 
                                                    label = paste(panel_labels[1], "%", sep=""), family = "serif", size = 3)
    p[[1 + (i-1)*3]] <- p[[1 + (i-1)*3]] + annotate("text", x = 2014, y = max(na.omit(PCA$x) %>% 
                                                                                group_by(year) %>% 
                                                                                summarise_each(funs(mean)) %>% 
                                                                                select(PC1)), 
                                                    label = panel_lab[1 + (i-1)*3], family = "serif", size = 3)
    p[[1 + (i-1)*3]] <- p[[1 + (i-1)*3]] + xlab("") + ylab(y_labels[i])
    p[[1 + (i-1)*3]] <- p[[1 + (i-1)*3]] + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                                                 panel.background = element_blank(), 
                                                 axis.line = element_line(colour = "black", size = 1),
                                                 axis.text = element_text(size = 9, color = "black"),
                                                 axis.title = element_text(size = 10, color = "black"),
                                                 axis.ticks = element_line(size = 0.5, color = "black"),
                                                 text = element_text(family = "serif"),
                                                 plot.title = element_text(hjust = 0.5, size = 10),
                                                 plot.margin = unit(c(-0.2, -0.1, -0.2, -0.5), "cm"))
    if(i<3){p[[1 + (i-1)*3]] <- p[[1 + (i-1)*3]] + theme(axis.text.x=element_blank())
    }else {p[[1 + (i-1)*3]] <- p[[1 + (i-1)*3]] + xlab("Year")}
    if(i==1)p[[1 + (i-1)*3]] <- p[[1 + (i-1)*3]] + ggtitle("PC1")
    
    p[[2 + (i-1)*3]] <- ggplot(PCA$x %>% group_by(year) %>% summarise_each(funs(mean))) 
    p[[2 + (i-1)*3]] <- p[[2 + (i-1)*3]] + geom_line(aes_string(x = "year", y = "PC2"), lwd = 0.5, col = "grey45")
    p[[2 + (i-1)*3]] <- p[[2 + (i-1)*3]] + annotate("text", x = 1989, y = min(na.omit(PCA$x) %>% 
                                                                                group_by(year) %>% 
                                                                                summarise_each(funs(mean)) %>% 
                                                                                select(PC2)) + 
                                                      0.05 * diff(range(na.omit(PCA$x) %>% 
                                                                          group_by(year) %>% 
                                                                          summarise_each(funs(mean)) %>% 
                                                                          select(PC2))), 
                                                    label = paste(panel_labels[2], "%", sep=""), family = "serif", size = 3)
    p[[2 + (i-1)*3]] <- p[[2 + (i-1)*3]] + annotate("text", x = 2014, y = max(na.omit(PCA$x) %>% 
                                                                                group_by(year) %>% 
                                                                                summarise_each(funs(mean)) %>% 
                                                                                select(PC2)), 
                                                    label = panel_lab[2 + (i-1)*3], family = "serif", size = 3)
    p[[2 + (i-1)*3]] <- p[[2 + (i-1)*3]] + xlab("") + ylab("")
    p[[2 + (i-1)*3]] <- p[[2 + (i-1)*3]]  + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                                                  panel.background = element_blank(), 
                                                  axis.line = element_line(colour = "black", size = 1),
                                                  axis.text = element_text(size = 9, color = "black"),
                                                  axis.title = element_text(size = 10, color = "black"),
                                                  axis.ticks = element_line(size = 0.5, color = "black"),
                                                  text = element_text(family = "serif"),
                                                  plot.title = element_text(hjust = 0.5, size = 10),
                                                  plot.margin = unit(c(-0.2, -0.1, -0.2, -0.5), "cm"))
    if(i<3){p[[2 + (i-1)*3]] <- p[[2 + (i-1)*3]] + theme(axis.text.x=element_blank())
    }else {p[[2 + (i-1)*3]] <- p[[2 + (i-1)*3]] + xlab("Year")}
    if(i==1)p[[2 + (i-1)*3]] <- p[[2 + (i-1)*3]] + ggtitle("PC2")
    
    p[[3 + (i-1)*3]] <- ggplot(PCA$x %>% group_by(year) %>% summarise_each(funs(mean))) 
    p[[3 + (i-1)*3]] <- p[[3 + (i-1)*3]]+ geom_line(aes_string(x = "year", y = "PC3"), lwd = 0.5, col = "grey45")
    p[[3 + (i-1)*3]] <- p[[3 + (i-1)*3]] + annotate("text", x = 1989, y = min(na.omit(PCA$x) %>% 
                                                                                group_by(year) %>% 
                                                                                summarise_each(funs(mean)) %>% 
                                                                                select(PC3)) + 
                                                      0.05 * diff(range(na.omit(PCA$x) %>% 
                                                                          group_by(year) %>% 
                                                                          summarise_each(funs(mean)) %>% 
                                                                          select(PC3))), 
                                                    label = paste(panel_labels[3], "%", sep=""), family = "serif", size = 3)
    p[[3 + (i-1)*3]] <- p[[3 + (i-1)*3]] + annotate("text", x = 2014, y = max(na.omit(PCA$x) %>% 
                                                                                group_by(year) %>% 
                                                                                summarise_each(funs(mean)) %>% 
                                                                                select(PC3)), 
                                                    label = panel_lab[3 + (i-1)*3], family = "serif", size = 3)
    p[[3 + (i-1)*3]] <- p[[3 + (i-1)*3]] + xlab("") + ylab("")
    p[[3 + (i-1)*3]] <- p[[3 + (i-1)*3]]  + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                                                  panel.background = element_blank(), 
                                                  axis.line = element_line(colour = "black", size = 1),
                                                  axis.text = element_text(size = 9, color = "black"),
                                                  axis.title = element_text(size = 10, color = "black"),
                                                  axis.ticks = element_line(size = 0.5, color = "black"),
                                                  text = element_text(family = "serif"),
                                                  plot.title = element_text(hjust = 0.5, size = 10),
                                                  plot.margin = unit(c(-0.2, -0.1, -0.2, -0.25), "cm"))
    if(i<3){p[[3 + (i-1)*3]] <- p[[3 + (i-1)*3]] + theme(axis.text.x=element_blank())
    } else {p[[3 + (i-1)*3]] <- p[[3 + (i-1)*3]] + xlab("Year")}
    if(i==1)p[[3 + (i-1)*3]] <- p[[3 + (i-1)*3]] + ggtitle("PC3")
  }
  
  # Create a grid of plots
  mygrid <- cowplot::plot_grid(plotlist = p, ncol=3, align="hv")
  mygrid <- cowplot::plot_grid(mygrid, scale = 0.95)
  
  ggsave("../output/QC_scores.tiff", mygrid, dpi = 300, units = "cm", width = 16, height = 16 * 45/60, compression = "lzw")
}


# Create list of PCAs
PCA_list <- list(phy_pca, zoo_pca, phe_pca)

# Plot results
plot_loadings(PCA_list)
plot_scores(PCA_list)

###################################
# OUTPUT PCs TO FILE
###################################
# Get first three principal components to output to file
PC_out <- data.table(phy_pca$x[, .(year, phy_PC1 = PC1, phy_PC2 = PC2, phy_PC3 = PC3)])
PC_out <- merge(PC_out, 
                zoo_pca$x[, .(zoo_PC1 = mean(PC1), zoo_PC2 = mean(PC2), zoo_PC3 = mean(PC3)), by = "year"], 
                by = "year", all = TRUE)
PC_out <- merge(PC_out, 
                phe_pca$x[, .(phe_PC1 = mean(PC1), phe_PC2 = mean(PC2), phe_PC3 = mean(PC3)), by = "year"], 
                by = "year", all = TRUE)

# Write to file
fwrite(PC_out, "../output/QC_PCs.csv", na = NA)

# Output PCAs to file
names(PCA_list) <- c("QC_PCA_phy", "QC_PCA_zoo", "QC_PCA_phe")
QC_PCA <- PCA_list
save(QC_PCA, file = "../output/QC_PCA.RData")

# View correlations
cor(PC_out[, -1], use = "pairwise.complete")
