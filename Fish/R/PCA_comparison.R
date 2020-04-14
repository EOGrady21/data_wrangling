####################################################################
# Author: Dan Reed (Daniel.Reed@dfo-mpo.gc.ca)
# Purpose: Compare PCAs from each region
# Date: Thu Feb 22 14:02:02 2018
####################################################################

# Load libraries
library(dplyr)
library(magrittr)
library(stringr)

# Load PCAs
load("../output/SS_PCA.RData")
load("../output/NL_PCA.RData")
load("../output/QC_PCA.RData")

# Combine lists of PCAs
PCAs <- c(SS_PCA, NL_PCA, QC_PCA)

# Function for extracting PC matrix from each PCA and prepending appropriate tag
rename_PCs <- function(PCA_name, PCAs){
  # Extract PCA
  PCA <- PCAs[[PCA_name]]
  
  # Get tag to prepend to PCs
  tag <- paste(str_extract(PCA_name, "^[:alpha:]{2}"), str_extract(PCA_name, "[^_]+$"),  sep = "_")
 
  # Add tag to PC names 
  colnames(PCA$x)[-1] <-  paste(tag, colnames(PCA$x)[-1], sep = "_") 
  
  # Only return PC matrix (annual average) and only first three PCs
  PCA$x %>% group_by(year) %>% summarise_all(mean) %>% select(1:4)
}

# Rename PCs, extract scores
PCA_scores <- lapply(names(PCAs), rename_PCs, PCAs = PCAs)

# Rename list elements
names(PCA_scores) <- names(PCAs)

# Function for merging various PCAs
merge_PCAs <- function(group_name){
  tmp <- Reduce(function(x, y) merge(x, y, by="year", all = TRUE), 
                PCA_scores[grepl(group_name, names(PCA_scores))])
}

# Consolidate PCAs into data frame for each group
phe <- merge_PCAs("phe")
phy <- merge_PCAs("phy")
zoo <- merge_PCAs("zoo")

# Expand names
PCA_names <- c(zoo = "Zooplankton", phe = "Phenology", phy = "Physical")

# Function for plotting correlation matrix
plot_pearsons <- function(PCs){
  # Calculate correlation coefficient
  df.cor <- melt(cor(PCs[,-1], use = "pairwise.complete"))
   
  # Fix names
  df.cor[, 1:2] <- lapply(df.cor[, 1:2], function(x)gsub("_[[:alnum:]].*_", " ", x))

  # Remove duplicates
  df.cor <-  df.cor[!duplicated(t(apply(df.cor, 1, sort))),]

  # Reorder for plotting
  df.cor <- cbind(as.data.frame(t(apply(as.matrix(df.cor)[,1:2], MAR = 1, sort))), value = as.numeric(df.cor[,3]))
  df.cor[df.cor$V1==df.cor$V2, "value"] <- NA
  df.cor$V2 <- with(df.cor, factor(V2, levels = rev(levels(V1))))

  # Plot results
  p <- ggplot(df.cor) + geom_tile(aes(V1, V2, fill=value)) 
  p <- p + geom_text(aes(V1, V2, label=round(value, 2)), family = "serif", size = 7)
  p <- p + theme(panel.grid.minor = element_blank(), 
                 panel.background = element_blank(), 
                 axis.line = element_line(colour = "black", size = 1),
                 axis.text = element_text(size = 18, color = "black"),
                 axis.title = element_text(size = 20, color = "black"),
                 axis.ticks = element_line(size = 1, color = "black"),
                 text = element_text(family = "serif"),
                 plot.title = element_text(hjust = 0.5, size = 30),
                 axis.text.x = element_text(angle = 90, vjust = 0.25),
                 legend.key.height = unit(3, "cm"),
                 legend.text = element_text(size = 18),
                 legend.title = element_text(size = 20),
                 panel.grid.major = element_line(size = 1, linetype = "dotted", colour = "gray90"))
  p <- p + xlab("") + ylab("")
  p <- p + scale_fill_gradient2(low="#74add1", high="#f46d43", mid = "white", midpoint = 0, limits = c(-1, 1), na.value = "transparent") + labs(fill = "Correlation")
  p <- p + ggtitle(PCA_names[gsub(".*_(.*)_.*", "\\1", colnames(PCs)[2])])
  
  # Create auxiliary data frame for plotting boxes
  aux.df <- ggplot_build(p)$data[[2]]
  aux.df %<>% filter(abs(label) >= 0.5, label != 1) 

  # Plot squares around correlated variables
  p <- p + geom_rect(data = aux.df, aes(xmin = x - 0.5, xmax = x + 0.5, ymin = y - 0.5, ymax = y + 0.5), size = 1.5, fill = NA, colour = "black")
  
  ggsave(paste("../output/PC_cf_", gsub(".*_(.*)_.*", "\\1", colnames(PCs)[2]), ".png", sep = ""), 
         p, width = 12, height = 11)
}

# Plot all variable groupings
plot_pearsons(phe)
plot_pearsons(phy)
plot_pearsons(zoo)



