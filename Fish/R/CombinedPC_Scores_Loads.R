####################################################################
# Author: Dan Reed (Daniel.Reed@dfo-mpo.gc.ca)
# Purpose: Compare zooplankton PC1 for all regions
# Date: Fri Apr 06 12:07:33 2018
####################################################################

# Load libraries
library(dplyr)
library(magrittr)
library(stringr)
library(ggplot2)
library(cowplot)
library(colorspace)
library(scales)

# Load PCAs
load("../output/SS_PCA.RData")
load("../output/NL_PCA.RData")
load("../output/QC_PCA.RData")

# Combine lists of PCAs
PCAs <- c(NL_PCA, QC_PCA, SS_PCA)

# Filter to just zooplankton PCAs
PCAs <- PCAs[grep("zoo", names(PCAs))]

# Create plot of loads 
plot_loads <- function(PCAs, PC_n, show_labels = TRUE, show_regions = TRUE, pane_labels){
  # List of plots
  g <- vector("list", 3)
  
  # Order of bars
  bar_order <- list(NL = c(2, 4, 3, 15, 10:12, 17, 13, 14, 16, 9, 7, 1, 6, 5, 8),
                    QC = c(12:14, 6, 1:5, 7:11),
                    SS = c(1, 3, 2, 13, 4:12, 14:20))
  bar_order <- lapply(bar_order, rev)
  
  # Region labels
  region_labs <- c("NLS", "GSL", "SS")
  
  # Region colours
  region_cols <- lighten(c("#ff6666", "darkorchid1", "#66ccff"), 0.2, space = "HLS")
  
  # Loop through PCAs
  for(i in 1:3){
    # Extract first PC and add label names
    df <- as.data.frame(PCAs[[i]]$rotation) %>% select(PC_n)
    df$labels <- factor(gsub("\\.", " ", rownames(df)), levels = gsub("\\.", " ", rownames(df))[bar_order[[i]]])
    df$lines <- NA
    df$lines[bar_order[[i]]] <- c("solid", "dotted")

    # Plot results
    g[[i]] <- ggplot(data = df)
    g[[i]] <- g[[i]] + geom_bar(aes_string(x = "labels", y = colnames(df)[1]), stat = "identity", fill = "white", colour = NA)
    g[[i]] <- g[[i]] + geom_vline(aes(xintercept = as.numeric(labels), linetype = lines), colour = "darkgray", alpha = 0.3, size = 1.25)
    g[[i]] <- g[[i]] + kitchendraw::theme_ocean()
    g[[i]] <- g[[i]] + theme(axis.text.y=element_blank(),
                             legend.position = "none",
                             text = element_text(family = "serif"),
                             axis.title = element_text(size = 20),
                             axis.text.x = element_text(size = 20),
                             panel.grid.major = element_blank())
    g[[i]] <- g[[i]] + geom_bar(aes_string(x = "labels", y = colnames(df)[1]), stat = "identity", fill = region_cols[i], colour = NA)
    if(show_labels)g[[i]] <- g[[i]] + geom_label(aes(x = labels, label = labels, y = -1), size = 3.5, hjust = 0, vjust = 0.25, 
                                                 family = "serif", fill = "white", label.size = unit(0, "lines"), label.padding = unit(0.1, "lines"))
    g[[i]] <- g[[i]] + coord_flip() + xlab(ifelse(show_regions, region_labs[i], "")) 
    if(i != 3) g[[i]] <- g[[i]] + theme(axis.text.x = element_blank()) + ylab("")
    else g[[i]] <- g[[i]] + ylab("Loadings")
    if(show_labels) g[[i]] <- g[[i]] + ylim(c(-1, 0.5)) 
    else g[[i]] <- g[[i]] + ylim(c(-0.6, 0.5)) 
    g[[i]] <- g[[i]] + annotate("text", y = Inf, x = -Inf, label = pane_labels[i], family = "serif", size = 7, vjust = -0.5, hjust = 1.25)
  }
  
  # Return plot grid
  plot_grid(plotlist = g, ncol = 1)
}

p_list <- vector("list", 4)
p_list[[1]] <- plot_loads(PCAs, 1, TRUE, TRUE, c("(a)", "(c)", "(e)"))

# Create plot of scores
plot_scores <- function(PCAs, PC_n, pane_labels){
  # List of plots
  g <- vector("list", 3)
  
  # Data frame for arrow annotations
  arrow_df <- data.frame(ymin = c(-4, NA, -2.5), ymax = c(-0.5, NA, 1.3), xmin = c(2003, NA, 2005), xmax = c(2014, NA, 2016))
  
  # Arrow annotation text
  arrow_text <- c("Increasing Calanus", NA, "Declining Calanus")
  
  # Loop through PCAs
  for(i in 1:3){
    # Deviance explained
    dev_expl <- paste(round(summary(PCAs[[i]])$importance[2, PC_n] * 100, 1), "%", sep = "")
    
    # Get time series for given PC
    df <- PCAs[[i]]$x %>% group_by(year) %>% summarise_all(mean) %>% select(c(1, PC_n + 1))
    
    # Plot results
    g[[i]] <- ggplot(data = df)
    g[[i]] <- g[[i]] + geom_line(aes_string(x = "year", y = colnames(df)[2]), size = 1.25, colour = "gray")
    g[[i]] <- g[[i]] + kitchendraw::theme_ocean()
    g[[i]] <- g[[i]] + theme(text = element_text(family = "serif"),
                             axis.text = element_text(size = 20),
                             axis.title = element_text(size = 20))
    g[[i]] <- g[[i]] + xlim(c(1992, 2017)) + xlab(ifelse(i == 3, "Year", "")) + ylab("")
    if(i != 3) g[[i]] <- g[[i]] + theme(axis.text.x = element_blank())
    g[[i]] <- g[[i]] + annotate("text", 1996, y = -Inf, label = dev_expl, vjust = -0.5, family = "serif", size = 7)
    g[[i]] <- g[[i]] + annotate("text", 2015, y = -Inf, label = pane_labels[i], vjust = -0.5, family = "serif", size = 7)
    g[[i]] <- g[[i]] + scale_y_continuous(breaks = pretty_breaks())
  }
  
  # Return plot grid
  plot_grid(plotlist = g, ncol = 1)
}

p_list[[2]] <- plot_scores(PCAs, 1, c("(b)", "(d)", "(f)"))

title <- ggdraw() + draw_label("Zooplankton PC1", fontfamily = "serif", size = 22)

PC1 <- plot_grid(plotlist = p_list[1:2])
PC1 <- plot_grid(title, PC1, rel_heights=c(0.05, 1), ncol = 1)

ggsave("../output/Combine_PCAs_pres.tiff", PC1, height = 12, width = 8, dpi = 300, compression = "lzw")
