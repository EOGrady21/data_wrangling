####################################################################
# Author: Dan Reed (Daniel.Reed@dfo-mpo.gc.ca)
# Purpose: Create bubble plot to look at commonalities
# Date: Wed Apr 04 10:02:48 2018
####################################################################

# Load libraries
library(readxl)
library(dplyr)
library(ggthemes)
library(tidyverse)
library(forcats)
library(cowplot)
library(magrittr)

###################################
# BUBBLE PLOT
###################################

#Load data
data <- read_xlsx("../data/cf.xlsx")

# Categorise significance by size
sig_size <- function(p){
  ifelse(p > 0.05, 1,
         ifelse(p <= 0.05 & p > 0.01, 1,
                ifelse(p <= 0.01 & p > 0.001, 2,
                       ifelse(p <= 0.001, 3, NA))))
}

# Categorise significance by symbol
sig_sym <- function(p){
  ifelse(p > 0.05, 0,
         ifelse(p <= 0.05 & p > 0.01, 1,
                ifelse(p <= 0.01 & p > 0.001, 1,
                       ifelse(p <= 0.001, 1, NA))))
}

data %<>%
  mutate(Significance = as.numeric(Significance),
         Fish = as.factor(paste(gsub("_", " ", Fish), " (", Region, ")", sep = ""))) %>%
  mutate(sig_size = as.factor(sig_size(Significance)),
         terms = as.factor(paste(Variable_group, PC, sep = "_")),
         sig_sym = as.factor(sig_sym(Significance)),
         dev_lab = paste(Dev_expl, "%", sep = ""))

# Create plot for metric 'm'
plot_model <- function(m, null_labels = TRUE){
  df <- data %>% filter(Metric == m)

  # Reshuffle regions, fish stocks, and GAM predictors
  df$Region <- factor(df$Region, levels = rev(c("NL", "GSL", "SS")))
  df$Fish <- factor(df$Fish, levels = unique(df$Fish[order(df$Region)]))
  df$terms <- factor(df$terms, levels = c("Phy_1", "Phy_2", "Phy_3", "Zoo_1", "Zoo_2", "Zoo_3", "Phe_1", "Phe_2", "Phe_3", "SSB_0"))

  # y-axis labels -- if required
  if(null_labels){
    y_labs <- NULL
  }else{
    y_labs <- c("Phy 1", "Phy 2", "Phy 3",
                "Zoo 1", "Zoo 2", "Zoo 3",
                "Phe 1", "Phe 2", "Phe 3", "SSB")
  }

  p <- ggplot() + ggtitle(c(K = "(a) Condition", A = "(b) Abundance", R = "(c) Recruitment")[m])
  p <- p + geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = 0.5, ymax = 3.5), fill = "#ff6666", alpha = 0.15, colour = NA)
  p <- p + geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = 3.5, ymax = 6.5), fill = "darkorchid1", alpha = 0.15, colour = NA)
  p <- p + geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = 6.5, ymax = 9.5), fill = "lightgray", alpha = 0.15, colour = NA)
  p <- p + geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = 9.5, ymax = 10.5), fill = "#66ccff", alpha = 0.15, colour = NA)
  p <- p + geom_point(data = df, aes(y = terms, 
                                     x = Fish, 
                                     colour = Variable_group, 
                                     shape = sig_sym, 
                                     size = sig_size,
                                     stroke = ifelse(sig_sym == 0, 2, 1)))
  p <- p + coord_flip() + xlab("") +ylab("")
  p <- p + theme_tufte() + theme(legend.position = "none",
                                 axis.text = element_text(size = 17),
                                 axis.line = element_line(size = 1),
                                 panel.grid = element_line(size = 0.5, 
                                                           linetype = "dashed",
                                                           colour = scales::alpha("gray50", 0.5)),
                                 plot.margin = unit(c(1,2,1,1), "lines"),
                                 plot.title = element_text(size = 20))
  p <- p + scale_size_manual(values = c(5, 10, 15)) + scale_shape_manual(values = c(4, 19))
  p <- p + scale_y_discrete(labels = y_labs)
  p <- p + scale_colour_manual(values =  c("gray", "#ff6666", "#66ccff", "darkorchid1"))
  p <- p + geom_label(data = df, aes(y = 11.1, x = Fish, label = dev_lab),
                      fontface = 1, family = "serif", fill = "white", label.size = unit(0, "lines"), size = 6)
  gt <- ggplot_gtable(ggplot_build(p))
  gt$layout$clip[gt$layout$name == "panel"] <- "off"
  gt
}

# Create each plot
p1 <- plot_model("K")
p2 <- plot_model("A")
p3 <- plot_model("R", FALSE)

# Legend data frame
leg_df <- data.frame(x = (1:4)*1.2, y = 1,
                     size = as.factor(c(3:1, 1)),
                     labs = c("p \u2264 0.001", "0.001 < p \u2264 0.01  ", "0.01 < p \u2264 0.05", "p > 0.05"),
                     shape = as.factor(c(rep(1, 3), 2)),
                     n_x = c(0.1, 0.1, 0.2, 0.1))
# Legend plot
g <- ggplot()
g <- g + geom_point(data = leg_df, aes(x = x, 
                                       y = y, 
                                       size = size, 
                                       shape = shape,
                                       stroke = ifelse(shape == 2, 2, 1)))
g <- g + scale_size_manual(values = c(5, 10, 15)) + scale_shape_manual(values = c(19, 4))
g <- g + theme_void() + theme(legend.position = "none",
                              plot.margin = unit(c(0,1,0,1), "cm"))
g <- g + geom_text(data = leg_df, aes(x = x, y = y, label = labs), nudge_y = -0.015, size = 7, family = "serif")
g <- g + ylim(c(0.98, 1.01)) + xlim(c(0, 5))

# Combine plots and save
cp <- plot_grid(p1, p2, p3, g, ncol = 1, rel_heights = c(1.2, 1, 1, 0.2))
ggsave("../output/bubbles.tiff", width = 10, height = 18, dpi = 300, compression = "lzw")


###################################
# Bar chart of variable groups
###################################
data <- read_xlsx("../data/cf.xlsx")

# Reorder factors for plotting
data %<>% 
  mutate(Variable_group = factor(Variable_group, levels = rev(c("Phy", "Zoo", "Phe", "SSB"))),
         Metric = factor(Metric, levels = c("K", "A", "R")))

# Convert significance to a numeric
data %<>% 
  mutate(Significance = as.numeric(Significance))

g <- ggplot(data %>% filter(Optimal_model == 1, Significance < 0.05), aes(x = Variable_group)) 
g <- g + geom_bar(position = "dodge", colour = NA, aes(fill = Variable_group), alpha = 0.5)
g <- g + geom_text(stat = "count", 
                   aes(label = paste(round(100 * (..count..)/tapply(..count..,..PANEL..,sum)[..PANEL..]), "%   ", sep = "")),
                   nudge_y = 1.35,
                   size = 6,
                   family = "serif")
g <- g + facet_wrap(~ Metric, 
                    labeller = as_labeller(c("A" = "Abundance", "K" = "Condition", "R" = "Recruitment")),
                    ncol = 1)
g <- g + theme(panel.background = element_blank(),
               panel.border = element_blank(),
               text = element_text(family = "serif", size = 20),
               strip.background = element_blank(),
               axis.line = element_line(size = 1),
               legend.position = "none",
               axis.text = element_text(size = 18, family = "serif"))
g <- g + ylab("# of significant terms") + xlab("") + coord_flip()
g <- g + scale_y_continuous(breaks = 0:8) 
g <- g + scale_x_discrete(labels = rev(c("Physical", "Zooplankton", "Phenology", "SSB")))
g <- g + scale_fill_manual(values = c("#66ccff", "gray",  "darkorchid1", "#ff6666"))
g <- g + geom_text(data = data.frame(y = 9, x = 4.4, lab = paste("(", letters[1:3], ")", sep = ""), Metric = c("K", "A", "R")), 
                   aes(x, y, label = lab), size = 6, family = "serif")
ggsave("../output/barchart.tiff", width = 5, height = 10, dpi = 300, compression = "lzw")