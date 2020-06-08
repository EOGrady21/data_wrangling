library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)

# load  data
tb <- dplyr::bind_rows(read_csv("../../Fish/output/HL2_assembled.csv") %>%
                         dplyr::mutate(label="Reed") %>%
                         dplyr::mutate(NAO=scale(NAO)),
                       read_csv("../../Steele/output/HL2_assembled_updated.csv") %>%
                         dplyr::mutate(label="Steele") %>%
                         dplyr::mutate(`Biomass max yearday HL2` = `Zooplankton max yearday HL2`),
                       read_csv("../output/HL2_assembled_BC.csv") %>%
                         dplyr::mutate(label="Casault"))

#------------------------------------------------------------------------------
# subset "physical" data
phys_var <- c("SST 4V","SST 4W","SST 4XSS","SST 4eGoM + BoF",
              "Bottom temp 4V","Bottom temp 4W","Bottom temp 4X","CIL volume SS",
              "St. Lawrence river flux","AMO","NAO", "Stratification", "Ice volume GSL+SS")

tb_phys <- tb %>%
  dplyr::select(c("year", phys_var, "label")) %>%
  dplyr::filter(year>=1999, year<=2019) %>%
  dplyr::distinct() %>%
  tidyr::gather(variable, value, phys_var) %>%
  dplyr::mutate(value=ifelse(value==-99, NA, value)) %>%
  dplyr::filter(!is.na(value))

# plot data
# set x-axis
x_limits <- c(min(tb_phys$year)-1, max(tb_phys$year)+1)
x_breaks <- seq(x_limits[1], x_limits[2], by=1)
x_labels <- x_breaks

# # set y-axis
# y_limits <- c(min(tb_phys$value, na.rm=T) - 0.1*mean(tb_phys$value, na.rm=T),
#               max(tb_phys$value, na.rm=T) + 0.1*mean(tb_phys$value, na.rm=T))

# plot data
p <-  ggplot2::ggplot() +
  # plot data - dots
  ggplot2::geom_point(data=tb_phys,
                      mapping=ggplot2::aes(x=year, y=value, color=label, shape=label),
                      show.legend=T,
                      size=1.5,
                      alpha=0.3,
                      # position=position_jitter(width = 0.075, height = 0.075)) +
                      position="identity") +
  # set coordinates system and axes
  ggplot2::coord_cartesian() +
  ggplot2::scale_x_continuous(name="Year", limits=x_limits, breaks=x_breaks, labels=x_labels, expand=c(0, 0)) +
  ggplot2::scale_y_continuous(name="", expand=c(0.1, 0.1)) +
  ggplot2::scale_color_manual(name="", values=c("red", "green", "blue")) +
  ggplot2::scale_shape_manual(name="", values=c(15, 16, 18)) +
  ggplot2::facet_wrap(vars(variable), ncol=2, scales="free_y")

# customize theme
p <- p +
  ggplot2::theme_bw() +
  # ggplot2::ggtitle(paste(ls_data$dataset, ls_data$variable, ls_data$units, sep=" : " )) +
  ggplot2::theme(
    text=ggplot2::element_text(size=8),
    axis.text.x=ggplot2::element_text(colour="black", angle=90, hjust=0.5, vjust=0.5),
    plot.title=ggplot2::element_text(colour="black", hjust=0, vjust=0, size=8),
    panel.grid.major=ggplot2::element_blank(),
    panel.border=ggplot2::element_rect(size=0.25, colour="black"),
    legend.direction="horizontal",
    legend.position="bottom",
    plot.margin=grid::unit(c(0.1,0.1,0.1,0.1), "cm"))

# print to pdf
pdf(file="~/Projects/fish_tmp/figures/Physical_Variables.pdf",
    paper="legal", width=8, height=13.5)
print(p)
dev.off()

# #------------------------------------------------------------------------------
# # subset "phenology" data - single region
# phen_var <- list(c("Ice area GSL+SS", "Ice end day GSL+SS",
#                    "Cfin I+II+III max yearday HL2", "Cfin max yearday HL2",
#                    "Biomass max yearday HL2"),
#                  c("SST warming", "Bloom start", "Bloom duration"))
# 
# for(i in seq(1:length(phen_var))){
#   tb_phen <- tb %>%
#     dplyr::select(c("year", phen_var[[i]], "label")) %>%
#     dplyr::filter(year>=1999, year<=2019) %>%
#     dplyr::distinct() %>%
#     tidyr::gather(variable, value, phen_var[[i]]) %>%
#     dplyr::mutate(value=ifelse(value==-99, NA, value)) %>%
#     dplyr::filter(!is.na(value))
#   
#   # plot data
#   # set x-axis
#   x_limits <- c(min(tb_phen$year)-1, max(tb_phen$year)+1)
#   x_breaks <- seq(x_limits[1], x_limits[2], by=1)
#   x_labels <- x_breaks
#   
#   # # set y-axis
#   # y_limits <- c(min(tb_phen$value, na.rm=T) - 0.1*mean(tb_phen$value, na.rm=T),
#   #               max(tb_phen$value, na.rm=T) + 0.1*mean(tb_phen$value, na.rm=T))
#   
#   # plot data
#   p <-  ggplot2::ggplot() +
#     # plot data - dots
#     ggplot2::geom_point(data=tb_phen,
#                         mapping=ggplot2::aes(x=year, y=value, color=label),
#                         show.legend=T,
#                         size=1.5,
#                         position=position_jitter(width = 0.075, height = 0.075)) +
#     # set coordinates system and axes
#     ggplot2::coord_cartesian() +
#     ggplot2::scale_x_continuous(name="Year", limits=x_limits, breaks=x_breaks, labels=x_labels, expand=c(0, 0)) +
#     ggplot2::scale_y_continuous(name="", expand=c(0.1, 0.1)) +
#     ggplot2::scale_color_manual(name="", values=c("red", "green", "blue"))
#   
#   if(i==1){
#     p <- p + ggplot2::facet_wrap(vars(variable), ncol=2, scales="free_y")
#   } else {
#     if(i==2){
#       p <- p + ggplot2::facet_wrap(vars(variable, region), ncol=2, scales="free_y")
#     }
#   }
#   
#   # customize theme
#   p <- p +
#     ggplot2::theme_bw() +
#     # ggplot2::ggtitle(paste(ls_data$dataset, ls_data$variable, ls_data$units, sep=" : " )) +
#     ggplot2::theme(
#       text=ggplot2::element_text(size=8),
#       axis.text.x=ggplot2::element_text(colour="black", angle=90, hjust=0.5, vjust=0.5),
#       plot.title=ggplot2::element_text(colour="black", hjust=0, vjust=0, size=8),
#       panel.grid.major=ggplot2::element_blank(),
#       panel.border=ggplot2::element_rect(size=0.25, colour="black"),
#       legend.direction="horizontal",
#       legend.position="bottom",
#       plot.margin=grid::unit(c(0.1,0.1,0.1,0.1), "cm"))
#   
#   # print to pdf
#   pdf(file=paste("~/Projects/fish_tmp/figures/Phenological_Variables_",i,".pdf", sep=""),
#       paper="legal", width=8, height=13.5)
#   print(p)
#   dev.off()
# }

#------------------------------------------------------------------------------
# subset "phenology" data - single then multiple regions
phen_var <- list(c("Ice area GSL+SS", "Ice end day GSL+SS",
                   "Cfin I+II+III max yearday HL2", "Cfin max yearday HL2",
                   "Biomass max yearday HL2"),
                 c("SST warming", "Bloom start", "Bloom duration"))

for(i in seq(1:length(phen_var))){
  if(i==1){
    tb_phen <- tb %>%
      dplyr::select(c("year", phen_var[[i]], "label")) %>%
      dplyr::filter(year>=1999, year<=2019) %>%
      dplyr::distinct() %>%
      tidyr::gather(variable, value, phen_var[[i]]) %>%
      dplyr::mutate(value=ifelse(value==-99, NA, value)) %>%
      dplyr::filter(!is.na(value))
  } else {
    if(i==2){
      tb_phen <- tb %>%
        dplyr::select(c("year", "region", phen_var[[i]], "label")) %>%
        dplyr::filter(year>=1999, year<=2019) %>%
        dplyr::distinct() %>%
        tidyr::gather(variable, value, phen_var[[i]]) %>%
        dplyr::mutate(value=ifelse(value==-99, NA, value)) %>%
        dplyr::filter(!is.na(value))
    }
  }
  
  # plot data
  # set x-axis
  x_limits <- c(min(tb_phen$year)-1, max(tb_phen$year)+1)
  x_breaks <- seq(x_limits[1], x_limits[2], by=1)
  x_labels <- x_breaks
  
  # # set y-axis
  # y_limits <- c(min(tb_phen$value, na.rm=T) - 0.1*mean(tb_phen$value, na.rm=T),
  #               max(tb_phen$value, na.rm=T) + 0.1*mean(tb_phen$value, na.rm=T))
  
  # plot data
  p <-  ggplot2::ggplot() +
    # plot data - dots
    ggplot2::geom_point(data=tb_phen,
                        mapping=ggplot2::aes(x=year, y=value, color=label, shape=label),
                        show.legend=T,
                        size=1.5,
                        alpha=0.3,
                        # position=position_jitter(width = 0.075, height = 0.075)) +
                        position="identity") +
    # set coordinates system and axes
    ggplot2::coord_cartesian() +
    ggplot2::scale_x_continuous(name="Year", limits=x_limits, breaks=x_breaks, labels=x_labels, expand=c(0, 0)) +
    ggplot2::scale_y_continuous(name="", expand=c(0.1, 0.1)) +
    ggplot2::scale_color_manual(name="", values=c("red", "green", "blue")) +
    ggplot2::scale_shape_manual(name="", values=c(15, 16, 18))
  
  if(i==1){
    p <- p + ggplot2::facet_wrap(vars(variable), ncol=2, scales="free_y")
  } else {
    if(i==2){
      p <- p + ggplot2::facet_wrap(vars(variable, region), ncol=2, scales="free_y")
    }
  }
  
  # customize theme
  p <- p +
    ggplot2::theme_bw() +
    # ggplot2::ggtitle(paste(ls_data$dataset, ls_data$variable, ls_data$units, sep=" : " )) +
    ggplot2::theme(
      text=ggplot2::element_text(size=8),
      axis.text.x=ggplot2::element_text(colour="black", angle=90, hjust=0.5, vjust=0.5),
      plot.title=ggplot2::element_text(colour="black", hjust=0, vjust=0, size=8),
      panel.grid.major=ggplot2::element_blank(),
      panel.border=ggplot2::element_rect(size=0.25, colour="black"),
      legend.direction="horizontal",
      legend.position="bottom",
      plot.margin=grid::unit(c(0.1,0.1,0.1,0.1), "cm"))
  
  # print to pdf
  pdf(file=paste("~/Projects/fish_tmp/figures/Phenological_Variables_",i,".pdf", sep=""),
      paper="legal", width=8, height=13.5)
  print(p)
  dev.off()
}

#------------------------------------------------------------------------------
# subset "zooplankton" data - multiple regions
zoo_var <- list(c("Calanus finmarchicus", "Calanus glacialis", "Calanus hyperboreus"),
                c("Metridia longa", "Metridia lucens", "Metridia"),
                c("Temora", "Microcalanus", "Oithona"),
                c("Oithona similis", "Oithona atlantica", "Paracalanus"),
                c("Pseudocalanus", "Centropages typicus", "Centropages"),
                c("Scolecithricella minor", "Larvacea", "Gastropoda"),
                c("Bivalvia", "Euphausiacea"))

for(i in seq(1:length(zoo_var))){
  tb_zoo <- tb %>%
    dplyr::select(c("year", region, zoo_var[[i]], "label")) %>%
    dplyr::filter(year>=1999, year<=2019) %>%
    dplyr::distinct() %>%
    tidyr::gather(variable, value, zoo_var[[i]]) %>%
    dplyr::mutate(value=ifelse(value==-99, NA, value)) %>%
    dplyr::filter(!is.na(value))
  
  # plot data
  # set x-axis
  x_limits <- c(min(tb_zoo$year)-1, max(tb_zoo$year)+1)
  x_breaks <- seq(x_limits[1], x_limits[2], by=1)
  x_labels <- x_breaks
  
  # # set y-axis
  # y_limits <- c(min(tb_zoo$value, na.rm=T) - 0.1*mean(tb_zoo$value, na.rm=T),
  #               max(tb_zoo$value, na.rm=T) + 0.1*mean(tb_zoo$value, na.rm=T))
  
  # plot data
  p <-  ggplot2::ggplot() +
    # plot data - dots
    ggplot2::geom_point(data=tb_zoo,
                        mapping=ggplot2::aes(x=year, y=value, color=label, shape=label),
                        show.legend=T,
                        size=1.5,
                        alpha=0.3,
                        # position=position_jitter(width = 0.075, height = 0.075)) +
                        position="identity") +
    # set coordinates system and axes
    ggplot2::coord_cartesian() +
    ggplot2::scale_x_continuous(name="Year", limits=x_limits, breaks=x_breaks, labels=x_labels, expand=c(0, 0)) +
    ggplot2::scale_y_continuous(name="", expand=c(0.1, 0.1)) +
    ggplot2::scale_color_manual(name="", values=c("red", "green", "blue")) +
    ggplot2::scale_shape_manual(name="", values=c(15, 16, 18)) +
    ggplot2::facet_wrap(vars(variable, region), ncol=2, scales="free_y")
  
  # customize theme
  p <- p +
    ggplot2::theme_bw() +
    # ggplot2::ggtitle(paste(ls_data$dataset, ls_data$variable, ls_data$units, sep=" : " )) +
    ggplot2::theme(
      text=ggplot2::element_text(size=8),
      axis.text.x=ggplot2::element_text(colour="black", angle=90, hjust=0.5, vjust=0.5),
      plot.title=ggplot2::element_text(colour="black", hjust=0, vjust=0, size=8),
      panel.grid.major=ggplot2::element_blank(),
      panel.border=ggplot2::element_rect(size=0.25, colour="black"),
      legend.direction="horizontal",
      legend.position="bottom",
      plot.margin=grid::unit(c(0.1,0.1,0.1,0.1), "cm"))
  
  # print to pdf
  pdf(file=paste("~/Projects/fish_tmp/figures/Zooplankton_Variables_",i,".pdf", sep=""),
      paper="legal", width=8, height=13.5)
  print(p)
  dev.off()
}
