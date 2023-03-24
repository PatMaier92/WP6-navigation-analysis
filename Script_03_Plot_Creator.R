# ############################################################################ #
# ############################################################################ #
#                                                                              #
# ------------------------------ WP6 Data ------------------------------------ #
# Script_03_Plot_Creator                                                       #
# Author: Patrizia Maier                                                       #
#                                                                              #
# ############################################################################ #
# ############################################################################ #


# ------------------------------------------------------------------------------
# ::: LOAD PACKAGES ::: #
# ------------------------------------------------------------------------------

library(tidyverse)
library(gghalves)
library(cowplot)
library(patchwork)


# ------------------------------------------------------------------------------
# ::: LOAD RAW DATA ::: #
# ------------------------------------------------------------------------------

path <- "WP6_data/"

ind_file <-  paste(path, "WP6_individual_data.Rdata", sep="")
load(ind_file)
rm(ind_file)

data_individual <- data_individual %>% 
  mutate(groupNo=factor(groupNo, levels=c(1,0), labels=c("ALS", "Ctrl")))

in_file <- "WP6_data/WP6_starmate_probe_data.Rdata"
load(in_file)
rm(in_file)

data_sm_suc <- data_sm %>% 
  filter(success==1)
  

# ------------------------------------------------------------------------------
# ::: FUNCTIONS AND SETTINGS FOR PLOTTING ::: #
# ------------------------------------------------------------------------------

raincloud <- function(data, xvar, yvar, xlab, ylab, mylabel, mycolor, mycolor2, ymin="n", ymax="n", mysubtitle=NULL, facetvar="n"){
  p1 <- ggplot(data, aes(x=get(xvar), y=get(yvar), fill=get(xvar), color=get(xvar))) + 
    gghalves::geom_half_violin(position=position_nudge(x=0.125), width=0.5, side="r",  alpha=0.4) +
    gghalves::geom_half_boxplot(position=position_nudge(x=0.1), side="r",
                                outlier.shape=NA, center=TRUE, errorbar.draw=FALSE, width=0.1, alpha=1) +
    geom_point(position=position_jitter(w=0.1, h=0, seed=100), size=1.75, alpha=0.5) + 
    scale_fill_manual(name=NULL, labels=mylabel, values=mycolor) +
    scale_color_manual(values=c(mycolor2)) +
    scale_x_discrete(labels=mylabel, expand=c(0, 0.15)) +
    guides(fill="none", color="none") + 
    theme_cowplot(font_size=18) +
    theme(axis.text.y = element_text(angle = 0, color="black", size=18, face=1),
          axis.text.x = element_text(angle = 0, color="black", size=16, face=1)) + 
    labs(subtitle=mysubtitle,
         x=xlab,
         y=ylab)
  
  if (ymin == "n" & ymax == "n") {
    p1 <- p1 + coord_flip(clip="off")
  }
  else {
    p1 <- p1 + coord_flip(ylim=c(ymin, ymax), clip="off")
  }
  
  if (facetvar != "n") {
    p1 <- p1 + facet_wrap(facetvar)
  }
    
  return(p1)
}

raincloud_sub <- function(data, xvar, yvar, shapevar, xlab, ylab, mylabel, mycolor, mycolor2, myshapes, mysizes, ymin="n", ymax="n", mysubtitle=NULL){
  p1 <- ggplot(data, aes(x=get(xvar), y=get(yvar), fill=get(shapevar), color=get(shapevar), shape=get(shapevar), size=get(shapevar))) + 
    gghalves::geom_half_violin(aes(shape=NULL, size=NULL, fill=get(xvar), color=get(xvar)), position=position_nudge(x=0.225), width=0.5, side="r",  alpha=0.4, show.legend=F) +
    gghalves::geom_half_boxplot(aes(shape=NULL, size=NULL, fill=get(xvar), color=get(xvar)), position=position_nudge(x=0.2), side="r",
                                outlier.shape=NA, center=TRUE, errorbar.draw=FALSE, width=0.1, alpha=1, show.legend=F) +
    geom_point(position=position_jitter(w=0.18, h=0, seed=100), alpha=0.6) + 
    scale_shape_manual(name=NULL, labels=mylabel, values=myshapes) +
    scale_color_manual(name=NULL, labels=mylabel, values=mycolor2) +
    scale_fill_manual(name=NULL, labels=mylabel, values=mycolor) +
    scale_size_manual(name=NULL, labels=mylabel, values=mysizes) + 
    scale_x_discrete(labels=mylabel, expand=c(0.15, 0.15), limits=rev) +
    guides(size="none", shape=guide_legend(override.aes=list(size=5))) + 
    theme_cowplot(font_size=18) +
    theme(legend.key.size=unit(1,"cm"),
      axis.text.y=element_text(angle=0, color="black", size=18, face=1),
          axis.text.x=element_text(angle=0, color="black", size=16, face=1),
          plot.subtitle=element_text(face="bold")) + 
    labs(subtitle=mysubtitle,
         x=xlab,
         y=ylab)
  
  if (ymin == "n" & ymax == "n") {
    p1 <- p1 + coord_flip(clip="off", )
  }
  else {
    p1 <- p1 + coord_flip(ylim=c(ymin, ymax), clip="off")
  }
  
  return(p1)
}

# ------------------------------------------------------------------------------

# colors
group_colors <- c("Ctrl"="#ffCC00", "ALS"="#6699FF")
group_colors2 <- c("Ctrl"="#CC6600",  "ALS"="#003399")
mn_group_colors <- c("Ctrl"="#ffCC00", "ALS"="#6699FF", "PLS"="#e71356", "PMA"="#13e7a5")
mn_group_colors2 <- c("Ctrl"="#CC6600",  "ALS"="#003399", "PLS"="#9c0d3a", "PMA"="#0d9c6f")
mn_group_shapes <- c("Ctrl"=21, "ALS"=21, "PLS"=23, "PMA"=24)
mn_group_size <- c("Ctrl"=1.75, "ALS"=1.75, "PLS"=3, "PMA"=3)
ci_group_colors <- c("Ctrl"="#ffCC00", "ALS"="#6699FF", "ALSci"="#e71356")
ci_group_colors2 <- c("Ctrl"="#CC6600",  "ALS"="#003399", "ALSci"="#9c0d3a")
ci_group_shapes <- c("Ctrl"=21, "ALS"=21, "ALSci"=24)
ci_group_size <- c("Ctrl"=1.75, "ALS"=1.75, "ALSci"=3)

# labels 
group_label <- as_labeller(c("Ctrl" = "Ctrl", "ALS" = "ALS"))
mn_group_label <- as_labeller(c("ALS" = "ALS", "PLS" = "PLS", "PMA" = "PMA", "Ctrl" = "Ctrl"))
ci_group_label <- as_labeller(c("ALS" = "ALS", "ALSci" = "ALSci", "Ctrl" = "Ctrl"))

# ------------------------------------------------------------------------------
# ::: PLOTS ::: #
# ------------------------------------------------------------------------------

# ::: POST-NAVIGATIONAL MEMORY SCORES ::: #

# object identity
p2 <- raincloud(data_individual, "groupNo", "POST_objectIdentity", NULL, "score", 
                group_label, group_colors, group_colors2, ymin=0, ymax=1, mysubtitle="Object identity")

# object location
p3 <- raincloud(data_individual, "groupNo", "POST_objectLocation", NULL, "score",
                group_label, group_colors, group_colors2, ymin=0, ymax=1, mysubtitle="Object location")

# maze reconstruction 
p4 <- raincloud(data_individual, "groupNo", "POST_mazeReconstruction", NULL, "score",  
                group_label, group_colors, group_colors2, ymin=0, ymax=1, mysubtitle="Maze reconstruction")

# joint plot
p <- p2 + p3 + p4 + 
  plot_annotation(title="Post-navigational memory scores",
                  theme=theme(plot.title=element_text(size=20)))
ggsave("Plots/WP6_Post_Scoring.png", height=5, width=12, dpi=600)
rm(p, p2, p3, p4)

# ------------------------------------------------------------------------------

# ::: NEUROPSYCHOLOGICAL ASSESSMENT ::: # 

# ECAS
# executive
p2 <- raincloud(data_individual, "groupNo", "ECAS_executive", NULL, "score", 
                group_label, group_colors, group_colors2, mysubtitle="Executive")

# verbal fluency
p3 <- raincloud(data_individual, "groupNo", "ECAS_verbalFluency", NULL, "score", 
                group_label, group_colors, group_colors2, mysubtitle="Verbal fluency")

# language
p4 <- raincloud(data_individual, "groupNo", "ECAS_language", NULL, "score", 
                group_label, group_colors, group_colors2, mysubtitle="Language")

# memory 
p5 <- raincloud(data_individual, "groupNo", "ECAS_verbalMemory", NULL, "score", 
                group_label, group_colors, group_colors2, mysubtitle="Memory")

# visuo spatial 
p6 <- raincloud(data_individual, "groupNo", "ECAS_visuospatial", NULL, "score", 
                group_label, group_colors, group_colors2, mysubtitle="Spatial")

# joint plot
p <- p2 + p3 + p4 + p5 + p6 + plot_layout(nrow=1) + 
  plot_annotation(title="Edinburgh Cognitive and Behavioural ALS Screen (ECAS)",
                  theme=theme(plot.title=element_text(size=20)))
ggsave("Plots/WP6_ECAS.png", height=5, width=15, dpi=600)
rm(p, p2, p3, p4, p5, p6)


# SPART
# immediate & delayed (overall)
p1 <- raincloud(data_individual, "groupNo", "SPART_overallMemory", NULL, "recall score", 
                group_label, group_colors, group_colors2, mysubtitle="Spatial memory (SPART)")

# 5PT
#  productivity
p2 <- raincloud(data_individual, "groupNo", "5PT", NULL, "n unique figures", 
                group_label, group_colors, group_colors2, mysubtitle="Spatial fluency (5PT)")

# PTSOT 
# mean deviation
p3 <- raincloud(data_individual, "groupNo", "PTSOT", NULL, "mean angle deviation", 
                group_label, group_colors, group_colors2, mysubtitle="Spatial Orientation (PTSOT)")


# joint spatial plot 
p <- p1 + p2 + p3 + 
  plot_annotation(title="Neuropsychological assessment",
                  theme=theme(plot.title=element_text(size=20)))
ggsave("Plots/WP6_NP.png", height=5, width=12, dpi=600)
rm(p, p1, p2, p3)

# ------------------------------------------------------------------------------

# ::: SUPPLEMENT STARMAZE ::: # 

# --- ALS, PLS vs. PMA ---#
# all probe trials 
data.mn <- data_sm %>% 
  group_by(id, groupNo, trialCondition, MN_involvement) %>% 
  summarize_at(vars(success), mean, na.rm=T) %>% 
  ungroup() %>% 
  group_by(id, groupNo, MN_involvement) %>% 
  summarize_at(vars(success), mean, na.rm=T) %>% 
  ungroup()

data.mn %>% 
  group_by(MN_involvement) %>% 
  summarize_at(vars(success), list(m=mean, sd=sd), na.rm=T)

# success 
plot.mn_success <- raincloud_sub(data.mn, "groupNo", "success", "MN_involvement", NULL, "%", 
                                 mn_group_label, mn_group_colors, mn_group_colors2, mn_group_shapes, mn_group_size, 
                                 ymin=0, ymax=1, mysubtitle="Success rate") + labs(title="Probe trials") 
rm(data.mn)


# successful probe trials 
data.mn_suc <- data_sm_suc %>% 
  group_by(id, groupNo, trialCondition, MN_involvement) %>% 
  summarize_at(vars(latency_seconds, pathError_percent, searchAccuracy_percent), mean, na.rm=T) %>% 
  ungroup() %>% 
  group_by(id, groupNo, MN_involvement) %>% 
  summarize_at(vars(latency_seconds, pathError_percent, searchAccuracy_percent), mean, na.rm=T) %>% 
  ungroup()

data.mn_suc %>% 
  group_by(MN_involvement) %>% 
  summarize_at(vars(latency_seconds, pathError_percent, searchAccuracy_percent), list(m=mean, sd=sd), na.rm=T)


plot.mn_latency <- raincloud_sub(data.mn_suc, "groupNo", "latency_seconds", "MN_involvement", NULL, "seconds", 
                                 mn_group_label, mn_group_colors, mn_group_colors2, mn_group_shapes, mn_group_size, 
                                 ymin=10, ymax=50, mysubtitle="Latency\nto target location") + labs(title="Successful probe trials")

plot.mn_path <- raincloud_sub(data.mn_suc, "groupNo", "pathError_percent", "MN_involvement", NULL, "%", 
                                 mn_group_label, mn_group_colors, mn_group_colors2, mn_group_shapes, mn_group_size, 
                                 ymin=0, ymax=100, mysubtitle="Path error\nto target location")

plot.mn_search <- raincloud_sub(data.mn_suc, "groupNo", "searchAccuracy_percent", "MN_involvement", NULL, "%", 
                                 mn_group_label, mn_group_colors, mn_group_colors2, mn_group_shapes, mn_group_size, 
                                 ymin=0, ymax=75, mysubtitle="Search accuracy")
rm(data.mn_suc)


plot.mn <- plot.mn_success + guide_area() + plot_spacer() + 
  plot_spacer() + plot_spacer() + plot_spacer() + 
  plot.mn_latency + plot.mn_path + plot.mn_search +
  plot_layout(nrow=3, ncol=3, guides="collect", heights=c(0.475, 0.05, 0.475)) + theme(legend.position=c(0.4, 0.8)) 
ggsave("WP6_data/WP6_supplement_ALS_PLS_PMA.png", plot.mn, height=10, width=12, dpi=600)
rm(plot.mn, plot.mn_success, plot.mn_latency, plot.mn_path, plot.mn_search)



# --- ALSci vs. others ---#
# all probe trials 
data.ci <- data_sm %>% 
  group_by(id, groupNo, trialCondition, ALSci) %>% 
  summarize_at(vars(success), mean, na.rm=T) %>% 
  ungroup() %>% 
  group_by(id, groupNo, ALSci) %>% 
  summarize_at(vars(success), mean, na.rm=T) %>% 
  ungroup()

data.ci %>% 
  group_by(ALSci) %>% 
  summarize_at(vars(success), list(m=mean, sd=sd), na.rm=T)

# success 
plot.ci_success <- raincloud_sub(data.ci, "groupNo", "success", "ALSci", NULL, "%", 
                                 ci_group_label, ci_group_colors, ci_group_colors2, ci_group_shapes, ci_group_size, 
                                 ymin=0, ymax=1, mysubtitle="Success rate") + labs(title="Probe trials") 
rm(data.ci)


# successful probe trials 
data.ci_suc <- data_sm_suc %>% 
  group_by(id, groupNo, trialCondition, ALSci) %>% 
  summarize_at(vars(latency_seconds, pathError_percent, searchAccuracy_percent), mean, na.rm=T) %>% 
  ungroup() %>% 
  group_by(id, groupNo, ALSci) %>% 
  summarize_at(vars(latency_seconds, pathError_percent, searchAccuracy_percent), mean, na.rm=T) %>% 
  ungroup()

data.ci_suc %>% 
  group_by(ALSci) %>% 
  summarize_at(vars(latency_seconds, pathError_percent, searchAccuracy_percent), list(m=mean, sd=sd), na.rm=T)


plot.ci_latency <- raincloud_sub(data.ci_suc, "groupNo", "latency_seconds", "ALSci", NULL, "seconds", 
                                 ci_group_label, ci_group_colors, ci_group_colors2, ci_group_shapes, ci_group_size, 
                                 ymin=10, ymax=50, mysubtitle="Latency\nto target location") + labs(title="Successful probe trials")

plot.ci_path <- raincloud_sub(data.ci_suc, "groupNo", "pathError_percent", "ALSci", NULL, "%", 
                              ci_group_label, ci_group_colors, ci_group_colors2, ci_group_shapes, ci_group_size, 
                              ymin=0, ymax=100, mysubtitle="Path error\nto target location")

plot.ci_search <- raincloud_sub(data.ci_suc, "groupNo", "searchAccuracy_percent", "ALSci", NULL, "%", 
                                ci_group_label, ci_group_colors, ci_group_colors2, ci_group_shapes, ci_group_size, 
                                ymin=0, ymax=75, mysubtitle="Search accuracy")
rm(data.ci_suc)


plot.ci <- plot.ci_success + guide_area() + plot_spacer() + 
  plot_spacer() + plot_spacer() + plot_spacer() + 
  plot.ci_latency + plot.ci_path + plot.ci_search +
  plot_layout(nrow=3, ncol=3, guides="collect", heights=c(0.475, 0.05, 0.475)) + theme(legend.position=c(0.4, 0.8)) 
ggsave("WP6_data/WP6_supplement_ALSci.png", plot.ci, height=10, width=12, dpi=600)
rm(plot.ci, plot.ci_success, plot.ci_latency, plot.ci_path, plot.ci_search)
# ------------------------------------------------------------------------------

# clear workspace
rm(list = ls())