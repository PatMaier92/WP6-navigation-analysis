### -------------------- WP6 Data   --------------------- ###
### WP6_PlotCreator                                       ###
### Author: Patrizia Maier                                ###


# ::: get packages ::: #

library(tidyverse)
library(gghalves)
library(cowplot)
library(patchwork)


# ######################################################### #


# ::: get data ::: # 

path <- "WP6_data/"

ind_file <-  paste(path, "WP6_individual_data.Rdata", sep="")
load(ind_file)
rm(ind_file)

# trial_file <-  paste(path, "WP6_trial_data.Rdata", sep="")
# load(trial_file)
# rm(trial_file)


# ######################################################### #


# ::: functions for plotting ::: # 

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


scatter <- function(data, x, y, xlab, ylab, xmin="n", xmax="n", ymin="n", ymax="n"){
  p1 <- ggplot(data, aes(x=get(x), y=get(y))) +
    geom_point() +  
    geom_smooth(method=lm) + 
    theme_cowplot(font_size=18) +
    labs(x=xlab,
         y=ylab)
  
  if (xmin != "n" & xmax != "n") {
    p1 <- p1 + coord_cartesian(xlim=c(xmin, xmax))
  }
  
  if (ymin != "n" & ymax != "n") {
    p1 <- p1 + coord_cartesian(ylim=c(ymin, ymax))
  }
  
  return(p1)
}


# ######################################################### #


# ::: settings for plotting ::: #

# colors
group_colors <- c("Control"="#ffCC00", "MND"="#6699FF")
group_colors2 <- c("Control"="#CC6600",  "MND"="#003399")

# labels 
group_label <- as_labeller(c("Control" = "Ctrl", "MND" = "ALS"))


# ######################################################### #


# ::: plots for NON-NAVIGATIONAL MEMORY SCORES ::: # 

# total score
p1 <- raincloud(data_individual, "group", "Score_total",  NULL, "score",
          group_label, group_colors, group_colors2, ymin=0, ymax=1)

# object identity
p2 <- raincloud(data_individual, "group", "Object_identity_manual_s", NULL, "score", 
                group_label, group_colors, group_colors2, ymin=0, ymax=1, mysubtitle="Object identity")

# object location
p3 <- raincloud(data_individual, "group", "Object_location_GMDA_SQRTCanOrg_s", NULL, "score",
                group_label, group_colors, group_colors2, ymin=0, ymax=1, mysubtitle="Object location")

# maze reconstruction 
p4 <- raincloud(data_individual, "group", "Maze_reconstruction_manual_s", NULL, "score",  
                group_label, group_colors, group_colors2, ymin=0, ymax=1, mysubtitle="Maze reconstruction")

# joint plot
p <- p2 + p3 + p4 + 
  plot_annotation(title="Non-navigational memory scores",
                  theme=theme(plot.title=element_text(size=20)))
ggsave("Plots/WP6_Scoring_joint.png", height=5, width=12, dpi=600)
rm(p, p1, p2, p3, p4)


# ######################################################### #


# ::: plots for NEUROPSYCHOLOGICAL ASSESSMENT ::: # 

# ECAS
# total
p1 <- raincloud(data_individual, "group", "ECAS_total_score", NULL, "score", 
                group_label, group_colors, group_colors2, mysubtitle="ECAS Total")

# executive
p2 <- raincloud(data_individual, "group", "ECAS_sub_executive", NULL, "score", 
                group_label, group_colors, group_colors2, mysubtitle="Executive")

# verbal fluency
p3 <- raincloud(data_individual, "group", "ECAS_sub_verbal_fluency", NULL, "score", 
                group_label, group_colors, group_colors2, mysubtitle="Verbal fluency")

# language
p4 <- raincloud(data_individual, "group", "ECAS_sub_language", NULL, "score", 
                group_label, group_colors, group_colors2, mysubtitle="Language")

# memory 
p5 <- raincloud(data_individual, "group", "ECAS_sub_memory", NULL, "score", 
                group_label, group_colors, group_colors2, mysubtitle="Memory")

# spatial abilities 
p6 <- raincloud(data_individual, "group", "ECAS_sub_spatial", NULL, "score", 
                group_label, group_colors, group_colors2, mysubtitle="Spatial")

# joint plot
p <- p2 + p3 + p4 + p5 + p6 + plot_layout(nrow=1) + 
  plot_annotation(title="Edinburgh Cognitive and Behavioural ALS Screen (ECAS)",
                  theme=theme(plot.title=element_text(size=20)))
ggsave("Plots/WP6_ECAS_joint.png", height=5, width=15, dpi=600)
rm(p, p1, p2, p3, p4, p5, p6)


# SPART
# immediate & delayed (overall mean)
p1 <- raincloud(data_individual, "group", "SPART_mean_all", NULL, "recall score", 
                group_label, group_colors, group_colors2, mysubtitle="Spatial memory (SPART)")

# # immediate
# p1a <- raincloud(data_individual, "group", "SPART_mean_I", NULL, "recall score", 
#                  group_label, group_colors, group_colors2, mysubtitle="Immediate")
# 
# # delayed
# p1b <- raincloud(data_individual, "group", "SPART_q4_II", NULL, "recall score", 
#                  group_label, group_colors, group_colors2, mysubtitle="Delayed")
# 
# # joint plot 
# p <- p1a + p1b + 
#   plot_annotation(title="Spatial memory (SPART)",
#                   theme=theme(plot.title=element_text(size=20)))
# ggsave("Plots/WP6_SPART_joint.png", height=5, width=8, dpi=600)
# rm(p, p1a, p1b)


# 5PT
# general score: productivity
p2 <- raincloud(data_individual, "group", "FIVE_P_productivity", NULL, "n unique figures", 
                group_label, group_colors, group_colors2, mysubtitle="Spatial fluency (5PT)")

# # productivity
# p2a <- raincloud(data_individual, "group", "FIVE_P_productivity", NULL, "n unique figures", 
#                  group_label, group_colors, group_colors2, mysubtitle="Productivity")
# 
# # perseveration
# p2b <- raincloud(data_individual, "group", "FIVE_P_flexibility", NULL, "relative %", 
#                  group_label, group_colors, group_colors2, mysubtitle="Perseveration")
# 
# # strategy
# p2c <- raincloud(data_individual, "group", "FIVE_P_strategy", NULL, "relative %", 
#                  group_label, group_colors, group_colors2, mysubtitle="Strategy")
# 
# # joint plot 
# p <- p2a + p2b + p2c + 
#   plot_annotation(title="Spatial fluency (5PT)",
#                   theme=theme(plot.title=element_text(size=20)))
# ggsave("Plots/WP6_5PT_joint.png", height=5, width=9, dpi=600)
# rm(p, p2a, p2b, p2c)


# PTSOT 
# mean deviation
p3 <- raincloud(data_individual, "group", "PTSOT_mean_dev", NULL, "angle deviation", 
                group_label, group_colors, group_colors2, mysubtitle="Spatial Orientation (PTSOT)")


# joint spatial plot 
p <- p1 + p2 + p3 + 
  plot_annotation(title="Neuropsychological assessment",
                  theme=theme(plot.title=element_text(size=20)))
ggsave("Plots/WP6_NP_joint.png", height=5, width=12, dpi=600)
rm(p, p1, p2, p3)


# ######################################################### #


# ::: Scatter plots ::: #


# settings
im_width=9
im_height=6
im_dpi=600


# Clinical data
# ALS-FRS
p1 <- scatter(data_individual[data_individual$group=="MND",], "ALS-FRS-R", "Score_total", "ALS-FRS (0-48)", "Non-nav. mem. total score")

# ALS-FRS/month
p2 <- scatter(data_individual[data_individual$group=="MND",], "FRS-/Monat", "Score_total", "ALS-FRS (0-48) / month", "Non-nav. mem. total score")

# joint plot
p <- p1 + p2 
ggsave("Plots/Scatter/WP6_Scatter_Clinical.png", width=im_width, height=im_height, dpi=im_dpi)
rm(p, p1, p2)


# Demographics
# Age
p1 <- scatter(data_individual, "dfb_q2_age", "Score_total", "Age", "Non-nav. mem. total score")

# Years of education
p2 <- scatter(data_individual, "dfb_q3_years_edu_total", "Score_total",  "Years of education", "Non-nav. mem. total score")

# joint plot 
p <- p1 + p2
ggsave("Plots/Scatter/WP6_Scatter_Demo.png", width=im_width, height=im_height, dpi=im_dpi)
rm(p, p1, p2)


# ######################################################### #


# raincloud_sub <- function(data, xvar, yvar, ylab, xlab, sub, mytitle=NULL){
#   p1 <- ggplot(data, aes(x=get(xvar),y=get(yvar),fill=get(xvar))) + # set up data
#     geom_flat_violin(position=position_nudge(x=.02,y=0)) + # rain cloud: setting "adjust" for smoothness of kernel
#     geom_point(aes(shape = get(sub)), size = 3/2, position=position_jitter(w=.1,h=0,seed=100)) + # points
#     geom_point(aes(colour = get(sub), shape = get(sub)), size = 1/2, position=position_jitter(w=.1,h=0,seed=100)) + # point
#     geom_boxplot(aes(x=as.numeric(get(xvar))+0.2,y=get(yvar)), outlier.shape=NA, alpha=0.3, width=0.1, colour="BLACK") +
#     scale_shape_manual(values=c(15,16,17,18)) +
#     scale_colour_manual(values=c("skyblue","yellow","salmon","black")) +
#     scale_fill_grey(start=0.99, end=0.75) +
#     coord_flip() + # flip axes
#     guides(fill=FALSE) +
#     theme_classic() + 
#     theme(legend.position = "bottom",
#           legend.justification = c(0,0),
#           legend.text = element_text(size=12),
#           legend.title = element_blank()) +
#     labs(subtitle=mytitle,
#          x = xlab,
#          y = ylab)
# 
#   return(p1)
# }
