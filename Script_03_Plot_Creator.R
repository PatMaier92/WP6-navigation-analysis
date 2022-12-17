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


# scatter <- function(data, x, y, xlab, ylab, xmin="n", xmax="n", ymin="n", ymax="n"){
#   p1 <- ggplot(data, aes(x=get(x), y=get(y))) +
#     geom_point() +  
#     geom_smooth(method=lm) + 
#     theme_cowplot(font_size=18) +
#     labs(x=xlab,
#          y=ylab)
#   
#   if (xmin != "n" & xmax != "n") {
#     p1 <- p1 + coord_cartesian(xlim=c(xmin, xmax))
#   }
#   
#   if (ymin != "n" & ymax != "n") {
#     p1 <- p1 + coord_cartesian(ylim=c(ymin, ymax))
#   }
#   
#   return(p1)
# }

# ------------------------------------------------------------------------------

# colors
group_colors <- c("Ctrl"="#ffCC00", "ALS"="#6699FF")
group_colors2 <- c("Ctrl"="#CC6600",  "ALS"="#003399")

# labels 
group_label <- as_labeller(c("Ctrl" = "Ctrl", "ALS" = "ALS"))


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

# clear workspace
rm(list = ls())