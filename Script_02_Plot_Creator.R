### -------------------- WP6 Data   --------------------- ###
### WP6_PlotCreator                                       ###
### Author: Patrizia Maier                                ###


# ::: get packages ::: #

library(tidyverse)
library(gghalves)
library(cowplot)
library(gtsummary)
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


# ::: create overview tables with demographic data ::: # 

temp <- subset(data_individual, select=c(ID, group, MN, MNE_Untergruppe, ALS_Variante, is_category, `ALS-FRS-R`, `FRS-/Monat`,
                                         dfb_q1_sex, dfb_q2_age, dfb_q3_years_edu_total, dfb_q4_highestedu, 
                                         dfb_q5_language_german, dfb_q6_handiness, 
                                         dfb_q21_comp_expertise, dfb_q22_comp_freq,
                                         sbsds_total_score, id_t1_months, is_t1_months)) %>% 
  mutate(dfb_q1_sex=droplevels(dfb_q1_sex),
         dfb_q5_language_german=fct_recode(dfb_q5_language_german, yes = "Deutsch ist Muttersprache", no = "Deutsch ist nicht Muttersprache"),
         dfb_q6_handiness=fct_recode(dfb_q6_handiness, right = "rechtshändig", left = "linkshändig", both = "beidhändig"),
         dfb_q21_comp_expertise=as.numeric(dfb_q21_comp_expertise),
         dfb_q22_comp_freq=as.numeric(dfb_q22_comp_freq))

# ctrl vs. ALS
t1 <- temp %>% 
  select(-c(ID, MN, MNE_Untergruppe, ALS_Variante, is_category, `ALS-FRS-R`, `FRS-/Monat`, id_t1_months, is_t1_months)) %>% 
  tbl_summary(by=group, 
              label=list(dfb_q1_sex ~ "Gender", dfb_q2_age ~ "Age", dfb_q3_years_edu_total ~ "Years of education",
                         dfb_q4_highestedu ~ "Education level", dfb_q5_language_german ~ "German native speaker", 
                         dfb_q6_handiness ~ "Handedness", dfb_q21_comp_expertise ~ "Self-rated computer expertise", 
                         dfb_q22_comp_freq ~ "Self-rated computer use frequency", sbsds_total_score ~ "Self-rated spatial abilities (SBSDS)"),
              type=list(dfb_q21_comp_expertise ~ 'continuous', dfb_q22_comp_freq  ~ 'continuous'),
              statistic=list(all_continuous() ~ "{mean} (sd: {sd}) (IQR: {p25}-{p75})", all_categorical() ~ "{n} ({p}%)"),
              digits=list(all_continuous() ~ c(1, 2, 1, 1)),
              missing="no") %>% 
  add_p(test=list(all_continuous() ~ "wilcox.test", all_categorical() ~  "fisher.test")) %>% 
  modify_header(label = "**Variable**")

t1 %>% 
  as_flex_table() %>%
  flextable::save_as_docx(path="WP6_data/CTR_MND_Demographics.docx")


# ALS subgroups 
t2 <- temp %>% 
  filter(group=="MND") %>% 
  select(-c(ID, group, dfb_q1_sex, dfb_q2_age, dfb_q3_years_edu_total, dfb_q4_highestedu, 
            dfb_q5_language_german, dfb_q6_handiness, dfb_q21_comp_expertise, dfb_q22_comp_freq, sbsds_total_score)) %>% 
  tbl_summary(label=list(MN ~ "Motor neuron involvement", MNE_Untergruppe ~ "MND subgroup", ALS_Variante ~ "ALS variant",
                         is_category ~ "Initial symptoms", id_t1_months ~ "Time from diagnosis (months)",
                         is_t1_months ~ "Time from initial symptoms (months)"),
              statistic=list(all_continuous() ~ "{mean} (IQR: {p25}-{p75})", all_categorical() ~ "{n} ({p}%)"),
              digits=list(all_continuous() ~ c(1, 1))) %>% 
  add_n() %>%
  modify_header(label = "**Variable**")

t2 %>% 
  as_flex_table() %>%
  flextable::save_as_docx(path="WP6_data/MND_Sub_Demographics.docx")

rm(temp, t1, t2)


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

 
# scatter <- function(data, x, y, ylab, xlab){
#   p1 <- ggplot(data, aes(x=get(x), y=get(y))) +
#     geom_point() +  # scatter
#     geom_smooth(method=lm) + # prediction line 
#     theme_classic() + 
#     labs(x = xlab, 
#          y = ylab)
#   return(p1)
# }


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


# ## SCATTER PLOTS 
# # join data 
# t <- trial_data %>% 
#   filter(probe_trial==1) %>% 
#   group_by(id) %>% 
#   summarise(success=mean(success)) %>% 
#   left_join(data_individual, by=c("id"="ID"))
# 
# # settings
# im_width=4.5
# im_height=3.5
# im_dpi=600
# 
# 
# ## MND-specific
# # ALS-FRS
# p <- scatter(t[t$group=="MND",], "success", "ALS-FRS-R", "ALS-FRS (0-48)", "Starmaze: Mean success in probe trials")
# ggsave("Plots/Scatter/WP6_Scatter_SMscore_ALSFRS.png", width=im_width, height=im_height, dpi=im_dpi)
# rm(p)
# 
# p <- scatter(data_individual[data_individual$group=="MND",], "Score_total", "ALS-FRS-R", "ALS-FRS (0-48)", "Non-nav-mem: Total score")
# ggsave("Plots/Scatter/WP6_Scatter_NNscore_ALSFRS.png", width=im_width, height=im_height, dpi=im_dpi)
# rm(p)
# 
# # FRS-/month
# p <- scatter(t[t$group=="MND",], "success", "FRS-/Monat", "ALS-FRS (0-48) / month", "Starmaze: Mean success in probe trials")
# ggsave("Plots/Scatter/WP6_Scatter_SMscore_ALSFRS-month.png", width=im_width, height=im_height, dpi=im_dpi)
# rm(p)
# 
# p <- scatter(data_individual[data_individual$group=="MND",], "Score_total", "FRS-/Monat", "ALS-FRS (0-48) / month", "Non-nav-mem: Total score")
# ggsave("Plots/Scatter/WP6_Scatter_NNscore_ALSFRS-month.png", width=im_width, height=im_height, dpi=im_dpi)
# rm(p)
# 
# 
# ## Demographics  
# # Age 
# p <- scatter(t, "success", "dfb_q2_age", "Age", "Starmaze: Mean success in probe trials")
# ggsave("Plots/Scatter/WP6_Scatter_SMscore_Age.png", width=im_width, height=im_height, dpi=im_dpi)
# rm(p)
# 
# p <- scatter(t, "Score_total", "dfb_q2_age", "Age", "Non-nav-mem: Total score")
# ggsave("Plots/Scatter/WP6_Scatter_NNscore_Age.png", width=im_width, height=im_height, dpi=im_dpi)
# rm(p)
# 
# # Years of education 
# p <- scatter(t, "success", "dfb_q3_years_edu_total", "Years of education", "Starmaze: Mean success in probe trials")
# ggsave("Plots/Scatter/WP6_Scatter_SMscore_Education.png", width=im_width, height=im_height, dpi=im_dpi)
# rm(p)
# 
# p <- scatter(t, "Score_total", "dfb_q3_years_edu_total", "Years of education", "Non-nav-mem: Total score")
# ggsave("Plots/Scatter/WP6_Scatter_NNscore_Education.png", width=im_width, height=im_height, dpi=im_dpi)
# rm(p)
# 
# 
# # Tasks
# p <- scatter(t, "success", "Score_total", "Non-nav-mem: Total score", "Starmaze: Mean success in probe trials")
# ggsave("Plots/Scatter/WP6_Scatter_SMscore_NNscore.png", width=im_width, height=im_height, dpi=im_dpi)
# rm(p)
# 
# p <- scatter(t, "success", "Object_identity_manual_s", "Non-nav-mem: Object identity", "Starmaze: Mean success in probe trials")
# ggsave("Plots/Scatter/WP6_Scatter_SMscore_NN_oi.png", width=im_width, height=im_height, dpi=im_dpi)
# rm(p)
# 
# p <- scatter(t, "success", "Object_location_GMDA_SQRTCanOrg_s", "Non-nav-mem: Obejct location", "Starmaze: Mean success in probe trials")
# ggsave("Plots/Scatter/WP6_Scatter_SMscore_NN_ol.png", width=im_width, height=im_height, dpi=im_dpi)
# rm(p)
# 
# p <- scatter(t, "success", "Maze_reconstruction_manual_s", "Non-nav-mem: Maze reconstruction", "Starmaze: Mean success in probe trials")
# ggsave("Plots/Scatter/WP6_Scatter_SMscore_NN_mr.png", width=im_width, height=im_height, dpi=im_dpi)
# rm(p)
# 
# p <- scatter(t, "success", "mct_path", "Motor control: path length", "Starmaze: Mean success in probe trials")
# ggsave("Plots/Scatter/WP6_Scatter_SMscore_MCpath.png", width=im_width, height=im_height, dpi=im_dpi)
# rm(p)
# 
# 
# # Neuropsychology 
# p <- scatter(t, "success", "SPART_mean_all", "SPART: spatial memory score", "Starmaze: Mean success in probe trials")
# ggsave("Plots/Scatter/WP6_Scatter_SMscore_SPART.png", width=im_width, height=im_height, dpi=im_dpi)
# rm(p)
# 
# p <- scatter(t, "success", "FIVE_P_productivity", "5PT: spatial fluency score", "Starmaze: Mean success in probe trials")
# ggsave("Plots/Scatter/WP6_Scatter_SMscore_5PT.png", width=im_width, height=im_height, dpi=im_dpi)
# rm(p)
# 
# p <- scatter(t, "success", "PTSOT_mean_dev", "PTSOT perspective taking mean deviation", "Starmaze: Mean success in probe trials")
# ggsave("Plots/Scatter/WP6_Scatter_SMscore_PTSOT.png", width=im_width, height=im_height, dpi=im_dpi)
# rm(p)