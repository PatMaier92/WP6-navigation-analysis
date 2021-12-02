### -------------------- WP6 Data   --------------------- ###
### WP6_PlotCreator                                       ###
### Author: Patrizia Maier                                ###


### get packages
source("R_rainclouds.R")
library(tidyverse)
library(cowplot)
library(readxl)
library(gtsummary)
library(ggradar)
library(scales)
library(patchwork)


###########################################################################


## set path
path <- "WP6_data/"

# load data
ind_file <-  paste(path, "WP6_individual_data.Rdata", sep="")
load(ind_file)
rm(ind_file)

trial_file <-  paste(path, "WP6_trial_data.Rdata", sep="")
load(trial_file)
rm(trial_file)


###########################################################################


### demographic data 

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


###########################################################################


### functions for plotting

raincloud <- function(data, xvar, yvar, ylab, xlab, mytitle=NULL, facetvar="none"){
  p1 <- ggplot(data, aes(x=get(xvar),y=get(yvar),fill=get(xvar))) + # set up data 
    geom_flat_violin(position=position_nudge(x=0.2,y=0)) + # rain cloud: setting "adjust" for smoothness of kernel
    geom_boxplot(aes(x=as.numeric(get(xvar))+0.2,y=get(yvar)), outlier.shape=NA, alpha=0.3, width=0.1, colour="BLACK") + 
    geom_point(position=position_jitter(w=0.1,h=0,seed=100), size=1) +
    scale_fill_grey(start=0.99, end=0.75) +
    coord_flip() + 
    guides(fill=FALSE) + 
    theme_classic() + 
    theme(axis.title = element_text(size=10)) + 
    labs(subtitle=mytitle,
         x = xlab, 
         y = ylab) 
  
  if(facetvar != "none") {
    p1 <- p1 + facet_wrap(facetvar)
  }
    
  return(p1)
}

raincloud_sub <- function(data, xvar, yvar, ylab, xlab, sub, mytitle=NULL){
  p1 <- ggplot(data, aes(x=get(xvar),y=get(yvar),fill=get(xvar))) + # set up data
    geom_flat_violin(position=position_nudge(x=.02,y=0)) + # rain cloud: setting "adjust" for smoothness of kernel
    geom_point(aes(shape = get(sub)), size = 3/2, position=position_jitter(w=.1,h=0,seed=100)) + # points
    geom_point(aes(colour = get(sub), shape = get(sub)), size = 1/2, position=position_jitter(w=.1,h=0,seed=100)) + # point
    geom_boxplot(aes(x=as.numeric(get(xvar))+0.2,y=get(yvar)), outlier.shape=NA, alpha=0.3, width=0.1, colour="BLACK") +
    scale_shape_manual(values=c(15,16,17,18)) +
    scale_colour_manual(values=c("skyblue","yellow","salmon","black")) +
    scale_fill_grey(start=0.99, end=0.75) +
    coord_flip() + # flip axes
    guides(fill=FALSE) +
    theme_classic() + 
    theme(legend.position = "bottom",
          legend.justification = c(0,0),
          legend.text = element_text(size=12),
          legend.title = element_blank()) +
    labs(subtitle=mytitle,
         x = xlab,
         y = ylab)

  return(p1)
}

barplot <- function(data, xvar, yvar, fvar, facetvar, mylabels, mytitle, xlab, ylab, legendpos){
  p1 <- ggplot(data, aes(x=factor(get(xvar)), y=get(yvar), fill=get(fvar))) + 
    geom_bar(stat="identity", position=position_dodge(), color="black") + 
    scale_x_discrete(labels=mylabels) + 
    scale_fill_grey(start=0.99, end=0.75) +
    facet_wrap(facetvar) + 
    theme_classic() +
    theme(legend.position=legendpos,
          legend.justification=c(0,1),
          legend.title=element_blank(),
          axis.text.x=element_text(angle=90, hjust=1)) + 
    labs(title = mytitle,
         x = xlab,
         y = ylab)
  
  return(p1)
}

scatter <- function(data, x, y, ylab, xlab){
  p1 <- ggplot(data, aes(x=get(x), y=get(y))) +
    geom_point() +  # scatter
    geom_smooth(method=lm) + # prediction line 
    theme_classic() + 
    labs(x = xlab, 
         y = ylab)
  return(p1)
}

# mean_func <- function(data){
#   data <- data %>% 
#     summarise(success=mean(success),
#               time=mean(time),
#               path_length=mean(path_length),
#               path_error=mean(path_error),
#               avg_dist_target=mean(avg_dist_target),
#               distance_error=mean(distance_error),
#               avg_distance_fxy=mean(avg_distance_fxy))
#               # + zone entries & zone error
#               
#   return(data)
# }


###################################################################################################


### STARMAZE MOTOR CONTROL
# time
p1 <- raincloud(data_individual, "group", "mct_time", "seconds", NULL, "Time")

# path
p2 <- raincloud(data_individual, "group", "mct_path", "virtual meters", NULL, "Path length")

# velocity
p3 <- raincloud(data_individual, "group", "mct_velocity", "virtual meters/seconds", NULL, "Velocity")

p <- p1 + 
  (p2 & theme(axis.text.y = element_blank(),
              axis.ticks.y = element_blank())) + 
  (p3 & theme(axis.text.y = element_blank(),
              axis.ticks.y = element_blank())) + 
  plot_annotation(title = "Motor control task")
ggsave("Plots/WP6_MC_joint.png", height=4, width=8, dpi=600)
rm(p, p1, p2, p3)


###################################################################################################


### NON-NAVIGATIONAL MEMORY SCORES
# total
p1 <- raincloud(data_individual, "group", "Score_total", "score", NULL, "Total") + ylim(0,1)

# object identity
p2 <- raincloud(data_individual, "group", "Object_identity_manual_s", "score", NULL, "Object identity") + ylim(0,1)

# object location
p3 <- raincloud(data_individual, "group", "Object_location_GMDA_SQRTCanOrg_s", "score", NULL, "Object location") + ylim(0,1)

# maze reconstruction 
p4 <- raincloud(data_individual, "group", "Maze_reconstruction_manual_s", "score", NULL, "Maze reconstruction") + ylim(0,1)

# joint plot
p <- p2 + 
  (p3 & theme(axis.text.y = element_blank(),
                 axis.ticks.y = element_blank())) + 
  (p4 & theme(axis.text.y = element_blank(),
                axis.ticks.y = element_blank())) +
  plot_annotation(title="Non-navigational memory scores")
ggsave("Plots/WP6_Scoring_joint.png", height=4, width=8, dpi=600)
rm(p, p1, p2, p3, p4)


###################################################################################################


### NEUROPSYCHOLOGICAL ASSESSMENT 
# ECAS
# total
p1 <- raincloud(data_individual, "group", "ECAS_total_score", "score", NULL, "ECAS Total")

# memory 
p2 <- raincloud(data_individual, "group", "ECAS_sub_memory", "score", NULL, "Memory")

# spatial abilities 
p3 <- raincloud(data_individual, "group", "ECAS_sub_spatial", "score", NULL, "Spatial")

# language
p4 <- raincloud(data_individual, "group", "ECAS_sub_language", "score", NULL, "Language")

# verbal fluency
p5 <- raincloud(data_individual, "group", "ECAS_sub_verbal_fluency", "score", NULL, "Verbal fluency")

# executive
p6 <- raincloud(data_individual, "group", "ECAS_sub_executive", "score", NULL, "Executive")


# joint plot
p <- p2 + 
  (p3 & theme(axis.text.y = element_blank(),
              axis.ticks.y = element_blank())) + 
  (p4 & theme(axis.text.y = element_blank(),
              axis.ticks.y = element_blank())) +
  (p5 & theme(axis.text.y = element_blank(),
              axis.ticks.y = element_blank())) +
  (p6 & theme(axis.text.y = element_blank(),
              axis.ticks.y = element_blank())) +
  plot_layout(nrow=1) + 
  plot_annotation(title="Edinburgh Cognitive and Behavioural ALS Screen (ECAS)")
ggsave("Plots/WP6_ECAS_joint.png", height=4, width=9, dpi=600)
rm(p, p1, p2, p3, p4, p5, p6)


# SPART
# immediate & delayed (overall mean)
p1 <- raincloud(data_individual, "group", "SPART_mean_all", "recall score", NULL, "Spatial memory (SPART)")

# immediate
p1a <- raincloud(data_individual, "group", "SPART_mean_I", "recall score", NULL, "Immediate")

# delayed
p1b <- raincloud(data_individual, "group", "SPART_q4_II", "recall score", NULL, "Delayed")

# joint plot 
p <- p1a + 
  (p1b & theme(axis.text.y = element_blank(),
              axis.ticks.y = element_blank())) + 
  plot_annotation(title="Spatial memory (SPART)")
ggsave("Plots/WP6_SPART_joint.png", height=4, width=5, dpi=600)
rm(p, p1a, p1b)


# 5PT
# general score: productivity
p2 <- raincloud(data_individual, "group", "FIVE_P_productivity", "number of unique figures", NULL, "Spatial fluency (5PT)")

# productivity
p2a <- raincloud(data_individual, "group", "FIVE_P_productivity", "number of unique figures", NULL, "Productivity")

# perseveration
p2b <- raincloud(data_individual, "group", "FIVE_P_flexibility", "relative %", NULL, "Perseveration")

# strategy
p2c <- raincloud(data_individual, "group", "FIVE_P_strategy", "relative %", NULL, "Strategy")

# joint plot 
p <- p2a + 
  (p2b & theme(axis.text.y = element_blank(),
               axis.ticks.y = element_blank())) + 
  (p2c & theme(axis.text.y = element_blank(),
               axis.ticks.y = element_blank())) + 
  plot_annotation(title="Spatial fluency (5PT)")
ggsave("Plots/WP6_5PT_joint.png", height=4, width=6, dpi=600)
rm(p, p2a, p2b, p2c)


# PTSOT 
# mean deviation
p3 <- raincloud(data_individual, "group", "PTSOT_mean_dev", "mean angle deviation", NULL, "Spatial Orientation (PTSOT)")


# joint NP plot 
p <- p1 + 
  (p2 & theme(axis.text.y = element_blank(),
              axis.ticks.y = element_blank())) + 
  (p3 & theme(axis.text.y = element_blank(),
              axis.ticks.y = element_blank())) + 
  plot_annotation(title="Neuropsychological assessment")
ggsave("Plots/WP6_NP_joint.png", height=4, width=8, dpi=600)
rm(p, p1, p2, p3)


# # ggradar plot for comparison 
# t <- data_individual %>%
#   select(ID, group, ECAS_sub_executive, ECAS_sub_language, ECAS_sub_verbal_fluency, ECAS_sub_memory, ECAS_sub_spatial,
#          FIVE_P_productivity, SPART_mean_all, PTSOT_mean_dev) %>% 
#   drop_na() %>%
#   group_by(group) %>%
#   summarise(
#     `executive function (ECAS)`=mean(ECAS_sub_executive), 
#     `language (ECAS)`=mean(ECAS_sub_language), 
#     `verbal fluency (ECAS)`=mean(ECAS_sub_verbal_fluency), 
#     `verbal memory (ECAS)`=mean(ECAS_sub_memory), 
#     `visuo-spatial (ECAS)`=mean(ECAS_sub_spatial),
#     `spatial fluency (5PT)`=mean(FIVE_P_productivity), 
#     `spatial memory (SPART)`=mean(SPART_mean_all), 
#     `perspective taking (PTSOT)`=mean(PTSOT_mean_dev)
#   )
# 
# radar <- t %>%
#   ggradar(
#     group.line.width = 1, 
#     group.point.size = 3,
#     values.radar=c(0,35,70),
#     grid.min=0, grid.mid=35, grid.max=70,
#     gridline.mid.colour = "grey",
#     legend.position="bottom",
#     axis.label.offset=1.2,
#     axis.label.size=4,
#     grid.label.size=3,
#     legend.text.size=11,
#     plot.extent.x.sf=1.8,
#     plot.extent.y.sf=1.2
#   )
# radar + 
#   labs(subtitle="Neuropsychology for MND patients and controls (raw scores)") + 
#   theme(plot.subtitle=element_text(size=16))
# ggsave("Plots/WP6_ggradar.png") 
# rm(t)


###################################################################################################


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


##############################################################################


# ### Starmaze (Deetje)
# 
# ## settings
# im_width=5
# im_height=4
# im_dpi=600
# 
# 
# # Starmaze
# mylabels = as_labeller(c(`0` = "Inner area", `1` = "tower-forest", `2` = "forest-mountain",
#                           `3` = "mountain-forest (goal)", `4` = "forest-mountain", `5` = "mountain-tower"))
# 
# # FINAL LOCATION
# # overview
# t <- trial_data %>%
#   filter(probe_trial==1) %>%
#   mutate(trial_condition=fct_relevel(trial_condition, "training", "egocentric", "mixed", "allocentric")) %>%
#   group_by(group, trial_condition, final_alley) %>%
#   tally() %>%
#   mutate(percent = n / sum(n))
# 
# barplot(t, "final_alley", "percent", "group", "trial_condition", mylabels, "Final location: Probe trials", NULL, "Percentage (%)", "top")
# ggsave("Plots/SM/WP6_Final_location.png", width=4.5, height=5.5, dpi=600)
# rm(t)
# 
# 
# # ### for poster
# # t <- t %>%  filter(trial_condition != "training")
# #
# # mylabels = as_labeller(c(`0` = "Other", `1` = "alley 1", `2` = "alley 2",
# #                          `3` = "alley 3", `4` = "alley 4", `5` = "alley 5"))
# #
# # mylabels_2 = as_labeller(c(`egocentric` = "forced egocentric", `mixed` = "strategy preference test", `allocentric` = "forced allocentric"))
# #
# # barplot(t, "final_alley", "percent", "group", "trial_condition", mylabels, "Final location: Probe trials", NULL, "Percentage (%)", "top") +
# #   facet_wrap(~ trial_condition, nrow=1, labeller=mylabels_2) +  labs(title=NULL, y="Percentage (%) chosen goal location")
# # ggsave("Plots/SM/WP6_Final_location_poster.png", width=7, height=3.5, dpi=600)
# # ###
# 
# 
# # mean values
# trial_data %>%
#   filter(probe_trial==1) %>%
#   mutate(trial_condition=fct_relevel(trial_condition, "training", "egocentric", "mixed", "allocentric")) %>%
#   group_by(group, trial_condition) %>%
#   summarize(mean_score = mean(success))
# 
# 
# trial_data %>%
#   filter(probe_trial==1) %>%
#   mutate(trial_condition=fct_relevel(trial_condition, "training", "egocentric", "mixed", "allocentric")) %>%
#   group_by(group) %>%
#   summarize(mean_score = mean(success))
# 
# 
# # details
# # egocentric
# t <- trial_data %>%
#   mutate(final_alley=factor(final_alley)) %>%
#   filter(probe_trial==1 & trial_condition=="egocentric") %>%
#   mutate(trial_num_new=case_when(trial_num==10 ~ "1. probe", trial_num==13 ~ "2. probe")) %>%
#   group_by(group, trial_num_new, final_alley) %>%
#   tally() %>%
#   complete(final_alley, fill=list(n=0)) %>%
#   mutate(percent = n / sum(n))
# 
# barplot(t, "final_alley", "percent", "group", "trial_num_new", mylabels, "Final location: Egocentric probe", NULL, "Percentage (%)", "top")
# ggsave("Plots/SM/WP6_Final_location_ego.png", width=3.5, height=4, dpi=600)
# rm(t)
# 
# # mixed
# t <- trial_data %>%
#   mutate(final_alley=factor(final_alley)) %>%
#   filter(probe_trial==1 & trial_condition=="mixed") %>%
#   mutate(trial_num_new=case_when(trial_num==14 ~ "1. probe (G)", trial_num==16 ~ "2. probe (G)", trial_num==28 ~ "3. probe (G)")) %>%
#   group_by(group, trial_num_new, final_alley) %>%
#   tally() %>%
#   complete(final_alley, fill=list(n=0)) %>%
#   mutate(percent = n / sum(n))
# 
# barplot(t, "final_alley", "percent", "group", "trial_num_new", mylabels, "Final location: Mixed probe", NULL, "Percentage (%)", "top")
# ggsave("Plots/SM/WP6_Final_location_mixed.png", width=4.5, height=4.5, dpi=600)
# rm(t)
# 
# # allocentric
# t <- trial_data %>%
#   mutate(final_alley=factor(final_alley)) %>%
#   filter(probe_trial==1 & trial_condition=="allocentric") %>%
#   mutate(trial_num_new=case_when(trial_num==17 ~ "1. probe (C)", trial_num==18 ~ "2. probe (I)", trial_num==27 ~ "3. probe (C)",
#                                  trial_num==29 ~ "4. probe (I)", trial_num==30 ~ "5. probe (E)")) %>%
#   group_by(group, trial_num_new, final_alley) %>%
#   tally() %>%
#   complete(final_alley, fill=list(n=0)) %>%
#   mutate(percent = n / sum(n))
# 
# barplot(t, "final_alley", "percent", "group", "trial_num_new", mylabels, "Final location: Allocentric probe", NULL, "Percentage (%)", "top")
# ggsave("Plots/SM/WP6_Final_location_allo.png", width=5.5, height=5.5, dpi=600)
# rm(t)
# 
# 
# # STRATEGY
# # all probe trials
# t <- trial_data %>%
#   mutate(search_strategy_no=factor(search_strategy_no)) %>%
#   mutate(trial_condition=fct_relevel(trial_condition, "training", "egocentric", "mixed", "allocentric")) %>%
#   filter(probe_trial==1) %>%
#   group_by(group, trial_condition, search_strategy_no) %>%
#   tally() %>%
#   complete(search_strategy_no, fill=list(n=0)) %>%
#   mutate(percent = n / sum(n))
# 
# mylabels = as_labeller(c(`1` = "Direct path", `2` = "Direct fail", `3` = "Detour",
#                          `4` = "Reoriented", `6` = "Serial",`7` = "Random"))
# 
# barplot(t, "search_strategy_no", "percent", "group", "trial_condition", mylabels, "Strategy (all probe trials)", NULL, "Percentage (%)", "top")
# ggsave("Plots/SM/WP6_Strategy.png", width=3.5, height=4.5, dpi=600)
# rm(t)
# 
# # successful probe trials
# t <- trial_data %>%
#   mutate(search_strategy_no=factor(search_strategy_no)) %>%
#   mutate(trial_condition=fct_relevel(trial_condition, "training", "egocentric", "mixed", "allocentric")) %>%
#   filter(probe_trial==1, success==1) %>%
#   group_by(group, trial_condition, search_strategy_no) %>%
#   tally() %>%
#   complete(search_strategy_no, fill=list(n=0)) %>%
#   mutate(percent = n / sum(n))
# 
# barplot(t, "search_strategy_no", "percent", "group", "trial_condition", mylabels, "Strategy (successful probe trials)", NULL, "Percentage (%)", "top")
# ggsave("Plots/SM/WP6_Strategy_cor.png", width=3.5, height=4.5, dpi=600)
# rm(t)
# 
# # feedback trials
# t <- trial_data %>%
#   mutate(search_strategy_no=factor(search_strategy_no)) %>%
#   mutate(trial_condition=fct_relevel(trial_condition, "training", "egocentric", "mixed", "allocentric")) %>%
#   filter(probe_trial==0) %>%
#   group_by(group, trial_condition, search_strategy_no) %>%
#   tally() %>%
#   complete(search_strategy_no, fill=list(n=0)) %>%
#   mutate(percent = n / sum(n))
# 
# barplot(t, "search_strategy_no", "percent", "group", "trial_condition", mylabels, "Strategy (feedback trials)", NULL, "Percentage (%)", "top")
# ggsave("Plots/SM/WP6_Strategy_training.png", width=3.5, height=4.5, dpi=600)
# rm(t)
# 
# 
# # PATH EFFICIENCY
# 
# ## settings
# im_width=5.5
# im_height=5.5
# im_dpi=600
# 
# 
# # all probe trials
# t <- trial_data %>%
#   filter(probe_trial==1) %>%
#   mutate(trial_condition=fct_relevel(trial_condition, "training", "egocentric", "mixed", "allocentric")) %>%
#   group_by(id, group, trial_condition)
# t <- mean_func(t)
# 
# raincloud(t, "group", "time", "Time in seconds", NULL, mytitle="Time to reach target (all probe trials)", facetvar="trial_condition")
# ggsave("Plots/SM/WP6_Time_probe.png", width=im_width, height=im_height, dpi=im_dpi)
# 
# raincloud(t, "group", "path_length", "Path length", NULL, mytitle="Path to reach target (all probe trials)", facetvar="trial_condition")
# ggsave("Plots/SM/WP6_Path_probe.png", width=im_width, height=im_height, dpi=im_dpi)
# 
# raincloud(t, "group", "path_error", "Path error", NULL, mytitle="Path deviation from ideal path (all probe trials)", facetvar="trial_condition")
# ggsave("Plots/SM/WP6_Path_error_probe.png", width=im_width, height=im_height, dpi=im_dpi)
# 
# raincloud(t, "group", "avg_dist_target", "Avg. distance to target", NULL, mytitle="Avg. distance to reach target (all probe trials)", facetvar="trial_condition")
# ggsave("Plots/SM/WP6_Distance_probe.png", width=im_width, height=im_height, dpi=im_dpi)
# 
# raincloud(t, "group", "distance_error", "Avg. distance error", NULL, mytitle="Avg. distance deviation from ideal avg. distance (all probe trials)", facetvar="trial_condition")
# ggsave("Plots/SM/WP6_Distance_error_probe.png", width=im_width, height=im_height, dpi=im_dpi)
# 
# rm(t)
# 
# 
# # all correct probe trials
# t <- trial_data %>%
#   filter(probe_trial==1, success==1) %>%
#   mutate(trial_condition=fct_relevel(trial_condition, "training", "egocentric", "mixed", "allocentric")) %>%
#   group_by(id, group, trial_condition)
# t <- mean_func(t)
# 
# raincloud(t, "group", "time", "Time in seconds", NULL, mytitle="Time to reach target (successful probe trials)", facetvar="trial_condition")
# ggsave("Plots/SM/WP6_Time_probe_cor.png", width=im_width, height=im_height, dpi=im_dpi)
# 
# raincloud(t, "group", "path_length", "Path length", NULL, mytitle="Path to reach target (successful probe trials)", facetvar="trial_condition")
# ggsave("Plots/SM/WP6_Path_probe_cor.png", width=im_width, height=im_height, dpi=im_dpi)
# 
# raincloud(t, "group", "path_error", "Path error", NULL, mytitle="Path deviation from ideal path (successful probe trials)", facetvar="trial_condition")
# ggsave("Plots/SM/WP6_Path_error_probe_cor.png", width=im_width, height=im_height, dpi=im_dpi)
# 
# raincloud(t, "group", "avg_dist_target", "Avg. distance to target", NULL, mytitle="Avg. distance to reach target (successful probe trials)", facetvar="trial_condition")
# ggsave("Plots/SM/WP6_Distance_probe_cor.png", width=im_width, height=im_height, dpi=im_dpi)
# 
# raincloud(t, "group", "distance_error", "Avg. distance error", NULL, mytitle="Avg. distance deviation from ideal avg. distance (successful probe trials)", facetvar="trial_condition")
# ggsave("Plots/SM/WP6_Distance_error_probe_cor.png", width=im_width, height=im_height, dpi=im_dpi)
# 
# rm(t)
#
#
# ### for poster
# # data
# # add subgroups for raincloud_sub
# d <- data_individual %>%
#   mutate(Sub=factor(case_when(is.na(MNE_Untergruppe) ~ "Control", T ~ as.character(MNE_Untergruppe)),
#                     levels=c("ALS", "PLS", "PMA", "Control")))
# q <- d %>% select(ID, Sub) %>% rename(id=ID)
# r <- t %>%
#   filter(trial_condition %in% c("egocentric", "allocentric")) %>%
#   left_join(q)
#
# # without subgroup
# p1 <- raincloud(r, "group", "time", "seconds", NULL, mytitle="Time per trial", facetvar="trial_condition")
# p2 <- raincloud(r, "group", "path_length", "virtual meters", NULL, mytitle="Path length", facetvar="trial_condition")
#
# # joint plot
# p <- p1 / p2
# ggsave("Plots/SM/WP6_SM_joint_all.png", height=6, width=5, dpi=600)
#
#
# # with subgroup (only MND)
# raincloud_sub_mne <- function(data, xvar, yvar, ylab, xlab, sub, mytitle=NULL){
#   p1 <- ggplot(data, aes(x=get(xvar),y=get(yvar),fill=get(xvar))) + # set up data
#     geom_flat_violin(position=position_nudge(x=.12,y=0)) + # rain cloud: setting "adjust" for smoothness of kernel
#     geom_point(aes(shape = get(sub)), size = 3/1.5, position=position_jitter(w=.1,h=.05,seed=100)) + # points
#     geom_point(aes(colour = get(sub), shape = get(sub)), size = 1/1.5, position=position_jitter(w=.1,h=.05,seed=100)) + # point
#     scale_shape_manual(values=c(15,16,17,18)) +
#     scale_colour_manual(values=c("skyblue","yellow","salmon","black")) +
#     scale_fill_grey(start=0.75, end=0.75) +
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
#
# p1 <- raincloud_sub_mne(r %>% filter(group=="MND"), "group", "time", "seconds", NULL, "Sub", mytitle="Time per trial") + facet_wrap(~ trial_condition)
# ggsave("Plots/SM/WP6_SM_time_sub.png", height=3, width=6, dpi=600)
# p2 <- raincloud_sub_mne(r %>% filter(group=="MND"), "group", "path_length", "virtual meters", NULL, "Sub", mytitle="Path length") + facet_wrap(~ trial_condition)
# ggsave("Plots/SM/WP6_SM_path_sub.png", height=3, width=6, dpi=600)
#
# # joint plot
# p <- p1 / p2 +
#   plot_layout(guides="collect") & theme(legend.position = "bottom", legend.justification = c(0,0))
# ggsave("Plots/SM/WP6_SM_joint_sub.png", height=7, width=5, dpi=600)
#
# ###
# 
# 
# 
# # all training trials
# t <- trial_data %>%
#   filter(probe_trial==0) %>%
#   mutate(trial_condition=fct_relevel(trial_condition, "training", "egocentric", "mixed", "allocentric")) %>%
#   group_by(id, group, trial_condition)
# t <- mean_func(t)
# 
# raincloud(t, "group", "time", "Time in seconds", NULL, mytitle="Time to reach target (training trials)", facetvar="trial_condition")
# ggsave("Plots/SM/WP6_Time_training.png", width=im_width, height=im_height, dpi=im_dpi)
# 
# raincloud(t, "group", "path_length", "Path length", NULL, mytitle="Path to reach target (training trials)", facetvar="trial_condition")
# ggsave("Plots/SM/WP6_Path_training.png", width=im_width, height=im_height, dpi=im_dpi)
# 
# raincloud(t, "group", "path_error", "Path error", NULL, mytitle="Path deviation from ideal path (training trials)", facetvar="trial_condition")
# ggsave("Plots/SM/WP6_Path_error_training.png", width=im_width, height=im_height, dpi=im_dpi)
# 
# raincloud(t, "group", "avg_dist_target", "Avg. distance to target", NULL, mytitle="Avg. distance to reach target (training trials)", facetvar="trial_condition")
# ggsave("Plots/SM/WP6_Distance_training.png", width=im_width, height=im_height, dpi=im_dpi)
# 
# raincloud(t, "group", "distance_error", "Avg. distance error", NULL, mytitle="Avg. distance deviation from ideal avg. distance (training trials)", facetvar="trial_condition")
# ggsave("Plots/SM/WP6_Distance_error_training.png", width=im_width, height=im_height, dpi=im_dpi)
# 
# rm(t)
