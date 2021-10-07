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


### functions

# # calculating mean and sd 
# data_summary <- function(data, varname, groupnames){
#   require(plyr)
#   summary_func <- function(x, col){
#     c(mean = mean(x[[col]], na.rm=TRUE),
#       sd = sd(x[[col]], na.rm=TRUE))
#   }
#   data_sum <- ddply(data, groupnames, .fun=summary_func,
#                   varname)
#   data_sum <- rename(data_sum, c("mean" = varname))
#   return(data_sum)
# }

# function for calculating means
mean_func <- function(data){
  data <- data %>% 
    summarise(success=mean(success),
              time=mean(time),
              path_length=mean(path_length),
              path_error=mean(path_error),
              avg_dist_target=mean(avg_dist_target),
              distance_error=mean(distance_error),
              avg_distance_fxy=mean(avg_distance_fxy))
              # + zone entries & zone error
              
  return(data)
}

raincloud <- function(data, xvar, yvar, ylab, xlab, mytitle=NULL, facetvar="none"){
  p1 <- ggplot(data, aes(x=get(xvar),y=get(yvar),fill=get(xvar))) + # set up data 
    geom_flat_violin(position=position_nudge(x=.2,y=0)) + # rain cloud: setting "adjust" for smoothness of kernel
    geom_boxplot(aes(x=as.numeric(get(xvar))+0.2,y=get(yvar)), outlier.shape=NA, alpha=0.3, width=0.1, colour="BLACK") + 
    geom_point(position=position_jitter(w=.1,h=0.05,seed=100), size=1) + 
    scale_fill_grey(start=0.99, end=0.75) +
    coord_flip() + 
    guides(fill=FALSE) + 
    theme_classic() + 
    labs(subtitle=mytitle,
         x = xlab, 
         y = ylab) 
  
  if(facetvar != "none") {
    p1 <- p1 + facet_wrap(facetvar)
  }
    
  return(p1)
}

# raincloud_sub <- function(data, x, y, ylab, xlab, sub){
#   p1 <- ggplot(data_individual, aes_string(x=x,y=y,fill=x)) + # set up data 
#     geom_flat_violin(position=position_nudge(x=.2,y=0)) + # rain cloud: setting "adjust" for smoothness of kernel
#     geom_point(aes_string(shape = sub), size = 3, position=position_jitter(w=.1,h=.05,seed=100)) + # points
#     geom_point(aes_string(colour = sub, shape = sub), size = 1, position=position_jitter(w=.1,h=.05,seed=100)) + # point
#     geom_boxplot(aes_string(x=as.numeric(x)+0.2,y=y), outlier.shape=NA, alpha=0.3, width=0.1, colour="BLACK") + 
#     scale_shape_manual(values=c(15,16,17,18)) + 
#     scale_colour_manual(values=c("skyblue","yellow","salmon","white")) + 
#     scale_fill_grey(start=0.99, end=0.75) +
#     coord_flip() + # flip axes
#     guides(fill=FALSE) +
#     theme_minimal() + 
#     theme(legend.position = "bottom",
#           legend.text = element_text(size=12),
#           legend.title = element_blank()) +
#     labs(x = xlab, 
#          y = ylab)
#   
#   return(p1)
# }

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
          axis.text.x=element_text(angle=65, hjust=1)) + 
    labs(title = mytitle,
         x = xlab,
         y = ylab)
  
  return(p1)
}

scatter <- function(data, x, y, ylab, xlab){
  p1 <- ggplot(data, aes(x=get(x), y=get(y))) +
    geom_point() +  # scatter
    geom_smooth(method=lm) + # prediction line 
    theme_minimal() + 
    labs(x = xlab, 
         y = ylab)
  return(p1)
}


###########################################################################
### demographic data 

temp <- subset(data_individual, select=c(ID, group, MN, MNE_Untergruppe, ALS_Variante, is_category, `ALS-FRS-R`, `FRS-/Monat`,
                                    dfb_q1_sex, dfb_q2_age, dfb_q3_years_edu_total, dfb_q4_highestedu, 
                                    dfb_q5_language_german, dfb_q6_handiness, 
                                    dfb_q21_comp_expertise, dfb_q22_comp_freq,
                                    sbsds_total_score, id_t1_months)) %>% 
  mutate(dfb_q1_sex=droplevels(dfb_q1_sex),
         dfb_q5_language_german=fct_recode(dfb_q5_language_german, yes = "Deutsch ist Muttersprache", no = "Deutsch ist nicht Muttersprache"),
         dfb_q6_handiness=fct_recode(dfb_q6_handiness, right = "rechtshändig", left = "linkshändig", both = "beidhändig"),
         dfb_q21_comp_expertise=as.numeric(dfb_q21_comp_expertise),
         dfb_q22_comp_freq=as.numeric(dfb_q22_comp_freq))

t1 <- temp %>% 
  select(-c(ID, MN, MND_Untergruppe, ALS_Variante, is_category, `ALS-FRS-R`, `FRS-/Monat`, id_t1_months)) %>% 
  tbl_summary(by=group, 
              label=list(dfb_q1_sex ~ "Gender", dfb_q2_age ~ "Age", dfb_q3_years_edu_total ~ "Years of education",
                         dfb_q4_highestedu ~ "Education level", dfb_q5_language_german ~ "German native speaker", 
                         dfb_q6_handiness ~ "Handedness", dfb_q21_comp_expertise ~ "Self-rated computer expertise", 
                         dfb_q22_comp_freq ~ "Self-rated computer use frequency", sbsds_total_score ~ "Self-rated spatial abilities (SBSDS)"),
              type=list(dfb_q21_comp_expertise ~ 'continuous', dfb_q22_comp_freq  ~ 'continuous'),
              statistic=list(all_continuous() ~ "{mean} ({sd})", all_categorical() ~ "{n} ({p}%)"),
              digits=list(all_continuous() ~ c(1, 2)),
              missing="no") %>% 
  add_p(test=list(all_continuous() ~ "t.test", all_categorical() ~  "fisher.test")) %>% 
  modify_header(label = "**Variable**")

t1 %>% 
  as_flex_table() %>%
  flextable::save_as_docx(path="WP6_data/CTR_MND_Demographics.docx")

t2 <- temp %>% 
  filter(group=="MND") %>% 
  select(-c(ID, group, dfb_q1_sex, dfb_q2_age, dfb_q3_years_edu_total, dfb_q4_highestedu, 
            dfb_q5_language_german, dfb_q6_handiness, dfb_q21_comp_expertise, dfb_q22_comp_freq, sbsds_total_score)) %>% 
  tbl_summary(label=list(MN ~ "Motor neuron involvement", MNE_Untergruppe ~ "MND subgroup", ALS_Variante ~ "ALS variant",
                         is_category ~ "Initial symptoms", id_t1_months ~ "Time between diagnosis and study (months)"),
              statistic=list(all_continuous() ~ "{mean} (IQR: {p25}-{p75})", all_categorical() ~ "{n} ({p}%)"),
              digits=list(all_continuous() ~ c(1, 1))) %>% 
  add_n() %>%
  modify_header(label = "**Variable**")

t2 %>% 
  as_flex_table() %>%
  flextable::save_as_docx(path="WP6_data/MND_Sub_Demographics.docx")

rm(temp, t1, t2)


####################################################################################################
### Plotting


# ## settings 
# im_width=5
# im_height=4
# im_dpi=600


# Starmaze
mylabels = as_labeller(c(`0` = "Inner area", `1` = "tower-forest", `2` = "forest-mountain",
                         `3` = "mountain-forest (goal)", `4` = "forest-mountain", `5` = "mountain-tower"))


# FINAL LOCATION 
# overview
t <- trial_data %>%  
  filter(probe_trial==1) %>% 
  mutate(trial_condition=fct_relevel(trial_condition, "training", "egocentric", "mixed", "allocentric")) %>% 
  group_by(group, trial_condition, final_alley) %>% 
  tally() %>% 
  mutate(percent = n / sum(n))

barplot(t, "final_alley", "percent", "group", "trial_condition", mylabels, "Final location: Probe trials", NULL, "Percentage (%)", "top")
ggsave("Plots/SM/WP6_Final_location.png", width=4.5, height=5.5, dpi=600)
rm(t)

t <- trial_data %>%  
  filter(probe_trial==1) %>% 
  mutate(trial_condition=fct_relevel(trial_condition, "training", "egocentric", "mixed", "allocentric")) %>% 
  group_by(group, trial_condition) %>% 
  summarize(mean_score = mean(success))
t

t <- trial_data %>%  
  filter(probe_trial==1) %>% 
  mutate(trial_condition=fct_relevel(trial_condition, "training", "egocentric", "mixed", "allocentric")) %>% 
  group_by(group) %>% 
  summarize(mean_score = mean(success))
t


# details 
# egocentric 
t <- trial_data %>%  
  mutate(final_alley=factor(final_alley)) %>% 
  filter(probe_trial==1 & trial_condition=="egocentric") %>%   
  mutate(trial_num_new=case_when(trial_num==10 ~ "1. probe", trial_num==13 ~ "2. probe")) %>% 
  group_by(group, trial_num_new, final_alley) %>% 
  tally() %>% 
  complete(final_alley, fill=list(n=0)) %>% 
  mutate(percent = n / sum(n))

barplot(t, "final_alley", "percent", "group", "trial_num_new", mylabels, "Final location: Egocentric probe", NULL, "Percentage (%)", "top")
ggsave("Plots/SM/WP6_Final_location_ego.png", width=3.5, height=4, dpi=600)
rm(t)

# mixed 
t <- trial_data %>%  
  mutate(final_alley=factor(final_alley)) %>% 
  filter(probe_trial==1 & trial_condition=="mixed") %>% 
  mutate(trial_num_new=case_when(trial_num==14 ~ "1. probe (G)", trial_num==16 ~ "2. probe (G)", trial_num==28 ~ "3. probe (G)")) %>% 
  group_by(group, trial_num_new, final_alley) %>% 
  tally() %>% 
  complete(final_alley, fill=list(n=0)) %>% 
  mutate(percent = n / sum(n))

barplot(t, "final_alley", "percent", "group", "trial_num_new", mylabels, "Final location: Mixed probe", NULL, "Percentage (%)", "top")
ggsave("Plots/SM/WP6_Final_location_mixed.png", width=4.5, height=4.5, dpi=600)
rm(t)

# allocentric 
t <- trial_data %>%  
  mutate(final_alley=factor(final_alley)) %>% 
  filter(probe_trial==1 & trial_condition=="allocentric") %>% 
  mutate(trial_num_new=case_when(trial_num==17 ~ "1. probe (C)", trial_num==18 ~ "2. probe (I)", trial_num==27 ~ "3. probe (C)",
                                 trial_num==29 ~ "4. probe (I)", trial_num==30 ~ "5. probe (E)")) %>% 
  group_by(group, trial_num_new, final_alley) %>% 
  tally() %>% 
  complete(final_alley, fill=list(n=0)) %>% 
  mutate(percent = n / sum(n))

barplot(t, "final_alley", "percent", "group", "trial_num_new", mylabels, "Final location: Allocentric probe", NULL, "Percentage (%)", "top")
ggsave("Plots/SM/WP6_Final_location_allo.png", width=5.5, height=5.5, dpi=600)
rm(t)


# PATH EFFICIENCY 

## settings 
im_width=5.5
im_height=5.5
im_dpi=600


# all probe trials 
t <- trial_data %>%  
  filter(probe_trial==1) %>% 
  mutate(trial_condition=fct_relevel(trial_condition, "training", "egocentric", "mixed", "allocentric")) %>% 
  group_by(id, group, trial_condition)
t <- mean_func(t)

raincloud(t, "group", "time", "Time in seconds", NULL, mytitle="Time to reach target (all probe trials)", facetvar="trial_condition")
ggsave("Plots/SM/WP6_Time_probe.png", width=im_width, height=im_height, dpi=im_dpi)

raincloud(t, "group", "path_length", "Path length", NULL, mytitle="Path to reach target (all probe trials)", facetvar="trial_condition")
ggsave("Plots/SM/WP6_Path_probe.png", width=im_width, height=im_height, dpi=im_dpi)

raincloud(t, "group", "path_error", "Path error", NULL, mytitle="Path deviation from ideal path (all probe trials)", facetvar="trial_condition")
ggsave("Plots/SM/WP6_Path_error_probe.png", width=im_width, height=im_height, dpi=im_dpi)

raincloud(t, "group", "avg_dist_target", "Avg. distance to target", NULL, mytitle="Avg. distance to reach target (all probe trials)", facetvar="trial_condition")
ggsave("Plots/SM/WP6_Distance_probe.png", width=im_width, height=im_height, dpi=im_dpi)

raincloud(t, "group", "distance_error", "Avg. distance error", NULL, mytitle="Avg. distance deviation from ideal avg. distance (all probe trials)", facetvar="trial_condition")
ggsave("Plots/SM/WP6_Distance_error_probe.png", width=im_width, height=im_height, dpi=im_dpi)

rm(t)


# all correct probe trials 
t <- trial_data %>%  
  filter(probe_trial==1, success==1) %>% 
  mutate(trial_condition=fct_relevel(trial_condition, "training", "egocentric", "mixed", "allocentric")) %>% 
  group_by(id, group, trial_condition)
t <- mean_func(t)

raincloud(t, "group", "time", "Time in seconds", NULL, mytitle="Time to reach target (successful probe trials)", facetvar="trial_condition")
ggsave("Plots/SM/WP6_Time_probe_cor.png", width=im_width, height=im_height, dpi=im_dpi)

raincloud(t, "group", "path_length", "Path length", NULL, mytitle="Path to reach target (successful probe trials)", facetvar="trial_condition")
ggsave("Plots/SM/WP6_Path_probe_cor.png", width=im_width, height=im_height, dpi=im_dpi)

raincloud(t, "group", "path_error", "Path error", NULL, mytitle="Path deviation from ideal path (successful probe trials)", facetvar="trial_condition")
ggsave("Plots/SM/WP6_Path_error_probe_cor.png", width=im_width, height=im_height, dpi=im_dpi)

raincloud(t, "group", "avg_dist_target", "Avg. distance to target", NULL, mytitle="Avg. distance to reach target (successful probe trials)", facetvar="trial_condition")
ggsave("Plots/SM/WP6_Distance_probe_cor.png", width=im_width, height=im_height, dpi=im_dpi)

raincloud(t, "group", "distance_error", "Avg. distance error", NULL, mytitle="Avg. distance deviation from ideal avg. distance (successful probe trials)", facetvar="trial_condition")
ggsave("Plots/SM/WP6_Distance_error_probe_cor.png", width=im_width, height=im_height, dpi=im_dpi)

rm(t)


# all training trials 
t <- trial_data %>%  
  filter(probe_trial==0) %>% 
  mutate(trial_condition=fct_relevel(trial_condition, "training", "egocentric", "mixed", "allocentric")) %>% 
  group_by(id, group, trial_condition)
t <- mean_func(t)

raincloud(t, "group", "time", "Time in seconds", NULL, mytitle="Time to reach target (training trials)", facetvar="trial_condition")
ggsave("Plots/SM/WP6_Time_training.png", width=im_width, height=im_height, dpi=im_dpi)

raincloud(t, "group", "path_length", "Path length", NULL, mytitle="Path to reach target (training trials)", facetvar="trial_condition")
ggsave("Plots/SM/WP6_Path_probe_training.png", width=im_width, height=im_height, dpi=im_dpi)

raincloud(t, "group", "path_error", "Path error", NULL, mytitle="Path deviation from ideal path (training trials)", facetvar="trial_condition")
ggsave("Plots/SM/WP6_Path_error_probe_training.png", width=im_width, height=im_height, dpi=im_dpi)

raincloud(t, "group", "avg_dist_target", "Avg. distance to target", NULL, mytitle="Avg. distance to reach target (training trials)", facetvar="trial_condition")
ggsave("Plots/SM/WP6_Distance_probe_training.png", width=im_width, height=im_height, dpi=im_dpi)

raincloud(t, "group", "distance_error", "Avg. distance error", NULL, mytitle="Avg. distance deviation from ideal avg. distance (training trials)", facetvar="trial_condition")
ggsave("Plots/SM/WP6_Distance_error_probe_training.png", width=im_width, height=im_height, dpi=im_dpi)

rm(t)


# Starmaze Motor Control

## settings 
im_width=5
im_height=4
im_dpi=600

# # time # tbd correct error in Matlab saving
# p <- raincloud(data_individual, "group", "mct_time", "Motor control task: Time", NULL)
# ggsave("Plots/MC/WP6_Motor_control_time.png", width=im_width, height=im_height, dpi=im_dpi)
# rm(p)

# path
p <- raincloud(data_individual, "group", "mct_path", "Motor control task: Path length", NULL)
ggsave("Plots/MC/WP6_Motor_control_path.png", width=im_width, height=im_height, dpi=im_dpi)
rm(p)

# velocity
p <- raincloud(data_individual, "group", "mct_velocity", "Motor control task: Velocity", NULL)
ggsave("Plots/MC/WP6_Motor_control_velocity.png", width=im_width, height=im_height, dpi=im_dpi)
rm(p)



## neuropsychology 

## settings 
im_width=5
im_height=4
im_dpi=600

# ECAS
# total
p <- raincloud(data_individual, "group", "ECAS_total_score", "ECAS - Total Score", NULL)
ggsave("Plots/ECAS/WP6_ECAS_total_score.png", width=im_width, height=im_height, dpi=im_dpi)
rm(p)

# p <- raincloud_sub(data_individual, "group", "ECAS_total_score", "ECAS - Total Score", "Group", MN)
# ggsave("Plots/ECAS/WP6_ECAS_total_score_subgroup_mn.png")
# rm(p)
# 
# p <- raincloud_sub(data_individual, "group", "ECAS_total_score", "ECAS - Total Score", "Group", is_category)
# ggsave("Plots/ECAS/WP6_ECAS_total_score_subgroup_cat.png")
# rm(p)

# memory 
p <- raincloud(data_individual, "group", "ECAS_sub_memory", "ECAS - Memory Score", NULL)
ggsave("Plots/ECAS/WP6_ECAS_memory.png", width=im_width, height=im_height, dpi=im_dpi)
rm(p)

# spatial abilities 
p <- raincloud(data_individual, "group", "ECAS_sub_spatial", "ECAS - Spatial Score", NULL)
ggsave("Plots/ECAS/WP6_ECAS_visuospatial.png", width=im_width, height=im_height, dpi=im_dpi)
rm(p)

# language
p <- raincloud(data_individual, "group", "ECAS_sub_language", "ECAS - Language Score", "Group", NULL)
ggsave("Plots/ECAS/WP6_ECAS_language.png", width=im_width, height=im_height, dpi=im_dpi)
rm(p)

# verbal fluency
p <- raincloud(data_individual, "group", "ECAS_sub_verbal_fluency", "ECAS - Verbal Fluency Score", NULL)
ggsave("Plots/ECAS/WP6_ECAS_verbal_fluency.png", width=im_width, height=im_height, dpi=im_dpi)
rm(p)

# executive
p <- raincloud(data_individual, "group", "ECAS_sub_executive", "ECAS - Executive Score", NULL)
ggsave("Plots/ECAS/WP6_ECAS_executive.png", width=im_width, height=im_height, dpi=im_dpi)
rm(p)


# SPART
# immediate & delayed (overall mean)
p <- raincloud(data_individual, "group", "SPART_mean_all", "SPART - Spatial Memory (immediate and delayed)", NULL)
ggsave("Plots/SPART/WP6_SPART_overall.png", width=im_width, height=im_height, dpi=im_dpi)
rm(p)

# # immediate
# p <- raincloud(data_individual, "group", "SPART_mean_I", "SPART - Memory Recall", NULL)
# ggsave("Plots/SPART/WP6_SPART_immediate_recall.png", width=im_width, height=im_height, dpi=im_dpi)
# rm(p)
# 
# # delayed
# p <- raincloud(data_individual, "group", "SPART_q4_II", "SPART - Delayed Memory Recall", NULL)
# ggsave("Plots/SPART/WP6_SPART_delayed_recall.png", width=im_width, height=im_height, dpi=im_dpi)
# rm(p)


# 5PT
# productivity
p <- raincloud(data_individual, "group", "FIVE_P_productivity", "5PT - Spatial Fluency (number unique figures)", NULL)
ggsave("Plots/5PT/WP6_5PT_Productivity.png", width=im_width, height=im_height, dpi=im_dpi)
rm(p)

# # perseveration
# p <- raincloud(data_individual, "group", "FIVE_P_flexibility", "5 Points - Perseveration in %", NULL)
# ggsave("Plots/5PT/WP6_5PT_Perseveration.png", width=im_width, height=im_height, dpi=im_dpi)
# rm(p)
# 
# # strategy
# p <- raincloud(data_individual, "group", "FIVE_P_strategy", "5 Points - Strategy in %", NULL)
# ggsave("Plots/5PT/WP6_5PT_Strategy.png", width=im_width, height=im_height, dpi=im_dpi)
# rm(p)


# PTSOT 
# mean deviation
p <- raincloud(data_individual, "group", "PTSOT_mean_dev", "PTSOT - Perspective Taking (mean angle deviation)", NULL)
ggsave("Plots/PTSOT/WP6_PTSOT_mean_deviation.png", width=im_width, height=im_height, dpi=im_dpi)
rm(p)


# santa barbara sense of direction scale 
p <- raincloud(data_individual, "group", "sbsds_total_score", "SBSDS - Subjective Spatial Abilities", NULL)
ggsave("Plots/SBSDS/WP6_SBSDS.png", width=im_width, height=im_height, dpi=im_dpi)
rm(p)



# ggradar plot for comparison 
t <- data_individual %>%
  select(ID, group, ECAS_sub_executive, ECAS_sub_language, ECAS_sub_verbal_fluency, ECAS_sub_memory, ECAS_sub_spatial,
         FIVE_P_productivity, SPART_mean_all, PTSOT_mean_dev) %>% 
  drop_na() %>%
  group_by(group) %>%
  summarise(
    `executive function (ECAS)`=mean(ECAS_sub_executive), 
    `language (ECAS)`=mean(ECAS_sub_language), 
    `verbal fluency (ECAS)`=mean(ECAS_sub_verbal_fluency), 
    `verbal memory (ECAS)`=mean(ECAS_sub_memory), 
    `visuo-spatial (ECAS)`=mean(ECAS_sub_spatial),
    `spatial fluency (5PT)`=mean(FIVE_P_productivity), 
    `spatial memory (SPART)`=mean(SPART_mean_all), 
    `perspective taking (PTSOT)`=mean(PTSOT_mean_dev)
  )

radar <- t %>%
  ggradar(
    group.line.width = 1, 
    group.point.size = 3,
    values.radar=c(0,35,70),
    grid.min=0, grid.mid=35, grid.max=70,
    gridline.mid.colour = "grey",
    legend.position="bottom",
    axis.label.offset=1.2,
    axis.label.size=4,
    grid.label.size=3,
    legend.text.size=11,
    plot.extent.x.sf=1.8,
    plot.extent.y.sf=1.2,
  )
radar + 
  labs(subtitle="Neuropsychology for MND patients and controls (raw scores)") + 
  theme(plot.subtitle=element_text(size=16))
ggsave("Plots/WP6_ggradar.png") 



## Starmaze non-navigational memory task: Scoring 
# total
p1 <- raincloud(data_individual, "group", "Score_total", "Non-navigational memory: Total score", NULL) + ylim(0,1)
ggsave("Plots/Scoring/WP6_Scoring_total.png", width=im_width, height=im_height, dpi=im_dpi)


# object identity
p2 <- raincloud(data_individual, "group", "Object_identity_manual_s", "Non-navigational memory: Object identity", NULL) + ylim(0,1)
ggsave("Plots/Scoring/WP6_Scoring_object_identity.png", width=im_width, height=im_height, dpi=im_dpi)


# object location
p3 <- raincloud(data_individual, "group", "Object_location_GMDA_SQRTCanOrg_s", "Non-navigational memory: Object location (GMDA)", NULL) + ylim(0,1)
ggsave("Plots/Scoring/WP6_Scoring_object_location.png", width=im_width, height=im_height, dpi=im_dpi)


# maze reconstruction 
p4 <- raincloud(data_individual, "group", "Maze_reconstruction_manual_s", "Non-navigational memory: Maze reconstruction", NULL) + ylim(0,1)
ggsave("Plots/Scoring/WP6_Scoring_maze_reconstruction.png", width=im_width, height=im_height, dpi=im_dpi)

# joint plot
p <- (p2 & labs(y="object identity")) + 
  (p3 & labs(y="object location")) + 
  (p4 & labs(y="maze reconstruction")) + plot_annotation(title="Non-navigational memory tasks (standardized scores)")
ggsave("Plots/Scoring/WP6_Scoring_joint.png")



# scatter
# ALS-FRS
p <- scatter(data_individual[data_individual$group=="MND",], "Score_total", "ALS-FRS-R", "ALS-FRS (0-48)", "Non-nav-mem: Total score")
ggsave("Plots/Scatter/WP6_Scatter_Score_ALSFRS.png")
rm(p)

# FRS-/month
p <- scatter(data_individual[data_individual$group=="MND",], "Score_total", "FRS-/Monat", "ALS-FRS (0-48) / month", "Non-nav-mem: Total score")
ggsave("Plots/Scatter/WP6_Scatter_Score_ALSFRS-month.png")
rm(p)

# Age 
p <- scatter(data_individual, "Score_total", "dfb_q2_age", "Age", "Non-nav-mem: Total score")
ggsave("Plots/Scatter/WP6_Scatter_Score_Age.png")
rm(p)

# Years of education 
p <- scatter(data_individual, "Score_total", "dfb_q3_years_edu_total", "Years of education", "Non-nav-mem: Total score")
ggsave("Plots/Scatter/WP6_Scatter_Score_Education.png")
rm(p)

