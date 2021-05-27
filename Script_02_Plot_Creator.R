### -------------------- WP6 Data   --------------------- ###
### WP6_PlotCreator                                       ###
### Author: Patrizia Maier                                ###


### get packages
source("R_rainclouds.R")
library(tidyverse)
library(cowplot)


## input date 
date = readline(prompt = "Please enter the date string of the result file ")


###########################################################################


## set path
path <- "WP6_data/"

infileR <-  paste(path, "WP6_data_", date, ".Rdata", sep="")

load(infileR)
rm(infileR, date, path)


###########################################################################


### functions

# calculating mean and sd 
data_summary <- function(data, varname, groupnames){
  require(plyr)
  summary_func <- function(x, col){
    c(mean = mean(x[[col]], na.rm=TRUE),
      sd = sd(x[[col]], na.rm=TRUE))
  }
  data_sum<-ddply(data, groupnames, .fun=summary_func,
                  varname)
  data_sum <- rename(data_sum, c("mean" = varname))
  return(data_sum)
}

raincloud <- function(data, x, y, ylab, xlab){
  p1 <- ggplot(data, aes(x=x,y=y,fill=x)) + # set up data 
    geom_flat_violin(position=position_nudge(x=.2,y=0)) + # rain cloud: setting "adjust" for smoothness of kernel
    geom_point(position=position_jitter(w=.1,h=0.05)) + # points
    geom_boxplot(aes(x=as.numeric(x)+0.2,y=y), outlier.shape=NA, alpha=0.3, width=0.1, colour="BLACK") + 
    scale_fill_grey(start=0.99, end=0.75) +
    coord_flip() + # flip axes
    guides(fill=FALSE) + # legend off
    theme_cowplot(font_size=18) + # nicer theme
    ylab(ylab) + xlab(xlab) # labels
  
  return(p1)
}

raincloud_sub <- function(data, x, y, ylab, xlab, sub){
  p1 <- ggplot(data_individual, aes(x=x,y=y,fill=x)) + # set up data 
    geom_flat_violin(position=position_nudge(x=.2,y=0)) + # rain cloud: setting "adjust" for smoothness of kernel
    geom_point(aes(shape = sub), size = 5, position=position_jitter(w=.1,h=.05, seed=1)) + # points
    geom_point(aes(colour = sub, shape = sub), size = 3, position=position_jitter(w=.1,h=.05, seed=1)) + # point
    geom_boxplot(aes(x=as.numeric(x)+0.2,y=y), outlier.shape=NA, alpha=0.3, width=0.1, colour="BLACK") + 
    scale_shape_manual(values=c(15,16,17,18)) + 
    scale_colour_manual(values=c("skyblue","yellow","salmon","white")) + 
    scale_fill_grey(start=0.99, end=0.75) +
    coord_flip() + # flip axes
    guides(fill=FALSE) + # legend off
    theme_cowplot(font_size=18) + # nicer theme
    ylab(ylab) + xlab(xlab) # labels
  
  return(p1)
}

scatter <- function(data, x, y, ylab, xlab){
  p1 <- ggplot(data, aes(x=x, y=y)) +
    geom_point() +  # scatter
    geom_smooth(method=lm) + # prediction line 
    theme_cowplot(font_size=18) + # nicer theme
    ylab(ylab) + xlab(xlab) # labels
  return(p1)
}

barplot <- function(data, x, y, ylab, xlab){
  p1 <- ggplot(data, aes(x=x,y=y ,fill=Group)) + # set up data 
    geom_bar(stat="identity", position=position_dodge(0.5)) + 
    scale_fill_grey(start=0.9, end=0.75) + 
    theme_cowplot(font_size=18) + # nicer theme
    ylab(ylab) + xlab(xlab) # labels
  return(p1)
}


###########################################################################


# ### prepare data set / variables
# ## starmaze data
# sm_data$Group <- factor(sm_data$Group)
# sm_data$Group <- fct_recode(sm_data$Group, "MNE"="1", "Control"="0")
# 
# 
# sm_data$Block <- NA
# sm_data$Block[sm_data$Trial %in% c(1,2,3,4,5,6,7,8,9)] <- "Learn"
# sm_data$Block[sm_data$Trial %in% c(10,11,12,13)] <- "Ego"
# sm_data$Block[sm_data$Trial %in% c(14,15,16)] <- "Allo_u"
# sm_data$Block[sm_data$Trial %in% c(17,18,19,20,21,22,23,24,25,26,27,28,29,30,31)] <- "Allo_i"
# sm_data$Block <- factor(sm_data$Block, levels = c("Learn", "Ego", "Allo_u", "Allo_i"))
# 
# sm_data$Trial <- factor(sm_data$Trial)
# 
# # subset only noF Trials 
# sm_data_noF <- sm_data[sm_data$Feedback == 0,]
# 
# 
# # add starmaze means to data_individual
# gd_S <- data_summary(sm_data_noF, varname="success", 
#                      groupnames=c("ID", "Group"))
# gd_P <- data_summary(sm_data_noF, varname="path_accuracy", 
#                      groupnames=c("ID", "Group"))
# gd_T <- data_summary(sm_data_noF, varname="time_accuracy", 
#                      groupnames=c("ID", "Group"))
# data_individual$Success_SM <- gd_S$success
# data_individual$Path_SM <- gd_P$path_accuracy
# data_individual$Time_SM <- gd_T$time_accuracy
# rm(gd_S, gd_P, gd_T)


###########################################################################
### demographic data 

temp <- subset(data_individual, select=c(ID, Group, MNE_Untergruppe, `ALS-FRS-R`, `FRS-/Monat`,
                                    dfb_q1_sex, dfb_q2_age, dfb_q3_years_edu_total, dfb_q4_highestedu, 
                                    dfb_q5_language_german, dfb_q6_handiness, 
                                    dfb_q21_comp_expertise, dfb_q22_comp_freq))


summary(temp[temp$Group == "MNE",])

summary(temp[temp$Group == "Control",])

rm(temp)


####################################################################################################
### plotting

## neuropsychology 
# ECAS
# total
p <- raincloud(data_individual, data_individual$Group, data_individual$ECAS_total_score, "ECAS - Total Score", "Group")
ggsave("Plots/WP6_ECAS_total_score.png")
rm(p)

p <- raincloud_sub(data_individual, data_individual$Group, data_individual$ECAS_total_score, "ECAS - Total Score", "Group", data_individual$MNE_Untergruppe)
ggsave("Plots/WP6_ECAS_total_score_subgroup.png")
rm(p)

# memory 
p <- raincloud(data_individual, data_individual$Group, data_individual$ECAS_sub_memory, "ECAS - Memory Score", "Group")
ggsave("Plots/WP6_ECAS_memory.png")
rm(p)

p <- raincloud_sub(data_individual, data_individual$Group, data_individual$ECAS_sub_memory, "ECAS - Memory Score", "Group", data_individual$MNE_Untergruppe)
ggsave("Plots/WP6_ECAS_memory_subgroup.png")
rm(p)

# spatial abilities 
p <- raincloud(data_individual, data_individual$Group, data_individual$ECAS_sub_spatial, "ECAS - Spatial Score", "Group")
ggsave("Plots/WP6_ECAS_visuospatial.png")
rm(p)

p <- raincloud_sub(data_individual, data_individual$Group, data_individual$ECAS_sub_spatial, "ECAS - Spatial Score", "Group", data_individual$MNE_Untergruppe)
ggsave("Plots/WP6_ECAS_visuospatial_subgroup.png")
rm(p)

# language
p <- raincloud(data_individual, data_individual$Group, data_individual$ECAS_sub_language, "ECAS - Language Score", "Group")
ggsave("Plots/WP6_ECAS_language.png")
rm(p)

p <- raincloud_sub(data_individual, data_individual$Group, data_individual$ECAS_sub_language, "ECAS - Language Score", "Group", data_individual$MNE_Untergruppe)
ggsave("Plots/WP6_ECAS_language_subgroup.png")
rm(p)

# verbal fluency
p <- raincloud(data_individual, data_individual$Group, data_individual$ECAS_sub_verbal_fluency, "ECAS - Verbal Fluency Score", "Group")
ggsave("Plots/WP6_ECAS_verbal_fluency.png")
rm(p)

p <- raincloud_sub(data_individual, data_individual$Group, data_individual$ECAS_sub_verbal_fluency, "ECAS - Verbal Fluency Score", "Group", data_individual$MNE_Untergruppe)
ggsave("Plots/WP6_ECAS_verbal_fluency_subgroup.png")
rm(p)

# executive
p <- raincloud(data_individual, data_individual$Group, data_individual$ECAS_sub_executive, "ECAS - Executive Score", "Group")
ggsave("Plots/WP6_ECAS_executive.png")
rm(p)

p <- raincloud_sub(data_individual, data_individual$Group, data_individual$ECAS_sub_executive, "ECAS - Executive Score", "Group", data_individual$MNE_Untergruppe)
ggsave("Plots/WP6_ECAS_executive_subgroup.png")
rm(p)



# SPART
# immediate
p <- raincloud(data_individual, data_individual$Group, data_individual$SPART_mean_I, "SPART - Memory Recall", "Group")
ggsave("Plots/WP6_SPART_immediate_recall.png")
rm(p)

p <- raincloud_sub(data_individual, data_individual$Group, data_individual$SPART_mean_I, "SPART - Memory Recall", "Group", data_individual$MNE_Untergruppe)
ggsave("Plots/WP6_SPART_immediate_recall_subgroup.png")
rm(p)

# delayed
p <- raincloud(data_individual, data_individual$Group, data_individual$SPART_q4_II, "SPART - Delayed Memory Recall", "Group")
ggsave("Plots/WP6_SPART_delayed_recall.png")
rm(p)

p <- raincloud_sub(data_individual, data_individual$Group, data_individual$SPART_q4_II, "SPART - Delayed Memory Recall", "Group", data_individual$MNE_Untergruppe)
ggsave("Plots/WP6_SPART_delayed_recall_subgroup.png")
rm(p)


# 5PT
# productivity
p <- raincloud(data_individual, data_individual$Group, data_individual$FIVE_P_productivity, "5 Points - Number Unique", "Group")
ggsave("Plots/WP6_5PT_Productivity.png")
rm(p)

p <- raincloud_sub(data_individual, data_individual$Group, data_individual$FIVE_P_productivity, "5 Points - Number Unique", "Group", data_individual$MNE_Untergruppe)
ggsave("Plots/WP6_5PT_Productivity_subgroup.png")
rm(p)

# perseveration
p <- raincloud(data_individual, data_individual$Group, data_individual$FIVE_P_flexibility, "5 Points - Perseveration in %", "Group")
ggsave("Plots/WP6_5PT_Perseveration.png")
rm(p)

p <- raincloud_sub(data_individual, data_individual$Group, data_individual$FIVE_P_flexibility, "5 Points - Perseveration in %", "Group", data_individual$MNE_Untergruppe)
ggsave("Plots/WP6_5PT_Perseveration_subgroup.png")
rm(p)

# strategy
p <- raincloud(data_individual, data_individual$Group, data_individual$FIVE_P_strategy, "5 Points - Strategy in %", "Group")
ggsave("Plots/WP6_5PT_Strategy.png")
rm(p)

p <- raincloud_sub(data_individual, data_individual$Group, data_individual$FIVE_P_strategy, "5 Points - Strategy in %", "Group", data_individual$MNE_Untergruppe)
ggsave("Plots/WP6_5PT_Strategy_subgroup.png")
rm(p)



# PTSOT 
# mean deviation
p <- raincloud(data_individual, data_individual$Group, data_individual$PTSOT_mean_dev, "PTSOT - Mean deviation from correct angle", "Group")
ggsave("Plots/WP6_PTSOT_mean_deviation.png")
rm(p)

p <- raincloud_sub(data_individual, data_individual$Group, data_individual$PTSOT_mean_dev, "PTSOT - Mean deviation from correct angle", "Group", data_individual$MNE_Untergruppe)
ggsave("Plots/WP6_PTSOT_mean_deviation_subgroup.png")
rm(p)


# number items 
p <- raincloud(data_individual, data_individual$Group, data_individual$PTSOT_num_items, "PTSOT - Number items", "Group")
ggsave("Plots/WP6_PTSOT_number_items.png")
rm(p)

p <- raincloud_sub(data_individual, data_individual$Group, data_individual$PTSOT_num_items, "PTSOT - Number items", "Group", data_individual$MNE_Untergruppe)
ggsave("Plots/WP6_PTSOT_number_items_subgroup.png")
rm(p)



## santa barbara sense of direction scale 
p <- raincloud(data_individual, data_individual$Group, data_individual$sbsds_total_score, "SBSDS - Subjective Spatial Abilities", "Group")
ggsave("Plots/WP6_SBSDS.png")
rm(p)

p <- raincloud_sub(data_individual, data_individual$Group, data_individual$sbsds_total_score, "SBSDS - Subjective Spatial Abilities", "Group", data_individual$MNE_Untergruppe)
ggsave("Plots/WP6_SBSDS_subgroup.png")
rm(p)



## scoring starmaze
# total
p <- raincloud(data_individual, data_individual$Group, data_individual$Score_total, "Scoring - Total", "Group")
ggsave("Plots/WP6_Scoring_total.png")
rm(p)

p <- raincloud_sub(data_individual, data_individual$Group, data_individual$Score_total, "Scoring - Total", "Group", data_individual$MNE_Untergruppe)
ggsave("Plots/WP6_Scoring_total_subgroup.png")
rm(p)


# object identity
p <- raincloud(data_individual, data_individual$Group, data_individual$Object_Identity, "Scoring - Object Identity", "Group")
ggsave("Plots/WP6_Scoring_object_identity.png")
rm(p)

p <- raincloud_sub(data_individual, data_individual$Group, data_individual$Object_Identity, "Scoring - Object Identity", "Group", data_individual$MNE_Untergruppe)
ggsave("Plots/WP6_Scoring_object_identity_subgroup.png")
rm(p)


# object location
p <- raincloud(data_individual, data_individual$Group, data_individual$Object_location, "Scoring - Object Location", "Group")
ggsave("Plots/WP6_Scoring_object_location.png")
rm(p)

p <- raincloud_sub(data_individual, data_individual$Group, data_individual$Object_location, "Scoring - Object Location", "Group", data_individual$MNE_Untergruppe)
ggsave("Plots/WP6_Scoring_object_location_subgroup.png")
rm(p)


# maze reconstruction 
p <- raincloud(data_individual, data_individual$Group, data_individual$Maze_reconstruction, "Scoring - Maze Reconstruction", "Group")
ggsave("Plots/WP6_Scoring_maze_reconstruction.png")
rm(p)

p <- raincloud_sub(data_individual, data_individual$Group, data_individual$Maze_reconstruction, "Scoring - Maze Reconstruction", "Group", data_individual$MNE_Untergruppe)
ggsave("Plots/WP6_Scoring_maze_reconstruction_subgroup.png")
rm(p)


# scatter
# ALS-FRS
p <- scatter(data_individual[data_individual$Group=="MNE",], data_individual[data_individual$Group=="MNE",]$Score_total, data_individual[data_individual$Group=="MNE",]$`ALS-FRS-R`, "ALS-FRS (0-48)", "Score total")
ggsave("Plots/WP6_Scatter_Score_ALSFRS.png")
rm(p)

# FRS-/month
p <- scatter(data_individual[data_individual$Group=="MNE",], data_individual[data_individual$Group=="MNE",]$Score_total, data_individual[data_individual$Group=="MNE",]$`FRS-/Monat`, "ALS-FRS (0-48) / month", "Score total")
ggsave("Plots/WP6_Scatter_Score_ALSFRS-month.png")
rm(p)

# SBSDS 
p <- scatter(data_individual, data_individual$Score_total, data_individual$sbsds_total_score, "SBSDS", "Score total")
ggsave("Plots/WP6_Scatter_Score_SBSDS.png")
rm(p)

# # success 
# p <- scatter(data_individual[data_individual$Group=="MNE",], data_individual[data_individual$Group=="MNE",]$Success_SM, data_individual[data_individual$Group=="MNE",]$`ALS-FRS-R`,
#         "ALS-FRS (0-48)", "Success in %")
# ggsave("Plots/WP6_Scatter_SM_Success_ALSFRS.png")
# rm(p)
# 
# p <- scatter(data_individual[data_individual$Group=="MNE",], data_individual[data_individual$Group=="MNE",]$Success_SM, data_individual[data_individual$Group=="MNE",]$`FRS-/Monat`,
#              "ALS-FRS (0-48) / month", "Success in %")
# ggsave("Plots/WP6_Scatter_SM_Success_ALSFRS_month.png")
# rm(p)
# 
# p <- scatter(data_individual, data_individual$Success_SM, data_individual$sbsds_total_score,
#              "SBSDS", "Success in %")
# ggsave("Plots/WP6_Scatter_SM_Success_SBSDS.png")
# rm(p)
# 
# p <- scatter(data_individual, data_individual$Success_SM, data_individual$dfb_q2_age,
#              "Age", "Success in %")
# ggsave("Plots/WP6_Scatter_SM_Success_Age.png")
# rm(p)
# 
# p <- scatter(data_individual, data_individual$Success_SM, data_individual$dfb_q3_years_edu_total,
#              "Years of education", "Success in %")
# ggsave("Plots/WP6_Scatter_SM_Success_Education.png")
# rm(p)
# 
# p <- scatter(data_individual, data_individual$Success_SM, data_individual$Score_total,
#              "Scoring Total", "Success in %")
# ggsave("Plots/WP6_Scatter_SM_Success_Score.png")
# rm(p)