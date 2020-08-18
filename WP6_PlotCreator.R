### Plots for WP6 Data

# ## install packages
# install.packages("tidyverse")
# install.packages("readxl")
# install.packages("dplyr")
# install.packages("plyr")
# install.packages("magrittr")
# install.packages("reshape2")
# install.packages("openxlsx")
# install.packages("cowplot")

### get packages
source("R_rainclouds.R")
library(readxl)
library(openxlsx)
library(tidyverse)
library(cowplot)

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
  p1 <- ggplot(indiv_data, aes(x=x,y=y,fill=x)) + # set up data 
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

### set directory and filename
path <- "T:/Analysis/WP6_data_20-08"
setwd(path)


## get data from excel sheets
file <- "WP6_data_20-08-07.xlsx"
sm_data <- read_xlsx(file, sheet = "Data_trial", col_names = T)
indiv_data <- read_xlsx(file, sheet = "Data_individual", col_names = T)

###########################################################################

### prepare data set / variables
## indiv data 
indiv_data$Group <- factor(indiv_data$Group)
indiv_data$Group <- plyr::revalue(indiv_data$Group, c("Experimental"="MNE", "Controll"="Control"))

indiv_data$dfb_q1_sex <- factor(indiv_data$dfb_q1_sex)
indiv_data$dfb_q1_sex <-plyr::revalue(indiv_data$dfb_q1_sex, c("männlich"="male", "weiblich"="female"))

indiv_data$dfb_q4_highestedu <- factor(indiv_data$dfb_q4_highestedu)

indiv_data$dfb_q5_language_german <- factor(indiv_data$dfb_q5_language_german)

indiv_data$dfb_q6_handiness <- factor(indiv_data$dfb_q6_handiness)

indiv_data$Subgroup <- factor(indiv_data$Subgroup)
indiv_data$Subgroup <- plyr::revalue(indiv_data$Subgroup, c("1"="spinal ALS", "2"="bulbar ALS", "3"="PLS", "5" = "Control"))


## starmaze data
sm_data$Group <- factor(sm_data$Group)
sm_data$Group <- plyr::revalue(sm_data$Group, c("1"="MNE", "0"="Control"))


sm_data$Block <- NA
sm_data$Block[sm_data$Trial %in% c(1,2,3,4,5,6,7,8,9)] <- "Learn"
sm_data$Block[sm_data$Trial %in% c(10,11,12,13)] <- "Ego"
sm_data$Block[sm_data$Trial %in% c(14,15,16)] <- "Allo_u"
sm_data$Block[sm_data$Trial %in% c(17,18,19,20,21,22,23,24,25,26,27,28,29,30,31)] <- "Allo_i"
sm_data$Block <- factor(sm_data$Block, levels = c("Learn", "Ego", "Allo_u", "Allo_i"))

sm_data$Trial <- factor(sm_data$Trial)

# subset only noF Trials 
sm_data_noF <- sm_data[sm_data$Feedback == 0,]


# add starmaze means to indiv_data
gd_S <- data_summary(sm_data_noF, varname="success", 
                     groupnames=c("ID", "Group"))
gd_P <- data_summary(sm_data_noF, varname="path_accuracy", 
                     groupnames=c("ID", "Group"))
gd_T <- data_summary(sm_data_noF, varname="time_accuracy", 
                     groupnames=c("ID", "Group"))
indiv_data$Success_SM <- gd_S$success
indiv_data$Path_SM <- gd_P$path_accuracy
indiv_data$Time_SM <- gd_T$time_accuracy
rm(gd_S, gd_P, gd_T)


###########################################################################
### demographic data 

temp <- subset(indiv_data, select=c(ID, Group, Subgroup, ALS.FRS.R, FRS..Monat,
                                    dfb_q1_sex, dfb_q2_age, dfb_q3_years_edu_total, dfb_q4_highestedu, 
                                    dfb_q5_language_german, dfb_q6_handiness))
                                    # sbsds_total_score, 
                                    # PTSOT_num_items, PTSOT_mean_dev,
                                    # FIVE_P_productivity, FIVE_P_flexibility, FIVE_P_strategy, FIVE_P_rule_broken,
                                    # #SPART_q1-3_I, SPART_q4_II, SPART_delay_min
                                    # ECAS_total_score, ECAS_sub_memory, ECAS_sub_spatial, ECAS_sub_language, ECAS_sub_verbal_fluency, ECAS_sub_executive,
                                    # Score_total, Object_Identity, Object_location, Maze_reconstruction
                                

summary(temp[temp$Group == "MNE",])

summary(temp[temp$Group == "Control",])

rm(temp)

############################################################################

## other useful functions
# remove missing data
# data <- data[complete.cases(data), ]

# # convert to int
# int_vars <- c("Participant","Age") # add here 
# data[,int_vars] <- lapply(data[,int_vars], as.integer)
# rm(int_vars)

# # convert to factor
# data$Group <- factor(data$Group, levels=c(0,1), labels=c("Control", "ALS"))

# convert several variables to factor 
# ss_vars <- grep("_ss_", names(data))
# data[,ss_vars] <- lapply(data[,ss_vars], factor, levels=c(0,1,2,3), labels=c("fail", "ego", "allo", "switch"))

# rename severalcolumns   
# data <- plyr::rename(data, c("PT-1...86" = "t1_ss_mixed_pt_1", # t1 mixed
#                        "PT-2...87" = "t1_ss_mixed_pt_2"))

####################################################################################################
### plotting

## neuropsychology 
# ECAS
# total
p <- raincloud(indiv_data, indiv_data$Group, indiv_data$ECAS_total_score, "ECAS - Total Score", "Group")
ggsave("Plots/WP6_ECAS_total_score.png")
rm(p)

p <- raincloud_sub(indiv_data, indiv_data$Group, indiv_data$ECAS_total_score, "ECAS - Total Score", "Group", indiv_data$Subgroup)
ggsave("Plots/WP6_ECAS_total_score_subgroup.png")
rm(p)

# memory 
p <- raincloud(indiv_data, indiv_data$Group, indiv_data$ECAS_sub_memory, "ECAS - Memory Score", "Group")
ggsave("Plots/WP6_ECAS_memory.png")
rm(p)

p <- raincloud_sub(indiv_data, indiv_data$Group, indiv_data$ECAS_sub_memory, "ECAS - Memory Score", "Group", indiv_data$Subgroup)
ggsave("Plots/WP6_ECAS_memory_subgroup.png")
rm(p)

# spatial abilities 
p <- raincloud(indiv_data, indiv_data$Group, indiv_data$ECAS_sub_spatial, "ECAS - Spatial Score", "Group")
ggsave("Plots/WP6_ECAS_visuospatial.png")
rm(p)

p <- raincloud_sub(indiv_data, indiv_data$Group, indiv_data$ECAS_sub_spatial, "ECAS - Spatial Score", "Group", indiv_data$Subgroup)
ggsave("Plots/WP6_ECAS_visuospatial_subgroup.png")
rm(p)

# language
p <- raincloud(indiv_data, indiv_data$Group, indiv_data$ECAS_sub_language, "ECAS - Language Score", "Group")
ggsave("Plots/WP6_ECAS_language.png")
rm(p)

p <- raincloud_sub(indiv_data, indiv_data$Group, indiv_data$ECAS_sub_language, "ECAS - Language Score", "Group", indiv_data$Subgroup)
ggsave("Plots/WP6_ECAS_language_subgroup.png")
rm(p)

# verbal fluency
p <- raincloud(indiv_data, indiv_data$Group, indiv_data$ECAS_sub_verbal_fluency, "ECAS - Verbal Fluency Score", "Group")
ggsave("Plots/WP6_ECAS_verbal_fluency.png")
rm(p)

p <- raincloud_sub(indiv_data, indiv_data$Group, indiv_data$ECAS_sub_verbal_fluency, "ECAS - Verbal Fluency Score", "Group", indiv_data$Subgroup)
ggsave("Plots/WP6_ECAS_verbal_fluency_subgroup.png")
rm(p)

# executive
p <- raincloud(indiv_data, indiv_data$Group, indiv_data$ECAS_sub_executive, "ECAS - Executive Score", "Group")
ggsave("Plots/WP6_ECAS_executive.png")
rm(p)

p <- raincloud_sub(indiv_data, indiv_data$Group, indiv_data$ECAS_sub_executive, "ECAS - Executive Score", "Group", indiv_data$Subgroup)
ggsave("Plots/WP6_ECAS_executive_subgroup.png")
rm(p)



# SPART
# immediate
p <- raincloud(indiv_data, indiv_data$Group, indiv_data$SPART_mean_I, "SPART - Memory Recall", "Group")
ggsave("Plots/WP6_SPART_immediate_recall.png")
rm(p)

p <- raincloud_sub(indiv_data, indiv_data$Group, indiv_data$SPART_mean_I, "SPART - Memory Recall", "Group", indiv_data$Subgroup)
ggsave("Plots/WP6_SPART_immediate_recall_subgroup.png")
rm(p)

# delayed
p <- raincloud(indiv_data, indiv_data$Group, indiv_data$SPART_q4_II, "SPART - Delayed Memory Recall", "Group")
ggsave("Plots/WP6_SPART_delayed_recall.png")
rm(p)

p <- raincloud_sub(indiv_data, indiv_data$Group, indiv_data$SPART_q4_II, "SPART - Delayed Memory Recall", "Group", indiv_data$Subgroup)
ggsave("Plots/WP6_SPART_delayed_recall_subgroup.png")
rm(p)


# 5PT
# productivity
p <- raincloud(indiv_data, indiv_data$Group, indiv_data$FIVE_P_productivity, "5 Points - Number Unique", "Group")
ggsave("Plots/WP6_5PT_Productivity.png")
rm(p)

p <- raincloud_sub(indiv_data, indiv_data$Group, indiv_data$FIVE_P_productivity, "5 Points - Number Unique", "Group", indiv_data$Subgroup)
ggsave("Plots/WP6_5PT_Productivity_subgroup.png")
rm(p)

# perseveration
p <- raincloud(indiv_data, indiv_data$Group, indiv_data$FIVE_P_flexibility, "5 Points - Perseveration in %", "Group")
ggsave("Plots/WP6_5PT_Perseveration.png")
rm(p)

p <- raincloud_sub(indiv_data, indiv_data$Group, indiv_data$FIVE_P_flexibility, "5 Points - Perseveration in %", "Group", indiv_data$Subgroup)
ggsave("Plots/WP6_5PT_Perseveration_subgroup.png")
rm(p)

# strategy
p <- raincloud(indiv_data, indiv_data$Group, indiv_data$FIVE_P_strategy, "5 Points - Strategy in %", "Group")
ggsave("Plots/WP6_5PT_Strategy.png")
rm(p)

p <- raincloud_sub(indiv_data, indiv_data$Group, indiv_data$FIVE_P_strategy, "5 Points - Strategy in %", "Group", indiv_data$Subgroup)
ggsave("Plots/WP6_5PT_Strategy_subgroup.png")
rm(p)



# PTSOT 
# mean deviation
p <- raincloud(indiv_data, indiv_data$Group, indiv_data$PTSOT_mean_dev, "PTSOT - Mean deviation from correct angle", "Group")
ggsave("Plots/WP6_PTSOT_mean_deviation.png")
rm(p)

p <- raincloud_sub(indiv_data, indiv_data$Group, indiv_data$PTSOT_mean_dev, "PTSOT - Mean deviation from correct angle", "Group", indiv_data$Subgroup)
ggsave("Plots/WP6_PTSOT_mean_deviation_subgroup.png")
rm(p)


# number items 
p <- raincloud(indiv_data, indiv_data$Group, indiv_data$PTSOT_num_items, "PTSOT - Number items", "Group")
ggsave("Plots/WP6_PTSOT_number_items.png")
rm(p)

p <- raincloud_sub(indiv_data, indiv_data$Group, indiv_data$PTSOT_num_items, "PTSOT - Number items", "Group", indiv_data$Subgroup)
ggsave("Plots/WP6_PTSOT_number_items_subgroup.png")
rm(p)



## santa barbara sense of direction scale 
p <- raincloud(indiv_data, indiv_data$Group, indiv_data$sbsds_total_score, "SBSDS - Subjective Spatial Abilities", "Group")
ggsave("Plots/WP6_SBSDS.png")
rm(p)

p <- raincloud_sub(indiv_data, indiv_data$Group, indiv_data$sbsds_total_score, "SBSDS - Subjective Spatial Abilities", "Group", indiv_data$Subgroup)
ggsave("Plots/WP6_SBSDS_subgroup.png")
rm(p)



## scoring starmaze
# total
p <- raincloud(indiv_data, indiv_data$Group, indiv_data$Score_total, "Scoring - Total", "Group")
ggsave("Plots/WP6_Scoring_total.png")
rm(p)

p <- raincloud_sub(indiv_data, indiv_data$Group, indiv_data$Score_total, "Scoring - Total", "Group", indiv_data$Subgroup)
ggsave("Plots/WP6_Scoring_total_subgroup.png")
rm(p)


# object identity
p <- raincloud(indiv_data, indiv_data$Group, indiv_data$Object_Identity, "Scoring - Object Identity", "Group")
ggsave("Plots/WP6_Scoring_object_identity.png")
rm(p)

p <- raincloud_sub(indiv_data, indiv_data$Group, indiv_data$Object_Identity, "Scoring - Object Identity", "Group", indiv_data$Subgroup)
ggsave("Plots/WP6_Scoring_object_identity_subgroup.png")
rm(p)


# object location
p <- raincloud(indiv_data, indiv_data$Group, indiv_data$Object_location, "Scoring - Object Location", "Group")
ggsave("Plots/WP6_Scoring_object_location.png")
rm(p)

p <- raincloud_sub(indiv_data, indiv_data$Group, indiv_data$Object_location, "Scoring - Object Location", "Group", indiv_data$Subgroup)
ggsave("Plots/WP6_Scoring_object_location_subgroup.png")
rm(p)


# maze reconstruction 
p <- raincloud(indiv_data, indiv_data$Group, indiv_data$Maze_reconstruction, "Scoring - Maze Reconstruction", "Group")
ggsave("Plots/WP6_Scoring_maze_reconstruction.png")
rm(p)

p <- raincloud_sub(indiv_data, indiv_data$Group, indiv_data$Maze_reconstruction, "Scoring - Maze Reconstruction", "Group", indiv_data$Subgroup)
ggsave("Plots/WP6_Scoring_maze_reconstruction_subgroup.png")
rm(p)


# scatter
p <- scatter(indiv_data[indiv_data$Group=="MNE",], indiv_data[indiv_data$Group=="MNE",]$Score_total, indiv_data[indiv_data$Group=="MNE",]$ALS.FRS.R, "ALS-FRS (0-48)", "Score total")
ggsave("Plots/WP6_Scatter_Score_ALSFRS.png")
rm(p)

p <- scatter(indiv_data[indiv_data$Group=="MNE",], indiv_data[indiv_data$Group=="MNE",]$Score_total, indiv_data[indiv_data$Group=="MNE",]$FRS..Monat, "ALS-FRS (0-48) / month", "Score total")
ggsave("Plots/WP6_Scatter_Score_ALSFRS-month.png")
rm(p)

p <- scatter(indiv_data, indiv_data$Score_total, indiv_data$sbsds_total_score, "SBSDS", "Score total")
ggsave("Plots/WP6_Scatter_Score_SBSDS.png")
rm(p)




## starmaze data
# trial wise barplots
# success
gd <- data_summary(sm_data_noF, varname="success", 
                   groupnames=c("Group", "Trial", "Block"))
p <- barplot(gd, gd$Trial, gd$success, "Success in %", "Trials")
ggsave("Plots/WP6_SM_noF_Success_Trial.png")
rm(p, gd)

# path accuracy
gd <- data_summary(sm_data_noF, varname="path_accuracy", 
                   groupnames=c("Group", "Trial", "Block"))
p <- barplot(gd, gd$Trial, gd$path_accuracy, "% Deviation from ideal path", "Trials")
ggsave("Plots/WP6_SM_noF_PathAcc_Trial.png")
rm(p, gd)

# time accuracy
gd <- data_summary(sm_data_noF, varname="time_accuracy", 
                   groupnames=c("Group", "Trial", "Block"))
p <- barplot(gd, gd$Trial, gd$time_accuracy, "% Deviation from ideal time", "Trials")
ggsave("Plots/WP6_SM_noF_TimeAcc_Trial.png")
rm(p, gd)


# block wise barplots 
# success 
gd <- data_summary(sm_data_noF, varname="success", 
                   groupnames=c("Group", "Block"))
p <- barplot(gd, gd$Block, gd$success, "Success in %", "Blocks")
ggsave("Plots/WP6_SM_noF_Success_Block.png")
rm(p, gd)

# path accuracy
gd <- data_summary(sm_data_noF, varname="path_accuracy", 
                   groupnames=c("Group", "Block"))
p <- barplot(gd, gd$Block, gd$path_accuracy, "% Deviation from ideal path", "Blocks")
ggsave("Plots/WP6_SM_noF_PathAcc_Block.png")
rm(p, gd)

# time accuracy
gd <- data_summary(sm_data_noF, varname="time_accuracy", 
                   groupnames=c("Group", "Block"))
p <- barplot(gd, gd$Block, gd$time_accuracy, "% Deviation from ideal time", "Blocks")
ggsave("Plots/WP6_SM_noF_TimeAcc_Block.png")
rm(p, gd)


# total rainclouds
# success
p <- raincloud(indiv_data, indiv_data$Group, indiv_data$Success_SM, "Success in %", "Group")
ggsave("Plots/WP6_SM_noF_Success_Raincloud.png")
rm(p)

p <- raincloud_sub(indiv_data, indiv_data$Group, indiv_data$Success_SM, "Success in %", "Group", indiv_data$Subgroup)
ggsave("Plots/WP6_SM_noF_Success_Raincloud_subgroup.png")
rm(p)


# path accuracy
p <- raincloud(indiv_data, indiv_data$Group, indiv_data$Path_SM, "% Deviation from ideal path", "Group")
ggsave("Plots/WP6_SM_noF_PathAcc_Raincloud.png")
rm(p)

p <- raincloud_sub(indiv_data, indiv_data$Group, indiv_data$Path_SM, "% Deviation from ideal path", "Group", indiv_data$Subgroup)
ggsave("Plots/WP6_SM_noF_PathAcc_Raincloud_subgroup.png")
rm(p)


# time accuracy
p <- raincloud(indiv_data, indiv_data$Group, indiv_data$Time_SM, "% Deviation from ideal time", "Group")
ggsave("Plots/WP6_SM_noF_TimeAcc_Raincloud.png")
rm(p)

p <- raincloud_sub(indiv_data, indiv_data$Group, indiv_data$Time_SM, "% Deviation from ideal time", "Group", indiv_data$Subgroup)
ggsave("Plots/WP6_SM_noF_TimeAcc_Raincloud_subgroup.png")
rm(p)


# with other data set 
# gd <- data_summary(sm_data_noF, varname="success", 
#                    groupnames=c("ID", "Group"))
# 
# raincloud(gd, gd$Group, gd$success, "Success in %", "Group")

# block wise
# gd <- data_summary(sm_data, varname="path_abs", 
#                    groupnames=c("ID", "Group", "Block"))
#
# p1 <- ggplot(gd, aes(x=Group,y=success,fill=Group)) + # set up data 
#   geom_flat_violin(position=position_nudge(x=.2,y=0)) + # rain cloud: setting "adjust" for smoothness of kernel
#   geom_point(position=position_jitter(w=.1,h=0.05)) + 
#   geom_boxplot(aes(x=as.numeric(Group)+0.2,y=success), outlier.shape=NA, alpha=0.3, width=0.1, colour="BLACK") + 
#   facet_wrap(~Block) + 
#   scale_fill_grey(start=0.99, end=0.75) +
#   coord_flip() + # flip axes
#   guides(fill=FALSE) + # legend off
#   theme_cowplot(font_size=18) + # nicer theme
#   ylab("Success in %") + xlab("Group") # labels
# p1


# scatters
# success 
p <- scatter(indiv_data[indiv_data$Group=="MNE",], indiv_data[indiv_data$Group=="MNE",]$Success_SM, indiv_data[indiv_data$Group=="MNE",]$ALS.FRS.R,
        "ALS-FRS (0-48)", "Success in %")
ggsave("Plots/WP6_Scatter_SM_Success_ALSFRS.png")
rm(p)

p <- scatter(indiv_data[indiv_data$Group=="MNE",], indiv_data[indiv_data$Group=="MNE",]$Success_SM, indiv_data[indiv_data$Group=="MNE",]$FRS..Monat,
             "ALS-FRS (0-48) / month", "Success in %")
ggsave("Plots/WP6_Scatter_SM_Success_ALSFRS_month.png")
rm(p)

p <- scatter(indiv_data, indiv_data$Success_SM, indiv_data$sbsds_total_score,
             "SBSDS", "Success in %")
ggsave("Plots/WP6_Scatter_SM_Success_SBSDS.png")
rm(p)

p <- scatter(indiv_data, indiv_data$Success_SM, indiv_data$dfb_q2_age,
             "Age", "Success in %")
ggsave("Plots/WP6_Scatter_SM_Success_Age.png")
rm(p)

p <- scatter(indiv_data, indiv_data$Success_SM, indiv_data$dfb_q3_years_edu_total,
             "Years of education", "Success in %")
ggsave("Plots/WP6_Scatter_SM_Success_Education.png")
rm(p)

p <- scatter(indiv_data, indiv_data$Success_SM, indiv_data$Score_total,
             "Scoring Total", "Success in %")
ggsave("Plots/WP6_Scatter_SM_Success_Score.png")
rm(p)

