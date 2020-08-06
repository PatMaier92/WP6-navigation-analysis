### Data files for ALS pilot data 
# only one timepoint of testing (T1)

## install packages
# install.packages("ggplot2")
# install.packages("readxl")
# install.packages("dplyr")
# install.packages("plyr")
# install.packages("knitr")
# install.packages("magrittr")
# install.packages("kableExtra")
# install.packages("reshape2")
# install.packages("openxlsx")

## get packages
library(ggplot2)
library(readxl)
library(plyr)
library(dplyr)
library(knitr)
library(magrittr)
library(kableExtra)
library(reshape2)
library(openxlsx)

## set directory and filename
setwd("S:/C15/WNE/FO/SFB1315-B05/Data/WP6 - ALS/Analysed_Data/Results_WP_6/")
in_filename <- "WP6_table_vs2_results_overview_DeetjeEMail.xls"

## get data from excel sheets
data_path_all <- read_xls(in_filename, sheet = "Path-Analysis", col_names=T)
data_path_training <- read_xls(in_filename, sheet = "Path-An-Mixed", col_names=T)
data_path_ego <- read_xls(in_filename, sheet = "Path-An-Ego", col_names=T)
data_path_allo <- read_xls(in_filename, sheet = "Path-An-Allo", col_names=T)

#data_exploration_all <- read_xls(in_filename, sheet = "Exploration-Analysis", col_names=T)
data_exploration_training <- read_xls(in_filename, sheet = "Exploration-An-Mixed", col_names=T)
data_exploration_ego <- read_xls(in_filename, sheet = "Exploration-An-Ego", col_names=T)
data_exploration_allo <- read_xls(in_filename, sheet = "Exploration-An-Allo", col_names=T)

data_scoring <- read_xls(in_filename, sheet = "Scoring", col_names=T)


## clean data structure
# get relevant variables
int_vars <- c("Participant", "Group", "Subgroup", "Age", "Sex", "test_duration") 
              #"time_all_mean","path_all_mean", "path_accuracy_all_mean", 
              #"final_dist_ego_tar_all_mean", "final_dist_allo_tar_all_mean", 
              #"average_dis_ego_tar_all_mean", "average_dis_allo_tar_all_mean", 
              #"distance_accuracy_all_mean", "path_score_all_mean", "direct_path_all" ) # add here
data_path_all <- data_path_all[,int_vars]
rm(int_vars)

int_vars <- c("time__mix_pt__mean", "time__mix_nF__mean", "time__mix_wF__mean", 
              "path__mix_pt__mean", "path__mix_nF__mean", "path__mix_wF__mean", 
              "path_accuracy__mix_pt__mean", "path_accuracy__mix_nF__mean", "path_accuracy__mix_wF__mean", 
              # "final_dist_ego_tar__mix_pt__mean", "final_dist_ego_tar__mix_nF__mean", "final_dist_ego_tar__mix_wF__mean",  
              # "final_dist_allo_tar__mix_pt__mean", "final_dist_allo_tar__mix_nF__mean", "final_dist_allo_tar__mix_wF__mean", 
              # "average_dis_ego_tar__mix_pt__mean", "average_dis_ego_tar__mix_nF__mean", "average_dis_ego_tar__mix_wF__mean", 
              # "average_dis_allo_tar__mix_pt__mean", "average_dis_allo_tar__mix_nF__mean", "average_dis_allo_tar__mix_wF__mean", 
              "distance_accuracy__mix_pt__mean", "distance_accuracy__mix_nF__mean", "distance_accuracy__mix_wF__mean", 
              "path_score__mix_pt__mean", "path_score__mix_nF__mean", "path_score__mix_wF__mean",  
              "direct_path__mix_pt_", "direct_path__mix_nF_", "direct_path__mix_wF_") # add here
data_path_training <- data_path_training[,int_vars]
rm(int_vars)

int_vars <- c("time__ego__all_mean", "time__ego_pt__mean", "time__ego_wF__mean", 
              "path__ego__all_mean", "path__ego_pt__mean", "path__ego_wF__mean", 
              "path_accuracy__ego__all_mean", "path_accuracy__ego_pt__mean", "path_accuracy__ego_wF__mean", 
              # "final_dist_ego_tar__ego__all_mean", "final_dist_ego_tar__ego_pt__mean", "final_dist_ego_tar__ego_wF__mean", 
              # "average_dis_ego_tar__ego__all_mean", "average_dis_ego_tar__ego_pt__mean", "average_dis_ego_tar__ego_wF__mean", 
              "distance_accuracy__ego__all_mean", "distance_accuracy__ego_pt__mean", "distance_accuracy__ego_wF__mean", 
              "path_score__ego__all_mean", "path_score__ego_pt__mean", "path_score__ego_wF__mean", 
              "direct_path__ego__all", "direct_path__ego_pt_", "direct_path__ego_wF_") # add here
data_path_ego <- data_path_ego[,int_vars]
rm(int_vars)

int_vars <- c("time__allo__all_mean", "time__allo_pt__mean", "time__allo_wF__mean", 
              "path__allo__all_mean", "path__allo_pt__mean", "path__allo_wF__mean", 
              "path_accuracy__allo__all_mean", "path_accuracy__allo_pt__mean", "path_accuracy__allo_wF__mean", 
              # "final_dist_allo_tar__allo__all_mean", "final_dist_allo_tar__allo_pt__mean", "final_dist_allo_tar__allo_wF__mean", 
              # "average_dis_allo_tar__allo__all_mean", "average_dis_allo_tar__allo_pt__mean", "average_dis_allo_tar__allo_wF__mean", 
              "distance_accuracy__allo__all_mean", "distance_accuracy__allo_pt__mean", "distance_accuracy__allo_wF__mean", 
              "path_score__allo__all_mean", "path_score__allo_pt__mean", "path_score__allo_wF__mean", 
              "direct_path__allo__all", "direct_path__allo_pt_", "direct_path__allo_wF_") # add here
data_path_allo <- data_path_allo[,int_vars]
rm(int_vars)


# int_vars <- c("success_all", "semisuccess_all") # add here
# data_exploration_all <- data_exploration_all[,int_vars]
# rm(int_vars)

int_vars <- c("success_mix_pt__abs", "success_mix_nF__abs", "success_mix_wF__abs",
              "semisuccess_mix_pt__abs", "semisuccess_mix_nF__abs", "semisuccess_mix_wF__abs") # add here
data_exploration_training <- data_exploration_training[,int_vars]
rm(int_vars)

int_vars <- c("success_ego__abs", "success_ego_pt__abs", "success_ego_wF__abs",
              "semisuccess_ego__abs", "semisuccess_ego_pt__abs", "semisuccess_ego_wF__abs") # add here
data_exploration_ego <- data_exploration_ego[,int_vars]
rm(int_vars)

int_vars <- c("success_allo__abs", "success_allo_pt__abs", "success_allo_wF__abs",
              "semisuccess_allo__abs", "semisuccess_allo_pt__abs", "semisuccess_allo_wF__abs") # add here
data_exploration_allo <- data_exploration_allo[,int_vars]
rm(int_vars)


int_vars <- c("Score_Recall_Total", "Score_Recall_LM", "Score_Recall_Maze", 
              "Score_Recognition_Total", "Score_Recognition_LM", "Score_Recognition_LMA",
              "Score_Recall_2", "Score_Recall_Schatzkiste", "Score_Recall_Route") # add here
data_scoring <- data_scoring[,int_vars]
rm(int_vars)


# create big data file 
data <- data.frame(data_path_all, data_path_training, data_path_ego, data_path_allo,
                   #data_exploration_all, 
                   data_exploration_training, data_exploration_ego, data_exploration_allo,
                   data_scoring)

rm(data_path_all, data_path_training, data_path_ego, data_path_allo,
   #data_exploration_all, 
   data_exploration_training, data_exploration_ego, data_exploration_allo,
   data_scoring)


# remove missing data
data <- data[complete.cases(data), ]


# convert to int
int_vars <- c("Participant","Age") # add here 
data[,int_vars] <- lapply(data[,int_vars], as.integer)
rm(int_vars)

# convert to factor
data$Group <- factor(data$Group, levels=c(0,1), labels=c("Control", "ALS"))
data$Subgroup <- factor(data$Subgroup, levels=c(1,2,3), labels=c("ALS_1", "ALS_2", "none"))
data$Sex <- factor(data$Sex, levels=c(1,2), labels=c("male", "female"))


## other useful functuions 
# convert several variables to factor 
# ss_vars <- grep("_ss_", names(data))
# data[,ss_vars] <- lapply(data[,ss_vars], factor, levels=c(0,1,2,3), labels=c("fail", "ego", "allo", "switch"))

# rename severalcolumns   
# data <- plyr::rename(data, c("PT-1...86" = "t1_ss_mixed_pt_1", # t1 mixed
#                        "PT-2...87" = "t1_ss_mixed_pt_2"))

#############################################################################
## save data file 

setwd('S:/C15/WNE/FO/SFB1315-B05/Data/WP6 - ALS/Analysed_Data/Results_WP_6/Result Plots')
out_filename <- "WP6_table_overview_results_selected_variables.xlsx"

wb <- createWorkbook()
addWorksheet(wb, "Data")
writeData(wb, "Data", data)

saveWorkbook(wb, out_filename, overwrite = TRUE)
rm(wb)

#############################################################################
## basic demographics

freq <- table(data$Group)
freq_female <- table(data$Group[data$Sex=="female"])
freq_male <- table(data$Group[data$Sex=="male"])

mean_age <- tapply(data$Age, data$Group, mean)
min_age <- tapply(data$Age, data$Group, min)
max_age <- tapply(data$Age, data$Group, max)

summary_desc <- rbind(freq, freq_female, freq_male, mean_age, min_age, max_age)

rm(freq, freq_female, freq_male, mean_age, min_age, max_age)
