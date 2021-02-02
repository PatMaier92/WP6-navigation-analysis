### ------------------- WP6 Data  ----------------------- ###
### WP6_DataFileCreator.R                                 ###
### Author: Patrizia Maier                                ###


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
# install.packages("tidyverse")

## get packages
library(tidyverse)
library(readxl)
library(foreign)
library(openxlsx)


###############################################################################


## set path
path <- "T:/Analysis/WP6/Patrizia/WP6_data/Preliminary 2021 02/"

# read data 
# sm_file <- "WP6_table_2007.xlsx"
# sm_data <- read_xlsx(sm_file, sheet = "Tabelle1", col_names = T)
# participants <- unique(sm_data$ID)

score_file <- paste(path, "WP6_RecallRecognition_scoring_210202_all.xlsx", sep="")
score_data <- read_xlsx(score_file, sheet = "WP6_all", col_names=T)
participants <- unique(score_data$ID) # REMOVE LATER 

np_file <- paste(path, "Auswertung WP06_6200-6300_CLEANED_210202.sav", sep="") 
np_data <- read.spss(np_file, use.value.labels=T, to.data.frame=T)

clin_file <- paste(path, "WP6_data_clinical_data_ALS_210128.xlsx", sep="")
clin_data <- read_xlsx(clin_file, sheet = "Tabelle1", col_names=T)


###############################################################################


## clean data structures

# # STARMAZE TRIAL DATA
# # select data
# int_vars <- c("ID", "Group", "Trial", "Trial_Condition", "Feedback","Start_position",
#               "success", "time_abs", "time_accuracy", "path_abs", "path_accuracy","distance_accuracy", 
#               "final_distance_to_allo_abs", "final_distance_to_ego_abs","Path_score","Direct_Path",
#               "head_rotation_abs", "head_turn_abs") 
# sm_data <- sm_data[,int_vars]
# rm(int_vars)

# # correction of errors 
# # 1) for subjects who saw video (= 32 instead of 31 trials)
# # Trial: subtract -1 from all trial numbers > 4
# participants_video <- sm_data$ID[sm_data$Trial==32]
# sm_data$Trial[sm_data$ID %in% participants_video & sm_data$Trial > 4] <- sm_data$Trial[sm_data$ID %in% participants_video & sm_data$Trial > 4]-1
# # Feedback: correct order
# correct_feedback <- sm_data$Feedback[sm_data$ID == 6201]
# for (id in participants_video){
#   sm_data$Feedback[sm_data$ID == id] <- correct_feedback
# }
# # 2) general error 
# sm_data$Feedback[sm_data$Trial == 26] <- 1



# STARMAZE SCORING DATA 
# select data 
int_vars <- c("ID", "Score_total", "Object_Identity", "Object_location", "Maze_reconstruction...6",
              "Object_Identity_recall", "Object_Identity_recognition", 
              "Object_location_recall", "Object_location_recognition", "Obj_loc_pos", 
              "Maze_reconstruction...12") 
score_data <- score_data[,int_vars]
rm(int_vars)

# rename colums
names(score_data) <- c("ID", "Score_total", "Object_Identity", "Object_location", "Maze_reconstruction",
              "Object_Identity_recall", "Object_Identity_recognition", 
              "Object_location_recall", "Object_location_recognition_1", "Object_location_recognition_2", 
              "Maze_reconstruction_copy") 

# remove obervations 
score_data <- score_data[score_data$ID %in% participants, ]



# CLINICAL DATA
int_vars <- c("ID", "MNE-Untergruppe", "ALS-Variante", "MN", "Verlaufsform", 
              "Genetik", "NFL", "ALS-FRS-R", "FRS-/Monat", 
              "VK(%)", "PCF(l/min)", "SpO2", "NIV", "PEG", 
              "Erstsymptomatik", "Lokalisation Erstymsptomatik", "Seite Erstsymptomatik",
              "ES-T1 (Monate)", "ED - T1 (Monate)") 
clin_data <- clin_data[,int_vars]
rm(int_vars)
names(clin_data) <- c("ID", "MNE-Untergruppe", "ALS-Variante", "MN", "Verlaufsform", 
                      "Genetik", "NFL", "ALS-FRS-R", "FRS-/Monat", 
                      "VK(%)", "PCF(l/min)", "SpO2", "NIV", "PEG", 
                      "Erstsymptomatik", "Lokalisation_Erstsymp", "Seite_Erstsymp",
                      "ES_T1", "ED_T1") 

# add dummy observations
controls <- participants[!participants %in% clin_data$ID]
dummy_data <- cbind(controls, matrix(NA, nrow=length(controls), ncol=dim(clin_data)[2]-1))
dummy_data <- data.frame(dummy_data)
names(dummy_data) <- names(clin_data)
clin_data <- rbind(clin_data, dummy_data)
rm(dummy_data)

# remove observations
clin_data <- clin_data[clin_data$ID %in% participants, ]

# # recode unspecific as ALS (4 -> 1) 
# clin_data$Subgroup[clin_data$Subgroup==4] <- 1



# NEUROPSYCHOLOGY
# select data
not_int_vars <- c("ECAS_height", "ECAS_weight", "ECAS_BMI",
                  "ECAS_PEG","ECAS_EL_Escorial","ECAS_ALS","ECAS_Oxy","ECAS_FVC","ECAS_ALS_FRS_r", "INFO_0") 
np_data <- np_data[,!(names(np_data) %in% not_int_vars)]
rm(not_int_vars)

# rename columns
names(np_data)[names(np_data) == "info_id"] <- "ID"
names(np_data)[names(np_data) == "info_group"] <- "Group"

# remove observations
np_data <- np_data[np_data$ID %in% participants, ]



## create data frame with CLINICAL, NEUROPSYCHOLOGY AND SCORE DATA 
# make sure order is equal
sm_data <- sm_data[order(sm_data$ID),]
score_data <- score_data[order(score_data$ID),]
clin_data <- clin_data[order(clin_data$ID),]
np_data <- np_data[order(np_data$ID),]

# data frame 
data_individual <- data.frame(score_data, np_data, clin_data) 
rm(score_data, np_data, clin_data)


#############################################################################


data <- "210202"

## save data as Rdata 
out_fileR <-  paste(path, "WP6_data_", date, ".Rdata", sep="")
save(data_individual, file=out_fileR)


## save data as excel 
out_fileXLSX <-  paste(path, "WP6_data", date, ".xlsx", sep="")

wb <- createWorkbook()
addWorksheet(wb, "Data_individual")
writeData(wb, "Data_individual", data_individual)
addWorksheet(wb, "Data_trial")
writeData(wb, "Data_trial", sm_data)

saveWorkbook(wb, out_fileXLSX, overwrite = TRUE)
rm(wb)



## clear workspace
rm(list = ls())


#############################################################################


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
