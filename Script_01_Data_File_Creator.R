### ------------------- WP6 Data  ----------------------- ###
### WP6_DataFileCreator.R                                 ###
### Author: Patrizia Maier                                ###


## get packages
library(tidyverse)
library(readxl)
library(foreign)
library(openxlsx)


## input date 
date = readline(prompt = "Please enter the date string of the result file ")


###############################################################################


## set path
path <- "WP6_data/"


## read data 
# starmaze
# sm_file <- "WP6_table_2007.xlsx"
# sm_data <- read_xlsx(sm_file, sheet = "Tabelle1", col_names = T)
# participants <- unique(sm_data$ID)

# drawing score
score_file <- paste(path, "WP6_RecallRecognition_scoring_", date, ".xlsx", sep="")
score_data <- read_xlsx(score_file, sheet = "WP6_all", col_names=T, na = "NA")

# neuropsychology
np_file <- paste(path, "Auswertung WP06_6200-6300_CLEANED_", date, ".sav", sep="") 
np_data <- read.spss(np_file, use.value.labels=T, to.data.frame=T)
participants <- unique(np_data$info_id) # REMOVE LATER 
participants <- participants[!is.na(participants)]# REMOVE LATER 
participants <- participants[!participants %in% c(6202, 6203, 6205, 6209, 6229, 6235, 6306, 6311, 6320, 6331, 6337)]

# clinical data
clin_file <- paste(path, "WP6_data_clinical_data_ALS_", date, ".xlsx", sep="")
clin_data <- read_xlsx(clin_file, sheet = "Tabelle1", col_names=T)

# load ecas norm values
ecas_norms_2015 <- read_xlsx("Normen_ECAS_Lule_2015.xlsx", sheet="Tabelle1", col_names=T)
ecas_norms_2016 <- read_xlsx("Normen_ECAS_Loose_2016.xlsx", sheet="Tabelle1", col_names=T)

###############################################################################


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



## SCORING DATA 
# clean data 
sc_data <- score_data %>% 
  filter(!is.na(ID)) %>% 
  select(!Maze_reconstruction...12 & !Group) %>% 
  rename(Maze_reconstruction=Maze_reconstruction...6)

# remove excluded observations 
sc_data <- sc_data[sc_data$ID %in% participants, ]



## CLINICAL DATA
# clean data 
c_data <- clin_data %>% 
  select(ID, `MNE-Untergruppe`, `ALS-Variante`, MN, Verlaufsform, 
         Genetik, NFL_csf, NFL_Serum, `ALS-FRS-R`, `FRS-/Monat`, `VK(%)`, `PCF(l/min)`, SpO2,
         Körpergröße, KG, BMI, cs_bulbar, cs_pseudobulbar, cs_dysarthria,cs_dysphagia,
         cs_monoparesis, cs_paraparesis, cs_tetraparesis, cs_myatrophy, cs_fasciculation, 
         cs_fibrillation, cs_spasticity, cs_hyperreflexy, cs_motoricDisinihibition, cs_gaitDisorder,
         med_hypoventilation, med_Riluzol, `med:Cannabis`, med_Baclofen,
         med_siallorhoe, med_cns, med_depression, med_anxiety, med_other_1, med_other_2,
         med_other_3, medCon_additional_1, medCon_additional_2, medCon_additional_3,
         initial_symptome_timepoint, initial_symptom, is_region, is_lateralized, is_category, is_t1_months,
         initial_diagnosis_timepoint, id_t1_months
         ) %>% 
  rename(MNE_Untergruppe =`MNE-Untergruppe`, ALS_Variante=`ALS-Variante`) %>% 
  mutate(MNE_Untergruppe = factor(MNE_Untergruppe, labels=c("ALS", "PLS", "PMA")),
         ALS_Variante = factor(ALS_Variante, labels=c("ALS-Bulbär", "ALS", "ALS-spastisch", "ALS-pereoneal")),
         is_category = factor(is_category, labels=c("bulbär", "parese", "spastik")),
         MN=factor(MN, labels=c("1. MN", "2. MN", "1. und 2. MN")))
  
# add dummy observations for controls  
controls <- participants[!participants %in% c_data$ID]
c_data <- c_data %>% 
  add_row(ID=controls)

# remove excluded observations
c_data <- c_data[c_data$ID %in% participants, ]



## NEUROPSYCHOLOGY
# clean data
n_data <- np_data %>% 
  select(!c("ECAS_height", "ECAS_weight", "ECAS_PEG", "INFO_0", "info_date")) %>% 
  rename(ID=info_id, Group=info_group)

# remove excluded observations
n_data <- n_data[n_data$ID %in% participants, ]

# add cut-off values 
cut_off_func <- function(age, isced_school_II, norm_cat){
  my_cut_off <- case_when(
    
    # new norms from Loose 2016
    age < 60 & isced_school_II <= 12 ~ ecas_norms_2016$cut_under_60_underequal_12[ecas_norms_2016$measures==norm_cat],
    age < 60 & isced_school_II > 12 ~ ecas_norms_2016$cut_under_60_over_12[ecas_norms_2016$measures==norm_cat],
    age >= 60 & isced_school_II <= 12 ~ ecas_norms_2016$cut_overequal_60_underequal_12[ecas_norms_2016$measures==norm_cat],
    age >= 60 & isced_school_II > 12 ~ ecas_norms_2016$cut_overequal_60_over_12[ecas_norms_2016$measures==norm_cat]
    
    # old norms from Lulé 2015
    # age <= 60 & isced_school_II < 12 ~ ecas_norms_2015$cut_underequal_60_under_12[ecas_norms_2015$measures==norm_cat],
    # age > 60 & isced_school_II < 12 ~ ecas_norms_2015$cut_over_60_under_12[ecas_norms_2015$measures==norm_cat],
    # age <= 60 & isced_school_II >= 12 ~ ecas_norms_2015$cut_underequal_60_overequal_12[ecas_norms_2015$measures==norm_cat],
    # age > 60 & isced_school_II >= 12 ~ ecas_norms_2015$cut_over_60_overequal_12[ecas_norms_2015$measures==norm_cat]
    # 
    )
  
  return(my_cut_off)
  
  # example use: cut_off_func(n_data$dfb_q2_age[3], n_data$dfb_q3_years_school_clean[3], "ECAS_total")
  # [1] 78.8
}

n_data <- n_data %>% 
  mutate(
    ECAS_total_cut = cut_off_func(dfb_q2_age, dfb_q3_years_school_clean, "ECAS_total"),
    ECAS_total_below_cut = ECAS_total_score < ECAS_total_cut,
    ECAS_ALS_specific_cut = cut_off_func(dfb_q2_age, dfb_q3_years_school_clean, "ECAS_ALSspec"),
    ECAS_ALS_specific_below_cut = ECAS_ALS_specific < ECAS_ALS_specific_cut,
    ECAS_ALS_unspecific_cut = cut_off_func(dfb_q2_age, dfb_q3_years_school_clean, "ECAS_ALSnonspec"),
    ECAS_ALS_unspecific_below_cut = ECAS_ALS_unspecific < ECAS_ALS_unspecific_cut,
    ECAS_sub_language_cut = cut_off_func(dfb_q2_age, dfb_q3_years_school_clean, "Language"),
    ECAS_sub_language_below_cut = ECAS_sub_language < ECAS_sub_language_cut,
    ECAS_sub_memory_cut = cut_off_func(dfb_q2_age, dfb_q3_years_school_clean, "Memory"),
    ECAS_sub_memory_below_cut =ECAS_sub_memory < ECAS_sub_memory_cut,
    ECAS_sub_executive_cut = cut_off_func(dfb_q2_age, dfb_q3_years_school_clean, "Executive_function"),
    ECAS_sub_executive_below_cut = ECAS_sub_executive < ECAS_sub_executive_cut,
    ECAS_sub_verbal_fluency_cut = cut_off_func(dfb_q2_age, dfb_q3_years_school_clean, "Fluency"),
    ECAS_sub_verbal_fluency_below_cut = ECAS_sub_verbal_fluency < ECAS_sub_verbal_fluency_cut,
    ECAS_sub_spatial_cut = cut_off_func(dfb_q2_age, dfb_q3_years_school_clean, "Visuospatial"),
    ECAS_sub_spatial_below_cut = ECAS_sub_spatial < ECAS_sub_spatial_cut
  )


## create data frame with CLINICAL, NEUROPSYCHOLOGY AND SCORE DATA
# data <- sm_data %>% 
#   full_join(sc_data) %>% 
#   full_join(c_data) %>% 
#   full_join(n_data)

data_individual <- n_data %>% 
  full_join(c_data) %>% 
  full_join(sc_data)

rm(score_data, np_data, clin_data)

data_individual <- data_individual %>% 
  mutate(Group=fct_recode(Group, MNE="Experimental", Control="Controll"),
         dfb_q1_sex=fct_recode(dfb_q1_sex, male="männlich", female="weiblich"),
         dfb_q4_highestedu=factor(dfb_q4_highestedu),
         dfb_q5_language_german=factor(dfb_q5_language_german),
         dfb_q6_handiness=factor(dfb_q6_handiness))


#############################################################################


## save data as Rdata 
out_fileR <-  paste(path, "WP6_data_", date, ".Rdata", sep="")
save(data_individual, file=out_fileR)

# out_fileTR <-  paste(path, "WP6_trial_data_", date, ".Rdata", sep="")
# save(data_trial), file=out_fileTR)



## save data as excel 
out_fileXLSX <-  paste(path, "WP6_data_", date, ".xlsx", sep="")

wb <- createWorkbook()
addWorksheet(wb, "Data_individual")
writeData(wb, "Data_individual", data_individual)
#addWorksheet(wb, "Data_trial")
#writeData(wb, "Data_trial", sm_data)

saveWorkbook(wb, out_fileXLSX, overwrite = TRUE)
rm(wb)



## clear workspace
rm(list = ls())

