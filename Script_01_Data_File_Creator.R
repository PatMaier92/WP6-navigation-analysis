### ------------------- WP6 Data  ----------------------- ###
### WP6_DataFileCreator.R                                 ###
### Author: Patrizia Maier                                ###


## get packages
library(tidyverse)
library(readxl)
library(foreign)
library(openxlsx)


############################################################################### 

## path
path <- "WP6_data/"

# read data
# starmaze main
sm_file <- paste(path, "WP6_result_table_211123.xlsx", sep="")
sm_data <- read_xlsx(sm_file, sheet = "sm6", col_names = T)
participants <- unique(sm_data$id) # final sample of included participants
rm(sm_file)

# starmaze motor control 
motor_control_file <- paste(path, "WP6_SM_mct_table_211123.xlsx", sep="")
motor_control_data <- read_xlsx(motor_control_file, sheet = "SM_mct", col_names = T)
rm(motor_control_file)


# drawing score
score_file <- paste(path, "WP6_RecallRecognition_scoring_211210.xlsx", sep="")
score_data <- read_xlsx(score_file, sheet = "WP6_all", col_names=T, na = "NA")
score_data <- score_data[score_data$ID %in% participants, ] # remove excluded participants
rm(score_file)

# gmda scores
gmda_draw_file <-  paste(path, "WP6_GMDA_data_drawing_211123.Rdata", sep="")
load(gmda_draw_file)
gmda_draw <- temp[temp$ID %in% participants, ] # remove excluded participants
rm(temp, gmda_draw_file)

gmda_recog_file <-  paste(path, "WP6_GMDA_data_recognition_211123.Rdata", sep="")
load(gmda_recog_file)
gmda_recog <- temp[temp$ID %in% participants, ] # remove excluded participants
rm(temp, gmda_recog_file) 


# neuropsychology
np_file <- paste(path, "Auswertung WP06_6200-6300_CLEANED_211123.sav", sep="") 
np_data <- read.spss(np_file, use.value.labels=T, to.data.frame=T)
np_data <- np_data[np_data$info_id %in% participants, ] # remove excluded participants
rm(np_file)


# clinical data
clin_file <- paste(path, "WP6_data_clinical_data_ALS_210602.xlsx", sep="")
clin_data <- read_xlsx(clin_file, sheet = "Tabelle1", col_names=T)
clin_data <- clin_data[clin_data$ID %in% participants, ] # remove excluded participants
rm(clin_file)


# load ecas norm values
ecas_norms_2016 <- read_xlsx("Normen_ECAS_Loose_2016.xlsx", sheet="Tabelle1", col_names=T)

###############################################################################


## STARMAZE TRIAL DATA
sm_data$trial_condition[sm_data$trial_num %in% c(22,25,28)] <- 1 # mixed in allocentric is allocentric

trial_data <- sm_data %>% 
  select(!c(wp, group_no, trial_type)) %>% 
  rename(group=group_name, probe_trial=feedback) %>%
  mutate(group=fct_recode(group, MND="MNE", Control="Control"),
         trial_condition=fct_recode(factor(trial_condition), training="0", allocentric="1", egocentric="2", mixed="3"),
         probe_trial=case_when(probe_trial==0 ~ 1,
                               probe_trial==1 ~ 0)) 
rm(sm_data)


## STARMAZE MOTOR CONTROL DATA 
mc_data <- motor_control_data %>% 
  select(!c(wp, mct_sumBodyRotation, mct_sumHeadRotation, mct_bodyRotation, mct_headRotation)) %>% 
  mutate(group=factor(case_when(group_no==1 ~ "MND", TRUE ~ "Control")),
         mct_time=as.numeric(mct_time),
         mct_path=as.numeric(mct_path),
         mct_velocity=as.numeric(mct_velocity)) %>% 
  select(!group_no) %>% 
  rename(ID=id) 
rm(motor_control_data)
  

## SCORING DATA 
# clean data 
sc_data <- score_data %>% 
  select(!Maze_reconstruction...12 & !Group) %>% 
  rename(old_Score_total_manual=Score_total,
         old_Object_location_manual=Object_location,
         old_Object_location_manual_drawing=Object_location_recall,
         old_Object_location_manual_recognition=Object_location_recognition,
         old_Object_location_manual_position=Obj_loc_pos,
         Maze_reconstruction_manual=Maze_reconstruction...6,
         Object_identity_manual=Object_Identity,
         Object_identity_manual_drawing=Object_Identity_recall,
         Object_identity_manual_recognition=Object_Identity_recognition) %>% 
  mutate(Maze_reconstruction_manual_s=Maze_reconstruction_manual / 16,
         Object_identity_manual_s=Object_identity_manual / 28)
rm(score_data)

# add gmda data 
g_d <- gmda_draw %>% 
  ungroup() %>% 
  filter(Measure=="SQRT(CanOrg)") %>% 
  mutate(ID=as.numeric(ID)) %>% 
  select(ID, drawing) %>% 
  rename("GMDA_SQRTCanOrg_drawing"= drawing)

g_r <- gmda_recog %>% 
  ungroup() %>% 
  filter(Measure=="SQRT(CanOrg)") %>% 
  mutate(ID=as.numeric(ID)) %>% 
  select(ID, recognition) %>% 
  rename("GMDA_SQRTCanOrg_recognition"= recognition)

sc_data <- sc_data %>% 
  left_join(g_d) %>% 
  left_join(g_r) %>% 
  mutate(Object_location_GMDA_SQRTCanOrg_s=(GMDA_SQRTCanOrg_drawing + GMDA_SQRTCanOrg_recognition)/2,
         Score_total=(Maze_reconstruction_manual_s + Object_identity_manual_s + Object_location_GMDA_SQRTCanOrg_s)/3)
rm(g_r, g_d, gmda_draw, gmda_recog)


## CLINICAL DATA
# clean data 
c_data <- clin_data %>% 
  select(ID, `MNE-Untergruppe`, `ALS-Variante`, MN, Verlaufsform, 
         Genetik, NFL_csf, NFL_Serum, `ALS-FRS-R`, `FRS-/Monat`, `VK(%)`, `PCF(l/min)`, SpO2,
         Körpergröße, KG, BMI, cs_bulbar, cs_pseudobulbar, cs_dysarthria,cs_dysphagia,
         cs_monoparesis, cs_paraparesis, cs_tetraparesis, cs_myatrophy, cs_fasciculation, 
         cs_fibrillation, cs_spasticity, cs_hyperreflexy, cs_motoricDisinihibition, cs_gaitDisorder,
         med_hypoventilation, med_Riluzol, `med_Cannabis`, med_Baclofen,
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
rm(clin_data)  

# add dummy observations for controls  
controls <- participants[!participants %in% c_data$ID]
c_data <- c_data %>% 
  add_row(ID=controls)


## NEUROPSYCHOLOGY
# clean data
n_data <- np_data %>% 
  select(!c(info_group, info_t1_star_start, info_t1_star_end, info_t1_starmaze_notes,
            ECAS_start, ECAS_end, ECAS_fam_1degree, ECAS_fam_2degree,
            ECAS_first_symptoms, ECAS_first_diagnosis, ECAS_not_invasive_resp,
            ECAS_status_beginn_bulbuar, ECAS_status_beginn_lower, ECAS_status_beginn_upper,
            ECAS_status_now_bulbuar, ECAS_status_now_lower, ECAS_status_now_upper,
            ECAS_height, ECAS_weight, ECAS_PEG,INFO_0, info_date
            )) %>% 
  mutate(dfb_q1_sex=fct_recode(dfb_q1_sex, male="männlich", female="weiblich"),
         dfb_q4_highestedu=factor(dfb_q4_highestedu),
         dfb_q5_language_german=factor(dfb_q5_language_german),
         dfb_q6_handiness=factor(dfb_q6_handiness)) %>% 
  rename(ID=info_id)   
rm(np_data)

# add cut-off values 
cut_off_func <- function(age, isced_school_II, norm_cat){
  my_cut_off <- case_when(
    
    # new norms from Loose 2016
    age < 60 & isced_school_II <= 12 ~ ecas_norms_2016$cut_under_60_underequal_12[ecas_norms_2016$measures==norm_cat],
    age < 60 & isced_school_II > 12 ~ ecas_norms_2016$cut_under_60_over_12[ecas_norms_2016$measures==norm_cat],
    age >= 60 & isced_school_II <= 12 ~ ecas_norms_2016$cut_overequal_60_underequal_12[ecas_norms_2016$measures==norm_cat],
    age >= 60 & isced_school_II > 12 ~ ecas_norms_2016$cut_overequal_60_over_12[ecas_norms_2016$measures==norm_cat]
    )
  
  return(my_cut_off)
  
  # example use: cut_off_func(n_data$dfb_q2_age[3], n_data$dfb_q3_years_school_clean[3], "ECAS_total")
  # [1] 78.8
}

n_data <- n_data %>% 
  mutate(
    ECAS_total_cut = cut_off_func(dfb_q2_age, dfb_q3_years_school_clean, "ECAS_total"),
    ECAS_total_below_cut = ECAS_total_score <= ECAS_total_cut,
    ECAS_ALS_specific_cut = cut_off_func(dfb_q2_age, dfb_q3_years_school_clean, "ECAS_ALSspec"),
    ECAS_ALS_specific_below_cut = ECAS_ALS_specific <= ECAS_ALS_specific_cut,
    ECAS_ALS_unspecific_cut = cut_off_func(dfb_q2_age, dfb_q3_years_school_clean, "ECAS_ALSnonspec"),
    ECAS_ALS_unspecific_below_cut = ECAS_ALS_unspecific <= ECAS_ALS_unspecific_cut,
    ECAS_sub_language_cut = cut_off_func(dfb_q2_age, dfb_q3_years_school_clean, "Language"),
    ECAS_sub_language_below_cut = ECAS_sub_language <= ECAS_sub_language_cut,
    ECAS_sub_memory_cut = cut_off_func(dfb_q2_age, dfb_q3_years_school_clean, "Memory"),
    ECAS_sub_memory_below_cut =ECAS_sub_memory <= ECAS_sub_memory_cut,
    ECAS_sub_executive_cut = cut_off_func(dfb_q2_age, dfb_q3_years_school_clean, "Executive_function"),
    ECAS_sub_executive_below_cut = ECAS_sub_executive <= ECAS_sub_executive_cut,
    ECAS_sub_verbal_fluency_cut = cut_off_func(dfb_q2_age, dfb_q3_years_school_clean, "Fluency"),
    ECAS_sub_verbal_fluency_below_cut = ECAS_sub_verbal_fluency <= ECAS_sub_verbal_fluency_cut,
    ECAS_sub_spatial_cut = cut_off_func(dfb_q2_age, dfb_q3_years_school_clean, "Visuospatial"),
    ECAS_sub_spatial_below_cut = ECAS_sub_spatial <= ECAS_sub_spatial_cut
  )
rm(ecas_norms_2016)


## create data frame with MOTOR CONTROL, CLINICAL, NEUROPSYCHOLOGY AND SCORE DATA
data_individual <- mc_data %>% 
  full_join(n_data) %>% 
  full_join(c_data) %>% 
  full_join(sc_data)
rm(mc_data, n_data, c_data, sc_data)


#############################################################################


## save data as Rdata 
out_fileR <-  paste(path, "WP6_individual_data.Rdata", sep="")
save(data_individual, file=out_fileR)

out_fileTR <-  paste(path, "WP6_trial_data.Rdata", sep="")
save(trial_data, file=out_fileTR)


## save data as excel 
out_fileXLSX <-  paste(path, "WP6_individual_data.xlsx", sep="")

wb <- createWorkbook()
addWorksheet(wb, "Data_individual")
writeData(wb, "Data_individual", data_individual)
#addWorksheet(wb, "Data_trial")
#writeData(wb, "Data_trial", sm_data)

saveWorkbook(wb, out_fileXLSX, overwrite = TRUE)
rm(wb)


## save data as csv (for JASP)
out_fileCSV <-  paste(path, "WP6_individual_data.csv", sep="")
write.csv(data_individual, file=out_fileCSV, row.names=FALSE)

out_fileCSVTR <-  paste(path, "WP6_trial_data.csv", sep="")
write.csv(trial_data, file=out_fileCSVTR, row.names=FALSE)


## clear workspace
rm(list = ls())

