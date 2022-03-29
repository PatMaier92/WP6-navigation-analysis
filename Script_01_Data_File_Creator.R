### ------------------- WP6 Data  ----------------------- ###
### WP6_DataFileCreator.R                                 ###
### Author: Patrizia Maier                                ###


# ::: get packages ::: #

library(tidyverse)
library(readxl)
library(foreign)
library(openxlsx)


# ######################################################### #


# ::: get data ::: # 

path <- "WP6_data/"


# starmaze main
trial_file <- paste(path, "fam_220107.xlsx", sep="")
trial_data <- read_xlsx(trial_file, col_names = T)
participants <- unique(trial_data$id) # final sample of included participants
rm(trial_file)


# drawing score
score_file <- paste(path, "WP6_RecallRecognition_scoring_211210.xlsx", sep="")
score_data <- read_xlsx(score_file, sheet = "WP6_all", col_names=T, na = "NA")
score_data <- score_data[score_data$ID %in% participants, ] # remove excluded participants
rm(score_file)

# gmda scores
gmda_draw_file <- paste(path, "WP6_GMDA_data_drawing_211123.Rdata", sep="")
load(gmda_draw_file)
gmda_draw <- temp[temp$ID %in% participants, ] # remove excluded participants
rm(temp, gmda_draw_file)

gmda_recog_file <- paste(path, "WP6_GMDA_data_recognition_211123.Rdata", sep="")
load(gmda_recog_file)
gmda_recog <- temp[temp$ID %in% participants, ] # remove excluded participants
rm(temp, gmda_recog_file) 


# neuropsychology
np_file <- paste(path, "Auswertung WP06_6200-6300_CLEANED_211123.sav", sep="") 
np_data <- read.spss(np_file, use.value.labels=T, to.data.frame=T)
np_data <- np_data[np_data$info_id %in% participants, ] # remove excluded participants
rm(np_file)

# load ecas norm values
ecas_norms_2016 <- read_xlsx("Normen_ECAS_Loose_2016.xlsx", sheet="Tabelle1", col_names=T)


# clinical data
clin_file <- paste(path, "WP6_data_clinical_data_ALS_210602.xlsx", sep="")
clin_data <- read_xlsx(clin_file, sheet = "Tabelle1", col_names=T)
clin_data <- clin_data[clin_data$ID %in% participants, ] # remove excluded participants
rm(clin_file)


# ######################################################### #


# ::: SCORING DATA ::: # 

# clean data 
sc_data <- score_data %>% 
  select(ID, Group, Maze_reconstruction...6, Object_Identity, Object_Identity_recall, Object_Identity_recognition) %>% 
  rename(Maze_reconstruction_manual=Maze_reconstruction...6,
         Object_identity_manual=Object_Identity,
         Object_identity_manual_drawing=Object_Identity_recall,
         Object_identity_manual_recognition=Object_Identity_recognition) %>% 
  mutate(Group=factor(Group, levels=c(0,1), labels=c("Ctrl", "ALS")),
         Maze_reconstruction_manual_s=Maze_reconstruction_manual / 16,
         Object_identity_manual_s=Object_identity_manual / 28)
rm(score_data)

# add gmda data 
g_d <- gmda_draw %>% 
  ungroup() %>% 
  filter(Measure=="SQRT(CanOrg)") %>% 
  mutate(ID=as.numeric(ID)) %>% 
  select(ID, drawing) %>% 
  rename("Object_location_SQRTCanOrg_drawing"= drawing)

g_r <- gmda_recog %>% 
  ungroup() %>% 
  filter(Measure=="SQRT(CanOrg)") %>% 
  mutate(ID=as.numeric(ID)) %>% 
  select(ID, recognition) %>% 
  rename("Object_location_SQRTCanOrg_recognition"= recognition)

sc_data <- sc_data %>% 
  left_join(g_d) %>% 
  left_join(g_r) %>% 
  mutate(Object_location_SQRTCanOrg_s=(Object_location_SQRTCanOrg_drawing + Object_location_SQRTCanOrg_recognition)/2,
         Post_test_score_total=(Maze_reconstruction_manual_s + Object_identity_manual_s + Object_location_SQRTCanOrg_s)/3) %>% 
  relocate("Post_test_score_total", .after=("Group")) %>% 
  relocate(starts_with("Object_identity"), .after=("Post_test_score_total")) 
rm(g_r, g_d, gmda_draw, gmda_recog)


# ######################################################### #


# ::: CLINICAL DATA ::: # 

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


# ######################################################### #


# ::: NEUROPSYCHOLOGY ::: # 

# clean data
n_data <- np_data %>% 
  select(!c(info_group, info_t1_star_start, info_t1_star_end, info_t1_starmaze_notes,
            ECAS_start, ECAS_end, ECAS_fam_1degree, ECAS_fam_2degree,
            ECAS_first_symptoms, ECAS_first_diagnosis, ECAS_not_invasive_resp,
            starts_with("ECAS_status_"), ECAS_height, ECAS_weight, ECAS_PEG,INFO_0, info_date,
            starts_with("ECAS_q"), starts_with("SPART_time"), starts_with("SPART_delay"), 
            matches("SPART_q[123]"), starts_with("FIVE_P_raw"), starts_with("PTSOT_q"), 
            PTSOT_mean_dev_adjusted, starts_with("sbsds_q"), 
            ends_with("_precise"), dfb_q3_age_end_edu, 
            dfb_q3_years_school, dfb_q3_years_apprent, dfb_q3_years_uni,
            matches("dfb_q[789]"), matches("dfb_q1[012345789]"), matches("dfb_q2[0345678]"),
            )) %>% 
  mutate(dfb_q1_sex=fct_recode(dfb_q1_sex, 
                               male="männlich", female="weiblich", NULL="intersexuell"),
         dfb_q4_highestedu=factor(dfb_q4_highestedu),
         dfb_q5_language_german=factor(dfb_q5_language_german, 
                                       levels=c("Deutsch ist Muttersprache", "Deutsch ist nicht Muttersprache"), 
                                       labels=c("yes", "no")),
         dfb_q6_handiness=factor(dfb_q6_handiness,
                                 levels=c("rechtshändig", "linkshändig", "beidhändig"),
                                 labels=c("right", "left", "both")), 
         dfb_q16_subj_perf_starmaze=as.numeric(dfb_q16_subj_perf_starmaze),
         dfb_q21_comp_expertise=as.numeric(dfb_q21_comp_expertise),
         dfb_q22_comp_freq=as.numeric(dfb_q22_comp_freq)) %>% 
  rename(ID=info_id, SPART_mean_II=SPART_q4_II)   
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
  ) %>% 
  relocate(starts_with("ECAS_"), .before="SPART_mean_I")
rm(ecas_norms_2016)


# ######################################################### #

# ::: JOINT DATA FRAME ::: # 

# create data frame with CLINICAL, NEUROPSYCHOLOGY AND SCORE DATA
data_individual <- sc_data %>% 
  full_join(n_data) %>% 
  full_join(c_data) 
rm(n_data, c_data, sc_data)


# ######################################################### #


# ::: save data ::: #

# Rdata 
out_fileR <-  paste(path, "WP6_individual_data.Rdata", sep="")
save(data_individual, file=out_fileR)

out_fileTR <-  paste(path, "WP6_trial_data.Rdata", sep="")
save(trial_data, file=out_fileTR)


# excel 
out_fileXLSX <-  paste(path, "WP6_individual_data.xlsx", sep="")
wb <- createWorkbook()
addWorksheet(wb, "Data_individual")
writeData(wb, "Data_individual", data_individual)
saveWorkbook(wb, out_fileXLSX, overwrite = TRUE)


# csv (for JASP)
out_fileCSV <-  paste(path, "WP6_individual_data.csv", sep="")
write.csv(data_individual, file=out_fileCSV, row.names=FALSE)

out_fileCSVTR <-  paste(path, "WP6_trial_data.csv", sep="")
write.csv(trial_data, file=out_fileCSVTR, row.names=FALSE)


# ######################################################### #

# clear workspace
rm(list = ls())
