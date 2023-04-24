# ############################################################################ #
# ############################################################################ #
#                                                                              #
# ------------------------------ WP6 Data ------------------------------------ #
# Script_01_Data_File_Creator                                                  #
# Author: Patrizia Maier                                                       #
#                                                                              #
# ############################################################################ #
# ############################################################################ #


# ------------------------------------------------------------------------------
# ::: LOAD PACKAGES ::: #
# ------------------------------------------------------------------------------

library(tidyverse)
library(readxl)
library(foreign)
library(openxlsx)


# ------------------------------------------------------------------------------
# ::: LOAD RAW DATA ::: #
# ------------------------------------------------------------------------------

path <- "WP6_data/"


# navigation data (for selecting participants)
sm_file <- paste(path, "starmaze_results_table.xlsx", sep="")
data_sm <- read_xlsx(sm_file, col_names = T)
participants <- unique(data_sm$id) # final sample of included participants
rm(sm_file)


# post-navigational scores
score_file <- paste(path, "WP6_RecallRecognition_scoring_211210.xlsx", sep="")
score_data <- read_xlsx(score_file, sheet = "WP6_all", col_names=T, na = "NA")
score_data <- score_data[score_data$ID %in% participants, ] # remove excluded participants
rm(score_file)

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

# # load ecas norm values
# ecas_norms_2016 <- read_xlsx("Normen_ECAS_Loose_2016.xlsx", sheet="Tabelle1", col_names=T)


# clinical data
clin_file <- paste(path, "WP6_clinical_data_ALS_210602.xlsx", sep="")
clin_data <- read_xlsx(clin_file, sheet = "Tabelle1", col_names=T)
clin_data <- clin_data[clin_data$ID %in% participants, ] # remove excluded participants
rm(clin_file)


# ------------------------------------------------------------------------------
# ::: CLEAN DATA ::: #
# ------------------------------------------------------------------------------

# ::: POST-NAVIGATIONAL SCORES  ::: #

sc_data <- score_data %>% 
  select(ID, Group, Maze_reconstruction...6, Object_Identity) %>% 
  mutate(POST_mazeReconstruction=Maze_reconstruction...6 / 16,
         POST_objectIdentity=Object_Identity / 28) %>% 
  select(-Object_Identity, -Maze_reconstruction...6)

g_d <- gmda_draw %>% 
  ungroup() %>% 
  filter(Measure=="SQRT(CanOrg)") %>% 
  mutate(ID=as.numeric(ID)) %>% 
  select(ID, drawing)

g_r <- gmda_recog %>% 
  ungroup() %>% 
  filter(Measure=="SQRT(CanOrg)") %>% 
  mutate(ID=as.numeric(ID)) %>% 
  select(ID, recognition)

sc_data <- sc_data %>% 
  left_join(g_d, by="ID") %>% 
  left_join(g_r, by="ID") %>% 
  mutate(POST_objectLocation=(drawing + recognition)/2) %>% 
  select(-drawing, -recognition)

rm(g_r, g_d, gmda_draw, gmda_recog, score_data)

# ------------------------------------------------------------------------------

# ::: CLINICAL DATA  ::: #

c_data <- clin_data %>% 
  select(ID, `MNE-Untergruppe`, `ALS-Variante`, `ALS-FRS-R`, `FRS-/Monat`, is_t1_months, id_t1_months) %>% 
  rename(MN_involvement =`MNE-Untergruppe`, ALS_variant_d=`ALS-Variante`,
         `ALSFRS-R`=`ALS-FRS-R`, `ALSFRS-R progression/month`=`FRS-/Monat`,
         months_symptoms=is_t1_months, months_diagnosis=id_t1_months) %>% 
  mutate(MN_involvement = factor(MN_involvement, labels=c("ALS", "PLS", "PMA")),
         ALS_variant_d = factor(ALS_variant_d, labels=c("bulbar", "normal", "spastic", "pereoneal")),
         ALS_variant = fct_recode(ALS_variant_d, "spinal"="normal", "spinal"="spastic", "spinal"="pereoneal")) %>% 
  select(-ALS_variant_d)


# add dummy observations for controls  
controls <- participants[!participants %in% c_data$ID]
c_data <- c_data %>% 
  add_row(ID=controls)

rm(clin_data)  

# ------------------------------------------------------------------------------

# ::: NEUROPSYCHOLOGY DATA ::: # 

n_data <- np_data %>% 
  select(!c(info_group, info_t1_star_start, info_t1_star_end, info_t1_starmaze_notes, 
            ECAS_ESS_score, ECAS_written_spoken, ECAS_start, ECAS_end, ECAS_fam_1degree, ECAS_fam_2degree,
            ECAS_first_symptoms, ECAS_first_diagnosis, ECAS_not_invasive_resp, 
            starts_with("ECAS_status_"), ECAS_height, ECAS_weight, ECAS_PEG,INFO_0, info_date,
            starts_with("ECAS_q"), ECAS_ALS_specific, ECAS_ALS_unspecific, ECAS_total_score,
            starts_with("SPART_time"), starts_with("SPART_delay"), 
            matches("SPART_q[123]"), starts_with("FIVE_P_raw"), FIVE_P_flexibility, FIVE_P_strategy, FIVE_P_rule_broken, 
            starts_with("PTSOT_q"), PTSOT_num_items, PTSOT_mean_dev_adjusted, starts_with("sbsds_q"), 
            ends_with("_precise"), dfb_q3_age_end_edu, dfb_q3_years_school, dfb_q3_years_school_clean, 
            dfb_q3_years_apprent, dfb_q3_years_apprent_clean, dfb_q3_years_uni, dfb_q3_years_uni_clean, 
            dfb_q4_highestedu, matches("dfb_q[6789]"), matches("dfb_q1[0123456789]"), matches("dfb_q2[012345678]"))) %>% 
  mutate(dfb_q1_sex=fct_recode(dfb_q1_sex, 
                               male="männlich", female="weiblich", NULL="intersexuell"),
         dfb_q5_language_german=factor(dfb_q5_language_german, 
                                       levels=c("Deutsch ist Muttersprache", "Deutsch ist nicht Muttersprache"), 
                                       labels=c("yes", "no"))) %>% 
  rename(ID=info_id, ECAS_executive=ECAS_sub_executive, ECAS_verbalFluency=ECAS_sub_verbal_fluency, ECAS_language=ECAS_sub_language, 
         ECAS_verbalMemory=ECAS_sub_memory, ECAS_visuospatial=ECAS_sub_spatial, 
         SPART_immediateMemory=SPART_mean_I, SPART_delayedMemory=SPART_q4_II, SPART_overallMemory=SPART_mean_all, 
         `5PT`=FIVE_P_productivity, PTSOT=PTSOT_mean_dev, SantaBarbaraSenseOfDirectionScale=sbsds_total_score,
         sex=dfb_q1_sex, ageYears=dfb_q2_age, educationYears=dfb_q3_years_edu_total, languageGerman=dfb_q5_language_german)   

# # add cut-off values 
# cut_off_func <- function(age, isced_school_II, norm_cat){
#   my_cut_off <- case_when(
#     
#     # new norms from Loose 2016
#     age < 60 & isced_school_II <= 12 ~ ecas_norms_2016$cut_under_60_underequal_12[ecas_norms_2016$measures==norm_cat],
#     age < 60 & isced_school_II > 12 ~ ecas_norms_2016$cut_under_60_over_12[ecas_norms_2016$measures==norm_cat],
#     age >= 60 & isced_school_II <= 12 ~ ecas_norms_2016$cut_overequal_60_underequal_12[ecas_norms_2016$measures==norm_cat],
#     age >= 60 & isced_school_II > 12 ~ ecas_norms_2016$cut_overequal_60_over_12[ecas_norms_2016$measures==norm_cat]
#     )
#   
#   return(my_cut_off)
#   
#   # example use: cut_off_func(n_data$dfb_q2_age[3], n_data$dfb_q3_years_school_clean[3], "ECAS_total")
#   # [1] 78.8
# }
# 
# n_data <- n_data %>% 
#   mutate(
#     ECAS_sub_language_cut = cut_off_func(dfb_q2_age, dfb_q3_years_school_clean, "Language"),
#     ECAS_sub_language_below_cut = ECAS_sub_language <= ECAS_sub_language_cut,
#     ECAS_sub_memory_cut = cut_off_func(dfb_q2_age, dfb_q3_years_school_clean, "Memory"),
#     ECAS_sub_memory_below_cut =ECAS_sub_memory <= ECAS_sub_memory_cut,
#     ECAS_sub_executive_cut = cut_off_func(dfb_q2_age, dfb_q3_years_school_clean, "Executive_function"),
#     ECAS_sub_executive_below_cut = ECAS_sub_executive <= ECAS_sub_executive_cut,
#     ECAS_sub_verbal_fluency_cut = cut_off_func(dfb_q2_age, dfb_q3_years_school_clean, "Fluency"),
#     ECAS_sub_verbal_fluency_below_cut = ECAS_sub_verbal_fluency <= ECAS_sub_verbal_fluency_cut,
#     ECAS_sub_spatial_cut = cut_off_func(dfb_q2_age, dfb_q3_years_school_clean, "Visuospatial"),
#     ECAS_sub_spatial_below_cut = ECAS_sub_spatial <= ECAS_sub_spatial_cut
#   ) %>% 
#   relocate(starts_with("ECAS_"), .before="SPART_mean_I")
# rm(ecas_norms_2016)

rm(np_data)

# ------------------------------------------------------------------------------

# ::: STARMAZE DATA ::: # 

data_sm <- data_sm %>% 
  select(id, groupNo, trial, trialCondition, start, timeOut, feedback, success, latency_seconds, pathError_percent, searchAccuracy_percent) %>% 
  mutate(groupNo=factor(groupNo, levels=c(1,0), labels=c("ALS", "Ctrl")),
         feedback=if_else(feedback=="true", 1, 0), 
         trialCondition=factor(trialCondition, levels=c(0,1,2,3), labels=c("base", "allo", "ego", "allo"))) %>% 
  filter(timeOut==0, feedback==0, trial!=30) %>% 
  mutate(ALSci=case_when(id %in% c(6200, 6223, 6236, 6219, 6234, 6250, 6227) ~ "ALSci", 
                         id %in% c(6301, 6344, 6351, 6314, 6324, 6325, 6343) ~ "Ctrlci", 
                         id >=6300 ~ "Ctrl", T ~ "ALS"))


# ------------------------------------------------------------------------------
# ::: COMBINE AND SAVE DATA ::: #
# ------------------------------------------------------------------------------

# combine 
data_individual <- sc_data %>% 
  full_join(n_data) %>% 
  full_join(c_data) %>% 
  rename(id=ID, groupNo=Group)
rm(n_data, sc_data)

# save as Rdata 
out_fileR <-  paste(path, "WP6_individual_data.Rdata", sep="")
save(data_individual, file=out_fileR)

# # save as excel 
# out_fileXLSX <-  paste(path, "WP6_individual_data.xlsx", sep="")
# wb <- createWorkbook()
# addWorksheet(wb, "Data_individual")
# writeData(wb, "Data_individual", data_individual)
# saveWorkbook(wb, out_fileXLSX, overwrite = TRUE)

# combine 
c_data_s <- c_data %>% 
  select(ID, MN_involvement, ALS_variant) %>% 
  rename(id=ID)
data_sm <- data_sm %>% 
  left_join(c_data_s) %>% 
  mutate(MN_involvement=fct_expand(MN_involvement, "Ctrl"))
data_sm["MN_involvement"][data_sm$groupNo=="Ctrl", ] <- "Ctrl"
rm(c_data_s)

# save as Rdata 
out_fileR <-  paste(path, "WP6_starmaze_probe_data.Rdata", sep="")
save(data_sm, file=out_fileR)

# ------------------------------------------------------------------------------

# clear workspace
rm(list = ls())
