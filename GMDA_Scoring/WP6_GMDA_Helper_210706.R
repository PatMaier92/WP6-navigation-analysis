###         GMDA Helper Script          ###
# Author: Patrizia Maier                  #


# get packages 
library(tidyverse)
library(ggtext)
library(openxlsx)


######################################################################
# Read-in GMDA Scoring result files 


# define path to data 
my_path="GMDA/Data/"
#my_path="_processed_data/211123_Drawing/Data/"
#my_path="_processed_data/211123_Recognition/Data/"
pattern <- list.files(path=my_path, pattern = "_Summary\\.csv$")
file_list <- paste(my_path, pattern, sep="")


# gmda data 
gmda_data <- file_list %>%
  purrr::map_df(read_csv,
                col_names=c("Measure Type", "Filename", "Measure", "Score", "Score_2"), 
                col_type=cols(`Measure Type` = col_character(),
                              Filename = col_character(),
                              Measure = col_character(),
                              Score = col_double(),
                              Score_2 = col_double()),
                skip=9, 
                n_max=8) %>% 
  # correct for delimiter error in raw data 
  unite(Score, Score_2, col="Score", sep=".", na.rm=T) %>% 
  # add id and type info from Filename
  separate(Filename, sep="_", into=c("ID", "Type"))
  

# bdr data 
brd_data <- file_list %>%
  purrr::map_df(read_csv, 
                col_names=c("Measure Type", "Filename", "Measure", "Score"), 
                col_type=cols(`Measure Type` = col_character(),
                              Filename = col_character(),
                              Measure = col_character(),
                              Score = col_double()),
                skip=21, 
                n_max=10) %>% 
  # add id and type info from Filename
  separate(Filename, sep="_", into=c("ID", "Type"))


# # individual landmark gmda data
# gmda_ind_data <- file_list %>%
#   purrr::map_df(read_csv,
#                 col_names=c("Measure Type", "Filename", "Landmark", "Measure", "Score"),
#                 cols(`Measure Type` = col_character(),
#                      Filename = col_character(),
#                      Landmark = col_character(),
#                      Measure = col_character(),
#                      Score = col_double()),
#                 skip=35,
#                 n_max=35) %>% 
#   # add id and type info from Filename
#   separate(Filename, sep="_", into=c("ID", "Type"))


#######################################################################
# Preparation

is_outlier <- function(x) {
  return(x < quantile(x, 0.25) - 1.5 * IQR(x) | x > quantile(x, 0.75) + 1.5 * IQR(x))
}


level_order <- c("SQRT(CanOrg)",
                 "CanOrg",
                 "CanAcc",
                 "DistAcc",
                 "AngleAcc",
                 "r",
                 "theta") 


# get bad theta values --> need to redo Scoring after GIMP rotation
bad_theta <- brd_data %>% filter(Measure=="theta") %>% filter(abs(Score) > 30) 


# Data overview/compare groups
temp_brd <- brd_data %>% 
  select(!`Measure Type`) %>%
  filter(Measure %in% c("r", "theta"))


temp <- gmda_data %>% 
  select(!`Measure Type`) %>% 
  filter(!Measure %in% c("Rotational Bias", "Scaling Bias", "Num Landmarks Missing", "Canonical Organization")) %>% 
  mutate(Score=as.numeric(Score),
         Measure=case_when(Measure=="SQRT(Canonical Organization)" ~ "SQRT(CanOrg)",
                           Measure=="Canonical Accuracy" ~ "CanAcc",
                           Measure=="Distance Accuracy" ~ "DistAcc",
                           Measure=="Angle Accuracy" ~ "AngleAcc",
                           TRUE ~ "NA")) %>% 
  full_join(temp_brd, by=c("ID", "Type", "Measure", "Score")) %>% 
  pivot_wider(id_cols=c(ID, Measure),
              names_from=Type,
              values_from=Score) %>% 
  mutate(group=factor(case_when(ID>6300 ~ "Control",
                         TRUE ~ "MNE"), levels=c("MNE", "Control"))) %>% 
  group_by(group) %>% 
  arrange(ID) %>% 
  filter(Measure!="theta")


date <- "211123"
ggplot(temp, aes(x=factor(Measure, level=level_order), y=drawing)) +
# ggplot(temp, aes(x=factor(Measure, level=level_order), y=recognition)) +
  geom_boxplot(outlier.shape = NA) + 
  geom_point() + 
  #geom_text(aes(label=ID), size=3, na.rm = TRUE, hjust = -0.5) + 
  facet_wrap(~ group, nrow=1) + 
  ylim(0,1) + 
  theme_classic() +
  labs(title="GMDA scores",
       subtitle=paste("using original images, rotated if theta > 30, scoring template ", date, sep=""),
       y="Score",
       x="GMDA Measures")


# save output
date <- "211123"
mode <- "recognition"
mode <- "drawing"

# as Rdata 
out_file_R <-  paste("WP6_GMDA_data_", mode, "_", date, ".Rdata", sep="")
save(temp, file=out_file_R)

# as excel 
out_file_XLSX <-  paste("WP6_GMDA_data_", mode, "_", date, ".xlsx", sep="")

wb <- createWorkbook()
addWorksheet(wb, "GMDA")
writeData(wb, "GMDA", temp)

saveWorkbook(wb, out_file_XLSX, overwrite = TRUE)
rm(wb)


#######################################################################
# # Compare GMDA/BDR parameters of different scoring methods 
# temp_brd <- brd_data %>% 
#   select(!`Measure Type`) %>%
#   filter(Measure %in% c("r", "theta"))
# 
# 
# temp <- gmda_data %>%
#   select(!`Measure Type`) %>%
#   filter(!Measure %in% c("Rotational Bias", "Scaling Bias", "Num Landmarks Missing")) %>%
#   mutate(Score=as.numeric(Score),
#          Measure=case_when(Measure=="SQRT(Canonical Organization)" ~ "SQRT(CanOrg)",
#                            Measure=="Canonical Organization" ~ "CanOrg",
#                            Measure=="Canonical Accuracy" ~ "CanAcc",
#                            Measure=="Distance Accuracy" ~ "DistAcc",
#                            Measure=="Angle Accuracy" ~ "AngleAcc",
#                            TRUE ~ "r")) %>%
#   full_join(old_data2, by=c("ID", "Type", "Measure", "Score")) %>%
#   pivot_wider(id_cols=c(ID, Measure),
#               names_from=Type,
#               values_from=Score) %>%
#   mutate(recognition_rotated= as.numeric(recognition_rotated),
#          recognition = as.numeric(recognition),
#          diff= recognition_rotated - recognition,
#          outlier = ifelse(is_outlier(diff), ID, as.numeric(NA)))
# 
# 
# ggplot(temp, aes(x=factor(Measure, level=level_order), y=diff)) +
#   geom_boxplot(outlier.shape = NA) +
#   geom_point() +
#   geom_text(aes(label=outlier), size=3, na.rm = TRUE, hjust = -0.5) +
#   ylim(-0.5, 0.5) +
#   geom_hline(yintercept = 0.1) + 
#   geom_hline(yintercept = -0.1) + 
#   theme_classic() +
#   labs(title="Comparing GMDA scores for GIMP-preprocessed and original images",
#        y="Difference (gimp-original) Score",
#        x="GMDA Measures")
#

#######################################################################
# # Visualize GMDA template coordinates

# Coordinates from template
# Berg_ad,1,-249,87
# Wald_hj,2,-5,275
# Berg_da,3,231,84
# Turm_l,4,139,-196
# Wald_hj,5,-165,-199
# Schatz,6,-138,181
# Start,7,-7,-216

# x=c(-249,-5,231,139,-165,-138,-7)
# y=c(87,275,84,-196,-199,181,-216)
# names=c("berg_ad", "wald_hj", "berg_da", "turm_l", "wald_hj", "schatz", "start")
# 
# data=tibble(x,y,names)
# 
# ggplot(data, aes(x,y)) +
#   geom_point() + 
#   geom_text(label=names, nudge_x=40)
