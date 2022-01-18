### -------------------- WP6 Data   --------------------- ###
### WP6_Analyzer                                          ###
### Author: Patrizia Maier                                ###


# ::: get packages ::: #

library(tidyverse)
library(car)
# library(kableExtra)
# library(performance)


# ######################################################### #


# ::: get data ::: # 

path <- "WP6_data/"

ind_file <-  paste(path, "WP6_individual_data.Rdata", sep="")
load(ind_file)
rm(ind_file)

# trial_file <-  paste(path, "WP6_trial_data.Rdata", sep="")
# load(trial_file)
# rm(trial_file)


# ######################################################### #


# ::: functions ::: # 
assumption_test <- function(DV, IV){
  fit <- aov(DV ~ IV)
  
  par(mfrow=c(1,2))
  plot(fit, c(1,2))
  
  # homogeneity of variance
  # check plot 1
  print(leveneTest(DV ~ IV))
  
  # normality
  # check plot 2
  aov_residuals <- residuals(object = fit)
  print(shapiro.test(x = aov_residuals))
}


# ######################################################### #


# ::: DEMOGRAPHICS ::: # 

# Sex
fisher.test(data_individual$dfb_q1_sex, data_individual$group)

# German native
fisher.test(data_individual$dfb_q5_language_german, data_individual$group)

# Age
assumption_test(data_individual$dfb_q2_age, data_individual$group)
# homogenity of variance is given
# normality is NOT given
wilcox.test(dfb_q2_age ~ group, exact=F, data=data_individual)

# Years of education 
assumption_test(data_individual$dfb_q3_years_edu_total, data_individual$group)
# homogenity of variance is given
# normality is NOT given
wilcox.test(dfb_q3_years_edu_total ~ group, exact=F, data=data_individual)

# SBSDS
assumption_test(data_individual$sbsds_total_score, data_individual$group)
# homogenity of variance is given
# normality is NOT given 
wilcox.test(sbsds_total_score ~ group, exact=F, data=data_individual)


# ######################################################### #


# ::: NON-NAVIGATIONAL MEMORY SCORES ::: #

# descriptives
data_individual %>% rename(identity=Object_identity_manual_s, 
                           location=Object_location_GMDA_SQRTCanOrg_s,
                           maze=Maze_reconstruction_manual_s) %>% 
  group_by(group) %>% 
  summarise(across(.cols = c(identity, location, maze), list(mean=mean, sd=sd)))


# object identity
assumption_test(data_individual$Object_identity_manual_s, data_individual$group)
# homogenity of variance is given
# normality is NOT given 
wilcox.test(Object_identity_manual_s ~ group, exact=F, data=data_individual)
kruskal.test(Object_identity_manual_s ~ group, data=data_individual)


# object location
assumption_test(data_individual$Object_location_GMDA_SQRTCanOrg_s, data_individual$group)
# homogenity of variance is given
# normality is NOT given 
wilcox.test(Object_location_GMDA_SQRTCanOrg_s ~ group, exact=F, data=data_individual)
kruskal.test(Object_location_GMDA_SQRTCanOrg_s ~ group, data=data_individual)


# maze reconstruction
assumption_test(data_individual$Maze_reconstruction_manual_s, data_individual$group)
# homogenity of variance is given
# normality is NOT given 
wilcox.test(Maze_reconstruction_manual_s ~ group, exact=F, data=data_individual)
kruskal.test(Maze_reconstruction_manual_s ~ group, data=data_individual)


# ######################################################### #


# ::: NEUROPSYCHOLOGICAL ASSESSMENT ::: # 

# descriptives
t(data_individual %>% group_by(group) %>% 
  summarise(across(.cols = c(ECAS_sub_memory, ECAS_sub_spatial, ECAS_sub_language,
                             ECAS_sub_verbal_fluency, ECAS_sub_executive, 
                             SPART_mean_all, FIVE_P_productivity, PTSOT_mean_dev), 
                   list(mean=mean, sd=sd), na.rm=T)))


# ECAS memory
assumption_test(data_individual$ECAS_sub_memory, data_individual$group)
# homogenity of variance is given
# normality is NOT given 
wilcox.test(ECAS_sub_memory ~ group, exact=F, data=data_individual)
kruskal.test(ECAS_sub_memory ~ group, data=data_individual)

# check the subscales for immediate, delayed recall and recognition
t(data_individual %>% group_by(group) %>% 
    summarise(across(.cols = c(ECAS_sub_memory, ECAS_q3, ECAS_q15_delay_score, ECAS_q16), 
                     list(mean=mean, sd=sd), na.rm=T)))
wilcox.test(ECAS_q3 ~ group, exact=F, data=data_individual) # immediate recall
wilcox.test(ECAS_q15_delay_score ~ group, exact=F, data=data_individual) # delayed recall
wilcox.test(ECAS_q16 ~ group, exact=F, data=data_individual) # recognition


# ECAS visuospatial
assumption_test(data_individual$ECAS_sub_spatial, data_individual$group)
# homogenity of variance is given
# normality is NOT given 
wilcox.test(ECAS_sub_spatial ~ group, exact=F, data=data_individual)
kruskal.test(ECAS_sub_spatial ~ group, data=data_individual)


# ECAS language 
assumption_test(data_individual$ECAS_sub_language, data_individual$group)
# homogenity of variance is given
# normality is NOT given by plot 
wilcox.test(ECAS_sub_language ~ group, exact=F, data=data_individual)
kruskal.test(ECAS_sub_language ~ group, data=data_individual)


# ECAS fluency 
assumption_test(data_individual$ECAS_sub_verbal_fluency, data_individual$group)
# homogenity of variance is given
# normality is NOT given 
wilcox.test(ECAS_sub_verbal_fluency ~ group, exact=F, data=data_individual)
kruskal.test(ECAS_sub_verbal_fluency ~ group, data=data_individual)


# ECAS executive
assumption_test(data_individual$ECAS_sub_executive, data_individual$group)
# homogenity of variance is given
# normality is NOT given 
wilcox.test(ECAS_sub_executive ~ group, exact=F, data=data_individual)
kruskal.test(ECAS_sub_executive ~ group, data=data_individual)


# 5PT productivity 
assumption_test(data_individual$FIVE_P_productivity, data_individual$group)
# homogenity of variance is given
# normality is NOT given 
wilcox.test(FIVE_P_productivity ~ group, exact=F, data=data_individual)
kruskal.test(FIVE_P_productivity ~ group, data=data_individual)


# SPART immediate & delayed
assumption_test(data_individual$SPART_mean_all, data_individual$group)
# homogenity of variance is given
# normality is given 
t.test(SPART_mean_all ~ group, exact=F, data=data_individual, var.equal=T)
wilcox.test(SPART_mean_all ~ group, exact=F, data=data_individual)
kruskal.test(SPART_mean_all ~ group, data=data_individual)


# PTSOT mean deviation
assumption_test(data_individual$PTSOT_mean_dev, data_individual$group)
# homogenity of variance is given
# normality is NOT given 
wilcox.test(PTSOT_mean_dev ~ group, exact=F, data=data_individual)
kruskal.test(PTSOT_mean_dev ~ group, data=data_individual)


# ######################################################### #


# # create summary data
# list <- list(t1, t2, t3, t4, t5, t0)
# list <- list(t10, t11, t12, t13, t14, t15, t16, t17, t18)
# 
# out <- vector("list", length(list))
# index=1
# for (i in list)
# {
#   out[[index]] <- tibble(i$name, i$method, i$p.value)
#   index=index + 1
# }
# summary_data <- map_df(out, as.data.frame) %>% 
#   rename(name ="i$name", method="i$method", p_value="i$p.value") %>% 
#   arrange(p_value)
# summary_data


# ######################################################### #


# # ::: scatter & regression ::: #
# 
# # joint data 
# t <- trial_data %>%
#   rename(ID=id) %>%
#   filter(probe_trial==1) %>% 
#   group_by(ID, group) %>%
#   summarize(n=mean(success)) %>% 
#   left_join(data_individual)
# 
# 
# # demographics 
# model <- glm(n ~ dfb_q1_sex + dfb_q2_age + dfb_q3_years_edu_total + as.numeric(dfb_q21_comp_expertise) + as.numeric(dfb_q22_comp_freq), data=t)
# #plot(model) 
# # assumption check
# summary(model)
# # none of the demographics, intercept is significant?
# r2(model)
# 
# 
# # tasks 
# model <- glm(n ~ Object_identity_manual_s + Object_location_GMDA_SQRTCanOrg_s + Maze_reconstruction_manual_s, data=t)
# #plot(model) 
# # assumption check
# summary(model)
# # object location (?) and maze reconstruction (!), not object identity
# r2(model)
# 
# 
# # neuropsychology
# model <- glm(n ~ ECAS_sub_executive + ECAS_sub_verbal_fluency + ECAS_sub_spatial +
#                ECAS_sub_memory + ECAS_sub_language +
#                FIVE_P_productivity + SPART_mean_all + PTSOT_mean_dev, data=t)
# #plot(model) 
# # assumption check
# summary(model)
# # none of the np-tests
# r2(model)
# 
# model <- glm(n ~ SPART_mean_all + FIVE_P_productivity + PTSOT_mean_dev, data=t)
# #plot(model)
# # assumption check
# summary(model)
# # none of the np-tests
# r2(model)


# ######################################################### #

# clear workspace
rm(list = ls())

# ######################################################### #