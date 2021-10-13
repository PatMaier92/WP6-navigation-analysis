### -------------------- WP6 Data   --------------------- ###
### WP6_Analyzer                                          ###
### Author: Patrizia Maier                                ###


### get packages
library(tidyverse)
library(car)
library(kableExtra)
library(performance)


###########################################################################


## set path
path <- "WP6_data/"

# load data
ind_file <-  paste(path, "WP6_individual_data.Rdata", sep="")
load(ind_file)
rm(ind_file)

trial_file <-  paste(path, "WP6_trial_data.Rdata", sep="")
load(trial_file)
rm(trial_file)


###########################################################################


### functions
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


###########################################################################


# Demographics 
# Sex
t1 <- fisher.test(data_individual$dfb_q1_sex, data_individual$group)
t1$name <- "Sex"
t1

# Age
assumption_test(data_individual$dfb_q2_age, data_individual$group)
# homogenity of variance is given
# normality is NOT given
t2 <- wilcox.test(dfb_q2_age ~ group, exact=F, data=data_individual)
t2$name <- "Age"
t2

# Years of education 
assumption_test(data_individual$dfb_q3_years_edu_total, data_individual$group)
# homogenity of variance is given
# normality is NOT given
t3 <- wilcox.test(dfb_q3_years_edu_total ~ group, exact=F, data=data_individual)
t3$name <- "Years of education"
t3

# Computer expertise 
assumption_test(as.numeric(data_individual$dfb_q21_comp_expertise), data_individual$group)
# homogenity of variance is given
# normality is NOT given
t4 <- wilcox.test(as.numeric(dfb_q21_comp_expertise) ~ group, exact=F, data=data_individual)
t4$name <- "Self-rated computer expertise"
t4 # unfortunately significant p=0.02717

# Computer use frequency 
assumption_test(as.numeric(data_individual$dfb_q22_comp_freq), data_individual$group)
# homogenity of variance is given
# normality is NOT given
t5 <- wilcox.test(as.numeric(dfb_q22_comp_freq) ~ group, exact=F, data=data_individual)
t5$name <- "Self-rated computer use frequency"
t5 

# SBSDS
assumption_test(data_individual$sbsds_total_score, data_individual$group)
# homogenity of variance is given
# normality is NOT given 
t0 <- wilcox.test(sbsds_total_score ~ group, exact=F, data=data_individual)
t0$name <- "SBSDS"
t0


# Starmaze
t <- trial_data %>%
  filter(probe_trial==1)

# overall score
t %>% 
  group_by(group) %>%
  summarize(mean=mean(success)) 

assumption_test(t$success, t$group)
# homogenity of variance is NOT given
# normality is NOT given
wilcox.test(success ~ group, exact=F, data=t) 
chisq.test(t$group, t$success) 

# group per condition
# mean values
t %>%
  ungroup() %>% 
  group_by(group, trial_condition) %>%
  summarize(mean=mean(success))

# test 
wilcox.test(success ~ group, exact=F, data=t %>% filter(trial_condition=="training"))
wilcox.test(success ~ group, exact=F, data=t %>% filter(trial_condition=="egocentric"))
wilcox.test(success ~ group, exact=F, data=t %>% filter(trial_condition=="allocentric"))
wilcox.test(success ~ group, exact=F, data=t %>% filter(trial_condition=="mixed"))

# conditions
# mean values 
t %>%
  ungroup() %>% 
  group_by(trial_condition) %>%
  summarize(mean=mean(success))

# test 
kruskal.test(success ~ trial_condition, data=t)
pairwise.wilcox.test(t$success, t$trial_condition, p.adjust.method="bonf")

# path and time vars for successful probe trials 
# mean values 
t <- trial_data %>%
  filter(probe_trial==1, success == 1, trial_condition %in% c("egocentric", "allocentric")) 

t %>%
  group_by(group, trial_condition) %>%
  summarize(time=mean(time),
            path=mean(path_length))


assumption_test(t$time, t$group)
# homogenity of variance is NOT given
# normality is NOT given
wilcox.test(time ~ group, exact=F, data=t) 
wilcox.test(time ~ group, exact=F, data=t %>% filter(trial_condition=="egocentric")) 
wilcox.test(time ~ group, exact=F, data=t %>% filter(trial_condition=="allocentric")) 

assumption_test(t$path_length, t$group)
# homogenity of variance is NOT given
# normality is NOT given
wilcox.test(path_length ~ group, exact=F, data=t) 
wilcox.test(path_length ~ group, exact=F, data=t %>% filter(trial_condition=="egocentric"))
wilcox.test(path_length ~ group, exact=F, data=t %>% filter(trial_condition=="allocentric"))


# Non-nav. memory tests
# Overall
assumption_test(data_individual$Score_total, data_individual$group)
# homogenity of variance is given
# normality is NOT given
wilcox.test(Score_total ~ group,  exact=F, data=data_individual)

# Single
assumption_test(data_individual$Maze_reconstruction_manual_s, data_individual$group)
# homogenity of variance is given
# normality is NOT given
wilcox.test(Maze_reconstruction_manual_s ~ group, exact=F, data=data_individual)

assumption_test(data_individual$Object_identity_manual_s, data_individual$group)
# homogenity of variance is given
# normality is NOT given
wilcox.test(Object_identity_manual_s ~ group, exact=F, data=data_individual)

assumption_test(data_individual$Object_location_GMDA_SQRTCanOrg_s, data_individual$group)
# homogenity of variance is given
# normality is NOT given
wilcox.test(Object_location_GMDA_SQRTCanOrg_s ~ group, exact=F, data=data_individual)


# Motor control
# time
assumption_test(data_individual$mct_time, data_individual$group)
# homogenity of variance is given
# normality is NOT given
wilcox.test(mct_time ~ group, exact=F, data=data_individual)

# path 
assumption_test(data_individual$mct_path, data_individual$group)
# homogenity of variance is given
# normality is NOT given
wilcox.test(mct_path ~ group, exact=F, data=data_individual)

# path 
assumption_test(data_individual$mct_velocity, data_individual$group)
# homogenity of variance is given
# normality is NOT given
wilcox.test(mct_velocity ~ group, exact=F, data=data_individual)


# Neuropsychology
# ECAS total score 
assumption_test(data_individual$ECAS_total_score, data_individual$group)
# homogenity of variance is given
# normality is NOT given 
t10 <- wilcox.test(ECAS_total_score ~ group, exact=F, data=data_individual)
t10$name <- "ECAS_total"
t10

# ECAS language 
assumption_test(data_individual$ECAS_sub_language, data_individual$group)
# homogenity of variance is given
# normality is NOT given by plot 
t11 <- wilcox.test(ECAS_sub_language ~ group, exact=F, data=data_individual)
t11$name <- "ECAS_language"
t11

# ECAS memory
assumption_test(data_individual$ECAS_sub_memory, data_individual$group)
# homogenity of variance is given
# normality is NOT given 
t12 <- wilcox.test(ECAS_sub_memory ~ group, exact=F, data=data_individual)
t12$name <- "ECAS_memory"
t12

# ECAS visuospatial
assumption_test(data_individual$ECAS_sub_spatial, data_individual$group)
# homogenity of variance is given
# normality is NOT given 
t13 <- wilcox.test(ECAS_sub_spatial ~ group, exact=F, data=data_individual)
t13$name <- "ECAS_spatial"
t13

# ECAS executive
assumption_test(data_individual$ECAS_sub_executive, data_individual$group)
# homogenity of variance is given
# normality is NOT given 
t14 <- wilcox.test(ECAS_sub_executive ~ group, exact=F, data=data_individual)
t14$name <- "ECAS_executive"
t14

# ECAS fluency 
assumption_test(data_individual$ECAS_sub_verbal_fluency, data_individual$group)
# homogenity of variance is given
# normality is NOT given 
t15 <- wilcox.test(ECAS_sub_verbal_fluency ~ group, exact=F, data=data_individual)
t15$name <- "ECAS_fluency"
t15

# 5PT productivity 
assumption_test(data_individual$FIVE_P_productivity, data_individual$group)
# homogenity of variance is given
# normality is NOT given 
t16 <- wilcox.test(FIVE_P_productivity ~ group, exact=F, data=data_individual)
t16$name <- "FPT_productivity"
t16

# SPART immediate & delayed
assumption_test(data_individual$SPART_mean_all, data_individual$group)
# homogenity of variance is given
# normality is given 
t17 <- t.test(SPART_mean_all ~ group, exact=F, data=data_individual, var.equal=T)
t17$name <- "SPART_immediate"
t17

# PTSOT mean deviation
assumption_test(data_individual$PTSOT_mean_dev, data_individual$group)
# homogenity of variance is given
# normality is NOT given 
t18 <- wilcox.test(PTSOT_mean_dev ~ group, exact=F, data=data_individual)
t18$name <- "PTSOT_deviation"
t18


# Scatter/Regression
# joint data 
t <- trial_data %>%
  rename(ID=id) %>%
  filter(probe_trial==1) %>% 
  group_by(ID, group) %>%
  summarize(n=mean(success)) %>% 
  left_join(data_individual)


# demographics 
model <- glm(n ~ dfb_q1_sex + dfb_q2_age + dfb_q3_years_edu_total + as.numeric(dfb_q21_comp_expertise) + as.numeric(dfb_q22_comp_freq), data=t)
#plot(model) 
# assumption check
summary(model)
# none of the demographics, intercept is significant?
r2(model)


# tasks 
model <- glm(n ~ Object_identity_manual_s + Object_location_GMDA_SQRTCanOrg_s + Maze_reconstruction_manual_s, data=t)
#plot(model) 
# assumption check
summary(model)
# object location (?) and maze reconstruction (!), not object identity
r2(model)


# neuropsychology
model <- glm(n ~ ECAS_sub_executive + ECAS_sub_verbal_fluency + ECAS_sub_spatial +
               ECAS_sub_memory + ECAS_sub_language +
               FIVE_P_productivity + SPART_mean_all + PTSOT_mean_dev, data=t)
#plot(model) 
# assumption check
summary(model)
# none of the np-tests
r2(model)

model <- glm(n ~ SPART_mean_all + FIVE_P_productivity + PTSOT_mean_dev, data=t)
#plot(model)
# assumption check
summary(model)
# none of the np-tests
r2(model)


# create summary data
list <- list(t1, t2, t3, t4, t5, t0)
list <- list(t10, t11, t12, t13, t14, t15, t16, t17, t18)

out <- vector("list", length(list))
index=1
for (i in list)
{
  out[[index]] <- tibble(i$name, i$method, i$p.value)
  index=index + 1
}
summary_data <- map_df(out, as.data.frame) %>% 
  rename(name ="i$name", method="i$method", p_value="i$p.value") %>% 
  arrange(p_value)
summary_data

# # save
# filename <-  "WP6_data/corrected_p.html"
# table <- kbl(summary_data) %>% 
#   kable_classic() %>% 
#   save_kable(file = filename)

######################################################################

# HOW TO DO A MIXED ANOVA
# t <- trial_data %>% 
#   filter(probe_trial==1) %>% 
#   group_by(id, group, trial_condition) %>% 
#   summarize(success=mean(success))
# 
# t %>% 
#   group_by(group, trial_condition) %>% 
#   shapiro_test(success)
# # no normal distribution (!)
# 
# t %>% 
#   group_by(trial_condition) %>% 
#   levene_test(success ~ group)
# # homogenity of variances 
# 
# box_m(t[, "success", drop = FALSE], t$group)
# # homogenity of covariances 
# 
# res.aov <- anova_test(
#   data = t %>% ungroup(), dv = success, wid = id,
#   between = group, within = trial_condition
# )
# get_anova_table(res.aov)

