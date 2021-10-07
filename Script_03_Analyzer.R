### -------------------- WP6 Data   --------------------- ###
### WP6_Analyzer                                          ###
### Author: Patrizia Maier                                ###


### get packages
library(tidyverse)
library(car)


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


## analyze

# Demographics 
# Sex
t1 <- chisq.test(fb_q1_sex, group, data=data_individual)
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



# Starmaze
# overall score
t <- trial_data %>% 
  filter(probe_trial==1) %>% 
  group_by(id, group) %>% 
  summarize(mean=mean(success))

assumption_test(t$mean, t$group)
# homogenity of variance is given
# normality is NOT given
wilcox.test(mean ~ group, exact=F, data=t)

# per condition 
t <- trial_data %>% 
  filter(probe_trial==1) %>% 
  group_by(id, group, trial_condition) %>% 
  summarize(mean=mean(success))

wilcox.test(mean ~ group, exact=F, data=t %>% filter(trial_condition=="training"))
wilcox.test(mean ~ group, exact=F, data=t %>% filter(trial_condition=="egocentric"))
wilcox.test(mean ~ group, exact=F, data=t %>% filter(trial_condition=="allocentric"))
wilcox.test(mean ~ group, exact=F, data=t %>% filter(trial_condition=="mixed"))

ti <- t %>% 
  rename(ID=id) %>% 
  left_join(data_individual)
model <- lm(mean ~ dfb_q1_sex + dfb_q2_age + dfb_q3_years_edu_total, data=ti)
summary(model)

# # Residuen auf Normalverteilung überprüfen
# shapiro.test(residuals(model))
# 
# # Autokorrelation überprüfen (Durbin-Watson-Test)
# car::durbinWatsonTest(model)
# 
# # Multikollinearität mit VIF (variance influence factor) überprüfen
# car::vif(model)



# Neuropsychology
# ECAS total score 
assumption_test(data_individual$ECAS_total_score, data_individual$group)
# homogenity of variance is given
# normality is given 

t4 <- t.test(data_individual$ECAS_total_score ~ data_individual$group, alternative="less")
t4$name <- "ECAS_total"


# ECAS language 
assumption_test(data_individual$ECAS_sub_language, data_individual$group)
# homogenity of variance is given
# normality is NOT given by plot 

t5 <- wilcox.test(data_individual$ECAS_sub_language ~ data_individual$group, alternative="less")
t5$name <- "ECAS_language"


# ECAS memory
assumption_test(data_individual$ECAS_sub_memory, data_individual$group)
# homogenity of variance is given
# normality is NOT given 

t6 <- wilcox.test(data_individual$ECAS_sub_memory ~ data_individual$group, alternative="less")
t6$name <- "ECAS_memory"


# ECAS visuospatial
assumption_test(data_individual$ECAS_sub_spatial, data_individual$group)
# homogenity of variance is given
# normality is NOT given 

t7 <- wilcox.test(data_individual$ECAS_sub_spatial ~ data_individual$group, alternative="less")
t7$name <- "ECAS_spatial"


# ECAS executive
assumption_test(data_individual$ECAS_sub_executive, data_individual$group)
# homogenity of variance is given
# normality is NOT given 

t8 <- wilcox.test(data_individual$ECAS_sub_executive ~ data_individual$group, alternative="less")
t8$name <- "ECAS_executive"


# ECAS fluency 
assumption_test(data_individual$ECAS_sub_verbal_fluency, data_individual$group)
# homogenity of variance is given
# normality is NOT given 

t9 <- wilcox.test(data_individual$ECAS_sub_verbal_fluency ~ data_individual$group, alternative="less")
t9$name <- "ECAS_fluency"


# ECAS ALS specific 
assumption_test(data_individual$ECAS_ALS_specific, data_individual$group)
# homogenity of variance is given
# normality is NOT given 

t10 <- wilcox.test(data_individual$ECAS_ALS_specific ~ data_individual$group, alternative="less")
t10$name <- "ECAS_ALS_specific"


# ECAS ALS Nonspecific 
assumption_test(data_individual$ECAS_ALS_unspecific, data_individual$group)
# homogenity of variance is given
# normality is NOT given 

t11 <- wilcox.test(data_individual$ECAS_ALS_unspecific ~ data_individual$group, alternative="less")
t11$name <- "ECAS_ALS_nonspecific"


# 5PT productivity 
assumption_test(data_individual$FIVE_P_productivity, data_individual$group)
# homogenity of variance is given
# normality is given 

t12 <- t.test(data_individual$FIVE_P_productivity ~ data_individual$group, alternative="less")
t12$name <- "FPT_productivity"


# 5PT flexibility
assumption_test(data_individual$FIVE_P_flexibility, data_individual$group)
# homogenity of variance is given
# normality is NOT given 

t13 <- wilcox.test(data_individual$FIVE_P_flexibility ~ data_individual$group, alternative="less")
t13$name <- "FPT_flexibility"


# 5PT strategy
assumption_test(data_individual$FIVE_P_strategy, data_individual$group)
# homogenity of variance is given
# normality is NOT given 

t14 <- wilcox.test(data_individual$FIVE_P_strategy ~ data_individual$group, alternative="less")
t14$name <- "FPT_strategy"


# SPART immediate
assumption_test(data_individual$SPART_mean_I, data_individual$group)
# homogenity of variance is given
# normality is given 

t15 <- t.test(data_individual$SPART_mean_I ~ data_individual$group, alternative="less")
t15$name <- "SPART_immediate"


# SPART delayed
assumption_test(data_individual$SPART_q4_II, data_individual$group)
# homogenity of variance is given
# normality is NOT given 

t16 <- wilcox.test(data_individual$SPART_q4_II ~ data_individual$group, alternative="less")
t16$name <- "SPART_delayed" 


# PTSOT mean deviation
assumption_test(data_individual$PTSOT_mean_dev, data_individual$group)
# homogenity of variance is given
# normality is NOT given 

t17 <- wilcox.test(data_individual$PTSOT_mean_dev ~ data_individual$group, alternative="less")
t17$name <- "PTSOT_deviation"


# PTSOT number items
assumption_test(data_individual$PTSOT_num_items, data_individual$group)
# homogenity of variance is given
# normality is NOT given 

t18 <- wilcox.test(data_individual$PTSOT_num_items ~ data_individual$group, alternative="less")
t18$name <- "PTSOT_number"


# PTSOT adjusted deviation
assumption_test(data_individual$PTSOT_mean_dev_adjusted, data_individual$group)
# homogenity of variance is given
# normality is NOT given 

t19 <- wilcox.test(data_individual$PTSOT_mean_dev_adjusted ~ data_individual$group, alternative="less")
t19$name <- "PTSOT_adj_number" 


# SBSDS
assumption_test(data_individual$sbsds_total_score, data_individual$group)
# homogenity of variance is given
# normality is NOT given 

t20 <- wilcox.test(data_individual$sbsds_total_score ~ data_individual$group)
t20$name <- "SBSDS"



# multiple comparisons 
# create summary data
list <- list(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, 
             t11, t12, t13, t14, t15, t16, t17, t18, t19, t20)
list <- list(t4, t5, t6, t7, t8, t9, t10, 
             t11, t12, t13, t14, t15, t16, t17, t18, t19)
list <- list(t1, t2, t3, t20)

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


# add adjusted p values 
summary_data$Bonferroni_FWER =
  p.adjust(summary_data$p_value,
           method = "bonferroni")

summary_data$Bonf_Holm_FWER =
  p.adjust(summary_data$p_value,
           method = "holm")

summary_data$Benj_Hochberg_FDR =
  p.adjust(summary_data$p_value,
           method = "BH")


# save 
filename <-  "WP6_data/corrected_p_value.html"
table <- kbl(summary_data) %>% 
  kable_classic() %>% 
  save_kable(file = filename)

