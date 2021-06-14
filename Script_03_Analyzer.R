### -------------------- WP6 Data   --------------------- ###
### WP6_Analyzer                                          ###
### Author: Patrizia Maier                                ###


### get packages
library(tidyverse)
library(kableExtra)
library(gtsummary)
library(car)


## input date 
date = readline(prompt = "Please enter the date string of the result file ")


###########################################################################


## set path
path <- "WP6_data/"

infileR <-  paste(path, "WP6_data_", date, ".Rdata", sep="")

load(infileR)
rm(infileR, date, path)


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
t1 <- chisq.test(data_individual$dfb_q1_sex, data_individual$Group)
t1$name <- "Sex"


# Age
assumption_test(data_individual$dfb_q2_age, data_individual$Group)
# homogenity of variance is given
# normality is NOT given

t2 <- wilcox.test(data_individual$dfb_q2_age ~ data_individual$Group)
t2$name <- "Age"


# Years of education 
assumption_test(data_individual$dfb_q3_years_edu_total, data_individual$Group)
# homogenity of variance is given
# normality is NOT given

t3 <- wilcox.test(data_individual$dfb_q3_years_edu_total ~ data_individual$Group)
t3$name <- "Years of education"


# Neuropsychology
# ECAS total score 
assumption_test(data_individual$ECAS_total_score, data_individual$Group)
# homogenity of variance is given
# normality is given 

t4 <- t.test(data_individual$ECAS_total_score ~ data_individual$Group, alternative="less")
t4$name <- "ECAS_total"


# ECAS language 
assumption_test(data_individual$ECAS_sub_language, data_individual$Group)
# homogenity of variance is given
# normality is NOT given by plot 

t5 <- wilcox.test(data_individual$ECAS_sub_language ~ data_individual$Group, alternative="less")
t5$name <- "ECAS_language"


# ECAS memory
assumption_test(data_individual$ECAS_sub_memory, data_individual$Group)
# homogenity of variance is given
# normality is NOT given 

t6 <- wilcox.test(data_individual$ECAS_sub_memory ~ data_individual$Group, alternative="less")
t6$name <- "ECAS_memory"


# ECAS visuospatial
assumption_test(data_individual$ECAS_sub_spatial, data_individual$Group)
# homogenity of variance is given
# normality is NOT given 

t7 <- wilcox.test(data_individual$ECAS_sub_spatial ~ data_individual$Group, alternative="less")
t7$name <- "ECAS_spatial"


# ECAS executive
assumption_test(data_individual$ECAS_sub_executive, data_individual$Group)
# homogenity of variance is given
# normality is NOT given 

t8 <- wilcox.test(data_individual$ECAS_sub_executive ~ data_individual$Group, alternative="less")
t8$name <- "ECAS_executive"


# ECAS fluency 
assumption_test(data_individual$ECAS_sub_verbal_fluency, data_individual$Group)
# homogenity of variance is given
# normality is NOT given 

t9 <- wilcox.test(data_individual$ECAS_sub_verbal_fluency ~ data_individual$Group, alternative="less")
t9$name <- "ECAS_fluency"


# ECAS ALS specific 
assumption_test(data_individual$ECAS_ALS_specific, data_individual$Group)
# homogenity of variance is given
# normality is NOT given 

t10 <- wilcox.test(data_individual$ECAS_ALS_specific ~ data_individual$Group, alternative="less")
t10$name <- "ECAS_ALS_specific"


# ECAS ALS Nonspecific 
assumption_test(data_individual$ECAS_ALS_unspecific, data_individual$Group)
# homogenity of variance is given
# normality is NOT given 

t11 <- wilcox.test(data_individual$ECAS_ALS_unspecific ~ data_individual$Group, alternative="less")
t11$name <- "ECAS_ALS_nonspecific"


# 5PT productivity 
assumption_test(data_individual$FIVE_P_productivity, data_individual$Group)
# homogenity of variance is given
# normality is given 

t12 <- t.test(data_individual$FIVE_P_productivity ~ data_individual$Group, alternative="less")
t12$name <- "FPT_productivity"


# 5PT flexibility
assumption_test(data_individual$FIVE_P_flexibility, data_individual$Group)
# homogenity of variance is given
# normality is NOT given 

t13 <- wilcox.test(data_individual$FIVE_P_flexibility ~ data_individual$Group, alternative="less")
t13$name <- "FPT_flexibility"


# 5PT strategy
assumption_test(data_individual$FIVE_P_strategy, data_individual$Group)
# homogenity of variance is given
# normality is NOT given 

t14 <- wilcox.test(data_individual$FIVE_P_strategy ~ data_individual$Group, alternative="less")
t14$name <- "FPT_strategy"


# SPART immediate
assumption_test(data_individual$SPART_mean_I, data_individual$Group)
# homogenity of variance is given
# normality is given 

t15 <- t.test(data_individual$SPART_mean_I ~ data_individual$Group, alternative="less")
t15$name <- "SPART_immediate"


# SPART delayed
assumption_test(data_individual$SPART_q4_II, data_individual$Group)
# homogenity of variance is given
# normality is NOT given 

t16 <- wilcox.test(data_individual$SPART_q4_II ~ data_individual$Group, alternative="less")
t16$name <- "SPART_delayed" 


# PTSOT mean deviation
assumption_test(data_individual$PTSOT_mean_dev, data_individual$Group)
# homogenity of variance is given
# normality is NOT given 

t17 <- wilcox.test(data_individual$PTSOT_mean_dev ~ data_individual$Group, alternative="less")
t17$name <- "PTSOT_deviation"


# PTSOT number items
assumption_test(data_individual$PTSOT_num_items, data_individual$Group)
# homogenity of variance is given
# normality is NOT given 

t18 <- wilcox.test(data_individual$PTSOT_num_items ~ data_individual$Group, alternative="less")
t18$name <- "PTSOT_number"


# PTSOT adjusted deviation
assumption_test(data_individual$PTSOT_mean_dev_adjusted, data_individual$Group)
# homogenity of variance is given
# normality is NOT given 

t19 <- wilcox.test(data_individual$PTSOT_mean_dev_adjusted ~ data_individual$Group, alternative="less")
t19$name <- "PTSOT_adj_number" 


# SBSDS
assumption_test(data_individual$sbsds_total_score, data_individual$Group)
# homogenity of variance is given
# normality is NOT given 

t20 <- wilcox.test(data_individual$sbsds_total_score ~ data_individual$Group)
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

