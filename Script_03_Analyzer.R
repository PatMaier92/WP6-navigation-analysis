### -------------------- WP6 Data   --------------------- ###
### WP6_Analyzer                                          ###
### Author: Patrizia Maier                                ###


### get packages
source("R_rainclouds.R")
library(tidyverse)
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

# ECAS total score 
assumption_test(data_individual$ECAS_total_score, data_individual$Group)
# homogenity of variance is given
# normality is given 

summary(aov(data_individual$ECAS_total_score ~ data_individual$Group))
# p=0.0515


# ECAS language 
assumption_test(data_individual$ECAS_sub_language, data_individual$Group)
# homogenity of variance is given
# normality is NOT given by plot 

kruskal.test(data_individual$ECAS_sub_language ~ data_individual$Group)
# ns


# ECAS memory
assumption_test(data_individual$ECAS_sub_memory, data_individual$Group)
# homogenity of variance is given
# normality is NOT given 

kruskal.test(data_individual$ECAS_sub_memory ~ data_individual$Group)
# p=0.04


# ECAS visuospatial
assumption_test(data_individual$ECAS_sub_spatial, data_individual$Group)
# homogenity of variance is given
# normality is NOT given 

kruskal.test(data_individual$ECAS_sub_spatial ~ data_individual$Group)
# ns


# ECAS executive
assumption_test(data_individual$ECAS_sub_executive, data_individual$Group)
# homogenity of variance is given
# normality is NOT given 

kruskal.test(data_individual$ECAS_sub_executive ~ data_individual$Group)
# ns


# ECAS fluency 
assumption_test(data_individual$ECAS_sub_verbal_fluency, data_individual$Group)
# homogenity of variance is given
# normality is NOT given 

kruskal.test(data_individual$ECAS_sub_verbal_fluency ~ data_individual$Group)
# ns


# 5PT productivity 
assumption_test(data_individual$FIVE_P_productivity, data_individual$Group)
# homogenity of variance is given
# normality is given 

summary(aov(data_individual$FIVE_P_productivity ~ data_individual$Group))
# p=0.0408


# 5PT flexibility
assumption_test(data_individual$FIVE_P_flexibility, data_individual$Group)
# homogenity of variance is given
# normality is NOT given 

kruskal.test(data_individual$FIVE_P_flexibility ~ data_individual$Group)
# ns


# 5PT strategy
assumption_test(data_individual$FIVE_P_strategy, data_individual$Group)
# homogenity of variance is given
# normality is NOT given 

kruskal.test(data_individual$FIVE_P_strategy ~ data_individual$Group)
# ns


# SPART immediate
assumption_test(data_individual$SPART_mean_I, data_individual$Group)
# homogenity of variance is given
# normality is given 

summary(aov(data_individual$SPART_mean_I ~ data_individual$Group))
# ns 


# SPART delayed
assumption_test(data_individual$SPART_q4_II, data_individual$Group)
# homogenity of variance is given
# normality is NOT given 

kruskal.test(data_individual$SPART_q4_II ~ data_individual$Group)
# ns 


# PTSOT mean deviation
assumption_test(data_individual$PTSOT_mean_dev, data_individual$Group)
# homogenity of variance is given
# normality is NOT given 

kruskal.test(data_individual$PTSOT_mean_dev ~ data_individual$Group)
# ns 


# PTSOT number items
assumption_test(data_individual$PTSOT_num_items, data_individual$Group)
# homogenity of variance is given
# normality is NOT given 

kruskal.test(data_individual$PTSOT_num_items ~ data_individual$Group)
# ns 


# SBSDS
assumption_test(data_individual$sbsds_total_score, data_individual$Group)
# homogenity of variance is given
# normality is NOT given 

kruskal.test(data_individual$sbsds_total_score ~ data_individual$Group)
# ns 