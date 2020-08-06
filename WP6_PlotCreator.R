### Plots for ALS pilot data 
# only one timepoint of testing (T1)

# ## install packages
install.packages("ggplot2")
install.packages("readxl")
install.packages("dplyr")
install.packages("plyr")
install.packages("magrittr")
install.packages("reshape2")
install.packages("openxlsx")

## get packages
library(ggplot2)
library(readxl)
library(plyr)
library(dplyr)
library(magrittr)
library(reshape2)
library(openxlsx)

## set directory and filename
setwd('S:/C15/WNE/FO/SFB1315-B05/Data/WP6 - ALS/Analysed_Data/Results_WP_6/Result Plots/')
in_filename <- "WP6_table_overview_results_selected_variables.xlsx"

## get data from excel sheets
data <- read_xlsx(in_filename, sheet = "Data", col_names=T)
# read_xls für .xls files

# str(data)

# convert to factor
data$Group <- factor(data$Group, levels=c("ALS", "Control"))
data$Subgroup <- factor(data$Subgroup, levels=c("ALS_1", "ALS_2", "none"))
data$Sex <- factor(data$Sex, levels=c("female", "male"))

##############################################################################
## descriptives and plots for time

string <- "^time_.*_mean$"
v <- names(data)[grepl(string, names(data))]
subset <- cbind(data$Participant, data$Group, data$Sex, data[,v])
names(subset) <- c("id", "group", "sex","time_pm_nF_mean", "time_t_nF_mean", "time_t_wF_mean",
                        "time_pe_all_mean", "time_pe_nF_mean", "time_pe_wF_mean", 
                        "time_pa_all_mean", "time_pa_nF_mean", "time_pa_wF_mean")

subset_long <- melt(subset, id.vars=c("id", "group", "sex"))

subset_long$type <- NA

int_vars <- grep("_t_", subset_long$variable)
subset_long[int_vars, "type"] <- "training"

int_vars <- grep("_pm_", subset_long$variable)
subset_long[int_vars, "type"] <- "mixed"

int_vars <- grep("_pe_", subset_long$variable)
subset_long[int_vars, "type"] <- "ego"

int_vars <- grep("_pa_", subset_long$variable)
subset_long[int_vars, "type"] <- "allo"

subset_long$type <- factor(subset_long$type)


subset_long$feedback <- NA

int_vars <- grep("_all", subset_long$variable)
subset_long[int_vars, "feedback"] <- "all (wF + nF)"

int_vars <- grep("_wF", subset_long$variable)
subset_long[int_vars, "feedback"] <- "wF"

int_vars <- grep("_nF", subset_long$variable)
subset_long[int_vars, "feedback"] <- "nF"

subset_long$feedback <- factor(subset_long$feedback)


subset_gd <- subset_long %>%
  group_by(group, type, feedback) %>%
  summarise(
    value = mean(value)
  )

subset_long$feedback = factor(subset_long$feedback, levels=c("all (wF + nF)","wF","nF"))
subset_long$type = factor(subset_long$type, levels=c("training","mixed","ego", "allo"))

ggplot(subset_long, aes(y=value, x=group, fill=group)) + 
  geom_bar(data=subset_gd, stat="identity", color="black") + # bar plots  
  geom_point(alpha=1) + # individual points
  facet_wrap(~ type + feedback) + # different facets for plots 
  labs(x="\nGroups", y="Mean time in seconds\n") + # labels 
  ggtitle("Differences in time between ALS patients and controls") + # title
  theme(legend.position="none") # font size and co 
ggsave("WP6_Time_mean.png")

rm(subset, subset_gd, subset_long, v, string)


#######################################################################################################
## descriptives and plots for path length

string <- "^path__.*_mean$"
v <- names(data)[grepl(string, names(data))]
subset <- cbind(data$Participant, data$Group, data$Sex, data[,v])
names(subset) <- c("id", "group", "sex","path_len_pm_nF_mean", "path_len_t_nF_mean", "path_len_t_wF_mean",
                   "path_len_pe_all_mean", "path_len_pe_nF_mean", "path_len_pe_wF_mean", 
                   "path_len_pa_all_mean", "path_len_pa_nF_mean", "path_len_pa_wF_mean")

subset_long <- melt(subset, id.vars=c("id", "group", "sex"))

subset_long$type <- NA

int_vars <- grep("_t_", subset_long$variable)
subset_long[int_vars, "type"] <- "training"

int_vars <- grep("_pm_", subset_long$variable)
subset_long[int_vars, "type"] <- "mixed"

int_vars <- grep("_pe_", subset_long$variable)
subset_long[int_vars, "type"] <- "ego"

int_vars <- grep("_pa_", subset_long$variable)
subset_long[int_vars, "type"] <- "allo"

subset_long$type <- factor(subset_long$type)


subset_long$feedback <- NA

int_vars <- grep("_all", subset_long$variable)
subset_long[int_vars, "feedback"] <- "all (wF + nF)"

int_vars <- grep("_wF", subset_long$variable)
subset_long[int_vars, "feedback"] <- "wF"

int_vars <- grep("_nF", subset_long$variable)
subset_long[int_vars, "feedback"] <- "nF"

subset_long$feedback <- factor(subset_long$feedback)


subset_gd <- subset_long %>%
  group_by(group, type, feedback) %>%
  summarise(
    value = mean(value)
  )

subset_long$feedback = factor(subset_long$feedback, levels=c("all (wF + nF)","wF","nF"))
subset_long$type = factor(subset_long$type, levels=c("training","mixed","ego", "allo"))

ggplot(subset_long, aes(y=value, x=group, fill=group)) + 
  geom_bar(data=subset_gd, stat="identity", color="black") + # bar plots  
  geom_point(alpha=1) + # individual points
  facet_wrap(~ type + feedback) + # different facets for plots 
  labs(x="\nGroups", y="Mean path length\n") + # labels 
  ggtitle("Differences in mean path length between ALS patients and controls") + # title
  theme(legend.position="none") # font size and co 
ggsave("WP6_Path_length_mean.png")

rm(subset, subset_gd, subset_long, v, string)


#######################################################################################################
## descriptives and plots for path accuracy

string <- "^path_accuracy_.*_mean$"
v <- names(data)[grepl(string, names(data))]
subset <- cbind(data$Participant, data$Group, data$Sex, data[,v])
names(subset) <- c("id", "group", "sex","path_acc_pm_nF_mean", "path_acc_t_nF_mean", "path_acc_t_wF_mean",
                   "path_acc_pe_all_mean", "path_acc_pe_nF_mean", "path_acc_pe_wF_mean", 
                   "path_acc_pa_all_mean", "path_acc_pa_nF_mean", "path_acc_pa_wF_mean")

subset_long <- melt(subset, id.vars=c("id", "group", "sex"))

subset_long$type <- NA

int_vars <- grep("_t_", subset_long$variable)
subset_long[int_vars, "type"] <- "training"

int_vars <- grep("_pm_", subset_long$variable)
subset_long[int_vars, "type"] <- "mixed"

int_vars <- grep("_pe_", subset_long$variable)
subset_long[int_vars, "type"] <- "ego"

int_vars <- grep("_pa_", subset_long$variable)
subset_long[int_vars, "type"] <- "allo"

subset_long$type <- factor(subset_long$type)


subset_long$feedback <- NA

int_vars <- grep("_all", subset_long$variable)
subset_long[int_vars, "feedback"] <- "all (wF + nF)"

int_vars <- grep("_wF", subset_long$variable)
subset_long[int_vars, "feedback"] <- "wF"

int_vars <- grep("_nF", subset_long$variable)
subset_long[int_vars, "feedback"] <- "nF"

subset_long$feedback <- factor(subset_long$feedback)


subset_gd <- subset_long %>%
  group_by(group, type, feedback) %>%
  summarise(
    value = mean(value)
  )

subset_long$feedback = factor(subset_long$feedback, levels=c("all (wF + nF)","wF","nF"))
subset_long$type = factor(subset_long$type, levels=c("training","mixed","ego", "allo"))

ggplot(subset_long, aes(y=value, x=group, fill=group)) + 
  geom_bar(data=subset_gd, stat="identity", color="black") + # bar plots  
  geom_point(alpha=1) + # individual points
  facet_wrap(~ type + feedback) + # different facets for plots 
  labs(x="\nGroups", y="Mean path accuracy\n") + # labels 
  ggtitle("Differences in mean path accuracy between ALS patients and controls") + # title
  theme(legend.position="none") # font size and co 
ggsave("WP6_Path_accuracy_mean.png")

rm(subset, subset_gd, subset_long, v, string)

####################################################################################################
## descriptives and plots for distance_accuracy

string <- "^distance_accuracy_.*_mean$"
v <- names(data)[grepl(string, names(data))]
subset <- cbind(data$Participant, data$Group, data$Sex, data[,v])
names(subset) <- c("id", "group", "sex","dist_acc_pm_nF_mean", "dist_acc_t_nF_mean", "dist_acc_t_wF_mean",
                   "dist_acc_pe_all_mean", "dist_acc_pe_nF_mean", "dist_acc_pe_wF_mean", 
                   "dist_acc_pa_all_mean", "dist_acc_pa_nF_mean", "dist_acc_pa_wF_mean")

subset_long <- melt(subset, id.vars=c("id", "group", "sex"))

subset_long$type <- NA

int_vars <- grep("_t_", subset_long$variable)
subset_long[int_vars, "type"] <- "training"

int_vars <- grep("_pm_", subset_long$variable)
subset_long[int_vars, "type"] <- "mixed"

int_vars <- grep("_pe_", subset_long$variable)
subset_long[int_vars, "type"] <- "ego"

int_vars <- grep("_pa_", subset_long$variable)
subset_long[int_vars, "type"] <- "allo"

subset_long$type <- factor(subset_long$type)


subset_long$feedback <- NA

int_vars <- grep("_all", subset_long$variable)
subset_long[int_vars, "feedback"] <- "all (wF + nF)"

int_vars <- grep("_wF", subset_long$variable)
subset_long[int_vars, "feedback"] <- "wF"

int_vars <- grep("_nF", subset_long$variable)
subset_long[int_vars, "feedback"] <- "nF"

subset_long$feedback <- factor(subset_long$feedback)


subset_gd <- subset_long %>%
  group_by(group, type, feedback) %>%
  summarise(
    value = mean(value)
  )

subset_long$feedback = factor(subset_long$feedback, levels=c("all (wF + nF)","wF","nF"))
subset_long$type = factor(subset_long$type, levels=c("training","mixed","ego", "allo"))

ggplot(subset_long, aes(y=value, x=group, fill=group)) + 
  geom_bar(data=subset_gd, stat="identity", color="black") + # bar plots  
  geom_point(alpha=1) + # individual points
  facet_wrap(~ type + feedback) + # different facets for plots 
  labs(x="\nGroups", y="Mean distance accuracy\n") + # labels 
  ggtitle("Differences in mean distance accuracy between ALS patients and controls") + # title
  theme(legend.position="none") # font size and co 
ggsave("WP6_Distance_accuracy_mean.png")

rm(subset, subset_gd, subset_long, v, string)

####################################################################################################
## descriptives and plots for path score

string <- "^path_score_.*_mean$"
v <- names(data)[grepl(string, names(data))]
subset <- cbind(data$Participant, data$Group, data$Sex, data[,v])
names(subset) <- c("id", "group", "sex","path_score_pm_nF_mean", "path_score_t_nF_mean", "path_score_t_wF_mean",
                   "path_score_pe_all_mean", "path_score_pe_nF_mean", "path_score_pe_wF_mean", 
                   "path_score_pa_all_mean", "path_score_pa_nF_mean", "path_score_pa_wF_mean")

subset_long <- melt(subset, id.vars=c("id", "group", "sex"))

subset_long$type <- NA

int_vars <- grep("_t_", subset_long$variable)
subset_long[int_vars, "type"] <- "training"

int_vars <- grep("_pm_", subset_long$variable)
subset_long[int_vars, "type"] <- "mixed"

int_vars <- grep("_pe_", subset_long$variable)
subset_long[int_vars, "type"] <- "ego"

int_vars <- grep("_pa_", subset_long$variable)
subset_long[int_vars, "type"] <- "allo"

subset_long$type <- factor(subset_long$type)


subset_long$feedback <- NA

int_vars <- grep("_all", subset_long$variable)
subset_long[int_vars, "feedback"] <- "all (wF + nF)"

int_vars <- grep("_wF", subset_long$variable)
subset_long[int_vars, "feedback"] <- "wF"

int_vars <- grep("_nF", subset_long$variable)
subset_long[int_vars, "feedback"] <- "nF"

subset_long$feedback <- factor(subset_long$feedback)


subset_gd <- subset_long %>%
  group_by(group, type, feedback) %>%
  summarise(
    value = mean(value)
  )

subset_long$feedback = factor(subset_long$feedback, levels=c("all (wF + nF)","wF","nF"))
subset_long$type = factor(subset_long$type, levels=c("training","mixed","ego", "allo"))

ggplot(subset_long, aes(y=value, x=group, fill=group)) + 
  geom_bar(data=subset_gd, stat="identity", color="black") + # bar plots  
  geom_point(alpha=1) + # individual points
  facet_wrap(~ type + feedback) + # different facets for plots 
  labs(x="\nGroups", y="Mean path score (number of zones entered)\n") + # labels 
  ggtitle("Differences in mean path score (number of zones) between ALS patients and controls") + # title
  theme(legend.position="none") # font size and co 
ggsave("WP6_Path_score_mean.png")

rm(subset, subset_gd, subset_long, v, string)

####################################################################################################
## descriptives and plots for direct trials

string <- "^direct_path_.*$"
v <- names(data)[grepl(string, names(data))]
subset <- cbind(data$Participant, data$Group, data$Sex, data[,v])
names(subset) <- c("id", "group", "sex","direct_path_pm_nF_mean", "direct_path_t_nF_mean", "direct_path_t_wF_mean",
                   "direct_path_pe_all_mean", "direct_path_pe_nF_mean", "direct_path_pe_wF_mean", 
                   "direct_path_pa_all_mean", "direct_path_pa_nF_mean", "direct_path_pa_wF_mean")

subset_long <- melt(subset, id.vars=c("id", "group", "sex"))

subset_long$type <- NA

int_vars <- grep("_t_", subset_long$variable)
subset_long[int_vars, "type"] <- "training"

int_vars <- grep("_pm_", subset_long$variable)
subset_long[int_vars, "type"] <- "mixed"

int_vars <- grep("_pe_", subset_long$variable)
subset_long[int_vars, "type"] <- "ego"

int_vars <- grep("_pa_", subset_long$variable)
subset_long[int_vars, "type"] <- "allo"

subset_long$type <- factor(subset_long$type)


subset_long$feedback <- NA

int_vars <- grep("_all", subset_long$variable)
subset_long[int_vars, "feedback"] <- "all (wF + nF)"

int_vars <- grep("_wF", subset_long$variable)
subset_long[int_vars, "feedback"] <- "wF"

int_vars <- grep("_nF", subset_long$variable)
subset_long[int_vars, "feedback"] <- "nF"

subset_long$feedback <- factor(subset_long$feedback)


subset_gd <- subset_long %>%
  group_by(group, type, feedback) %>%
  summarise(
    value = mean(value)
  )

subset_long$feedback = factor(subset_long$feedback, levels=c("all (wF + nF)","wF","nF"))
subset_long$type = factor(subset_long$type, levels=c("training","mixed","ego", "allo"))

ggplot(subset_long, aes(y=value, x=group, fill=group)) + 
  geom_bar(data=subset_gd, stat="identity", color="black") + # bar plots  
  geom_point(alpha=1) + # individual points
  facet_wrap(~ type + feedback) + # different facets for plots 
  labs(x="\nGroups", y="Number of direct trials\n") + # labels 
  ggtitle("Differences in number of direct trials between ALS patients and controls") + # title
  theme(legend.position="none") # font size and co 
ggsave("WP6_Direct_trials.png")

rm(subset, subset_gd, subset_long, v, string)

####################################################################################################
## descriptives and plots for successful trials 

string <- "^success_.*$"
v <- names(data)[grepl(string, names(data))]
subset <- cbind(data$Participant, data$Group, data$Sex, data[,v])
names(subset) <- c("id", "group", "sex","success_pm_nF_mean", "success_t_nF_mean", "success_t_wF_mean",
                   "success_pe_all_mean", "success_pe_nF_mean", "success_pe_wF_mean", 
                   "success_pa_all_mean", "success_pa_nF_mean", "success_pa_wF_mean")

subset_long <- melt(subset, id.vars=c("id", "group", "sex"))

subset_long$type <- NA

int_vars <- grep("_t_", subset_long$variable)
subset_long[int_vars, "type"] <- "training"

int_vars <- grep("_pm_", subset_long$variable)
subset_long[int_vars, "type"] <- "mixed"

int_vars <- grep("_pe_", subset_long$variable)
subset_long[int_vars, "type"] <- "ego"

int_vars <- grep("_pa_", subset_long$variable)
subset_long[int_vars, "type"] <- "allo"

subset_long$type <- factor(subset_long$type)


subset_long$feedback <- NA

int_vars <- grep("_all", subset_long$variable)
subset_long[int_vars, "feedback"] <- "all (wF + nF)"

int_vars <- grep("_wF", subset_long$variable)
subset_long[int_vars, "feedback"] <- "wF"

int_vars <- grep("_nF", subset_long$variable)
subset_long[int_vars, "feedback"] <- "nF"

subset_long$feedback <- factor(subset_long$feedback)


subset_gd <- subset_long %>%
  group_by(group, type, feedback) %>%
  summarise(
    value = mean(value)
  )

subset_long$feedback = factor(subset_long$feedback, levels=c("all (wF + nF)","wF","nF"))
subset_long$type = factor(subset_long$type, levels=c("training","mixed","ego", "allo"))

ggplot(subset_long, aes(y=value, x=group, fill=group)) + 
  geom_bar(data=subset_gd, stat="identity", color="black") + # bar plots  
  geom_point(alpha=1) + # individual points
  facet_wrap(~ type + feedback) + # different facets for plots 
  labs(x="\nGroups", y="Number of successful trials\n") + # labels 
  ggtitle("Differences in number of successful trials between ALS patients and controls") + # title
  theme(legend.position="none") # font size and co 
ggsave("WP6_Successful_trials.png")

rm(subset, subset_gd, subset_long, v, string)


###########################################################################################################
# ## descriptives and plots for search strategies
# 
# string_strategy <- paste("^t", timepoint, "_ss_.*_pt", sep="")
# 
# v_strategy <- names(data)[grepl(string_strategy, names(data))]
# subset_strategy <- cbind(data$Participant, data$Group, data[,v_strategy])
# names(subset_strategy) <- c("id", "group", "mixed1", "mixed2", "mixed3", "mixed4", 
#                             "egocentric1", "egocentric2","egocentric3","allocentric1", "allocentric2", "allocentric3", "allocentric4")
# 
# subset_strategy_long <- cbind(melt(subset_strategy, id.vars=c("id", "group")), NA)
# names(subset_strategy_long)[5] <- c("condition")
# 
# subset_strategy_long$condition[grepl("^mixed.*", subset_strategy_long$variable)] <- "mix trials"
# subset_strategy_long$condition[grepl("^egocentric.*", subset_strategy_long$variable)] <- "ego trials"
# subset_strategy_long$condition[grepl("^allocentric.*", subset_strategy_long$variable)] <- "allo trials"
# 
# subset_strategy_long$variable <- gsub("[a-zA-Z]", "", subset_strategy_long$variable)
# names(subset_strategy_long)[3] <- "trial"
# 
# subset_strategy_long$trial <- factor(subset_strategy_long$trial)
# subset_strategy_long$value <- factor(subset_strategy_long$value)
# subset_strategy_long$value <- factor(subset_strategy_long$value, levels(subset_strategy_long$value)[c(3,2,1,4)])
# subset_strategy_long$condition <- factor(subset_strategy_long$condition)
# subset_strategy_long$condition <- factor(subset_strategy_long$condition, levels(subset_strategy_long$condition)[c(3,2,1)])
# 
# ggplot(subset_strategy_long, aes(x=group, fill=value, alpha=group)) + 
#   geom_bar(stat="count", color="black") + # bar plots
#   facet_grid(condition ~ value) + # different facets for plots 
#   labs(x="\nGroup", y="Frequency\n") + # labels 
#   ggtitle(paste("Search strategy at t", timepoint, "\n", sep="")) + # title
#   theme(legend.position="none", plot.title=element_text(hjust=0.5, size=20), 
#         axis.ticks.length=unit(5,"pt"), axis.title=element_text(size=18), axis.text=element_text(size=15), 
#         strip.text=element_text(size=15)) # font size and co 
# ggsave(paste("Search strategy at t", timepoint, ".png", sep=""))
# 
# rm(subset_strategy, subset_strategy_long, v_strategy, string_strategy)

################################################################################################
## descriptives and plots for scoring recall recognition

subset <- data.frame(cbind(data$Participant, data$Group, data$Sex,
                           data$Score_Recall_LM, data$Score_Recall_Maze, 
                           data$Score_Recall_2,
                           data$Score_Recognition_LM, data$Score_Recognition_LMA),
                     stringsAsFactors=F)

names(subset) <- c("id", "group", "sex", 
                   "Recall landmarks", "Recall maze", "Recall route",
                   "Recognition landmarks", "Recognition position")

# convert to int
int_vars <-c("id", "Recall landmarks", "Recall maze", "Recall route",
             "Recognition landmarks", "Recognition position") # add here 
subset[,int_vars] <- lapply(subset[,int_vars], as.numeric)
rm(int_vars)

# ##############
# # NEEDS TO BE CHECKED
# Standardisierung in Prozent durch Division mit Maximalpunktzahl
subset$`Recall landmarks` <- subset$`Recall landmarks`/17*100
subset$`Recall maze` <- subset$`Recall maze`/15*100
subset$`Recognition landmarks` <- subset$`Recognition landmarks`/15*100
subset$`Recognition position` <- subset$`Recognition position`/8*100
subset$`Recall route` <- subset$`Recall route`/8*100
# ##############

subset_long <- melt(subset, id.vars=c("id", "group", "sex"))

subset_long$group <- factor(subset_long$group, labels=c("ALS", "Control"))
subset_long$sex <- factor(subset_long$sex, labels=c("female", "male"))
subset_long$variable <- factor(subset_long$variable)

subset_gd <- subset_long %>%
  group_by(group, variable) %>%
  summarise(
    value = mean(value)
  )

ggplot(subset_long, aes(y=value, x=group, fill=variable, alpha=group)) + 
  geom_bar(data=subset_gd, stat="identity", color="black") + # bar plots  
  geom_point(alpha=1) + # individual points
  facet_wrap(~ variable, ncol=3) + # different facets for plots 
  labs(x="\nGroups", y="Score (in %)\n") + # labels 
  ggtitle("Post-Test-Score for recall and recognition for ALS patients and controls") + # title
  theme(legend.position="none") # font size and co 
ggsave("WP6_Score_recall_recognition.png")

rm(subset, subset_gd, subset_long)
