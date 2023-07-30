# Load packages
library(pwr) #loading package that includes power functions
library(tidyverse)
library(ggplot2)
library(gt)
library(dplyr)

#Set working directory
setwd("~/Desktop/PSYU917 - Dissertation/dissdata")

# Import data
adaptation_data <- read_csv("UG_22-23_data_adapttion.csv");
baseline_data <- read_csv("UG_22-23_data_baseline.csv");
adaptation_data


# Rename columns
new_colnames <-c("Participant", "Mean AA", "SD AA", "Mean VV", "SD VV", "Mean AV", "SD AV", "Mean VA", "SD VA")
# view(new_colnames)
names(adaptation_data) <- new_colnames
# view(names(adaptation_data) <- new_colnames)
adaptation_data$`Participant` <- 1:4
# Rename rows in column 1
# adaptation_data$`Participant Number` <- paste0("Participant ", 1:nrow(adaptation_data))

# Remove the original first column
# adaptation_data <- adaptation_data[-1]

# Create an APA 7 compliant table
nice_table_adapt <- adaptation_data %>%
  gt() %>%
  tab_header(
    title = "Table 1",
    subtitle = "My APA 7 Compliant Table"
  ) %>%
  tab_options(
    table.width = "100%",
    column_labels.font.size = 12,
    row_group.font.size = 12
  ) %>%
  tab_style(style = cell_text(align = "center"), locations = cells_body()) %>%
  cols_align(align = "center", columns = everything()) %>%
  tab_footnote("Note. This table follows APA 7 guidelines.")
# Display the table
nice_table_adapt

# Get adaptation columns
ad_col_1 <- c(adaptation_data[1,])
ad_col_2 <- c(adaptation_data[2,])
ad_col_3 <- c(adaptation_data[3,])
ad_col_4 <- c(adaptation_data[4,])

# Turn columns into table
rows_adaptation <- rbind(ad_col_1[-1], ad_col_2[-1], ad_col_3[-1], ad_col_4[-1]);
tab <- as.table(rows_adaptation)
dimnames(tab) <- list(participants = c("1", "2", "3", "4"),
                      conditions = c("AA", "AA-SD", "VV", "VV-SD", "AV", "AV-SD", "VA", "VA-SD"))
dimnames(tab)

# Baseline Data
base_AA <- baseline_data$AA
base_VV <- baseline_data$VV
base_AV <- baseline_data$AV
base_VA <- baseline_data$VA

# Adaptation Data
adaptation_AA <- adaptation_data$AA
adaptation_VV <- adaptation_data$VV
adaptation_AV <- adaptation_data$AV
adaptation_VA <- adaptation_data$VA

# Means
mean_base_AA <- mean(base_AA) # 420.25
mean_base_VV <- mean(base_VV) # 516.75
mean_base_AV <- mean(base_AV) # 474.5
mean_base_VA <- mean(base_VA) # 513.5

mean_adapt_AA <- mean(adaptation_AA) # 378
mean_adapt_VV <- mean(adaptation_VV) # 439.75
mean_adapt_AV <- mean(adaptation_AV) # 661.5
mean_adapt_VA <- mean(adaptation_VA) # 640.75

# Mean SDs
# mean(baseline_data$`Std Dev...3`) Ask what should the error bar sd should be for, the means of sds or the results.
sd_base_AA <- sd(baseline_data$AA)
sd_adapt_AA <- sd(adaptation_data$AA)

sd_base_VV <- sd(baseline_data$VV)
sd_adapt_VV <- sd(adaptation_data$VV)

sd_base_AV <- sd(baseline_data$AV)
sd_adapt_AV <- sd(adaptation_data$AV)

sd_base_VA <- sd(baseline_data$VA)
sd_adapt_VA <- sd(adaptation_data$VA)

# T-Tests Baseline vs Adaptation
base_adapt_AA <- t.test(base_AA, adaptation_AA, paired=TRUE) # p-value = 0.06348
base_adapt_VV <- t.test(base_VV, adaptation_VV, paired=TRUE) # p-value = 0.08124
base_adapt_AV <- t.test(base_AV, adaptation_AV, paired=TRUE) # p-value = 0.002979
base_adapt_VA <- t.test(base_VA, adaptation_VA, paired=TRUE) # p-value = 0.01795

base_adapt_AA

# T-Tests Adaptations
adapt_AAVA <- t.test(adaptation_AA, adaptation_VA, paired=TRUE) # p-value = 0.001812
adapt_VVAV <- t.test(adaptation_VV, adaptation_AV, paired=TRUE) # p-value = 0.001671

Base_AAVV <- t.test(base_AA, base_VV, paired=TRUE)
adapt_AAVV<- t.test(adaptation_AA, adaptation_VV, paired=TRUE)
adapt_AAVV
Base_AAVV

# Participants
participant_ages <- c(18, 21, 29, 33)
mean_age <- mean(participant_ages) # 25.25
sd_age <- sd(participant_ages) # 6.946222

# Bar graph -> Showing group mean results Baseline vs Experimental.
# COLOURS
baseline_original = '#C1E1C1'
baseline_compare_light = '#e3f2e3'
baseline_compare_dark = '#85c785'
adapt_original = '#C3B1E1'
adapt_compare_light = '#e3daf1'
adapt_compare_dark = '#aa90d5'

title_themes <- theme(title = element_text(face = "italic"), 
                       axis.title.y = element_text(face = "bold"), 
                       axis.title.x = element_text(face = "bold"),
                       legend.title = element_text(face = "bold"))
## Data
col_base_means <- c(mean_base_AA, mean_base_VV, mean_base_AV, mean_base_VA);
col_adapt_means <- c(mean_adapt_AA, mean_adapt_VV, mean_adapt_AV, mean_adapt_VA);
col_base_sds <- c(sd_base_AA, sd_base_VV, sd_base_AV, sd_base_VA);
col_adapt_sds <- c(sd_adapt_AA, sd_adapt_VV, sd_adapt_AV, sd_adapt_VA);
col_base_adapt_means <- c(col_base_means, col_adapt_means);
col_base_adapt_sds <- c(col_base_sds, col_adapt_sds);
col_type <- c("a_base","a_base","a_base","a_base","b_adapt","b_adapt","b_adapt","b_adapt")
conditions <- c("A", "B", "C", "D", "A", "B", "C", "D")
df_means <- data.frame(col_type, conditions, col_base_adapt_means, col_base_adapt_sds);
## Plot
error_plot_means <- ggplot(df_means, aes(y=col_base_adapt_means, x=conditions, fill=col_type)) +
  geom_bar(stat="identity", color="black", position = position_dodge()) +
  geom_errorbar(aes(ymin=col_base_adapt_means-col_base_adapt_sds, ymax=col_base_adapt_means+col_base_adapt_sds),
                width=.2, position = position_dodge(.9))

# # Mean Responses for Test-Sensory Pairings in Baseline vs Adaption Conditions
# error_plot_means + labs(title="Mean Responses for Test-Sensory Pairings in Baseline vs Adaption Conditions", 
#                         x="Test-Sensory Pairing", 
#                         y="Reproduction of 500ms", 
#                         fill="Condition") +
#   theme_classic() +
#   theme(title = element_text(face = "italic"), 
#         axis.title.y = element_text(face = "bold"), 
#         axis.title.x = element_text(face = "bold"),
#         legend.title = element_text(face = "bold")
#         ) +
#   scale_fill_manual(values=c(baseline_original,adapt_original), labels = c("Baseline", "Adaptation")) +
#   scale_x_discrete(labels=c('AA', 'VV', 'AV', 'VA'))

# Audition vs Visual ~ Pre and post adaptation – one graph
## PRE: unimodal: BAA+BVV, crossmodal: BAV+BVA
# modality, BAA BVV | BAV BVA, sd, 
modalities <- c("a", "a", "b", "b")
pre_conditions <- c("a", "b", "c", "d")
pre_modal_df <-data.frame(col_base_means, col_base_sds, modalities, pre_conditions)
## Plot
pre_modal_plot <- ggplot(pre_modal_df, aes(y=col_base_means, x=pre_conditions, fill=modalities)) +
  geom_bar(stat="identity", color="black", position = position_dodge()) +
  geom_errorbar(aes(ymin=col_base_means-col_base_sds, ymax=col_base_means+col_base_sds),
                width=.2, position = position_dodge(.9))

# pre_modal_plot + labs(title="Mean Responses Between Unimodal and Crossmodal Test-Sensory Pairings in Baseline Conditions", 
#                       x="Test-Sensory Pairing", 
#                       y="Reproduction of 500ms", 
#                       fill="Modality") +
#   theme_classic() +
#   theme(title = element_text(face = "italic"), 
#         axis.title.y = element_text(face = "bold"), 
#         axis.title.x = element_text(face = "bold"),
#         legend.title = element_text(face = "bold")
#   ) +
#   scale_fill_manual(values=c(baseline_compare_light,baseline_compare_dark), labels = c("Unimodal", "Crossmodal")) +
#   scale_x_discrete(labels=c('AA', 'VV', 'AV', 'VA'))

## POST: unimodal: AA+VV, crossmodal: AV+VA
post_modal_df <-data.frame(col_adapt_means, col_adapt_sds, modalities, pre_conditions)
## Plot
post_modal_plot <- ggplot(post_modal_df, aes(y=col_adapt_means, x=pre_conditions, fill=modalities)) +
  geom_bar(stat="identity", color="black", position = position_dodge()) +
  geom_errorbar(aes(ymin=col_adapt_means-col_adapt_sds, ymax=col_adapt_means+col_adapt_sds),
                width=.2, position = position_dodge(.9))

post_modal_plot + labs(title="Mean Responses Between Unimodal and Crossmodal Test-Sensory Pairings in Adaptation Conditions", 
                       x="Test-Sensory Pairing", 
                       y="Reproduction of 500ms", 
                       fill="Modality") +
  theme_classic() +
  title_themes +
  scale_fill_manual(values=c(adapt_compare_light,adapt_compare_dark), labels = c("Unimodal", "Crossmodal")) +
  scale_x_discrete(labels=c('AA', 'VV', 'AV', 'VA'))

# Audition vs Visual ~ modality and order of sense
# -	AA VV : AV VA
# mean_adapt_AA, mean_adapt_VV
aud_vis_AA_VV_means <- c(mean_adapt_AA, mean_adapt_VV)
aud_vis_AA_VV_sd <- c(sd_adapt_AA, sd_adapt_VV)
aud_vis_AA_VV_types <- c("audio", "visual")
aud_vis_AA_VV_conditions <- c("AA", "VV")
AA_VV_df <- data.frame(aud_vis_AA_VV_means, aud_vis_AA_VV_sd, aud_vis_AA_VV_types, aud_vis_AA_VV_conditions)

# Plot
# aud_vis_AA_VV_plot <- ggplot(AA_VV_df, aes(y=aud_vis_AA_VV_means, x=aud_vis_AA_VV_conditions, fill=aud_vis_AA_VV_types)) +
#   geom_bar(stat="identity", color="black", position = position_dodge()) +
#   geom_errorbar(aes(ymin=aud_vis_AA_VV_means-aud_vis_AA_VV_sd, ymax=aud_vis_AA_VV_means+aud_vis_AA_VV_sd),
#                 width=.2, position = position_dodge(.9))
# 
# aud_vis_AA_VV_plot + labs(title="Mean Responses in Auditory Compared to Visual Unimodal Conditions Post Adaptation", 
#                           x="Test-Sensory Pairing", 
#                           y="Reproduction of 500ms", 
#                           fill="Sensory Modality") +
#   theme_classic() +
#   title_themes +
#   scale_fill_manual(values=c(adapt_compare_light,adapt_compare_dark), labels = c("Audio", "Visual")) +
#   scale_x_discrete(labels=c('AA', 'VV'))
# 
# # mean_adapt_AV, mean_adapt_VA
# aud_vis_AV_VA_means <- c(mean_adapt_AV, mean_adapt_VA)
# aud_vis_AV_VA_sd <- c(sd_adapt_AV, sd_adapt_VA)
# aud_vis_AV_VA_types <- c("audio", "visual")
# aud_vis_AV_VA_conditions <- c("AV", "VA")
# AV_VA_df <- data.frame(aud_vis_AV_VA_means, aud_vis_AV_VA_sd, aud_vis_AV_VA_types, aud_vis_AV_VA_conditions)

# Plot
# aud_vis_AV_VA_plot <- ggplot(AV_VA_df, aes(y=aud_vis_AV_VA_means, x=aud_vis_AV_VA_conditions, fill=aud_vis_AV_VA_types)) +
#   geom_bar(stat="identity", color="black", position = position_dodge()) +
#   geom_errorbar(aes(ymin=aud_vis_AV_VA_means-aud_vis_AV_VA_sd, ymax=aud_vis_AV_VA_means+aud_vis_AV_VA_sd),
#                 width=.2, position = position_dodge(.9))
# 
# aud_vis_AV_VA_plot + labs(title="Post-Adaptation Mean Responses in Crossmodal Test-Pairings by Order of Sensory Signal", 
#                           x="Test-Sensory Pairing", 
#                           y="Reproduction of 500ms", 
#                           fill="First Signal") +
#   theme_classic() +
#   title_themes +
#   scale_fill_manual(values=c(adapt_compare_light, adapt_compare_dark), labels = c("Audio", "Visual")) +
#   scale_x_discrete(labels=c('AV', 'VA'))

# Differences pre-adaption responses and post-adaption responses
# -	Differences all on a bar graph 
mean_difference_AA <- mean(baseline_data$AA - adaptation_data$AA)
mean_difference_VV <- mean(baseline_data$VV - adaptation_data$VV)
mean_difference_AV <- mean(baseline_data$AV - adaptation_data$AV)
mean_difference_VA <- mean(baseline_data$VA - adaptation_data$VA)
sd_difference_AA <- sd(baseline_data$AA - adaptation_data$AA)
sd_difference_VV <- sd(baseline_data$VV - adaptation_data$VV)
sd_difference_AV <- sd(baseline_data$AV - adaptation_data$AV)
sd_difference_VA <- sd(baseline_data$VA - adaptation_data$VA)
difference_types <- c("a","b","c","d")
difference_conditions <- c("a", "a", "b", "b")
mean_difference_base_adapt <- c(mean_difference_AA, mean_difference_VV, mean_difference_AV, mean_difference_VA)
sd_difference_base_adapt <- c(sd_difference_AA, sd_difference_VV, sd_difference_AV, sd_difference_VA)
difference_df <- data.frame(mean_difference_base_adapt, sd_difference_base_adapt, difference_types, difference_conditions)

# Plot
# difference_plot <- ggplot(difference_df, aes(y=mean_difference_base_adapt, x=difference_types, fill=difference_conditions)) +
#   geom_bar(stat="identity", color="black", position = position_dodge()) +
#   geom_errorbar(aes(ymin=mean_difference_base_adapt-sd_difference_base_adapt, ymax=mean_difference_base_adapt+sd_difference_base_adapt),
#                 width=.2, position = position_dodge(.9))
# 
# difference_plot + labs(title="Differences Recorded Between Pre and Post-Adaption Response Means Across Test-Sensory Pairings", 
#                        x="Test-Sensory Pairing", 
#                        y="Reproduction of 500ms", 
#                        fill="Modality") +
#   theme_classic() +
#   title_themes +
#   scale_fill_manual(values=c('#fbcfe8','#bfdbfe'), labels = c("Unimodal", "Crossmodal")) +
#   scale_x_discrete(labels=c('AA', 'VV', 'AV', 'VA'))

# T-Tests Differences
difference_ttest_AA_VV <- t.test((baseline_data$AA - adaptation_data$AA), (baseline_data$VV - adaptation_data$VV), paired=TRUE) # p-value = 0.3869
difference_ttest_AV_VA <- t.test((baseline_data$AV - adaptation_data$AV), (baseline_data$VA - adaptation_data$VA), paired=TRUE) # p-value = 0.1697
difference_ttest_AA_AV <- t.test((baseline_data$AA - adaptation_data$AA), (baseline_data$AV - adaptation_data$AV), paired=TRUE) # p-value = 0.0003032
difference_ttest_VV_VA <- t.test((baseline_data$VV - adaptation_data$VV), (baseline_data$VA - adaptation_data$VA), paired=TRUE) # p-value = 0.009006

##### PLOTS #####

uni_modal = '#fdba74' #
cross_modal = '#bfdbfe'

aud = '#fde68a'
vis = '#bfdbfe'

bas = '#f8fafc'
ada = '#22c55e'

# Mean Responses for Test-Sensory Pairings in Baseline vs Adaption Conditions
error_plot_means + labs(title="Mean Responses for Test-Sensory Pairings in Baseline vs Adaption Conditions", 
                        x="Test-Sensory Pairing", 
                        y="Reproduction of 500ms", 
                        fill="Condition") +
  theme_classic() +
  theme(title = element_text(face = "italic"), 
        axis.title.y = element_text(face = "bold"), 
        axis.title.x = element_text(face = "bold"),
        legend.title = element_text(face = "bold")
  ) +
  scale_fill_manual(values=c(bas,ada), labels = c("Baseline", "Adaptation")) +
  scale_x_discrete(labels=c('AA', 'VV', 'AV', 'VA'))

# Mean Responses Between Unimodal and Crossmodal Test-Sensory Pairings in Baseline Conditions
pre_modal_plot + labs(title="Mean Responses Between Unimodal and Crossmodal Test-Sensory Pairings in Baseline Conditions", 
                      x="Test-Sensory Pairing", 
                      y="Reproduction of 500ms", 
                      fill="Modality") +
  theme_classic() +
  theme(title = element_text(face = "italic"), 
        axis.title.y = element_text(face = "bold"), 
        axis.title.x = element_text(face = "bold"),
        legend.title = element_text(face = "bold")
  ) +
  scale_fill_manual(values=c(uni_modal,cross_modal), labels = c("Unimodal", "Crossmodal")) +
  scale_x_discrete(labels=c('AA', 'VV', 'AV', 'VA'))

# Mean Responses Between Unimodal and Crossmodal Test-Sensory Pairings in Adaptation Conditions
post_modal_plot + labs(title="Mean Responses Between Unimodal and Crossmodal Test-Sensory Pairings in Adaptation Conditions", 
                       x="Test-Sensory Pairing", 
                       y="Reproduction of 500ms", 
                       fill="Modality") +
  theme_classic() +
  title_themes +
  scale_fill_manual(values=c(uni_modal,cross_modal), labels = c("Unimodal", "Crossmodal")) +
  scale_x_discrete(labels=c('AA', 'VV', 'AV', 'VA'))

# Mean Responses in Auditory Compared to Visual Unimodal Conditions Post Adaptation
aud_vis_AA_VV_plot <- ggplot(AA_VV_df, aes(y=aud_vis_AA_VV_means, x=aud_vis_AA_VV_conditions, fill=aud_vis_AA_VV_types)) +
  geom_bar(stat="identity", color="black", position = position_dodge()) +
  geom_errorbar(aes(ymin=aud_vis_AA_VV_means-aud_vis_AA_VV_sd, ymax=aud_vis_AA_VV_means+aud_vis_AA_VV_sd),
                width=.2, position = position_dodge(.9))

aud_vis_AA_VV_plot + labs(title="Mean Responses in Auditory Compared to Visual Unimodal Conditions Post Adaptation", 
                          x="Test-Sensory Pairing", 
                          y="Reproduction of 500ms", 
                          fill="Sensory Modality") +
  theme_classic() +
  title_themes +
  scale_fill_manual(values=c(aud,vis), labels = c("Audio", "Visual")) +
  scale_x_discrete(labels=c('AA', 'VV'))

# mean_adapt_AV, mean_adapt_VA
aud_vis_AV_VA_means <- c(mean_adapt_AV, mean_adapt_VA)
aud_vis_AV_VA_sd <- c(sd_adapt_AV, sd_adapt_VA)
aud_vis_AV_VA_types <- c("audio", "visual")
aud_vis_AV_VA_conditions <- c("AV", "VA")
AV_VA_df <- data.frame(aud_vis_AV_VA_means, aud_vis_AV_VA_sd, aud_vis_AV_VA_types, aud_vis_AV_VA_conditions)


# Post-Adaptation Mean Responses in Crossmodal Test-Pairings by Order of Sensory Signal
aud_vis_AV_VA_plot <- ggplot(AV_VA_df, aes(y=aud_vis_AV_VA_means, x=aud_vis_AV_VA_conditions, fill=aud_vis_AV_VA_types)) +
  geom_bar(stat="identity", color="black", position = position_dodge()) +
  geom_errorbar(aes(ymin=aud_vis_AV_VA_means-aud_vis_AV_VA_sd, ymax=aud_vis_AV_VA_means+aud_vis_AV_VA_sd),
                width=.2, position = position_dodge(.9))

aud_vis_AV_VA_plot + labs(title="Post-Adaptation Mean Responses in Crossmodal Test-Pairings by Order of Sensory Signal", 
                          x="Test-Sensory Pairing", 
                          y="Reproduction of 500ms", 
                          fill="First Signal") +
  theme_classic() +
  title_themes +
  scale_fill_manual(values=c(aud,vis), labels = c("Audio", "Visual")) +
  scale_x_discrete(labels=c('AV', 'VA'))

# Differences Recorded Between Pre and Post-Adaption Response Means Across Test-Sensory Pairings
difference_plot <- ggplot(difference_df, aes(y=mean_difference_base_adapt, x=difference_types, fill=difference_conditions)) +
  geom_bar(stat="identity", color="black", position = position_dodge()) +
  geom_errorbar(aes(ymin=mean_difference_base_adapt-sd_difference_base_adapt, ymax=mean_difference_base_adapt+sd_difference_base_adapt),
                width=.2, position = position_dodge(.9))

difference_plot + labs(title="Differences Recorded Between Pre and Post-Adaption Response Means Across Test-Sensory Pairings", 
                       x="Test-Sensory Pairing", 
                       y="Reproduction of 500ms", 
                       fill="Modality") +
  theme_classic() +
  title_themes +
  scale_fill_manual(values=c(uni_modal,cross_modal), labels = c("Unimodal", "Crossmodal")) +
  scale_x_discrete(labels=c('AA', 'VV', 'AV', 'VA'))



