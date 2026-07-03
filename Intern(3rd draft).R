#R SCRIPT: Reproduction-Longevity Trade-off Analysis
# Research Question: How does reproduction affect longevity?
# 1. LOAD REQUIRED PACKAGES
library(readxl)
library(ggplot2)
library(dplyr)
library(sjPlot)
library(ggpubr)
library(lme4)
library(lmerTest) #LMM Model
library(ggplot2)
library(tidyverse) # includes ggplot2, for data visualisation. dplyr, for data manipulation.
library(survival)
library(survminer)
library(ggsurvfit)
library(tidyr)
library(flexsurv)
library(patchwork)
library(broom)  # For tidy model outputs
# Setting up environment
# Clean environment
rm(list = ls(all.names = TRUE)) # will clear all objects including hidden objects
gc() # free up memory and report the memory usage
options(max.print = .Machine$integer.max, scipen = 999, stringsAsFactors = F, dplyr.summarise.inform = F) # avoid truncated output in R console and scientific notation

# Set seed
set.seed(42)

# Theme
biostatsquid_theme <- theme(plot.title = element_text(size = rel(2)),
                            panel.grid.major.y = element_line(colour = 'gray'),
                            panel.grid.minor.y = element_line(colour = 'gray'),
                            panel.grid.major.x = element_blank(),
                            panel.grid.minor.x = element_blank(),
                            plot.background = element_rect(fill = NULL, colour = 'white'),
                            panel.background = element_rect(fill = 'white'),
                            # Axis stuff
                            axis.line = element_line(colour = 'black', linewidth = 1),
                            axis.text = element_text(colour = "black", face = 'bold'),
                            axis.text.x = element_text(size = rel(1)),
                            axis.text.y = element_text(size = rel(1)),
                            axis.title = element_text(size = rel(1.2)),
                            axis.ticks = element_line(colour = 'black', linewidth = 1.2),
                            # Legend stuff
                            legend.position = "bottom",
                            legend.margin = margin(6, 6, 6, 6),
                            legend.title = element_text(face = 'bold'),
                            legend.background = element_blank(),
                            legend.box.background = element_rect(colour = "black"))

# Read the data (adjust file path)
df <- readxl::read_excel('/Users/damianlau/Desktop/HKU Stuff/HKU Year 3 Sum Sem/BIOL4964/Data_Base_1.xlsx')



# Check lifespan column
df$lifespan <- as.numeric(df$lifespan)

# Status column (all died)
df$status <- 1

# Fit Kaplan-Meier
fit <- survfit(Surv(lifespan, status) ~ Sex, data = df)

# Combine Treatment and Sex
df$group <- interaction(df$Treatment, df$Sex)

fit_group_1 <- survfit(Surv(lifespan, status) ~ group, data = df)
ggsurvplot(fit_group_1, data = df,
           pval = TRUE,
           conf.int = TRUE,
           xlab = "Adult Lifespan (Days)",
           ylab = "Survival Probability",
           title = "Survival by Treatment and Sex",
           palette = c("blue", "lightblue", "red", "pink"),
           legend.title = "Group",
           legend.labs = c("Isolation Female", "Isolation Male", 
                           "Reproduction Female", "Reproduction Male"),
           ggtheme = theme_bw() + 
             theme(plot.title = element_text(hjust = 0.5, face = "bold")))

# Plot 1: Females
# Female subset (After Fixing the time period)
ggsurvplot(survfit(Surv(lifespan, status) ~ Treatment, 
                   data = subset(df, Sex == "Female")),
           data = subset(df, Sex == "Female"),
           title = "Females: Isolation vs. Reproduction",
           xlab = "Adult Lifespan (Days)",
           ylab = "Survival Probability",
           pval = TRUE,
           conf.int = TRUE,
           palette = c("blue", "red"),
           legend.labs = c("Isolation", "Reproduction"),
           ggtheme = theme_bw() + 
             theme(plot.title = element_text(hjust = 0.5, face = "bold")))

#plot 2: Male
# Male subset (After fixing the time period)
ggsurvplot(survfit(Surv(lifespan, status) ~ Treatment, data = subset(df, Sex == "Male")),
           data = subset(df, Sex == "Male"),
           title = "Males: Isolation vs. Reproduction",
           xlab = "Adult Lifespan (Days)",
           ylab = "Survival Probability",
           pval = TRUE,
           conf.int = TRUE,
           palette = c("lightblue", "pink"),
           legend.labs = c("Isolation", "Reproduction"),
           ggtheme = theme_bw() + 
             theme(plot.title = element_text(hjust = 0.5, face = "bold")))

#Violin Plot
violin_plot <- ggplot(df, aes(x = Treatment, y = lifespan, fill = Treatment)) +
  
  # Violin layer (shows full distribution)
  geom_violin(trim = FALSE, alpha = 0.7, width = 0.9) +
  
  # Boxplot layer (shows median, IQR, outliers)
  geom_boxplot(width = 0.15, alpha = 0.5, 
               outlier.shape = 21, 
               outlier.size = 1.5, 
               outlier.fill = "white") +
  
  # Individual points (jittered to avoid overlap)
  geom_jitter(width = 0.08, size = 0.8, alpha = 0.4) +
  
  # Facet by sex
  facet_wrap(~ Sex, scales = "free_x") +
  
  # Custom colors
  scale_fill_manual(values = c("Isolation" = "#2E86AB", 
                               "Reproduction" = "#D64933")) +
  
  # Labels
  labs(
    title = "Adult Lifespan by Treatment and Sex",
    x = "Treatment Group",
    y = "Adult Lifespan (Days)",
    fill = "Treatment"
  ) +
  
  # Clean theme
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 11, color = "gray40"),
    axis.title = element_text(size = 13),
    axis.text = element_text(size = 11),
    strip.text = element_text(size = 14, face = "bold"),
    legend.position = "bottom",
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 11),
    panel.grid.minor = element_blank(),
    panel.spacing = unit(1, "lines")
  )

# Display violin plot
print(violin_plot)


# Create a survival object
surv_obj <- Surv(time = df$lifespan, event = df$status)
head(surv_obj)

#The log rank test lets us test whether there is a difference in survival times 
#between groups of patients.
# Log rank test
table(df$Treatment)

#Visualize
s2 <- survfit(surv_obj ~ Treatment, data = df)
ggsurvplot(s2, data = df,
           size = 1,
           palette = c('#E7B800', '#2e9fdf'),
           censor.shape = '|', censor.size = 4,
           conf.int = TRUE,
           pval = TRUE,
           risk.table = TRUE,
           risk.table.col = 'strata',
           legend.labs = list('0' = 'Isolation', '1' = 'Reproduction'),
           risk.table.height = 0.25,
           title = "Log Rank Test by Treatment and Sex Without Eclosion Timing",
           ggtheme = theme_bw() +
             theme(
               plot.title = element_text(hjust = 0.5, face = "bold"),
               plot.subtitle = element_text(hjust = 0.5, face = "bold")
             ))

logrank_female <- survdiff(Surv(lifespan, status) ~ Treatment, 
                           data = subset(df, Sex == "Female"))
print(logrank_female)

logrank_male <- survdiff(Surv(lifespan, status) ~ Treatment, 
                         data = subset(df, Sex == "Male"))
print(logrank_male)



# Descriptive Summaries (MAIN ANALYSES WITHOUT ECLOSION TIMING)
summary_step1 <- df %>%
  group_by(Sex, Treatment) %>%
  summarise(
    n = n(),
    events = sum(status == 1, na.rm = TRUE),
    censored = sum(status == 0, na.rm = TRUE),
    mean = mean(lifespan, na.rm = TRUE),
    median = median(lifespan, na.rm = TRUE),
    IQR = IQR(lifespan, na.rm = TRUE),
    min = min(lifespan, na.rm = TRUE),
    max = max(lifespan, na.rm = TRUE),
    .groups = "drop"
  )

print(summary_step1)

#Cox Proportional Hazards Model
cox_step1 <- coxph(Surv(lifespan, status) ~ Sex * Treatment, data = df)
summary(cox_step1)

# Test Proportional Hazards Assumption
cox_ph_test <- cox.zph(cox_step1)
print(cox_ph_test)
#Expect to see high p value (null hypothesis: Hazard ratio is constant over time)
#High P-value-> Fail to reject-> Null Hypothesis Holds

#Gompertz Models by Sex
#Describes how the hazard changes over time
##How to read the table
#Shape:a-> a>0 hazard increasing; a=0 hazard is constant; a<0 hazard is decreasing
#Rate: The baseline hazard at time = 0 for the reference group
#treatmentReproduction: Log-hazard Ratio
#exp: Hazard Ratio (Exponential)

# Females
females_no_eclosion <- df %>% filter(Sex == "Female")
gompertz_f <- flexsurvreg(
  Surv(lifespan, status) ~ Treatment,
  data = females_no_eclosion,
  dist = "gompertz"
)
print(gompertz_f)
summary(gompertz_f)

# Males
males_no_eclosion <- df %>% filter(Sex == "Male")
gompertz_m <- flexsurvreg(
  Surv(lifespan, status) ~ Treatment,
  data = males_no_eclosion,
  dist = "gompertz"
)
print(gompertz_m)
summary(gompertz_m)





### Add Eclosion Timing as a factor [Early Reproduction]
#Filter to the cohort
df_cohort_1 <- df %>%
  filter(Eclosion >= as.Date("28-Jan", format = "%d-%b") & 
           Eclosion <= as.Date("5-Feb", format = "%d-%b")) %>%  
  # Ensure treatment is a factor with proper order
  mutate(
    treatment = factor(Treatment, levels = c("Isolation", "Reproduction")),
    sex = factor(Sex, levels = c("Female", "Male"))
  )

##Check Sample Size
sample_sizes <- df_cohort_1 %>%
  group_by(Sex, Treatment) %>%
  summarise(
    n = n(),
    deaths = sum(status == 1),
    censored = sum(status == 0),
    .groups = "drop"
  )

print("Sample sizes after Filtering:")
print(sample_sizes)

fit_group_2 <- survfit(Surv(lifespan, status) ~ group, data = df_cohort_1)
ggsurvplot(fit_group_1, data = df,
           pval = TRUE,
           conf.int = TRUE,
           xlab = "Adult Lifespan (Days)",
           ylab = "Survival Probability",
           title = "Survival by Treatment and Sex",
           subtitle = paste0("Cohort: Eclosed Jan 28 - Feb 5, 2025 (n = ", 
                             nrow(subset(df_cohort_1)), ")"),
           palette = c("blue", "lightblue", "red", "pink"),
           legend.title = "Group",
           legend.labs = c("Isolation Female", "Isolation Male", 
                           "Reproduction Female", "Reproduction Male"),
           ggtheme = theme_bw() + 
             theme(plot.title = element_text(hjust = 0.5, face = "bold"),
                   plot.subtitle = element_text(hjust = 0.5, face = "bold")))

# Sex-specific models
fit_female <- survfit(Surv(lifespan, status) ~ Treatment, 
                      data = subset(df_cohort_1, Sex == "Female"))

fit_male <- survfit(Surv(lifespan, status) ~ Treatment, 
                    data = subset(df_cohort_1, Sex == "Male"))

# Plot 1: Females
# Female subset (After Fixing the time period)
ggsurvplot(survfit(Surv(lifespan, status) ~ Treatment, 
                   data = subset(df_cohort_1, Sex == "Female")),
           data = subset(df_cohort_1, Sex == "Female"),
           title = "Females: Isolation vs. Reproduction",
           subtitle = paste0("Cohort: Eclosed Jan 28 - Feb 5, 2025 (n = ", 
                             nrow(subset(df_cohort_1, Sex == "Female")), ")"),
           xlab = "Adult Lifespan (Days)",
           ylab = "Survival Probability",
           pval = TRUE,
           conf.int = TRUE,
           palette = c("blue", "red"),
           legend.labs = c("Isolation", "Reproduction"),
           ggtheme = theme_bw() + 
             theme(plot.title = element_text(hjust = 0.5, face = "bold"),
                   plot.subtitle = element_text(hjust = 0.5, face = "bold")))

#plot 2: Male
# Male subset (After fixing the time period)
ggsurvplot(survfit(Surv(lifespan, status) ~ Treatment, data = subset(df_cohort_1, Sex == "Male")),
           data = subset(df_cohort_1, Sex == "Male"),
           title = "Males: Isolation vs. Reproduction",
           xlab = "Adult Lifespan (Days)",
           ylab = "Survival Probability",
           subtitle = paste0("Cohort: Eclosed Jan 28 - Feb 5, 2025 (n = ", 
                             nrow(subset(df_cohort_1, Sex == "Male")), ")"),
           pval = TRUE,
           conf.int = TRUE,
           palette = c("lightblue", "pink"),
           legend.labs = c("Isolation", "Reproduction"),
           ggtheme = theme_bw() + 
             theme(plot.title = element_text(hjust = 0.5, face = "bold"),
                   plot.subtitle = element_text(hjust = 0.5, face = "bold")))


#Violin Plot
violin_plot <- ggplot(df_cohort_1, aes(x = Treatment, y = lifespan, fill = Treatment)) +
  
  # Violin layer (shows full distribution)
  geom_violin(trim = FALSE, alpha = 0.7, width = 0.9) +
  
  # Boxplot layer (shows median, IQR, outliers)
  geom_boxplot(width = 0.15, alpha = 0.5, 
               outlier.shape = 21, 
               outlier.size = 1.5, 
               outlier.fill = "white") +
  
  # Individual points (jittered to avoid overlap)
  geom_jitter(width = 0.08, size = 0.8, alpha = 0.4) +
  
  # Facet by sex
  facet_wrap(~ Sex, scales = "free_x") +
  
  # Custom colors
  scale_fill_manual(values = c("Isolation" = "#2E86AB", 
                               "Reproduction" = "#D64933")) +
  
  # Labels
  labs(
    title = "Adult Lifespan by Treatment and Sex",
    x = "Treatment Group",
    y = "Adult Lifespan (Days)",
    subtitle = paste0("Cohort: Eclosed Jan 28 - Feb 5, 2025 (n = ", 
                      nrow(subset(df_cohort_1)), ")"),
    fill = "Treatment"
  ) +
  
  # Clean theme
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 11, color = "gray40"),
    axis.title = element_text(size = 13),
    axis.text = element_text(size = 11),
    strip.text = element_text(size = 14, face = "bold"),
    legend.position = "bottom",
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 11),
    panel.grid.minor = element_blank(),
    panel.spacing = unit(1, "lines")
  )

# Display violin plot
print(violin_plot)




# Create a survival object
surv_obj <- Surv(time = df$lifespan, event = df$status)
head(surv_obj)

#The log rank test lets us test whether there is a difference in survival times 
#between groups of patients.
# Log rank test
table(df$Treatment)

#Visualize
s2 <- survfit(Surv(lifespan, status) ~ Treatment, data = df_cohort_1)

ggsurvplot(s2, data = df_cohort_1,
           size = 1,
           palette = c('#E7B800', '#2e9fdf'),
           censor.shape = '|', censor.size = 4,
           conf.int = TRUE,
           pval = TRUE,
           risk.table = TRUE,
           risk.table.col = 'strata',
           legend.labs = list('0' = 'Isolation', '1' = 'Reproduction'),
           risk.table.height = 0.25,
           title = "Log Rank Test by Treatment and Sex With Eclosion Timing",
           subtitle = paste0("Cohort: Eclosed Jan 28 - Feb 5, 2025 (n = ", 
                             nrow(subset(df_cohort_1)), ")"),
           ggtheme = theme_bw() +
             theme(
               plot.title = element_text(hjust = 0.5, face = "bold"),
               plot.subtitle = element_text(hjust = 0.5, face = "bold")
             ))


logrank_female <- survdiff(Surv(lifespan, status) ~ Treatment, 
                           data = subset(df_cohort_1, Sex == "Female"))
print(logrank_female)

logrank_male <- survdiff(Surv(lifespan, status) ~ Treatment, 
                         data = subset(df_cohort_1, Sex == "Male"))
print(logrank_male)



# Descriptive Summaries (MAIN ANALYSES WITH ECLOSION TIMING)
summary_step2 <- df_cohort_1 %>%
  group_by(Sex, Treatment) %>%
  summarise(
    n = n(),
    events = sum(status == 1, na.rm = TRUE),
    censored = sum(status == 0, na.rm = TRUE),
    mean = mean(lifespan, na.rm = TRUE),
    median = median(lifespan, na.rm = TRUE),
    IQR = IQR(lifespan, na.rm = TRUE),
    min = min(lifespan, na.rm = TRUE),
    max = max(lifespan, na.rm = TRUE),
    .groups = "drop"
  )

print(summary_step2)

#Cox Proportional Hazards Model
cox_step2 <- coxph(Surv(lifespan, status) ~ Sex * Treatment, data = df_cohort_1)
summary(cox_step2)

# Test Proportional Hazards Assumption
cox_ph_test <- cox.zph(cox_step2)
print(cox_ph_test)
#Expect to see high p value (null hypothesis: Hazard ratio is constant over time)
#High P-value-> Fail to reject-> Null Hypothesis Holds

#Gompertz Models by Sex
#Describes how the hazard changes over time
##How to read the table
#Shape:a-> a>0 hazard increasing; a=0 hazard is constant; a<0 hazard is decreasing
#Rate: The baseline hazard at time = 0 for the reference group
#treatmentReproduction: Log-hazard Ratio
#exp: Hazard Ratio (Exponential)

# Females
females_with_eclosion_1 <- df_cohort_1 %>% filter(Sex == "Female")
gompertz_f_with_eclosion_1 <- flexsurvreg(
  Surv(lifespan, status) ~ Treatment,
  data = females_with_eclosion_1,
  dist = "gompertz"
)
print(gompertz_f_with_eclosion_1)
summary(gompertz_f_with_eclosion_1)

# Males
males_with_eclosion_1 <- df_cohort_1 %>% filter(Sex == "Male")
gompertz_m_with_eclosion_1 <- flexsurvreg(
  Surv(lifespan, status) ~ Treatment,
  data = males_with_eclosion_1,
  dist = "gompertz"
)
print(gompertz_m_with_eclosion_1)
summary(gompertz_m_with_eclosion_1)




### Add Eclosion Timing as a factor [Late Reproduction]
#Filter to the cohort
df_cohort_2 <- df %>%
  filter(Eclosion >= as.Date("6-Feb", format = "%d-%b") & 
           Eclosion <= as.Date("19-Feb", format = "%d-%b")) %>%  
  # Ensure treatment is a factor with proper order
  mutate(
    treatment = factor(Treatment, levels = c("Isolation", "Reproduction")),
    sex = factor(Sex, levels = c("Female", "Male"))
  )

##Check Sample Size
sample_sizes <- df_cohort_2 %>%
  group_by(Sex, Treatment) %>%
  summarise(
    n = n(),
    deaths = sum(status == 1),
    censored = sum(status == 0),
    .groups = "drop"
  )

print("Sample sizes after Filtering:")
print(sample_sizes)

fit_group_2 <- survfit(Surv(lifespan, status) ~ group, data = df_cohort_2)
ggsurvplot(fit_group_1, data = df,
           pval = TRUE,
           conf.int = TRUE,
           xlab = "Adult Lifespan (Days)",
           ylab = "Survival Probability",
           title = "Survival by Treatment and Sex",
           subtitle = paste0("Cohort: Eclosed Feb 6 - Feb 19, 2025 (n = ", 
                             nrow(subset(df_cohort_2)), ")"),
           palette = c("blue", "lightblue", "red", "pink"),
           legend.title = "Group",
           legend.labs = c("Isolation Female", "Isolation Male", 
                           "Reproduction Female", "Reproduction Male"),
           ggtheme = theme_bw() + 
             theme(plot.title = element_text(hjust = 0.5, face = "bold"),
                   plot.subtitle = element_text(hjust = 0.5, face = "bold")))

# Sex-specific models
fit_female <- survfit(Surv(lifespan, status) ~ Treatment, 
                      data = subset(df_cohort_2, Sex == "Female"))

fit_male <- survfit(Surv(lifespan, status) ~ Treatment, 
                    data = subset(df_cohort_2, Sex == "Male"))

# Plot 1: Females
# Female subset (After Fixing the time period)
ggsurvplot(survfit(Surv(lifespan, status) ~ Treatment, 
                   data = subset(df_cohort_2, Sex == "Female")),
           data = subset(df_cohort_2, Sex == "Female"),
           title = "Females: Isolation vs. Reproduction",
           subtitle = paste0("Cohort: Eclosed Feb 6 - Feb 19, 2025 (n = ", 
                             nrow(subset(df_cohort_2, Sex == "Female")), ")"),
           xlab = "Adult Lifespan (Days)",
           ylab = "Survival Probability",
           pval = TRUE,
           conf.int = TRUE,
           palette = c("blue", "red"),
           legend.labs = c("Isolation", "Reproduction"),
           ggtheme = theme_bw() + 
             theme(plot.title = element_text(hjust = 0.5, face = "bold"),
                   plot.subtitle = element_text(hjust = 0.5, face = "bold")))

#plot 2: Male
# Male subset (After fixing the time period)
ggsurvplot(survfit(Surv(lifespan, status) ~ Treatment, data = subset(df_cohort_2, Sex == "Male")),
           data = subset(df_cohort_2, Sex == "Male"),
           title = "Males: Isolation vs. Reproduction",
           xlab = "Adult Lifespan (Days)",
           ylab = "Survival Probability",
           subtitle = paste0("Cohort: Eclosed Feb 6 - Feb 19, 2025 (n = ", 
                             nrow(subset(df_cohort_2, Sex == "Male")), ")"),
           pval = TRUE,
           conf.int = TRUE,
           palette = c("lightblue", "pink"),
           legend.labs = c("Isolation", "Reproduction"),
           ggtheme = theme_bw() + 
             theme(plot.title = element_text(hjust = 0.5, face = "bold"),
                   plot.subtitle = element_text(hjust = 0.5, face = "bold")))


#Violin Plot
violin_plot <- ggplot(df_cohort_2, aes(x = Treatment, y = lifespan, fill = Treatment)) +
  
  # Violin layer (shows full distribution)
  geom_violin(trim = FALSE, alpha = 0.7, width = 0.9) +
  
  # Boxplot layer (shows median, IQR, outliers)
  geom_boxplot(width = 0.15, alpha = 0.5, 
               outlier.shape = 21, 
               outlier.size = 1.5, 
               outlier.fill = "white") +
  
  # Individual points (jittered to avoid overlap)
  geom_jitter(width = 0.08, size = 0.8, alpha = 0.4) +
  
  # Facet by sex
  facet_wrap(~ Sex, scales = "free_x") +
  
  # Custom colors
  scale_fill_manual(values = c("Isolation" = "#2E86AB", 
                               "Reproduction" = "#D64933")) +
  
  # Labels
  labs(
    title = "Adult Lifespan by Treatment and Sex",
    x = "Treatment Group",
    y = "Adult Lifespan (Days)",
    subtitle = paste0("Cohort: Eclosed Feb 6 - Feb 19, 2025 (n = ", 
                      nrow(subset(df_cohort_2)), ")"),
    fill = "Treatment"
  ) +
  
  # Clean theme
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 11, color = "gray40"),
    axis.title = element_text(size = 13),
    axis.text = element_text(size = 11),
    strip.text = element_text(size = 14, face = "bold"),
    legend.position = "bottom",
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 11),
    panel.grid.minor = element_blank(),
    panel.spacing = unit(1, "lines")
  )

# Display violin plot
print(violin_plot)




# Create a survival object
surv_obj <- Surv(time = df$lifespan, event = df$status)
head(surv_obj)

#The log rank test lets us test whether there is a difference in survival times 
#between groups of patients.
# Log rank test
table(df$Treatment)

#Visualize
s2 <- survfit(Surv(lifespan, status) ~ Treatment, data = df_cohort_2)

ggsurvplot(s2, data = df_cohort_2,
           size = 1,
           palette = c('#E7B800', '#2e9fdf'),
           censor.shape = '|', censor.size = 4,
           conf.int = TRUE,
           pval = TRUE,
           risk.table = TRUE,
           risk.table.col = 'strata',
           legend.labs = list('0' = 'Isolation', '1' = 'Reproduction'),
           risk.table.height = 0.25,
           title = "Log Rank Test by Treatment and Sex With Eclosion Timing",
           subtitle = paste0("Cohort: Eclosed Feb 6 - Feb 19, 2025 (n = ", 
                             nrow(subset(df_cohort_2)), ")"),
           ggtheme = theme_bw() +
             theme(
               plot.title = element_text(hjust = 0.5, face = "bold"),
               plot.subtitle = element_text(hjust = 0.5, face = "bold")
             ))


logrank_female <- survdiff(Surv(lifespan, status) ~ Treatment, 
                           data = subset(df_cohort_2, Sex == "Female"))
print(logrank_female)

logrank_male <- survdiff(Surv(lifespan, status) ~ Treatment, 
                         data = subset(df_cohort_2, Sex == "Male"))
print(logrank_male)



# Descriptive Summaries (MAIN ANALYSES WITH ECLOSION TIMING)
summary_step2 <- df_cohort_2 %>%
  group_by(Sex, Treatment) %>%
  summarise(
    n = n(),
    events = sum(status == 1, na.rm = TRUE),
    censored = sum(status == 0, na.rm = TRUE),
    mean = mean(lifespan, na.rm = TRUE),
    median = median(lifespan, na.rm = TRUE),
    IQR = IQR(lifespan, na.rm = TRUE),
    min = min(lifespan, na.rm = TRUE),
    max = max(lifespan, na.rm = TRUE),
    .groups = "drop"
  )

print(summary_step2)

#Cox Proportional Hazards Model
cox_step2 <- coxph(Surv(lifespan, status) ~ Sex * Treatment, data = df_cohort_2)
summary(cox_step2)

# Test Proportional Hazards Assumption
cox_ph_test <- cox.zph(cox_step2)
print(cox_ph_test)
#Expect to see high p value (null hypothesis: Hazard ratio is constant over time)
#High P-value-> Fail to reject-> Null Hypothesis Holds

#Gompertz Models by Sex
#Describes how the hazard changes over time
##How to read the table
#Shape:a-> a>0 hazard increasing; a=0 hazard is constant; a<0 hazard is decreasing
#Rate: The baseline hazard at time = 0 for the reference group
#treatmentReproduction: Log-hazard Ratio
#exp: Hazard Ratio (Exponential)

# Females
females_with_eclosion_2 <- df_cohort_2 %>% filter(Sex == "Female")
gompertz_f_with_eclosion_2 <- flexsurvreg(
  Surv(lifespan, status) ~ Treatment,
  data = females_with_eclosion_2,
  dist = "gompertz"
)
print(gompertz_f_with_eclosion_2)
summary(gompertz_f_with_eclosion_2)

# Males
males_with_eclosion_2 <- df_cohort_2 %>% filter(Sex == "Male")
gompertz_m_with_eclosion_2 <- flexsurvreg(
  Surv(lifespan, status) ~ Treatment,
  data = males_with_eclosion_2,
  dist = "gompertz"
)
print(gompertz_m_with_eclosion_2)
summary(gompertz_m_with_eclosion_2)

# Gompertz Model Comparison (AIC)
cat("Females - No Eclosion: AIC =", AIC(gompertz_f), "\n")
cat("Females - With Early Eclosion Time: AIC =", AIC(gompertz_f_with_eclosion_1), "\n")
cat("Females - With Late Eclosion Time: AIC =", AIC(gompertz_f_with_eclosion_2), "\n")
cat("Males - No Eclosion: AIC =", AIC(gompertz_m), "\n")
cat("Males - with Early Eclosion Time: AIC =", AIC(gompertz_m_with_eclosion_1), "\n")
cat("Males - with Early Eclosion Time: AIC =", AIC(gompertz_m_with_eclosion_2), "\n")



