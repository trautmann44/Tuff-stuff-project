
rm(list = ls())

packages_needed <- c("ggplot2", "fitdistrplus", "car", "lmtest", "pwrss", "pastecs",
                     "broom", 'corrplot', 'rstatix', 'tidyr', 'dplyr', 'writexl', 
                     'gridExtra', 'Hmisc', 'tibble', 'RVAideMemoire', 'jtools', 
                     'modeest','EnvStats', 'irr', 'sandwich')

lapply(packages_needed, FUN = require, character.only = T)

# ----------------------------------------------- SIMULATED DATA ----------------------------------------------- #

simulated_data <- data.frame("DEXA_fat" = rnorm(26, 18, 6),
                      "DEXA_total_mass" = rnorm(26, 90, 10), 
                      "DEXA_fat_mass" = rnorm(26, 25, 5),
                      "DEXA_lean_mass" = rnorm(26, 70, 4),
                      "DEXA_bone_MC" = rnorm(26, 4, 1),
                      "END_vo2_max" = rnorm(26, 50, 5),
                      "END_maxHR_after_tread" = rnorm(26, 190, 10),
                      "END_cooper_dist" = rnorm(26, 3000, 250),
                      "END_maxHR_ACoop" = rnorm(26, 185, 10), 
                      "STR_push_ups" = round(rnorm(26, 30, 5), 0),
                      "STR_pull_ups" = round(rnorm(26, 13, 5), 0), 
                      "STR_sit_ups" = round(rnorm(26, 50, 10), 0), 
                      "STR_CMJ" = rnorm(26, 35, 7), 
                      "STR_SMT" = rnorm(26, 420, 50), 
                      "STR_1RM_deadlift" = rnorm(26, 200, 60), 
                      "STR_HS" = rnorm(26, 60, 15), 
                      "STR_shuttlerun" = rnorm(26, 35, 7), 
                      "TEST_STR_casualty_drag" = rnorm(26, 17, 5),
                      "TEST_STR_single_lift_mass" = rnorm(26, 50, 10), 
                      "TEST_END_water_can_carry" = rnorm(26, 180, 50), 
                      "TEST_STREND_lift_carry_time" = rnorm(26, 12, 3), 
                      "TEST_END_loaded_march" = rnorm(26, 16, 3), 
                      "TEST_STR_vehicle_casevac" = round(rnorm(26, 14, 4), 0))


# ----------------------------------------------- RAW DATA SET ------------------------------------------------ #

setwd("C:/Users/malir/OneDrive - Univerzita Karlova/Plocha/Statistics/Data")
# OR
setwd("E:/data/Statistics/Data")

#raw_data_01 <- readxl::read_excel("Data_vojaci.xlsx", sheet = "tuffstuff")
raw_data_01 <- readxl::read_excel("tuff_stuff_old.xlsx", sheet = "tuffstuff")
data_HG <- readxl::read_excel("ICC_HG.xlsx")
#raw_data_02 <- readxl::read_excel("tuff_stuff_new.xlsx", sheet = "tuffstuff")

raw_data_01 <- na.omit(raw_data_01)
#raw_data_02 <- na.omit(raw_data_02)



str(raw_data_01)

raw_data_01 <- as.data.frame(lapply(raw_data_01, as.numeric))
#raw_data_02 <- as.data.frame(lapply(raw_data_02, as.numeric))
warnings()

numerical_vars <- sapply(raw_data_01, is.numeric)
raw_data_01[, numerical_vars] <- round(raw_data_01[, numerical_vars], digits = 3)

raw_data <- raw_data_01
str(raw_data)
nrow(raw_data)
View(raw_data)

# ------------------------------------------------------------------------------------------------------------- #
# -------------------------------------------------- DESCRIPTIVES --------------------------------------------- #
# ------------------------------------------------------------------------------------------------------------- #

descriptive <- round(pastecs::stat.desc(raw_data, p = 0.95, norm = T), 3)
descriptive <- as.data.frame(descriptive)

descriptive$metric <- rownames(descriptive)
#descriptive <- descriptive[, c(33, 1:32)]
print(descriptive)

(descriptive_body <- descriptive[c(4:6, 8:10, 13, 15), c(1:12)])
(descriptive_body <- as.data.frame(t(descriptive_body)))

(descriptive_DV <- descriptive[c(4:6, 8:10, 13, 15), c(26:31)])
(descriptive_DV <- as.data.frame(t(descriptive_DV)))

(descriptive_tests <- descriptive[c(4:6, 8:10, 13, 15), c(13:25)])
(descriptive_tests <- as.data.frame(t(descriptive_tests)))

descriptive_all <- rbind(descriptive_body, descriptive_tests, descriptive_DV)
print(descriptive_all)

# ------------------------------------------------------------------------------------------------------------- #
# -------------------------------------- Variables correlation and reduction ---------------------------------- #
# ------------------------------------------------------------------------------------------------------------- #

# mode
#library(modeest)
mlv(raw_data$Push_ups, method = "mfv")
table(raw_data$Push_ups)

# IQR
# library(EnvStats)
IQR(raw_data$Push_ups)

(IQR <- apply(raw_data, 2, IQR))
(mode <- apply(raw_data, 2, mlv))

IQR <- as.data.frame(IQR)
mode <- as.data.frame(mode)


# ------------------------------------------------------------------------------------------------------------- #
# ------------------------------ checking the normality of all dependent variables ---------------------------- #

# ------------------------------------ normality among dependent variables ------------------------------------ #

normality_DV <- raw_data %>%
  shapiro_test(Casualty_Drag,
               Single_lift_mass_max,
               Water_can_carry,
               Repeated_LiftCarry,
               Fire_movement,
               Loaded_March) %>%
  arrange(variable)
print(as_tibble(normality_DV), n = nrow(normality_DV))
normality_DV <- normality_DV[c(1,5,6,4,2,3),]
# Because three of six variables are not normally distributed, we used Spearman Rank-Order Correlation.

# ----------------------------------- normality among strength and endurance tests --------------------------- #

normality_body <- raw_data %>%
  shapiro_test(ID, Age,
               Height,
               Weight,
               Sub_mass,
               Sub_lean,
               Sub_fat, 
               Sub_BMC,
               Sub_fat_percent,
               Sub_lean_percent,
               Sub_BMC_percent,
               Sub_BMD) %>%
  arrange(variable)
print(as_tibble(normality_body), n = nrow(normality_body))
normality_body <- normality_body[c(3,1,2,12,11,9,7,4,8,10,5,6),]
# The Sub_fat variable is barely not-normally distributed, thus we used Spearmans rho

# ----------------------------------- normality among strength and endurance tests --------------------------- #

normality_tests <- raw_data %>%
  shapiro_test(VC,
               VO2max_L_min,
               VO2max_ml_kg_min,
               Dist_coop, 
               Push_ups, 
               Pull_ups, 
               Sit_ups,
               CMJ, 
               Seated_medball_throw, 
               X1RM_Deadlift,
               HandGrip_str_L,
               HandGrip_str_R,
               ShuttleRun) %>%
  arrange(variable)
print(as_tibble(normality_tests), n = nrow(normality_tests))
normality_tests <- normality_tests[c(10,11,12,2,6,5,9,1,7,13,3,4,8),]

# The Push_ups variable is barely not-normally distributed, thus we used Spearmans rho

normality_all <- rbind(normality_body, normality_tests, normality_DV)

normality_all <- round(normality_all[,c('statistic', 'p')], 3)
print(normality_all)

(descriptives <- cbind(descriptive_all, normality_all, IQR, mode))

descriptives <- tibble::rownames_to_column(descriptives, var = "variable")

print(descriptives)

# ------------------------------------------------------------------------------------------------------------- #


shapiro.test(raw_data_01$Push_ups)

IQR(raw_data_01$Fire_movement)

write_xlsx(descriptives, "E:/data/Statistics/Data/decsriptives.xlsx")
write_xlsx(descriptives, "C:/Users/malir/OneDrive - Univerzita Karlova/Plocha/Data/decsriptives.xlsx")

# ------------------------------------------------------------------------------------------------------------- #
# ------------------------------ checking the correlation of all variables ------------------------------------ #


ggplot(raw_data, aes(x = Casualty_Drag)) + geom_density()

# checking the correlation between all response variables.
cor_responses <- Hmisc::rcorr(as.matrix(raw_data), type = "spearman")


corrplot::corrplot(cor_responses$r[26:31, 26:31],
                   #p.mat = cor_responses_DV$P[5:12,5:12],
                   sig.level = c(0.001, 0.01, 0.05), insig = 'label_sig',
                   method = "color", diag = T, tl.col="black",addCoef.col="black")


shapiro.test(raw_data$Repeated_LiftCarry)
cor.test(raw_data$Loaded_March, raw_data$Repeated_LiftCarry, method = "spearman")

RVAideMemoire::spearman.ci(raw_data$Loaded_March, raw_data$Repeated_LiftCarry, nrep = 1000, conf.level = 0.95)

#cor.test# We see that relationships are not so strong, so we can utilize all response variables.

# ---------------------------- correlation among body characteristics and composition ------------------------- #

corrplot::corrplot(cor_responses$r[4:12, 4:12],
                   #p.mat = cor_responses_DV$P[5:12,5:12],
                   sig.level = c(0.001, 0.01, 0.05), insig = 'label_sig',
                   method = "color", diag = T, tl.col="black",addCoef.col="black")



# ------------------------------ correlation among strength and endurance tests ------------------------------- #

corrplot::corrplot(cor_responses$r[13:25, 13:25],
                   #p.mat = cor_responses_DV$P[5:12,5:12],
                   sig.level = c(0.001, 0.01, 0.05), insig = 'label_sig',
                   method = "color", diag = T, tl.col="black",addCoef.col="black", tl.cex = 0.7)



# ----------------------------------------------- ICC HG ------------------------------------------------------ #


ICC_HG_wide <- data_HG %>%
  pivot_wider(
    id_cols = c(ID, Name),
    names_from = Repetition,
    values_from = c(HG_left, HG_right),
    names_glue = "{.value}_{Repetition}"
  )

head(ICC_HG_wide)

HG_left <- irr::icc(ICC_HG_wide[, c('HG_left_1', 'HG_left_2', 'HG_left_3')],
                        model = "oneway", type = "consistency", unit = "average")
HG_left_summary <- cbind("ICC value" = round(HG_left$value, 3),
                             "Lower 95%CI" = round(HG_left$lbound, 3),
                             "Upper 95%CI" = round(HG_left$ubound, 3),
                             "p-value" = HG_left$p.value)
rownames(HG_left_summary) <- "HG_left_summary"


HG_right <- irr::icc(ICC_HG_wide[, c('HG_right_1', 'HG_right_2', 'HG_right_3')],
                    model = "oneway", type = "consistency", unit = "average")
HG_right_summary <- cbind("ICC value" = round(HG_right$value, 3),
                         "Lower 95%CI" = round(HG_right$lbound, 3),
                         "Upper 95%CI" = round(HG_right$ubound, 3),
                         "p-value" = HG_right$p.value)
rownames(HG_right_summary) <- "HG_right_summary"

print(HG_left_summary)
print(HG_right_summary)


# hand grip average making
shapiro.test(raw_data$HandGrip_str_L)
shapiro.test(raw_data$HandGrip_str_R)

ggplot(data = raw_data, aes(x = HandGrip_str_L, y = HandGrip_str_R)) + geom_point() + geom_abline()

(cor_handgrip <- cor.test(raw_data$HandGrip_str_L, raw_data$HandGrip_str_R, method = "pearson"))

raw_data$Handgrip_avg <- rowMeans(raw_data[, c('HandGrip_str_L', 'HandGrip_str_R')], na.rm = T)

# coefficient of variation (percentage)

(HG_left_CV <- (sd(raw_data_01$HandGrip_str_L) / mean(raw_data_01$HandGrip_str_L)) * 100)
(HG_right_CV <- (sd(raw_data_01$HandGrip_str_R) / mean(raw_data_01$HandGrip_str_R)) * 100)

# ----------------------------------------------- ICC of CMJ & SMBT ------------------------------------------- #

rep_measures <- readxl::read_excel('E:/data/Statistics/Data/tuff_stuff_jump_smbt_with_ICC.xlsx', 
                                   sheet = 'Data')

str(rep_measures)

rep_measures_wide <- rep_measures %>%
  pivot_wider(
    id_cols = c(ID, Name),
    names_from = Repetition,
    values_from = c(Jump_Height, SMBT),
    names_sep = "_"
  )


ICC_CMJ <- irr::icc(rep_measures_wide[, c('Jump_Height_1', 'Jump_Height_2', 'Jump_Height_3')],
                    model = "twoway", type = "consistency", unit = "average")
ICC_SMBT <- irr::icc(rep_measures_wide[, c('SMBT_1', 'SMBT_2', 'SMBT_3')],
                     model = "twoway", type = "consistency", unit = "average")
ICC_CMJ
ICC_SMBT

# ------------------------------------------------------------------------------------------------------------- #
# -------------------------------------------------- REGRESSIONS ---------------------------------------------- #
# ------------------------------------------------------------------------------------------------------------- #

# 1st model: loaded_march ~ cooper_distance + sit_ups + pull_ups + push_ups

# Assumptions:

# 1 distribution of the DV (normal or not; if not - which?)
# 2 multicollienarity of the predictors
# 3 heteroscedasticity
# 4 independence of errors
# 5 normal distributions of errors
# 6 influential data points detection



# ------------------------------------------------------------------------------------------------------------- #

# -------------------------------------------- 1 distribution of the DV ------------------------------- #

load_norm <- fitdist(raw_data$Loaded_March, distr = "norm")
load_lnorm <- fitdist(raw_data$Loaded_March, distr = "lnorm")
plot(load_norm)
plot(load_lnorm)
?fitdist
(shapiro.test(raw_data$Loaded_March))

# ----------------------------------------------------------------------------------------------------------------------- #
# --------------------------------------------- model 1 ----------------------------------------------------------------- #
# ----------------------------------------------------------------------------------------------------------------------- #

str(raw_data)
model_1a <- lm(Loaded_March ~ Weight + Sub_fat_percent + VO2max_ml_kg_min + ShuttleRun + Dist_coop,
              data = raw_data)

# ---------------------------------------- 2 multicollienarity of the predictors ---------------------- #

# The threshold set to 5.0 according to Akinwande et al. (2015).

(VIF_model_1a <- car::vif(model_1a))
par(mar = c(5, 10, 4, 8) + 0.2)
barplot(VIF_model_1a, main = 'VIF values for the First model',beside=TRUE, horiz = T,
        col = 'red', xlim = c(0,7), border = "black", axes = T,
        cex.names = 1.0, las = 1, xlab = "Variance Inflation Factor")
abline(v = 5, col = "black", lty = 2)

# correlation of Cooper distance and VO2 max

# WO multicollinearity removing the VO2max_ml_kg_min

model_1 <- lm(Loaded_March ~ Weight + Sub_fat_percent + ShuttleRun + Dist_coop,
              data = raw_data)

model_1$coefficients[-1]

summary(model_1)
(confs_model_1 <- confint(model_1))

(confs_model_1_low <- confs_model_1[, 1])
confs_model_1_low[-1]
(confs_model_1_upp <- confs_model_1[, 2])

confs_model_1_upp[-1]

tidy(model_1, conf.int = T)

glance(model_1)

augmented <- augment(model_1)
print(augmented, n = Inf)

# making and printing table with all results of coefficients ------------------------------------------- #
(confs_model_1 <- confint(model_1))
(estimates <- coef(summary(model_1)))
(model_1_info <- cbind(estimates, confs_model_1))
model_1_info <- as.data.frame(model_1_info)
model_1_info <- model_1_info %>%
  rename('Coefficients (?)' = 'Estimate', 'p value' = 'Pr(>|t|)', 'CI lower' = '2.5 %', 'CI upper' = '97.5 %')
print(model_1_info)
model_1_info <- model_1_info %>%
  mutate_if(is.numeric, round, digits = 5)
print(model_1_info)
model_1_info$Predictor <- rownames(model_1_info)
model_1_info <- model_1_info[, c(7, 1:6)]
print(model_1_info)
write_xlsx(model_1_info, "E:/data/Statistics/Data/model_1_info.xlsx")

# ------------------------------------------------------------------------------------------------------ #

# plot contributions of coefficients with CI's

model_1_coeff <- data.frame(mean = model_1$coefficients[-1],
                   lower = confs_model_1_low[-1],
                   upper = confs_model_1_upp[-1],
                   variable = c("Weight", "Sub_fat_percent", "ShuttleRun", "Dist_coop"))


model_1_coeff$variable <- factor(model_1_coeff$variable, levels = model_1_coeff$variable)

# plot contributions of coefficients with CI's
(plot_coeff <- ggplot(model_1_coeff, aes(x = variable, y = mean, ymin = lower, ymax = upper)) +
  geom_point(position = position_dodge(width = 0.2)) +
  geom_errorbar(position = position_dodge(width = 0.2), width = 0.1) +
  coord_flip()+
  theme_classic()+
  scale_y_continuous(limits = c(-50.0,20.0))+
  geom_hline(yintercept = 0.0, lwd = 0.2, lty = 2)+
  theme(text = element_text(size=12, family="Comic Sans MS", face = "bold"), plot.margin = margin(1, 1, 3, 1, "cm"),
        axis.ticks.y = element_blank(),
        panel.grid = element_blank(),element_line(linetype = 1),
        axis.line.y = element_blank(),axis.title.x = element_text(vjust = -5, hjust = 0.2),
        legend.position = "none")+
  ylab(expression(paste("")))+
  xlab(""))

# --------  Dist coop #

model_1$coefficients[-1][4]


model_1_coeff_coop_dist <- data.frame(mean = model_1$coefficients[-1][4],
                            lower = confs_model_1_low[-1][4],
                            upper = confs_model_1_upp[-1][4],
                            variable = "Dist_coop")

(plot_coeff_coop_dist <- ggplot(model_1_coeff_coop_dist, aes(x = variable, y = mean, ymin = lower, ymax = upper)) +
  geom_point(position = position_dodge(width = 0.2)) +
  geom_errorbar(position = position_dodge(width = 0.1), width = 0.05) +
  coord_flip()+
  theme_classic()+
  scale_y_continuous(limits = c(-0.5,0.5))+
  geom_hline(yintercept = 0.0, lwd = 0.2, lty = 2)+
  theme(text = element_text(size=12, family="Comic Sans MS", face = "bold"), plot.margin = margin(1, 1, 3, 1, "cm"),
        axis.ticks.y = element_blank(),
        panel.grid = element_blank(),element_line(linetype = 1),
        axis.line.y = element_blank(),axis.title.x = element_text(vjust = -5, hjust = 0.2),
        legend.position = "none")+
  ylab(expression(paste("")))+
  xlab(""))

grid.arrange(plot_coeff, plot_coeff_coop_dist, nrow = 1,
             bottom = "Estimates of coefficients and their 95% confidence intervals")

# --------- Plotting the cooper distance slope

(eff_plot_1 <- jtools::effect_plot(model = model_1,
                    pred = Dist_coop,
                    data = raw_data,
                    interval = T,
                    plot.points = T,
                    x.label = "Cooper test (m)", y.label = "Loaded march (m)") +
    annotate("text", 
             x = -Inf, y = -Inf, 
             label = "(A)", 
             hjust = -4.5, vjust = -14.5, 
             size = 5, fontface = "bold"))

# ---------------------------------------- 2 multicollienarity of the predictors ---------------------- #

# The threshold set to 5.0 according to Akinwande et al. (2015).

(VIF_model_1 <- car::vif(model_1))
par(mar = c(5, 10, 4, 8) + 0.2)
barplot(VIF_model_1, main = 'VIF values for the First model',beside=TRUE, horiz = T,
        col = 'red', xlim = c(0,7), border = "black", axes = T,
        cex.names = 1.0, las = 1, xlab = "Variance Inflation Factor")
abline(v = 5, col = "black", lty = 2)

# correlation of Cooper distance and VO2 max


# -------------------------------------------- 3 heteroscedasticity ----------------------------------- #

(car::ncvTest(model = model_1))
plot(model_1)

# ---------------------------------------- 4 independence of errors ----------------------------------- #

lmtest::dwtest(Loaded_March ~ Weight + Sub_fat_percent + ShuttleRun + Dist_coop,
       data = raw_data)

# ---------------------------------------- 5 normal distributions of errors --------------------------- #

model_1_residuals <- resid(model_1)
qqnorm(model_1_residuals)
qqline(model_1_residuals)

# with 95%CIs
car::qqPlot(model_1_residuals, xlab = "Quantiles", ylab = "Model 1 residuals", grid = F, pch = 20)


# --------------------------------------- 6 influential data points detection ---------------------------- #

# "As a rule of thumb, cases are regarded as too influential if the associated value for Cook's Distance exceeds the cut-off
# value of (Van der Meer et al., 2010):" 4/n  (or 4/ (N-k-1)) in which n refers to the number of groups in the grouping factor 
# under evaluation. 

N = nrow(raw_data)
k = 4
(cutoff = 4/(N-k-1))

# plot the Cook's distances of all data points
cooks_d_model_1 <- cooks.distance(model_1)
plot(cooks_d_model_1, type = "b", ylab = "Cook's Distance",
     xlab = "Observation Index", pch = 20, col = "red", ylim = c(0, 0.2), )
abline(h = cutoff, col = "black", lty = 2)

# which points have larger Cook's distance than the threshold (instance > 0.167)
which(cooks_d_model_1 > cutoff)

# model without influential points
model_1_WO_IP <- lm(Loaded_March ~ Weight + Sub_fat_percent + ShuttleRun + Dist_coop,
                                    data = raw_data[-c(11,26), ])
summary(model_1_WO_IP)

(confs_model_1_WO_IP <- confint(model_1_WO_IP))
(estimates_WO_IP_1 <- coef(summary(model_1_WO_IP)))
(model_1_info_WO_IP <- cbind(estimates_WO_IP_1, confs_model_1_WO_IP))
model_1_info_WO_IP <- as.data.frame(model_1_info_WO_IP)
model_1_info_WO_IP <- model_1_info_WO_IP %>%
  rename('Coefficients (?)' = 'Estimate', 'p value' = 'Pr(>|t|)', 'CI lower' = '2.5 %', 'CI upper' = '97.5 %')
print(model_1_info_WO_IP)
model_1_info_WO_IP <- model_1_info_WO_IP %>%
  mutate_if(is.numeric, round, digits = 5)
print(model_1_info_WO_IP)
model_1_info_WO_IP$Predictor <- rownames(model_1_info_WO_IP)
model_1_info_WO_IP <- model_1_info_WO_IP[, c(7, 1:6)]
print(model_1_info_WO_IP)
write_xlsx(model_1_info_WO_IP, "E:/data/Statistics/Data/model_1_info_WO_IP.xlsx")

# percentage differences between both models
100*(coef(model_1_WO_influential_points) - coef(model_1)) / coef(model_1)

# ----------------------------------------------------------------------------------------------------------------------- #
# --------------------------------------------- model 2 ----------------------------------------------------------------- #
# ----------------------------------------------------------------------------------------------------------------------- #

str(raw_data)
model_2 <- lm(Water_can_carry ~ Weight + Sub_fat_percent + ShuttleRun + Dist_coop,
              data = raw_data)

summary(model_2)

tidy(model_2, conf.int = T)

glance(model_2)

augmented_2 <- augment(model_2)
print(augmented_2, n = Inf)

##
model_2$coefficients[-1]

(confs_model_2 <- confint(model_2))

(confs_model_2_low <- confs_model_2[, 1])
confs_model_2_low[-1]
(confs_model_2_upp <- confs_model_2[, 2])
confs_model_2_upp[-1]

# making and printing table with all results of coefficients ------------------------------------------- #
(confs_model_2 <- confint(model_2))
(estimates <- coef(summary(model_2)))
(model_2_info <- cbind(estimates, confs_model_2))
model_2_info <- as.data.frame(model_2_info)
model_2_info <- model_2_info %>%
  rename('Coefficients (?)' = 'Estimate', 'p value' = 'Pr(>|t|)', 'CI lower' = '2.5 %', 'CI upper' = '97.5 %')
print(model_2_info)
model_2_info <- model_2_info %>%
  mutate_if(is.numeric, round, digits = 5)
print(model_2_info)
model_2_info$Predictor <- rownames(model_2_info)
model_2_info <- model_2_info[, c(7, 1:6)]
print(model_2_info)
write_xlsx(model_2_info, "E:/data/Statistics/Data/model_2_info.xlsx")
# ------------------------------------------------------------------------------------------------------ #
# plot contributions of coefficients with CI's

model_2_coeff <- data.frame(mean = model_2$coefficients[-1],
                            lower = confs_model_2_low[-1],
                            upper = confs_model_2_upp[-1],
                            variable = c("Weight", "Sub_fat_percent", "ShuttleRun", "Dist_coop"))


model_2_coeff$variable <- factor(model_2_coeff$variable, levels = model_2_coeff$variable)

# plot contributions of coefficients with CI's
(plot_coeff_2 <- ggplot(model_2_coeff, aes(x = variable, y = mean, ymin = lower, ymax = upper)) +
    geom_point(position = position_dodge(width = 0.2)) +
    geom_errorbar(position = position_dodge(width = 0.2), width = 0.1) +
    coord_flip()+
    theme_classic()+
    scale_y_continuous(limits = c(-30.0,12.0))+
    geom_hline(yintercept = 0.0, lwd = 0.2, lty = 2)+
    theme(text = element_text(size=12, family="Comic Sans MS", face = "bold"), plot.margin = margin(1, 1, 3, 1, "cm"),
          axis.ticks.y = element_blank(),
          panel.grid = element_blank(),element_line(linetype = 1),
          axis.line.y = element_blank(),axis.title.x = element_text(vjust = -5, hjust = 0.2),
          legend.position = "none")+
    ylab(expression(paste("")))+
    xlab(""))

# --------  Dist coop #

model_2$coefficients[-1][4]


model_2_coeff_coop_dist <- data.frame(mean = model_2$coefficients[-1][4],
                                      lower = confs_model_2_low[-1][4],
                                      upper = confs_model_2_upp[-1][4],
                                      variable = "Dist_coop")

(plot_coeff_coop_dist_2 <- ggplot(model_2_coeff_coop_dist, aes(x = variable, y = mean, ymin = lower, ymax = upper)) +
    geom_point(position = position_dodge(width = 0.2)) +
    geom_errorbar(position = position_dodge(width = 0.1), width = 0.05) +
    coord_flip()+
    theme_classic()+
    scale_y_continuous(limits = c(-0.1,0.2))+
    geom_hline(yintercept = 0.0, lwd = 0.2, lty = 2)+
    theme(text = element_text(size=12, family="Comic Sans MS", face = "bold"), plot.margin = margin(1, 1, 3, 1, "cm"),
          axis.ticks.y = element_blank(),
          panel.grid = element_blank(),element_line(linetype = 1),
          axis.line.y = element_blank(),axis.title.x = element_text(vjust = -5, hjust = 0.2),
          legend.position = "none")+
    ylab(expression(paste("")))+
    xlab(""))


grid.arrange(plot_coeff_2, plot_coeff_coop_dist_2, nrow = 1,
             bottom = "Estimates of coefficients and their 95% confidence intervals")

# --------- Plotting the weight slope

(eff_plot_2 <- jtools::effect_plot(model = model_2,
                    pred = Weight,
                    data = raw_data,
                    interval = T,
                    plot.points = T, x.label = "Body mass (kg)", y.label = "Water can carry (m)") +
    annotate("text", 
             x = -Inf, y = -Inf, 
             label = "(B)", 
             hjust = -4.5, vjust = -14.5, 
             size = 5, fontface = "bold"))

# ---------------------------------------- 2 multicollienarity of the predictors ---------------------- #

(VIF_model_2 <- car::vif(model_2))
par(mar = c(5, 10, 4, 8) + 0.2)
barplot(VIF_model_2, main = 'VIF values for the First model',beside=TRUE, horiz = T,
        col = 'red', xlim = c(0,7), border = "black", axes = T,
        cex.names = 1.0, las = 1, xlab = "Variance Inflation Factor")
abline(v = 5, col = "black", lty = 2)

# -------------------------------------------- 3 heteroscedasticity ----------------------------------- #

(car::ncvTest(model = model_2))
plot(model_2)

# ---------------------------------------- 4 independence of errors ----------------------------------- #

dwtest(Water_can_carry ~ Weight + Sub_fat_percent + ShuttleRun + Dist_coop,
       data = raw_data)

# ---------------------------------------- 5 normal distributions of errors --------------------------- #

model_2_residuals <- resid(model_2)
qqnorm(model_2_residuals)
qqline(model_2_residuals)

# with 95%CIs
car::qqPlot(model_2_residuals, xlab = "Quantiles", ylab = "Model 2 residuals", grid = F, pch = 20)

# --------------------------------------- 6 influential data points detection ---------------------------- #


# "As a rule of thumb, cases are regarded as too influential if the associated value for Cook's Distance exceeds the cut-off
# value of (Van der Meer et al., 2010):" 4/n  (or 4/ (N-k-1)) in which n refers to the number of groups in the grouping factor 
# under evaluation. 

N = nrow(raw_data)
k = 4
(cutoff = 4/(N-k-1))

# plot the Cook's distances of all data points
cooks_d_model_2 <- cooks.distance(model_2)
plot(cooks_d_model_2, type = "b", ylab = "Cook's Distance",
     xlab = "Observation Index", pch = 20, col = "red")
abline(h = cutoff,lty = 2)

# which points have larger Cook's distance than the threshold (instance > 0.167)
which(cooks_d_model_2 > cutoff)

round(cooks_d_model_2[c(11, 23, 25, 26)], digits = 4)
# model without influential points
model_2_WO_IP <- lm(Water_can_carry ~ Weight + Sub_fat_percent + ShuttleRun + Dist_coop,
                                    data = raw_data[-c(11, 23, 25), ])

summary(model_2_WO_IP)


tidy(model_2_WO_IP, conf.int = T)

glance(model_2_WO_IP)

augmented_2_WO_IP <- augment(model_2_WO_IP)
print(augmented_2_WO_IP, n = Inf)

##
model_2_WO_IP$coefficients[-1]

(confs_model_2_WO_IP <- confint(model_2_WO_IP))

(confs_model_2_low_WO_IP <- confs_model_2_WO_IP[, 1])
confs_model_2_low_WO_IP[-1]
(confs_model_2_upp_WO_IP<- confs_model_2_WO_IP[, 2])
confs_model_2_upp_WO_IP[-1]

(confs_model_2_WO_IP <- confint(model_2_WO_IP))
(estimates_WO_IP_2 <- coef(summary(model_2_WO_IP)))
(model_2_info_WO_IP <- cbind(estimates_WO_IP_2, confs_model_2_WO_IP))
model_2_info_WO_IP <- as.data.frame(model_2_info_WO_IP)
model_2_info_WO_IP <- model_2_info_WO_IP %>%
  rename('Coefficients (?)' = 'Estimate', 'p value' = 'Pr(>|t|)', 'CI lower' = '2.5 %', 'CI upper' = '97.5 %')
print(model_2_info_WO_IP)
model_2_info_WO_IP <- model_2_info_WO_IP %>%
  mutate_if(is.numeric, round, digits = 5)
print(model_2_info_WO_IP)
model_2_info_WO_IP$Predictor <- rownames(model_2_info_WO_IP)
model_2_info_WO_IP <- model_2_info_WO_IP[, c(7, 1:6)]
print(model_2_info_WO_IP)
write_xlsx(model_2_info_WO_IP, "E:/data/Statistics/Data/model_2_info_WO_IP.xlsx")


# plot contributions of coefficients with CI's

model_2_coeff_WO_IP <- data.frame(mean = model_2_WO_IP$coefficients[-1],
                            lower = confs_model_2_low_WO_IP[-1],
                            upper = confs_model_2_upp_WO_IP[-1],
                            variable = c("Weight", "Sub_fat_percent", "ShuttleRun", "Dist_coop"))


model_2_coeff_WO_IP$variable <- factor(model_2_coeff_WO_IP$variable, levels = model_2_coeff_WO_IP$variable)

# plot contributions of coefficients with CI's
(plot_coeff_2_WO_IP <- ggplot(model_2_coeff_WO_IP, aes(x = variable, y = mean, ymin = lower, ymax = upper)) +
    geom_point(position = position_dodge(width = 0.2)) +
    geom_errorbar(position = position_dodge(width = 0.2), width = 0.1) +
    coord_flip()+
    theme_classic()+
    scale_y_continuous(limits = c(-24.0,12.0))+
    geom_hline(yintercept = 0.0, lwd = 0.2, lty = 2)+
    theme(text = element_text(size=12, family="Comic Sans MS", face = "bold"), plot.margin = margin(1, 1, 3, 1, "cm"),
          axis.ticks.y = element_blank(),
          panel.grid = element_blank(),element_line(linetype = 1),
          axis.line.y = element_blank(),axis.title.x = element_text(vjust = -5, hjust = 0.2),
          legend.position = "none")+
    ylab(expression(paste("")))+
    xlab(""))

# --------  Dist coop #

model_2_WO_IP$coefficients[-1][4]


model_2_coeff_coop_dist_WO_IP <- data.frame(mean = model_2_WO_IP$coefficients[-1][4],
                                      lower = confs_model_2_low_WO_IP[-1][4],
                                      upper = confs_model_2_upp_WO_IP[-1][4],
                                      variable = "Dist_coop")

(plot_coeff_coop_dist_2_WO_IP <- ggplot(model_2_coeff_coop_dist_WO_IP, 
                                        aes(x = variable, y = mean, ymin = lower, ymax = upper)) +
    geom_point(position = position_dodge(width = 0.2)) +
    geom_errorbar(position = position_dodge(width = 0.1), width = 0.05) +
    coord_flip()+
    theme_classic()+
    scale_y_continuous(limits = c(-0.1,0.2))+
    geom_hline(yintercept = 0.0, lwd = 0.2, lty = 2)+
    theme(text = element_text(size=12, family="Comic Sans MS", face = "bold"), plot.margin = margin(1, 1, 3, 1, "cm"),
          axis.ticks.y = element_blank(),
          panel.grid = element_blank(),element_line(linetype = 1),
          axis.line.y = element_blank(),axis.title.x = element_text(vjust = -5, hjust = 0.2),
          legend.position = "none")+
    ylab(expression(paste("")))+
    xlab(""))


grid.arrange(plot_coeff_2_WO_IP, plot_coeff_coop_dist_2_WO_IP, nrow = 1,
             bottom = "Estimates of coefficients and their 95% confidence intervals")




# percentage differences between both models
100*(coef(model_2_WO_IP) - coef(model_2)) / coef(model_2)


# -------------------------------------------- 3 heteroscedasticity ----------------------------------- #

(car::ncvTest(model = model_2_WO_IP))
plot(model_2_WO_IP)

model_2_WO_IP_log <- lm(log(Water_can_carry) ~ Weight + Sub_fat_percent + ShuttleRun + Dist_coop,
                        data = raw_data[-c(11, 23, 25), ])
(car::ncvTest(model = model_2_WO_IP_log))

# ---------------------------------------- 4 independence of errors ----------------------------------- #

dwtest(Water_can_carry ~ Weight + Sub_fat_percent + ShuttleRun + Dist_coop,
       data = raw_data[-c(11, 23, 25), ])

# ---------------------------------------- 5 normal distributions of errors --------------------------- #

model_2_residuals_WO_IP <- resid(model_2_WO_IP)
qqnorm(model_2_residuals_WO_IP)
qqline(model_2_residuals_WO_IP)

# with 95%CIs
car::qqPlot(model_2_residuals_WO_IP, xlab = "Quantiles", ylab = "Model 2 residuals", grid = F, pch = 20)

# --------------------------------------- 6 influential data points detection ---------------------------- #

# ----------------------------------------------------------------------------------------------------------------------- #
# --------------------------------------------- model 3 ----------------------------------------------------------------- #
# ----------------------------------------------------------------------------------------------------------------------- #

str(raw_data)

model_3a <- lm(Casualty_Drag ~ Weight + Sub_fat_percent + Push_ups + Pull_ups + Sit_ups + CMJ +
                Seated_medball_throw + X1RM_Deadlift + Handgrip_avg, data = raw_data)

summary(model_3a)

tidy(model_3a, conf.int = T)

glance(model_3a)

augmented_3a <- augment(model_3a)
print(augmented_3a, n = Inf)



# ---------------------------------------- 2 multicollinearity of the predictors ---------------------- #

(VIF_model_3a <- car::vif(model_3a))
par(mar = c(5, 10, 4, 8) + 0.2)
barplot(VIF_model_3a, main = 'VIF values',beside=TRUE, horiz = T,
        col = 'red', xlim = c(0,7), border = "black", axes = T,
        cex.names = 0.8, las = 1)
abline(v = 5, col = "black", lty = 2)

# WO multicollinearity

model_3 <- lm(Casualty_Drag ~ Sub_fat_percent + Pull_ups + Sit_ups + CMJ +
                 Seated_medball_throw + X1RM_Deadlift + Handgrip_avg, data = raw_data)

summary(model_3)

tidy(model_3, conf.int = T)

glance(model_3)

augmented_3 <- augment(model_3)
print(augmented_3, n = Inf)

(VIF_model_3 <- car::vif(model_3))
par(mar = c(5, 10, 4, 8) + 0.2)
barplot(VIF_model_3, main = 'VIF values',beside=TRUE, horiz = T,
        col = 'red', xlim = c(0,7), border = "black", axes = T,
        cex.names = 0.8, las = 1)
abline(v = 5, col = "black", lty = 2)

##

model_3$coefficients[-1]

(confs_model_3 <- confint(model_3))

(confs_model_3_low <- confs_model_3[, 1])
confs_model_3_low[-1]
(confs_model_3_upp <- confs_model_3[, 2])
confs_model_3_upp[-1]

# making and printing table with all results of coefficients ------------------------------------------- #
(confs_model_3 <- confint(model_3))
(estimates_3 <- coef(summary(model_3)))
(model_3_info <- cbind(estimates_3, confs_model_3))
model_3_info <- as.data.frame(model_3_info)
model_3_info <- model_3_info %>%
  rename('Coefficients (?)' = 'Estimate', 'p value' = 'Pr(>|t|)', 'CI lower' = '2.5 %', 'CI upper' = '97.5 %')
print(model_3_info)
model_3_info <- model_3_info %>%
  mutate_if(is.numeric, round, digits = 5)
print(model_3_info)
model_3_info$Predictor <- rownames(model_3_info)
model_3_info <- model_3_info[, c(7, 1:6)]
print(model_3_info)
write_xlsx(model_3_info, "E:/data/Statistics/Data/model_3_info.xlsx")
# ------------------------------------------------------------------------------------------------------ #
# plot contributions of coefficients with CI's

model_3_coeff <- data.frame(mean = model_3$coefficients[-1],
                            lower = confs_model_3_low[-1],
                            upper = confs_model_3_upp[-1],
                            variable = c("Sub_fat_percent", "Pull_ups", 
                                         "Sit_ups", "CMJ", "Seated_medball_throw",
                                         "X1RM_Deadlift", "Handgrip_avg"))


model_3_coeff$variable <- factor(model_3_coeff$variable, levels = model_3_coeff$variable)

# plot contributions of coefficients with CI's
(plot_coeff_3 <- ggplot(model_3_coeff, aes(x = variable, y = mean, ymin = lower, ymax = upper)) +
    geom_point(position = position_dodge(width = 0.2)) +
    geom_errorbar(position = position_dodge(width = 0.2), width = 0.1) +
    coord_flip()+
    theme_classic()+
    scale_y_continuous(limits = c(-3.0,2.5))+
    geom_hline(yintercept = 0.0, lwd = 0.2, lty = 2)+
    theme(text = element_text(size=12, family="Comic Sans MS", face = "bold"), plot.margin = margin(1, 1, 3, 1, "cm"),
          axis.ticks.y = element_blank(),
          panel.grid = element_blank(),element_line(linetype = 1),
          axis.line.y = element_blank(),axis.title.x = element_text(vjust = -5, hjust = 0.2),
          legend.position = "none")+
    ylab(expression(paste("Estimates of coefficients and their 95% confidence intervals")))+
    xlab(""))


# -------------------------------------------- 3 heteroscedasticity ----------------------------------- #

(car::ncvTest(model = model_3))
plot(model_3)


model_3_log <- lm(log(Casualty_Drag) ~ Sub_fat_percent + Pull_ups + Sit_ups + CMJ +
                 Seated_medball_throw + X1RM_Deadlift + Handgrip_avg, data = raw_data)
(car::ncvTest(model = model_3_log))
summary(model_3_log)

(confs_model_3_log <- confint(model_3_log))
(estimates_3_log <- coef(summary(model_3_log)))
(model_3_info_log <- cbind(estimates_3_log, confs_model_3_log))
model_3_info_log <- as.data.frame(model_3_info_log)
model_3_info_log <- model_3_info_log %>%
  rename('Coefficients (?)' = 'Estimate', 'p value' = 'Pr(>|t|)', 'CI lower' = '2.5 %', 'CI upper' = '97.5 %')
print(model_3_info_log)
model_3_info_log <- model_3_info_log %>%
  mutate_if(is.numeric, round, digits = 5)
print(model_3_info_log)
model_3_info_log$Predictor <- rownames(model_3_info_log)
model_3_info_log <- model_3_info_log[, c(7, 1:6)]
print(model_3_info_log)
write_xlsx(model_3_info_log, "E:/data/Statistics/Data/model_3_info_log.xlsx")

exp(model_3_info_log$`Coefficients (?)`[8])
exp(model_3_info_log$`CI lower`[8])
exp(model_3_info_log$`CI upper`[8])
exp(-0.02569)

library("lmtest") # for coeftest
library("sandwich") # for vcovHC
# estimating heteroscedasticity-robust standard errors
coeftest(model_3, vcov = vcovHC(model_3))
summary(model_3)

# ---------------------------------------- 4 independence of errors ----------------------------------- #

dwtest(log(Casualty_Drag) ~ Sub_fat_percent + Pull_ups + Sit_ups + CMJ +
         Seated_medball_throw + X1RM_Deadlift + Handgrip_avg, data = raw_data)

# ---------------------------------------- 5 normal distributions of errors --------------------------- #

model_3_residuals <- resid(model_3_log)
qqnorm(model_3_residuals)
qqline(model_3_residuals)

# with 95%CIs
car::qqPlot(model_3_residuals, xlab = "Quantiles", ylab = "Model 3 residuals", grid = F, pch = 20)

# --------------------------------------- 6 influential data points detection ---------------------------- #

# "As a rule of thumb, cases are regarded as too influential if the associated value for Cook's Distance exceeds the cut-off
# value of (Van der Meer et al., 2010):" 4/n  (or 4/ (N-k-1)) in which n refers to the number of groups in the grouping factor 
# under evaluation. 

N = nrow(raw_data)
k = 7
(cutoff = 4/(N-k-1))

# which points have larger Cook's distance than the threshold (instance > 0.167)

cooks_d_model_3 <- cooks.distance(model_3_log)
plot(cooks_d_model_3, type = "b", ylab = "Cook's Distance",
     xlab = "Observation Index", pch = 20, col = "red")
abline(h = cutoff,lty = 2)

which(cooks_d_model_3 > cutoff)
cooks_d_model_3[c(2, 20, 23, 26)]

# model without influential points
model_3_WO_IP <- lm(log(Casualty_Drag) ~ Sub_fat_percent + Pull_ups + Sit_ups + CMJ +
                                       Seated_medball_throw + X1RM_Deadlift + Handgrip_avg, data = raw_data[-c(26), ])

summary(model_3_WO_IP)

(confs_model_3_log_WO_IP <- confint(model_3_WO_IP))
(estimates_3_log_WO_IP <- coef(summary(model_3_WO_IP)))
(model_3_info_log_WO_IP <- cbind(estimates_3_log_WO_IP, confs_model_3_log_WO_IP))
model_3_info_log_WO_IP <- as.data.frame(model_3_info_log_WO_IP)
model_3_info_log_WO_IP <- model_3_info_log_WO_IP %>%
  rename('Coefficients (?)' = 'Estimate', 'p value' = 'Pr(>|t|)', 'CI lower' = '2.5 %', 'CI upper' = '97.5 %')
print(model_3_info_log_WO_IP)
model_3_info_log_WO_IP <- model_3_info_log_WO_IP %>%
  mutate_if(is.numeric, round, digits = 5)
print(model_3_info_log_WO_IP)
model_3_info_log_WO_IP$Predictor <- rownames(model_3_info_log_WO_IP)
model_3_info_log_WO_IP <- model_3_info_log_WO_IP[, c(7, 1:6)]
print(model_3_info_log_WO_IP)
write_xlsx(model_3_info_log_WO_IP, "E:/data/Statistics/Data/model_3_info_log_WO_IP.xlsx")


(confs_model_3_WO_influential_points <- confint(model_3a_WO_influential_points))


# ------------------------------------------------------------------------------------------------------ #
# plot contributions of coefficients with CI's

(confs_model_3_WO_IP <- confint(model_3_WO_IP))

(confs_model_3_low_WO_IP <- confs_model_3_WO_IP[, 1])
confs_model_3_low_WO_IP[-1]
(confs_model_3_upp_WO_IP <- confs_model_3_WO_IP[, 2])
confs_model_3_upp_WO_IP[-1]

model_3_coeff_WO_IP <- data.frame(mean = model_3_WO_IP$coefficients[-1],
                            lower = confs_model_3_low_WO_IP[-1],
                            upper = confs_model_3_upp_WO_IP[-1],
                            variable = c("Sub_fat_percent", "Pull_ups", 
                                         "Sit_ups", "CMJ", "Seated_medball_throw",
                                         "X1RM_Deadlift", "Handgrip_avg"))


model_3_coeff_WO_IP$variable <- factor(model_3_coeff_WO_IP$variable, levels = model_3_coeff_WO_IP$variable)

# plot contributions of coefficients with CI's
(plot_coeff_3_WO_IP <- ggplot(model_3_coeff_WO_IP, aes(x = variable, y = mean, ymin = lower, ymax = upper)) +
    geom_point(position = position_dodge(width = 0.2)) +
    geom_errorbar(position = position_dodge(width = 0.2), width = 0.1) +
    coord_flip()+
    theme_classic()+
    scale_y_continuous(limits = c(-0.1,0.06))+
    geom_hline(yintercept = 0.0, lwd = 0.2, lty = 2)+
    theme(text = element_text(size=12, family="Comic Sans MS", face = "bold"), plot.margin = margin(1, 1, 3, 1, "cm"),
          axis.ticks.y = element_blank(),
          panel.grid = element_blank(),element_line(linetype = 1),
          axis.line.y = element_blank(),axis.title.x = element_text(vjust = -5, hjust = 0.2),
          legend.position = "none")+
    ylab(expression(paste("Estimates of coefficients and their 95% confidence intervals")))+
    xlab(""))



# percentage differences between both models
100*(coef(model_3a_WO_influential_points) - coef(model_3a)) / coef(model_3a)

# ----------------------------------------------------------------------------------------------------------------------- #
# --------------------------------------------- model 4 ----------------------------------------------------------------- #
# ----------------------------------------------------------------------------------------------------------------------- #

str(raw_data)
model_4 <- lm(Single_lift_mass_max ~ Sub_fat_percent + Pull_ups + Sit_ups + CMJ +
                Seated_medball_throw + X1RM_Deadlift + Handgrip_avg, data = raw_data)

summary(model_4)

tidy(model_4, conf.int = T)

glance(model_4)

augmented_4 <- augment(model_4)
print(augmented_4, n = Inf)

(confs_model_4 <- confint(model_4))
(estimates_4 <- coef(summary(model_4)))
(model_4_info <- cbind(estimates_4, confs_model_4))
model_4_info <- as.data.frame(model_4_info)
model_4_info <- model_4_info %>%
  rename('Coefficients (?)' = 'Estimate', 'p value' = 'Pr(>|t|)', 'CI lower' = '2.5 %', 'CI upper' = '97.5 %')
print(model_4_info)
model_4_info <- model_4_info %>%
  mutate_if(is.numeric, round, digits = 5)
print(model_4_info)
model_4_info$Predictor <- rownames(model_4_info)
model_4_info <- model_4_info[, c(7, 1:6)]
print(model_4_info)
write_xlsx(model_4_info, "E:/data/Statistics/Data/model_4_info.xlsx")


# dotplot with error bars
model_4$coefficients[-1]

(confs_model_4_low <- confs_model_4[, 1])
confs_model_4_low[-1]
(confs_model_4_upp <- confs_model_4[, 2])
confs_model_4_upp[-1]

# plot contributions of coefficients with CI's

model_4_coeff <- data.frame(mean = model_4$coefficients[-1],
                            lower = confs_model_4_low[-1],
                            upper = confs_model_4_upp[-1],
                            variable = c("Sub_fat_percent", "Pull_ups", 
                                         "Sit_ups", "CMJ", "Seated_medball_throw",
                                         "X1RM_Deadlift", "handgrip_avg"))


model_4_coeff$variable <- factor(model_4_coeff$variable, levels = model_4_coeff$variable)

# plot contributions of coefficients with CI's
(plot_coeff_4 <- ggplot(model_4_coeff, aes(x = variable, y = mean, ymin = lower, ymax = upper)) +
    geom_point(position = position_dodge(width = 0.2)) +
    geom_errorbar(position = position_dodge(width = 0.2), width = 0.1) +
    coord_flip()+
    theme_classic()+
    scale_y_continuous(limits = c(-1.6,1.6))+
    geom_hline(yintercept = 0.0, lwd = 0.2, lty = 2)+
    theme(text = element_text(size=12, family="Comic Sans MS", face = "bold"), plot.margin = margin(1, 1, 3, 1, "cm"),
          axis.ticks.y = element_blank(),
          panel.grid = element_blank(),element_line(linetype = 1),
          axis.line.y = element_blank(),axis.title.x = element_text(vjust = -5, hjust = 0.2),
          legend.position = "none")+
    ylab(expression(paste("Estimates of coefficients and their 95% confidence intervals")))+
    xlab(""))



# --------- Plotting discernible slopes

(eff_plot_4a <- jtools::effect_plot(model = model_4,
                    pred = Seated_medball_throw,
                    data = raw_data,
                    interval = T,
                    plot.points = T, x.label = "Seated medicine ball throw (cm)", y.label = "Maximal single lift (kg)") +
    annotate("text", 
             x = -Inf, y = -Inf, 
             label = "(C)", 
             hjust = -4.5, vjust = -14.5, 
             size = 5, fontface = "bold"))

(eff_plot_4b <- jtools::effect_plot(model = model_4,
                              pred = Handgrip_avg,
                              data = raw_data,
                              interval = T,
                              plot.points = T, x.label = "Average handgrip (kg)", y.label = "Maximal single lift (kg)") +
    annotate("text", 
             x = -Inf, y = -Inf, 
             label = "(D)", 
             hjust = -4.5, vjust = -14.5, 
             size = 5, fontface = "bold"))

grid.arrange(eff_plot_4a, eff_plot_4b, nrow = 1)


# ---------------------------------------- 2 multicollienarity of the predictors ---------------------- #

(VIF_model_4 <- car::vif(model_4))
par(mar = c(5, 10, 4, 8) + 0.2)
barplot(VIF_model_4, main = 'VIF values',beside=TRUE, horiz = T,
        col = 'red', xlim = c(0,7), border = "black", axes = T,
        cex.names = 0.8, las = 1)
abline(v = 5, col = "black", lty = 2)

# -------------------------------------------- 3 heteroscedasticity ----------------------------------- #

(car::ncvTest(model = model_4))
plot(model_4)

# ---------------------------------------- 4 independence of errors ----------------------------------- #

dwtest(Single_lift_mass_max ~ Sub_fat_percent + Pull_ups + Sit_ups + CMJ +
         Seated_medball_throw + X1RM_Deadlift + Handgrip_avg, data = raw_data)

# ---------------------------------------- 5 normal distributions of errors --------------------------- #

model_4_residuals <- resid(model_4)
qqnorm(model_4_residuals)
qqline(model_4_residuals)

# with 95%CIs
car::qqPlot(model_4_residuals, xlab = "Quantiles", ylab = "Model 4 residuals", grid = F, pch = 20)

# --------------------------------------- 6 influential data points detection ---------------------------- #

# "As a rule of thumb, cases are regarded as too influential if the associated value for Cook?s Distance exceeds the cut-off
# value of (Van der Meer et al., 2010):" 4/n  (or 4/ (N-k-1)) in which n refers to the number of groups in the grouping factor 
# under evaluation. 

N = nrow(raw_data)
k = 7
(cutoff = 4/(N-k-1))

# which points have larger Cook's distance than the threshold (instance > 0.167)

cooks_d_model_4 <- cooks.distance(model_4)
plot(cooks_d_model_4, type = "b", ylab = "Cook's Distance",
     xlab = "Observation Index", pch = 20, col = "red")
abline(h = cutoff,lty = 2)

which(cooks_d_model_4 > cutoff)

# model without influential points
model_4_WO_influential_points <- lm(Single_lift_mass_max ~ Sub_fat_percent + Pull_ups + Sit_ups + CMJ +
                                      Seated_medball_throw + X1RM_Deadlift + Handgrip_avg, data = raw_data[-c(10,26), ])

# percentage differences between both models
100*(coef(model_4_WO_influential_points) - coef(model_4)) / coef(model_4)



# ----------------------------------------------------------------------------------------------------------------------- #
# --------------------------------------------- model 5 ----------------------------------------------------------------- #
# ----------------------------------------------------------------------------------------------------------------------- #

str(raw_data)
model_5 <- lm(Repeated_LiftCarry ~ Weight + Sub_fat_percent + VO2max_ml_kg_min + ShuttleRun + 
                Dist_coop + Push_ups + Pull_ups + Sit_ups, data = raw_data)

summary(model_5)

tidy(model_5, conf.int = T)

glance(model_5)

augmented_5 <- augment(model_5)
print(augmented_5, n = Inf)

# ---------------------------------------- 2 multicollienarity of the predictors ---------------------- #

(VIF_model_5 <- car::vif(model_5))
par(mar = c(5, 10, 4, 8) + 0.2)
barplot(VIF_model_5, main = 'VIF values',beside=TRUE, horiz = T,
        col = 'red', xlim = c(0,7), border = "black", axes = T,
        cex.names = 0.8, las = 1)
abline(v = 5, col = "black", lty = 2)

# ---------------------------------- refit model 5 --------------------------------- #

model_5a <- lm(Repeated_LiftCarry ~ Weight + Sub_fat_percent + ShuttleRun +
                 Dist_coop + Pull_ups + Sit_ups, data = raw_data)

(VIF_model_5a <- car::vif(model_5a))
par(mar = c(5, 10, 4, 8) + 0.2)
barplot(VIF_model_5a, main = 'VIF values',beside=TRUE, horiz = T,
        col = 'red', xlim = c(0,7), border = "black", axes = T,
        cex.names = 0.8, las = 1)
abline(v = 5, col = "black", lty = 2)


summary(model_5a)

tidy(model_5a, conf.int = T)

glance(model_5a)

augmented_5a <- augment(model_5a)
print(augmented_5a, n = Inf)


(confs_model_5a <- confint(model_5a))
(estimates_5a <- coef(summary(model_5a)))
(model_5a_info <- cbind(estimates_5a, confs_model_5a))
model_5a_info <- as.data.frame(model_5a_info)
model_5a_info <- model_5a_info %>%
  rename('Coefficients (?)' = 'Estimate', 'p value' = 'Pr(>|t|)', 'CI lower' = '2.5 %', 'CI upper' = '97.5 %')
print(model_5a_info)
model_5a_info <- model_5a_info %>%
  mutate_if(is.numeric, round, digits = 5)
print(model_5a_info)
model_5a_info$Predictor <- rownames(model_5a_info)
model_5a_info <- model_5a_info[, c(7, 1:6)]
print(model_5a_info)
write_xlsx(model_5a_info, "E:/data/Statistics/Data/model_5a_info.xlsx")


# plot contributions of coefficients with CI's

(confs_model_5a_low <- confs_model_5a[, 1])
confs_model_5a_low[-1]
(confs_model_5a_upp <- confs_model_5a[, 2])
confs_model_5a_upp[-1]

model_5a_coeff <- data.frame(mean = model_5a$coefficients[-1],
                            lower = confs_model_5a_low[-1],
                            upper = confs_model_5a_upp[-1],
                            variable = c("Weigth", "Sub_fat_percent", 
                                         "ShuttleRun", "Dist_coop", "Pull_ups",
                                         "Sit_ups"))


model_5a_coeff$variable <- factor(model_5a_coeff$variable, levels = model_5a_coeff$variable)

# plot contributions of coefficients with CI's
(plot_coeff_5a <- ggplot(model_5a_coeff, aes(x = variable, y = mean, ymin = lower, ymax = upper)) +
    geom_point(position = position_dodge(width = 0.2)) +
    geom_errorbar(position = position_dodge(width = 0.2), width = 0.1) +
    coord_flip()+
    theme_classic()+
    scale_y_continuous(limits = c(-5.6,20.0))+
    geom_hline(yintercept = 0.0, lwd = 0.2, lty = 2)+
    theme(text = element_text(size=12, family="Comic Sans MS", face = "bold"), plot.margin = margin(1, 1, 3, 1, "cm"),
          axis.ticks.y = element_blank(),
          panel.grid = element_blank(),element_line(linetype = 1),
          axis.line.y = element_blank(),axis.title.x = element_text(vjust = -5, hjust = 0.2),
          legend.position = "none")+
    ylab(expression(paste("")))+
    xlab(""))


# --------  Dist coop #

model_5a$coefficients[-1][4]


model_5_coeff_coop_dist <- data.frame(mean = model_5a$coefficients[-1][4],
                                            lower = confs_model_5a_low[-1][4],
                                            upper = confs_model_5a_upp[-1][4],
                                            variable = "Dist_coop")

(plot_coeff_coop_dist_5 <- ggplot(model_5_coeff_coop_dist, aes(x = variable, y = mean, ymin = lower, ymax = upper)) +
    geom_point(position = position_dodge(width = 0.2)) +
    geom_errorbar(position = position_dodge(width = 0.1), width = 0.05) +
    coord_flip()+
    theme_classic()+
    scale_y_continuous(limits = c(-0.1,0.01))+
    geom_hline(yintercept = 0.0, lwd = 0.2, lty = 2)+
    theme(text = element_text(size=12, family="Comic Sans MS", face = "bold"), plot.margin = margin(1, 1, 3, 1, "cm"),
          axis.ticks.y = element_blank(),
          panel.grid = element_blank(),element_line(linetype = 1),
          axis.line.y = element_blank(),axis.title.x = element_text(vjust = -5, hjust = 0.2),
          legend.position = "none")+
    ylab(expression(paste("")))+
    xlab(""))


grid.arrange(plot_coeff_5a, plot_coeff_coop_dist_5, nrow = 1,
             bottom = "Estimates of coefficients and their 95% confidence intervals")


(eff_plot_5 <- jtools::effect_plot(model = model_5a,
                           pred = Dist_coop,
                          data = raw_data,
                          interval = T,
                          plot.points = T, x.label = "Cooper test (m)", y.label = "Repeated lift carry (m)") +
    annotate("text", 
             x = -Inf, y = -Inf, 
             label = "(E)", 
             hjust = -4.5, vjust = -14.5, 
             size = 5, fontface = "bold"))

# -------------------------------------------- 3 heteroscedasticity ----------------------------------- #

(car::ncvTest(model = model_5a))
plot(model_5a)

# ---------------------------------------- 4 independence of errors ----------------------------------- #

dwtest(Repeated_LiftCarry ~ Weight + Sub_fat_percent + ShuttleRun +
         Dist_coop + Pull_ups + Sit_ups, data = raw_data)

# ---------------------------------------- 5 normal distributions of errors --------------------------- #

model_5_residuals <- resid(model_5a)
qqnorm(model_5_residuals)
qqline(model_5_residuals)

# with 95%CIs
car::qqPlot(model_5_residuals, xlab = "Quantiles", ylab = "Model 5 residuals", grid = F, pch = 20)

# --------------------------------------- 6 influential data points detection ---------------------------- #

# "As a rule of thumb, cases are regarded as too influential if the associated value for Cook?s Distance exceeds the cut-off
# value of (Van der Meer et al., 2010):" 4/n  (or 4/ (N-k-1)) in which n refers to the number of groups in the grouping factor 
# under evaluation. 

N = nrow(raw_data)
k = 6
(cutoff = 4/(N-k-1))

# which points have larger Cook's distance than the threshold (instance > 0.167)

cooks_d_model_5 <- cooks.distance(model_5a)
plot(cooks_d_model_5, type = "b", ylab = "Cook's Distance",
     xlab = "Observation Index", pch = 20, col = "red")
abline(h = cutoff,lty = 2)


which(cooks_d_model_5 > cutoff)
cooks_d_model_5[c(20,25,26)]

# model without influential points
model_5_WO_influential_points <- lm(Repeated_LiftCarry ~ Weight + Sub_fat_percent + ShuttleRun +
                                      Dist_coop + Pull_ups + Sit_ups, data = raw_data[-c(20,25,26), ])

# percentage differences between both models
100*(coef(model_5_WO_influential_points) - coef(model_5)) / coef(model_5)

# ----------------------------------------------------------------------------------------------------------------------- #
# --------------------------------------------- model 6 ----------------------------------------------------------------- #
# ----------------------------------------------------------------------------------------------------------------------- #

str(raw_data)
model_6 <- lm(Fire_movement ~ Weight + Sub_fat_percent + VO2max_ml_kg_min + ShuttleRun + 
                Dist_coop + Push_ups + Pull_ups + Sit_ups, data = raw_data)

summary(model_6)

tidy(model_6, conf.int = T)

glance(model_6)

augmented_6 <- augment(model_6)
print(augmented_6, n = Inf)
# ---------------------------------------- 2 multicollienarity of the predictors ---------------------- #

(VIF_model_6 <- car::vif(model_6))
par(mar = c(5, 10, 4, 8) + 0.2)
barplot(VIF_model_6, main = 'VIF values',beside=TRUE, horiz = T,
        col = 'red', xlim = c(0,7), border = "black", axes = T,
        cex.names = 0.8, las = 1)
abline(v = 5, col = "black", lty = 2)

# ---------------------------------- refit model 6 --------------------------------- #

model_6a <- lm(Fire_movement ~ Weight + Sub_fat_percent + ShuttleRun + 
                Dist_coop + Pull_ups + Sit_ups, data = raw_data)

(VIF_model_6a <- car::vif(model_6a))
par(mar = c(5, 10, 4, 8) + 0.2)
barplot(VIF_model_6a, main = 'VIF values',beside=TRUE, horiz = T,
        col = 'red', xlim = c(0,7), border = "black", axes = T,
        cex.names = 0.8, las = 1)
abline(v = 5, col = "black", lty = 2)

# ------------------------------------------------------------------------------------------------------ #

summary(model_6a)

tidy(model_6a, conf.int = T)

glance(model_6a)

augmented_6a <- augment(model_6a)
print(augmented_6a, n = Inf)


(confs_model_6a <- confint(model_6a))
(estimates_6a <- coef(summary(model_6a)))
(model_6a_info <- cbind(estimates_6a, confs_model_6a))
model_6a_info <- as.data.frame(model_6a_info)
model_6a_info <- model_6a_info %>%
  rename('Coefficients (?)' = 'Estimate', 'p value' = 'Pr(>|t|)', 'CI lower' = '2.5 %', 'CI upper' = '97.5 %')
print(model_6a_info)
model_6a_info <- model_6a_info %>%
  mutate_if(is.numeric, round, digits = 5)
print(model_6a_info)
model_6a_info$Predictor <- rownames(model_6a_info)
model_6a_info <- model_6a_info[, c(7, 1:6)]
print(model_6a_info)
write_xlsx(model_6a_info, "E:/data/Statistics/Data/model_6a_info.xlsx")


# plot contributions of coefficients with CI's

(confs_model_6a_low <- confs_model_6a[, 1])
confs_model_6a_low[-1]
(confs_model_6a_upp <- confs_model_6a[, 2])
confs_model_6a_upp[-1]

model_6a_coeff <- data.frame(mean = model_6a$coefficients[-1],
                             lower = confs_model_6a_low[-1],
                             upper = confs_model_6a_upp[-1],
                             variable = c("Weigth", "Sub_fat_percent", 
                                          "ShuttleRun", "Dist_coop", "Pull_ups",
                                          "Sit_ups"))


model_6a_coeff$variable <- factor(model_6a_coeff$variable, levels = model_6a_coeff$variable)

# plot contributions of coefficients with CI's
(plot_coeff_6a <- ggplot(model_6a_coeff, aes(x = variable, y = mean, ymin = lower, ymax = upper)) +
    geom_point(position = position_dodge(width = 0.2)) +
    geom_errorbar(position = position_dodge(width = 0.2), width = 0.1) +
    coord_flip()+
    theme_classic()+
    scale_y_continuous(limits = c(-0.4,2.0))+
    geom_hline(yintercept = 0.0, lwd = 0.2, lty = 2)+
    theme(text = element_text(size=12, family="Comic Sans MS", face = "bold"), plot.margin = margin(1, 1, 3, 1, "cm"),
          axis.ticks.y = element_blank(),
          panel.grid = element_blank(),element_line(linetype = 1),
          axis.line.y = element_blank(),axis.title.x = element_text(vjust = -5, hjust = 0.2),
          legend.position = "none")+
    ylab(expression(paste("")))+
    xlab(""))

# --------  Dist coop #

model_6a$coefficients[-1][4]


model_6_coeff_coop_dist <- data.frame(mean = model_6a$coefficients[-1][4],
                                      lower = confs_model_6a_low[-1][4],
                                      upper = confs_model_6a_upp[-1][4],
                                      variable = "Dist_coop")

(plot_coeff_coop_dist_6 <- ggplot(model_6_coeff_coop_dist, aes(x = variable, y = mean, ymin = lower, ymax = upper)) +
    geom_point(position = position_dodge(width = 0.2)) +
    geom_errorbar(position = position_dodge(width = 0.1), width = 0.05) +
    coord_flip()+
    theme_classic()+
    scale_y_continuous(limits = c(-0.001,0.004))+
    geom_hline(yintercept = 0.0, lwd = 0.2, lty = 2)+
    theme(text = element_text(size=12, family="Comic Sans MS", face = "bold"), plot.margin = margin(1, 1, 3, 1, "cm"),
          axis.ticks.y = element_blank(),
          panel.grid = element_blank(),element_line(linetype = 1),
          axis.line.y = element_blank(),axis.title.x = element_text(vjust = -5, hjust = 0.2),
          legend.position = "none")+
    ylab(expression(paste("")))+
    xlab(""))


grid.arrange(plot_coeff_6a, plot_coeff_coop_dist_6, nrow = 1,
             bottom = "Estimates of coefficients and their 95% confidence intervals")


(eff_plot_6 <- jtools::effect_plot(model = model_6a,
                              pred = ShuttleRun,
                              data = raw_data,
                              interval = T,
                              plot.points = T, x.label = "Shuttle run (s)", y.label = "Modified fire and movement test (s)") +
    annotate("text", 
             x = -Inf, y = -Inf, 
             label = "(F)", 
             hjust = -4.5, vjust = -14.5, 
             size = 5, fontface = "bold"))

# -------------------------------------------- 3 heteroscedasticity ----------------------------------- #

(car::ncvTest(model = model_6a))
plot(model_6a)

# ---------------------------------------- 4 independence of errors ----------------------------------- #

dwtest(Fire_movement ~ Weight + Sub_fat_percent + ShuttleRun + 
         Dist_coop + Pull_ups + Sit_ups, data = raw_data)

# ---------------------------------------- 5 normal distributions of errors --------------------------- #

model_6_residuals <- resid(model_6a)
qqnorm(model_6_residuals)
qqline(model_6_residuals)

# with 95%CIs
car::qqPlot(model_6_residuals, xlab = "Quantiles", ylab = "Model 6 residuals", grid = F, pch = 20)

# --------------------------------------- 6 influential data points detection ---------------------------- #

# "As a rule of thumb, cases are regarded as too influential if the associated value for Cook?s Distance exceeds the cut-off
# value of (Van der Meer et al., 2010):" 4/n  (or 4/ (N-k-1)) in which n refers to the number of groups in the grouping factor 
# under evaluation. 

N = nrow(raw_data)
k = 6
(cutoff = 4/(N-k-1))

# which points have larger Cook's distance than the threshold (instance > 0.167)

cooks_d_model_6 <- cooks.distance(model_6a)
plot(cooks_d_model_6, type = "b", ylab = "Cook's Distance",
     xlab = "Observation Index", pch = 20, col = "red")
abline(h = cutoff,lty = 2)


which(cooks_d_model_6 > cutoff)
cooks_d_model_6[2]

# model without influential points
model_6_WO_influential_points <- lm(Fire_movement ~ Sub_fat_percent + VO2max_ml_kg_min + ShuttleRun + 
                                      Dist_coop + Push_ups + Pull_ups + Sit_ups, data = raw_data[-c(10,26), ])

# percentage differences between both models
100*(coef(model_6_WO_influential_points) - coef(model_6)) / coef(model_6)

# ----------------------------------------------------------------------------------------------------------------------- #
# --------------------------------------------- grid of eff_plots ------------------------------------------------------- #
# ----------------------------------------------------------------------------------------------------------------------- #

(all_eff_plots <- gridExtra::grid.arrange(eff_plot_1, 
                        eff_plot_2, 
                        eff_plot_4a, 
                        eff_plot_4b, 
                        eff_plot_5, 
                        eff_plot_6, ncol = 3))

ggsave("E:/data/Statistics/Plots/tuff_stuff_plot.tiff", plot = all_eff_plots, device = "tiff",
       width = 10.0, height = 6, dpi = 300, units = "in")

# ----------------------------------------------------------------------------------------------------------------------- #
# --------------------------------------------- other staff ------------------------------------------------------------- #
# ----------------------------------------------------------------------------------------------------------------------- #

model_7 <- lm(Fire_movement ~ Sub_fat_percent + ShuttleRun + 
                Dist_coop + Weight, data = raw_data)

car::ncvTest(model_7)


model_8 <- lm(Single_lift_mass_max ~ Sub_fat_percent + ShuttleRun + 
                Dist_coop + Weight, data = raw_data)

car::ncvTest(model_8)

model_9 <- lm(Casualty_Drag ~ Sub_fat_percent + ShuttleRun + 
                Dist_coop + Weight, data = raw_data)

car::ncvTest(model_9)

model_10 <- lm(Repeated_LiftCarry ~ Sub_fat_percent + ShuttleRun + 
                Dist_coop + Weight, data = raw_data)

car::ncvTest(model_10)


model_11 <- lm(Water_can_carry ~ Sub_fat_percent + ShuttleRun + 
                 Dist_coop + Weight, data = raw_data)

car::ncvTest(model_11)


model_12 <- lm(Fire_movement ~ X1RM_Deadlift + HandGrip_str_R + HandGrip_str_L + 
                CMJ + Seated_medball_throw + Sit_ups + Pull_ups + Sub_lean_percent, data = raw_data)

car::ncvTest(model_12)

model_13 <- lm(Water_can_carry ~ X1RM_Deadlift + HandGrip_str_R + HandGrip_str_L + 
                 CMJ + Seated_medball_throw + Sit_ups + Pull_ups + Sub_lean_percent, data = raw_data)

car::ncvTest(model_14)

model_14 <- lm(Repeated_LiftCarry ~ X1RM_Deadlift + HandGrip_str_R + HandGrip_str_L + 
                 CMJ + Seated_medball_throw + Sit_ups + Pull_ups + Sub_lean_percent, data = raw_data)

car::ncvTest(model_14)

model_15 <- lm(Casualty_Drag ~ X1RM_Deadlift + HandGrip_str_R + HandGrip_str_L + 
                 CMJ + Seated_medball_throw + Sit_ups + Pull_ups + Sub_lean_percent, data = raw_data)

##################################################################################################

N = nrow(raw_data)
k = 6
(cutoff = 4/(N-k-1))

# which points have larger Cook's distance than the threshold (instance > 0.167)

cooks_d_model_14 <- cooks.distance(model_14)
plot(cooks_d_model_14, type = "b", ylab = "Cook's Distance",
     xlab = "Observation Index", pch = 20, col = "red")
abline(h = cutoff,lty = 2)


which(cooks_d_model_14 > cutoff)

##################################################################################################


car::ncvTest(model_15)

model_16 <- lm(Single_lift_mass_max~ X1RM_Deadlift + HandGrip_str_R + HandGrip_str_L + 
                 CMJ + Seated_medball_throw + Sit_ups + Pull_ups + Sub_lean_percent, data = raw_data)

car::ncvTest(model_16)

model_17 <- lm(Loaded_March ~ X1RM_Deadlift + HandGrip_str_R + HandGrip_str_L + 
                 CMJ + Seated_medball_throw + Sit_ups + Pull_ups + Sub_lean_percent, data = raw_data)

car::ncvTest(model_17)

# ------------------------------------------------------------------------------------------------------------- #
# -------------------------------------------------- SENSITIVITY ---------------------------------------------- #
# ------------------------------------------------------------------------------------------------------------- #

# Omnibus F Test
pwrss.f.reg(r2 = 0.50, k = 11, power = NULL, alpha = 0.05, n = 29)


