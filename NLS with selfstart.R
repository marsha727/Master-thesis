library(tidyverse)
library(nls.multstart)
library(AICcmodavg)

PCA_set <- readRDS("Langeweide/Statistics_file.rds")
#remove empty first row
PCA_set <- PCA_set[-1,]

#AFPS sigmoid#################################################################

# Define model function
AFPS_model_function_S1 <- function(alpha, beta, gamma, omega, GPP, SENTEK1, Tair) {
  alpha * GPP + beta / (1 + exp(-gamma * SENTEK1)) * exp(omega * Tair)
}

# Fit the model using nls_multstart
fits_S1 <- nls_multstart(NEE_CO2_MDS_small ~ AFPS_model_function_S1(alpha, beta, gamma, omega, GPP, SENTEK1, Tair),
                      data = PCA_set,
                      iter = 500,
                      start_lower = c(alpha = 0, beta = 0, gamma = 0, omega = 0),
                      start_upper = c(alpha = 1, beta = 170, gamma = 1, omega = 1),
                      lower = c(alpha = -Inf, beta = -Inf, gamma = -Inf, omega = -Inf),
                      supp_errors = 'Y')

#GWL sigmoid################################################################

# Define model function
GWL_model_function <- function(alpha, beta, gamma, omega, GPP, GWL, Tair) {
  alpha * GPP + beta / (1 + exp(gamma * GWL)) * exp(omega * Tair)
}

# Fit the model using nls_multstart
fits_GWL <- nls_multstart(NEE_CO2_MDS_small ~ GWL_model_function(alpha, beta, gamma, omega, GPP, GWL, Tair),
                      data = PCA_set,
                      iter = 500,
                      start_lower = c(alpha = 0, beta = 0, gamma = 0, omega = 0),
                      start_upper = c(alpha = 1, beta = 170, gamma = 1, omega = 1),
                      lower = c(alpha = -Inf, beta = -Inf, gamma = -Inf, omega = -Inf),
                      supp_errors = 'Y')

#Saturated growth (Michaelis menten)######################################

limited_model_S1 <- function(alpha, beta, gamma, omega, GPP, SENTEK1, Tair) {
  alpha * GPP + ((beta * SENTEK1) / (gamma + SENTEK1)) * exp(omega * Tair)
}

fits_limited_S1 <- nls_multstart(NEE_CO2_MDS_small ~ limited_model_S1(alpha, beta, gamma, omega, GPP, SENTEK1, Tair),
                              data = PCA_set,
                              iter = 500,
                              start_lower = c(alpha = 0, beta = 0, gamma = 0, omega = 0),
                              start_upper = c(alpha = 1, beta = 170, gamma = 1, omega = 1),
                              lower = c(alpha = -Inf, beta = -Inf, gamma = -Inf, omega = -Inf),
                              supp_errors = 'Y')


#Parabolic model##########################################################
parabolic_model_S1 <- function(alpha, beta, gamma, omega, epsilon, GPP, SENTEK1, Tair) {
  alpha * GPP + beta * exp(-0.5 * ((SENTEK1 - gamma) / omega)^2) * exp(epsilon * Tair)
}

fits_parabolic_S1 <- nls_multstart(NEE_CO2_MDS_small ~ parabolic_model_S1(alpha, beta, gamma, omega, epsilon, GPP, SENTEK1, Tair),
                              data = PCA_set,
                              iter = 500,
                              start_lower = c(alpha = 0, beta = 0, gamma = 0, omega = 0, epsilon = 0),
                              start_upper = c(alpha = 1, beta = 130, gamma = 20, omega = 10, epsilon = 1),
                              lower = c(alpha = -Inf, beta = -Inf, gamma = -Inf, omega = -Inf, epsilon = -Inf),
                              supp_errors = 'Y')

#ANALYSIS PLOTS##############################################################################

#SIGMOID AFPS
SENTEK1_model <- -0.011 * PCA_set$GPP + 138.1 / (1 + exp(-0.170967 * PCA_set$SENTEK1)) * exp(0.041 * PCA_set$Tair)
SENTEK3_model <- -0.064 * PCA_set$GPP + 120.1 / (1 + exp(-0.278 * PCA_set$SENTEK3)) * exp(0.048 * PCA_set$Tair)
TENSIO2_model <- -0.100 * PCA_set$GPP + 132.5 / (1 + exp(-0.800 * PCA_set$TENSIO2)) * exp(0.043 * PCA_set$Tair)
TENSIO3_model <- -0.110 * PCA_set$GPP + 144.1 / (1 + exp(-0.598 * PCA_set$TENSIO3)) * exp(0.039 * PCA_set$Tair)

SENTEK1_zero1 <- 138.1 / (1 + exp(-0.170967 * PCA_set$SENTEK1)) * exp(0.041 * 15)
SENTEK1_zero2 <- 138.1 / (1 + exp(-0.170967 * PCA_set$SENTEK1)) * exp(0.041 * PCA_set$Tair)

SENTEK3_zero1 <- 120.1 / (1 + exp(-0.278 * PCA_set$SENTEK3)) * exp(0.048 * 15)
SENTEK3_zero2 <- 120.1 / (1 + exp(-0.278 * PCA_set$SENTEK3)) * exp(0.048 * PCA_set$Tair)

TENSIO2_zero1 <- 132.5 / (1 + exp(-0.800 * PCA_set$TENSIO2)) * exp(0.043 * 15)
TENSIO2_zero2 <- 132.5 / (1 + exp(-0.800 * PCA_set$TENSIO2)) * exp(0.043 * PCA_set$Tair)

TENSIO3_zero1 <- 144.1 / (1 + exp(-0.598 * PCA_set$TENSIO3)) * exp(0.039 * 15)
TENSIO3_zero2 <- 144.1 / (1 + exp(-0.598 * PCA_set$TENSIO3)) * exp(0.039 * PCA_set$Tair)

ggplot(data = PCA_set, aes(x = SENTEK1, y = SENTEK1_model)) + geom_point() + geom_smooth(method = "loess", col = "red")

ggplot(data = PCA_set) + 
  geom_line(aes(x = SENTEK1, y = SENTEK1_zero1, color = "S1 GPP = 0"), linewidth = 1) +
  geom_point(aes(x = SENTEK1, y = SENTEK1_zero2, color = "S1 GPP = 0, Tair = Tair")) +
  geom_line(aes(x = SENTEK3, y = SENTEK3_zero1, color = "S3 GPP = 0"), linewidth = 1) +
  geom_point(aes(x = SENTEK3, y = SENTEK3_zero2, color = "S3 GPP = 0, Tair = Tair")) +
  labs(
    x = "GWL [cm]",
    y = "NEE CO2 [kg day-1 ha-1]",
    colour = " "
  ) +
  theme(
    panel.border = element_rect(color = "black", fill = NA, size = 1),
    panel.background = element_rect(fill = "white", color = "black")
  ) +
  scale_color_manual(values = c("S1 GPP = 0" = "black", "S1 GPP = 0, Tair = Tair" = "tomato",
                                "S3 GPP = 0" = "blue", "S3 GPP = 0, Tair = Tair" = "green"))

ggplot(data = PCA_set) + 
  geom_line(aes(x = TENSIO2, y = TENSIO2_zero1, color = "S1 GPP = 0"), linewidth = 1) +
  geom_point(aes(x = TENSIO2, y = TENSIO2_zero2, color = "S1 GPP = 0, Tair = Tair")) +
  geom_line(aes(x = TENSIO3, y = TENSIO3_zero1, color = "S3 GPP = 0"), linewidth = 1) +
  geom_point(aes(x = TENSIO3, y = TENSIO3_zero2, color = "S3 GPP = 0, Tair = Tair")) +
  labs(
    x = "GWL [cm]",
    y = "NEE CO2 [kg day-1 ha-1]",
    colour = " "
  ) +
  theme(
    panel.border = element_rect(color = "black", fill = NA, size = 1),
    panel.background = element_rect(fill = "white", color = "black")
  ) +
  scale_color_manual(values = c("S1 GPP = 0" = "black", "S1 GPP = 0, Tair = Tair" = "tomato",
                                "S3 GPP = 0" = "blue", "S3 GPP = 0, Tair = Tair" = "green"))

ggplot(data = PCA_set) + 
  geom_point(aes(x = SENTEK1, y = SENTEK1_zero2, color = "SENTEK GPP = 0, Tair = Tair"), 
             shape = 16, size = 1) +
  geom_point(aes(x = TENSIO2, y = TENSIO2_zero2, color = "TENSIO GPP = 0, Tair = Tair"), 
             shape = 17, size = 1) +
  geom_line(aes(x = SENTEK1, y = SENTEK1_zero1, linetype = "SENTEK GPP = 0"), 
            color = "black", size = 1) +
  geom_line(aes(x = TENSIO2, y = TENSIO2_zero1, linetype = "TENSIO GPP = 0"), 
            color = "black", size = 1) +
  labs(
    x = "AFPS [mm]",
    y = "NEE CO2 [kg day-1 ha-1]",
    colour = " ",
    linetype = " "
  ) +
  scale_color_manual(values = c("SENTEK GPP = 0, Tair = Tair" = "tomato", 
                                "TENSIO GPP = 0, Tair = Tair" = "skyblue")) +
  scale_linetype_manual(values = c("SENTEK GPP = 0" = "solid", 
                                   "TENSIO GPP = 0" = "dotdash"),
                        labels = c("SENTEK GPP = 0", "TENSIO GPP = 0")) +
  theme(
    panel.border = element_rect(color = "black", fill = NA, size = 1),
    panel.background = element_rect(fill = "white", color = "black"),
    legend.text = element_text(size = 8),
    legend.margin = margin(0, 0.5, 0, 1, "mm")
  ) +
  guides(color = guide_legend(override.aes = list(shape = c(16, 17), linetype = c("solid", "dashed"))))


#SATURATING FUNCTION##############################################################################
SENTEK1_sat <- -0.0086565 * PCA_set$GPP + ((149.538559 * PCA_set$SENTEK1) / (2.789256 + PCA_set$SENTEK1)) * exp(0.040339 * PCA_set$Tair)
SENTEK3_sat <- -0.061 * PCA_set$GPP + ((134.3 * PCA_set$SENTEK3) / (1.872 + PCA_set$SENTEK3)) * exp(0.047 * PCA_set$Tair)
TENSIO2_sat <- -0.087 * PCA_set$GPP + ((138.2 * PCA_set$TENSIO2) / (0.404 + PCA_set$TENSIO2)) * exp(0.042 * PCA_set$Tair)
TENSIO3_sat <- -0.097 * PCA_set$GPP + ((152.3 * PCA_set$TENSIO3) / (0.645 + PCA_set$TENSIO3)) * exp(0.039 * PCA_set$Tair)

SENTEK1_sat_zero1 <- ((149.538559 * PCA_set$SENTEK1) / (2.789256 + PCA_set$SENTEK1)) * exp(0.040339 * 15)
SENTEK1_sat_zero2 <- ((149.538559 * PCA_set$SENTEK1) / (2.789256 + PCA_set$SENTEK1)) * exp(0.040339 * PCA_set$Tair)

SENTEK3_sat_zero1 <- ((134.3 * PCA_set$SENTEK3) / (1.872 + PCA_set$SENTEK3)) * exp(0.047 * 15)
SENTEK3_sat_zero2 <- ((134.3 * PCA_set$SENTEK3) / (1.872 + PCA_set$SENTEK3)) * exp(0.047 * PCA_set$Tair)

TENSIO2_sat_zero1 <- ((138.2 * PCA_set$TENSIO2) / (0.404 + PCA_set$TENSIO2)) * exp(0.042 * 15)
TENSIO2_sat_zero2 <- ((138.2 * PCA_set$TENSIO2) / (0.404 + PCA_set$TENSIO2)) * exp(0.042 * PCA_set$Tair)

ggplot(data = PCA_set) + 
  geom_line(aes(x = SENTEK1, y = SENTEK1_sat_zero1, color = "S1 GPP = 0"), linewidth = 1) +
  geom_point(aes(x = SENTEK1, y = SENTEK1_sat_zero2, color = "S1 GPP = 0, Tair = Tair")) +
  geom_line(aes(x = SENTEK3, y = SENTEK3_sat_zero1, color = "S3 GPP = 0"), linewidth = 1) +
  geom_point(aes(x = SENTEK3, y = SENTEK3_sat_zero2, color = "S3 GPP = 0, Tair = Tair")) +
  labs(
    x = "GWL [cm]",
    y = "NEE CO2 [kg day-1 ha-1]",
    colour = " "
  ) +
  theme(
    panel.border = element_rect(color = "black", fill = NA, size = 1),
    panel.background = element_rect(fill = "white", color = "black")
  ) +
  scale_color_manual(values = c("S1 GPP = 0" = "black", "S1 GPP = 0, Tair = Tair" = "tomato",
                                "S3 GPP = 0" = "blue", "S3 GPP = 0, Tair = Tair" = "green"))


ggplot(data = PCA_set) + 
  geom_point(aes(x = SENTEK1, y = SENTEK1_sat_zero2, color = "SENTEK GPP = 0, Tair = Tair"), 
             shape = 16, size = 1) +
  geom_point(aes(x = TENSIO2, y = TENSIO2_sat_zero2, color = "TENSIO GPP = 0, Tair = Tair"), 
             shape = 17, size = 1) +
  geom_line(aes(x = SENTEK1, y = SENTEK1_sat_zero1, linetype = "SENTEK GPP = 0"), 
            color = "black", size = 1) +
  geom_line(aes(x = TENSIO2, y = TENSIO2_sat_zero1, linetype = "TENSIO GPP = 0"), 
            color = "black", size = 1) +
  labs(
    x = "AFPS [mm]",
    y = "NEE CO2 [kg day-1 ha-1]",
    colour = " ",
    linetype = " "
  ) +
  scale_color_manual(values = c("SENTEK GPP = 0, Tair = Tair" = "tomato", 
                                "TENSIO GPP = 0, Tair = Tair" = "skyblue")) +
  scale_linetype_manual(values = c("SENTEK GPP = 0" = "solid", 
                                   "TENSIO GPP = 0" = "dotdash"),
                        labels = c("SENTEK GPP = 0", "TENSIO GPP = 0")) +
  theme(
    panel.border = element_rect(color = "black", fill = NA, size = 1),
    panel.background = element_rect(fill = "white", color = "black"),
    legend.text = element_text(size = 8),
    legend.margin = margin(0, 0.5, 0, 1, "mm")
  ) +
  guides(color = guide_legend(override.aes = list(shape = c(16, 17), linetype = c("solid", "dashed"))))

#Bell curve##################################################################################
SENTEK1_bell <- 0.007101 * PCA_set$GPP + 160.2 * exp(-0.5 * ((PCA_set$SENTEK1 - 31.58) / 27.59)^2) * exp(0.03764 * PCA_set$Tair)
SENTEK3_bell <- -0.063 * PCA_set$GPP + 114.9 * exp(-0.5 * ((PCA_set$SENTEK3 - 13.58) / 9.81)^2) * exp(0.052 * PCA_set$Tair)
TENSIO2_bell <- -0.102 * PCA_set$GPP + 132.5 * exp(-0.5 * ((PCA_set$TENSIO2 - 9.56) / 12.28)^2) * exp(0.047 * PCA_set$Tair)
TENSIO3_bell <- -0.111 * PCA_set$GPP + 146.6 * exp(-0.5 * ((PCA_set$TENSIO3 - 12.24) / 14.33)^2) * exp(0.042 * PCA_set$Tair)

SENTEK1_bell_zero1 <- 160.2 * exp(-0.5 * ((PCA_set$SENTEK1 - 31.58) / 27.59)^2) * exp(0.03764 * 15)
SENTEK1_bell_zero2 <- 160.2 * exp(-0.5 * ((PCA_set$SENTEK1 - 31.58) / 27.59)^2) * exp(0.03764 * PCA_set$Tair)

SENTEK3_bell_zero1 <- 114.9 * exp(-0.5 * ((PCA_set$SENTEK3 - 13.58) / 9.81)^2) * exp(0.052 * 15)
SENTEK3_bell_zero2 <- 114.9 * exp(-0.5 * ((PCA_set$SENTEK3 - 13.58) / 9.81)^2) * exp(0.052 * PCA_set$Tair)

TENSIO2_bell_zero1 <- 132.5 * exp(-0.5 * ((PCA_set$TENSIO2 - 9.56) / 12.28)^2) * exp(0.047 * 15)
TENSIO2_bell_zero2 <- 132.5 * exp(-0.5 * ((PCA_set$TENSIO2 - 9.56) / 12.28)^2) * exp(0.047 * PCA_set$Tair)
  
ggplot(data = PCA_set) + 
  geom_line(aes(x = SENTEK1, y = SENTEK1_bell_zero1, color = "S1 GPP = 0"), linewidth = 1) +
  geom_point(aes(x = SENTEK1, y = SENTEK1_bell_zero2, color = "S1 GPP = 0, Tair = Tair")) +
  geom_line(aes(x = SENTEK3, y = SENTEK3_bell_zero1, color = "S3 GPP = 0"), linewidth = 1) +
  geom_point(aes(x = SENTEK3, y = SENTEK3_bell_zero2, color = "S3 GPP = 0, Tair = Tair")) +
  labs(
    x = "GWL [cm]",
    y = "NEE CO2 [kg day-1 ha-1]",
    colour = " "
  ) +
  theme(
    panel.border = element_rect(color = "black", fill = NA, size = 1),
    panel.background = element_rect(fill = "white", color = "black")
  ) +
  scale_color_manual(values = c("S1 GPP = 0" = "black", "S1 GPP = 0, Tair = Tair" = "tomato",
                                "S3 GPP = 0" = "blue", "S3 GPP = 0, Tair = Tair" = "green"))


ggplot(data = PCA_set) + 
  geom_point(aes(x = SENTEK1, y = SENTEK1_bell_zero2, color = "SENTEK GPP = 0, Tair = Tair"), 
             shape = 16, size = 1) +
  geom_point(aes(x = TENSIO2, y = TENSIO2_bell_zero2, color = "TENSIO GPP = 0, Tair = Tair"), 
             shape = 17, size = 1) +
  geom_line(aes(x = SENTEK1, y = SENTEK1_bell_zero1, linetype = "SENTEK GPP = 0"), 
            color = "black", size = 1) +
  geom_line(aes(x = TENSIO2, y = TENSIO2_bell_zero1, linetype = "TENSIO GPP = 0"), 
            color = "black", size = 1) +
  labs(
    x = "AFPS [mm]",
    y = "NEE CO2 [kg day-1 ha-1]",
    colour = " ",
    linetype = " "
  ) +
  scale_color_manual(values = c("SENTEK GPP = 0, Tair = Tair" = "tomato", 
                                "TENSIO GPP = 0, Tair = Tair" = "skyblue")) +
  scale_linetype_manual(values = c("SENTEK GPP = 0" = "solid", 
                                   "TENSIO GPP = 0" = "dotdash"),
                        labels = c("SENTEK GPP = 0", "TENSIO GPP = 0")) +
  theme(
    panel.border = element_rect(color = "black", fill = NA, size = 1),
    panel.background = element_rect(fill = "white", color = "black"),
    legend.text = element_text(size = 8),
    legend.margin = margin(0, 0.5, 0, 1, "mm")
  ) +
  guides(color = guide_legend(override.aes = list(shape = c(16, 17), linetype = c("solid", "dashed"))))


#AIC testing#######################################################################################                             
models <- list(fits_S1, fits_GWL, fits_limited_S1, fits_parabolic_S1)
model_names <- c("fits_S1", "fits_GWL", "fits_limited_S1", "fits_parabolic_S1")

models <- list(fits_S1, fits_S3, fits_T2, fits_T3, 
               fits_GWL, 
               fits_limited_S1, fits_limited_S3, fits_limited_T2, fits_limited_T3, 
               fits_parabolic_S1, fits_parabolic_S3, fits_parabolic_T2, fits_parabolic_T3)
model_names <- c("fits_S1", "fits_S3", "fits_T2", "fits_T3", 
                 "fits_GWL",
                 "fits_limited_S1", "fits_limited_S3", "fits_limited_T2", "fits_limited_T3", 
                 "fits_parabolic_S1", "fits_parabolic_S3", "fits_parabolic_T2", "fits_parabolic_T3")

models <- list(fits_S1, fits_T2, 
               fits_GWL, 
               fits_limited_S1, fits_limited_T2, 
               fits_parabolic_S1, fits_parabolic_T2)
model_names <- c("fits_S1", "fits_T2", 
                 "fits_GWL",
                 "fits_limited_S1", "fits_limited_T2", 
                 "fits_parabolic_S1", "fits_parabolic_T2")

models <- list(fits, fits2)
model_names <- c("fits", "fits2")

aictab(cand.set = models, modnames = model_names)

#goodness of fit##################################################
ggplot() + 
  geom_point(data = PCA_set, aes(x = NEE_CO2_MDS_small, y = SENTEK1_model)) +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
  expand_limits(x = c(0, 500), y = c(0, 500))

ggplot() + 
  geom_point(aes(x = PCA_set$NEE_CO2_MDS_small, y = TENSIO2_model)) +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
  expand_limits(x=c(0,500), y=c(0,500))

ggplot() + 
  geom_point(aes(x = PCA_set$NEE_CO2_MDS_small, y = SENTEK1_sat)) +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
  expand_limits(x=c(0,500), y=c(0,500))

ggplot() + 
  geom_point(aes(x = PCA_set$NEE_CO2_MDS_small, y = TENSIO2_sat)) +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
  expand_limits(x=c(0,500), y=c(0,500))

ggplot() + 
  geom_point(aes(x = PCA_set$NEE_CO2_MDS_small, y = SENTEK1_bell)) +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
  expand_limits(x=c(0,500), y=c(0,500))

ggplot() + 
  geom_point(aes(x = PCA_set$NEE_CO2_MDS_small, y = TENSIO2_bell)) +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
  expand_limits(x=c(0,500), y=c(0,500))

#different goodness of fit######################################
ggplot() + 
  geom_point(aes(x = PCA_set$SENTEK1, y = SENTEK1_model, color = "red")) +
  geom_point(aes(x = PCA_set$SENTEK1, y = PCA_set$NEE_CO2_MDS_small, color = "blue"))

ggplot() + 
  geom_point(aes(x = PCA_set$TENSIO2, y = TENSIO2_model, color = "red")) +
  geom_point(aes(x = PCA_set$TENSIO2, y = PCA_set$NEE_CO2_MDS_small, color = "blue"))

ggplot() + 
  geom_point(aes(x = PCA_set$SENTEK1, y = SENTEK1_sat, color = "red")) +
  geom_point(aes(x = PCA_set$SENTEK1, y = PCA_set$NEE_CO2_MDS_small, color = "blue"))

ggplot() + 
  geom_point(aes(x = PCA_set$TENSIO2, y = TENSIO2_sat, color = "red")) +
  geom_point(aes(x = PCA_set$TENSIO2, y = PCA_set$NEE_CO2_MDS_small, color = "blue"))

ggplot() + 
  geom_point(aes(x = PCA_set$SENTEK1, y = SENTEK1_bell, color = "red")) +
  geom_point(aes(x = PCA_set$SENTEK1, y = PCA_set$NEE_CO2_MDS_small, color = "blue"))

ggplot() + 
  geom_point(aes(x = PCA_set$TENSIO2, y = TENSIO2_bell, color = "red")) +
  geom_point(aes(x = PCA_set$TENSIO2, y = PCA_set$NEE_CO2_MDS_small, color = "blue"))

#Risidual check#####################################################################
residuals <- residuals(fits_GWL)

# Create plots to check residuals
par(mfrow = c(2, 2))  # Set up a 2x2 grid for plots
plot(predict(fits_GWL), residuals, xlab = "Fitted values", ylab = "Residuals", main = "Residuals vs Fitted")  # Residuals vs Fitted values
hist(residuals, xlab = "Residuals", main = "Histogram of Residuals")  # Histogram of residuals
qqnorm(residuals, main = "Q-Q plot")  # Q-Q plot
qqline(residuals)



