library(tidyverse)
library(AICcmodavg)
library(nlstools)
library(caret)

PCA_set <- readRDS("Langeweide/Statistics_file.rds")

#remove first row
PCA_set <- PCA_set[-1,]

#I have several nls models

#Original fit from Bart##################################################
initial_values <- list(
  alpha = 0.26,
  beta = 135,
  gamma = 0.1,
  omega = 0.040,
  c = 100
)

WL_model <- nls(NEE_CO2_MDS_small ~ alpha * GPP + beta / (1 + exp(gamma * (GWL + c))) * exp(omega * Tair),
                data = PCA_set,
                start = initial_values
)

summary(WL_model)

#test = full data, test_zero1 = Tair fixed at 15 C, test_zero2 = Tair = Tair
test_WL <- -0.073183 * PCA_set$GPP + 182.149347 / (1 + exp(0.023337 * PCA_set$GWL)) * exp(0.038484 * PCA_set$Tair)
test_WL_zero1 <- 182.149347 / (1 + exp(0.023337 * PCA_set$GWL)) * exp(0.038484 * 15)
test_WL_zero2 <- 182.149347 / (1 + exp(0.023337 * PCA_set$GWL)) * exp(0.038484 * PCA_set$Tair)

ggplot(data = PCA_set, aes(x = GWL, y = test_WL_zero1)) + geom_point() + geom_smooth(method = "loess", col = "red")

ggplot(data = PCA_set) + 
  geom_line(aes(x = GWL, y = test_WL_zero1, color = "GPP = 0"), linewidth = 1) +
  geom_point(aes(x = GWL, y = test_WL_zero2, color = "GPP = 0, Tair = Tair")) +
  labs(
    x = "GWL [cm]",
    y = "NEE CO2 [kg day-1 ha-1]"
  ) +
  theme(
    panel.border = element_rect(color = "black", fill = NA, size = 1),
    panel.background = element_rect(fill = "white", color = "black")
  ) +
  scale_color_manual(values = c("GPP = 0" = "black", "GPP = 0, Tair = Tair" = "tomato"))

  

#AFPS model unaltered###############################################
initial_values_AFPS <- list(
  alpha = 0.26,
  beta = 97.6,
  gamma = 0.039,
  omega = 0.059
)

AFPS_model <- nls(NEE_CO2_MDS_small ~ alpha * GPP + beta / (1 + exp(-gamma * SENTEK1)) * exp(omega * Tair),
                  data = PCA_set,
                  start = initial_values_AFPS
)

summary(AFPS_model)

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


ggplot() + geom_point(aes(x = PCA_set$NEE_CO2_MDS_small, y = SENTEK1_model)) + expand_limits(x=c(0,500), y=c(0,500))
ggplot() + geom_point(aes(x = PCA_set$NEE_CO2_MDS_small, y = SENTEK3_model)) + expand_limits(x=c(0,500), y=c(0,500))

ggplot() + geom_point(aes(x = PCA_set$SENTEK1, y = SENTEK1_model, color = "red"))
+ geom_point(aes(x = PCA_set$SENTEK1, y = PCA_set$NEE_CO2_MDS_small, color = "blue"))

#extended model from Bart################################################
initial_values2 <- list(
  alpha = -0.06,
  oli = - 0.39,
  beta = 78.28,
  gamma = 0.039,
  omega = 0.023
)

extended_model <- nls(NEE_CO2_MDS_small ~ alpha * GPP * exp(oli * Tair) + beta / (1 + exp(gamma * GWL)) * exp(omega * Tsoil_1_015),
                      data = PCA_set,
                      start = initial_values2
)

summary(extended_model)

test2 <- -1.18291 * PCA_set$GPP * exp(-0.36501 * PCA_set$Tair) + 162.76352 / (1 + exp(0.01094 * PCA_set$GWL)) * exp(0.05148 * PCA_set$Tsoil_1_015)

ggplot(data = PCA_set, aes(x = GWL, y = test2)) + geom_point() + geom_smooth(method = "loess", col = "red")


#Experimentation model########################################
test_values_limited <- list(
  alpha = 0.26,
  beta = 97.6,
  gamma = 0.039,
  omega = 0.059
)

limited_model <- nls(NEE_CO2_MDS_small ~ alpha * GPP + ((beta * SENTEK1) / (gamma + SENTEK3)) * exp(omega * Tair),
                  data = PCA_set,
                  start = test_values_limited)

summary(limited_model)

test4 <- -0.09791 * PCA_set$GPP + ((152.26 * PCA_set$TENSIO3) / (0.64 + PCA_set$TENSIO3)) * exp(0.038909 * PCA_set$Tair)
test4 <- -0.0086565 * PCA_set$GPP + ((149.538559 * PCA_set$SENTEK1) / (2.789256 + PCA_set$SENTEK1)) * exp(0.040339 * PCA_set$Tair)
test4_zero <- ((134.350 * PCA_set$SENTEK3) / (1.87474 + PCA_set$SENTEK3)) * exp(0.047339 * PCA_set$Tair)
test4_zero <- ((149.538559 * PCA_set$SENTEK1) / (2.789256 + PCA_set$SENTEK1)) * exp(0.040339 * 15)
test4 <- ((152.26 * PCA_set$TENSIO3) / (0.64 + PCA_set$TENSIO3)) * exp(0.038909 * 15)

ggplot(data = PCA_set, aes(x = SENTEK1, y = test4)) + geom_point() + geom_smooth(method = "loess", col = "red")
ggplot(data = PCA_set, aes(x = TENSIO2, y = TENSIO2_sat_zero1)) + geom_point() + geom_smooth(method = "loess", col = "red")

#######experiment with bellcurve##################################################
test_values_parabolic <- list(
  alpha = 0.1,
  beta = 135,
  gamma = 10,
  omega = 30,
  epsilon = 0.059
)


parabolic_model <- nls(NEE_CO2_MDS_small ~ alpha * GPP + beta * exp(-0.5 * ((SENTEK3 - gamma) / omega)^2) * exp(epsilon * Tair),
                  data = PCA_set,
                  start = test_values_parabolic)

summary(parabolic_model)

test_parabolic <- 0.007101 * PCA_set$GPP + 160.2 * exp(-0.5 * ((PCA_set$SENTEK1 - 31.58) / 27.59)^2) * exp(0.03764 * PCA_set$Tair)
test_parabolic_zero <- 160.2 * exp(-0.5 * ((PCA_set$SENTEK1 - 31.58) / 27.59)^2) * exp(0.03764 * 15)

test_parabolic <- -0.11 * PCA_set$GPP + 146.57 * exp(-0.5 * ((PCA_set$TENSIO3 - 12.24) / 14.32)^2) * exp(0.04258 * PCA_set$Tair)
test_parabolic_zero <- 146.57 * exp(-0.5 * ((PCA_set$TENSIO3 - 12.24) / 14.32)^2) * exp(0.04258 * 15)


ggplot(data = PCA_set, aes(x = SENTEK1, y = test_parabolic)) + geom_point() + 
  #geom_smooth(method = "loess", col = "red") + 
  geom_point(data = PCA_set, aes(x = TENSIO3, y = NEE_CO2_MDS_small, color = "blue"))

ggplot(data = PCA_set, aes(x = SENTEK1, y = test_parabolic_zero)) + geom_point()

summary(parabolic_model)

##################################################################
AIC(WL_model, extended_model)

predicted_values <- predict(WL_model)

cor(PCA_set$NEE_CO2_MDS_small, predict(WL_model))

plot(PCA_set$NEE_CO2_MDS_small, predicted_values, 
     xlab = "Observed NEE", ylab = "Predicted NEE",
     main = "Observed vs. Predicted NEE")
abline(0, 1, col = "red")

#this calculates R2 but this is not valid
sse <- WL_model$m$deviance()
null <- lm(NEE_CO2_MDS_small~1, PCA_set)
sst <- data.frame(summary.aov(null)[[1]])$Sum.Sq
percent_variation_explained = 100*(sst-sse)/sst

#same here R2 not valid
mse <- mean((PCA_set$NEE_CO2_MDS_small - predicted_values)^2)
rsquared <- cor(PCA_set$NEE_CO2_MDS_small, predicted_values)^2

AIC(WL_model)
AIC(extended_model)

linear2 <- lm(NEE_CO2_MDS_small ~ GWL + GPP + Tair + Tsoil_1_015, 
             data = PCA_set)
linear1 <- lm(NEE_CO2_MDS_small ~ SENTEK1 + GPP + Tair, 
             data = PCA_set)
linear3 <- lm(NEE_CO2_MDS_small ~ GWL + GPP + Tair + Tsoil_1_015 + ET, 
              data = PCA_set)

predicted_values <- predict(linear)

# Calculate residuals
residuals <- residuals(linear)

# Calculate RSE
n <- length(residuals)
p <- length(coef(linear))  # Number of coefficients including intercept

rse <- sqrt(sum(residuals^2) / (n - p))


#AFPS model unaltered###############################################
initial_values <- list(
  alpha = 0.178,
  beta = 136.28,
  gamma = 0.022,
  omega = 0.053
)

noGPP_values <- list(
  beta = 136.28,
  gamma = 0.022,
  omega = 0.053
)

onlyAFPS_values <-  list(
  beta = 136.28,
  gamma = 0.022
)

noTair_values <- list(
  omega = 0.053
)

AFPS_model <- nls(NEE_CO2_MDS_small ~ alpha * GPP + beta / (1 + exp(-gamma * SENTEK1)) * exp(omega * Tair),
                  data = PCA_set,
                  start = initial_values
)

noGPP <- nls(NEE_CO2_MDS_small ~ beta / (1 + exp(-gamma * SENTEK1)) * exp(omega * Tair),
             data = PCA_set,
             start = noGPP_values
)

onlyAFPS <- nls(NEE_CO2_MDS_small ~ beta / (1 + exp(-gamma * SENTEK1)),
                data = PCA_set,
                start = onlyAFPS_values
)

onlyTair <- nls(NEE_CO2_MDS_small ~ exp(omega * Tair),
                data = PCA_set,
                start = noTair_values
)

summary(AFPS_model)
summary(noGPP)
summary(onlyAFPS)
summary(onlyTair)

AIC(AFPS_model, noGPP, onlyAFPS, onlyTair)







