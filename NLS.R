library(tidyverse)
library(AICcmodavg)
library(nlstools)
library(caret)

PCA_set <- readRDS("Langeweide/Statistics_file.rds")

#remove first row
PCA_set <- PCA_set[-1,]

#Experimenting with the training and test data split############# 

#creating interval by weekly
num_intervals <- ceiling(nrow(PCA_set) / 7) 

# Create indices for train and test intervals
train_intervals <- seq(1, num_intervals, by = 2)  # Use every other 7-day interval for training
test_intervals <- seq(2, num_intervals, by = 2)   # Use the remaining 7-day intervals for testing

# Create train and test datasets
train_indices <- unlist(sapply(train_intervals, function(i) seq((i-1)*7 + 1, min(i*7, nrow(PCA_set)))))
test_indices <- unlist(sapply(test_intervals, function(i) seq((i-1)*7 + 1, min(i*7, nrow(PCA_set)))))

train <- PCA_set[train_indices, c("NEE_CO2_MDS_small", "SENTEK1", "SENTEK3", "TENSIO2", "TENSIO3", "OWASIS", "GWL", "GPP", "Tair", "Tsoil_1_015")]
y_test <- PCA_set[test_indices, "NEE_CO2_MDS_small"]
testing <- PCA_set[test_indices, c("NEE_CO2_MDS_small", "SENTEK1", "SENTEK3", "TENSIO2", "TENSIO3", "OWASIS", "GWL", "GPP", "Tair", "Tsoil_1_015")]

# Verify the dimensions of the resulting datasets
dim(train)
length(y_test)
dim(testing)

#I have several nls models

#Original fit from Bart##################################################
initial_values <- list(
  alpha = 0.17,
  beta = 136,
  gamma = 0.027,
  omega = 0.053
)

WL_model <- nls(NEE_CO2_MDS_small ~ alpha * GPP + beta / (1 + exp(gamma * GWL)) * exp(omega * Tair),
                data = PCA_set,
                start = initial_values
)

WL_noGPP_model <- nls(NEE_CO2_MDS_small ~ beta / (1 + exp(gamma * GWL)) * exp(omega * Tair),
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

  
predict_train <- predict(WL_model, newdata = train)
predict_test <- predict(WL_model, newdata = testing)

train_residuals <- train$NEE_CO2_MDS_small - predict_train
test_residuals <- testing$NEE_CO2_MDS_small - predict_test

train_mse <- mean(train_residuals^2)
test_mse <- mean(test_residuals^2)


predict_NEE <- function(NEE){
  predict(WL_model, newdata = data.frame(NEE = NEE_CO2_MDS_small))
}

train_control <- trainControl(method = "cv", k = 5) 
model_caret <- train(NEE_CO2_MDS_small ~ predict_NEE(NEE),
                     data = PCA_set,
                     method = "nls",
                     trControl = train_control
                     )




#AFPS model unaltered###############################################
initial_values_AFPS <- list(
  alpha = 0.087,
  beta = 97,
  gamma = 0.027,
  omega = 0.053
)

initial_values_AFPS_c <- list(
  alpha = 0.178,
  beta = 136.28,
  gamma = 0.059,
  omega = 0.043,
  c = 0.9
)

AFPS_model <- nls(NEE_CO2_MDS_small ~ alpha * GPP + beta / (1 + exp(-gamma * SENTEK1)) * exp(omega * Tair),
                  data = PCA_set,
                  start = initial_values_AFPS
)

AFPS_model_c <- nls(NEE_CO2_MDS_small ~ alpha * GPP + beta / (1 + exp(-gamma * (SENTEK1 + c))) * exp(omega * Tair),
                  data = PCA_set,
                  start = initial_values_AFPS_c
)

summary(AFPS_model)
summary(AFPS_model_c)

test_AFPS <- -0.046132 * PCA_set$GPP + 222.5864 / (1 + exp(-0.00683 * PCA_set$SENTEK1)) * exp(0.044859 * PCA_set$Tair)
test_AFPS_c <- 0.003648 * PCA_set$GPP + 135.7 / (1 + exp(-0.2747 * PCA_set$SENTEK1 + 0.9748)) * exp(0.04063 * PCA_set$Tair)

test_AFPS_zero1 <- 138.130226 / (1 + exp(-0.170967 * PCA_set$SENTEK1)) * exp(0.040627 * 15)
test_AFPS_zero2 <- 138.130226 / (1 + exp(-0.170967 * PCA_set$SENTEK1)) * exp(0.040627 * PCA_set$Tair)

test_AFPS_zero1_c <- 135.7 / (1 + exp(-0.2747 * PCA_set$SENTEK1 + 0.9748)) * exp(0.04063 * 15)
test_AFPS_zero2_c <- 135.7 / (1 + exp(-0.2747 * PCA_set$SENTEK1 + 0.9748)) * exp(0.04063 * PCA_set$Tair)

ggplot(data = PCA_set, aes(x = SENTEK1, y = test_AFPS_zero1)) + geom_point() + geom_smooth(method = "loess", col = "red")

ggplot(data = PCA_set) + 
  geom_line(aes(x = SENTEK1, y = test_AFPS_zero1_c, color = "GPP = 0"), linewidth = 1) +
  geom_point(aes(x = SENTEK1, y = test_AFPS_zero2_c, color = "GPP = 0, Tair = Tair")) +
  labs(
    x = "GWL [cm]",
    y = "NEE CO2 [kg day-1 ha-1]"
  ) +
  theme(
    panel.border = element_rect(color = "black", fill = NA, size = 1),
    panel.background = element_rect(fill = "white", color = "black")
  ) +
  scale_color_manual(values = c("GPP = 0" = "black", "GPP = 0, Tair = Tair" = "tomato"))



predict_train <- predict(AFPS_model, newdata = train)
predict_test <- predict(AFPS_model, newdata = testing)

train_residuals <- train$NEE_CO2_MDS_small - predict_train
test_residuals <- testing$NEE_CO2_MDS_small - predict_test

train_mse <- mean(train_residuals^2)
test_mse <- mean(test_residuals^2)




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
  alpha = 0.178,
  beta = 170.25,
  gamma = 10,
  omega = 0.053
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
ggplot(data = PCA_set, aes(x = SENTEK3, y = test4_zero)) + geom_point() + geom_smooth(method = "loess", col = "red")

#######experiment with bellcurve##################################################
test_values_parabolic <- list(
  alpha = -0.002,
  beta = 300,
  gamma = 10,
  omega = 30,
  epsilon = 0.055
)


parabolic_model <- nls(NEE_CO2_MDS_small ~ alpha * GPP + beta * exp(-0.5 * ((SENTEK3 - gamma) / omega)^2) * exp(epsilon * Tair),
                  data = PCA_set,
                  start = test_values_parabolic)

summary(parabolic_model)

test_parabolic <- 0.007101 * PCA_set$GPP + 160.2 * exp(-0.5 * ((PCA_set$SENTEK1 - 31.58) / 27.59)^2) * exp(0.03764 * PCA_set$Tair)
test_parabolic_zero <- 160.2 * exp(-0.5 * ((PCA_set$SENTEK1 - 31.58) / 27.59)^2) * exp(0.03764 * 15)

ggplot(data = PCA_set, aes(x = SENTEK1, y = test_parabolic)) + geom_point() + geom_smooth(method = "loess", col = "red")
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
