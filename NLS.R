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
  alpha = 0.178,
  beta = 136.1,
  gamma = 0.027,
  omega = 0.053
)

WL_model <- nls(NEE_CO2_MDS_small ~ alpha * GPP + beta / (1 + exp(gamma * GWL)) * exp(omega * Tair),
                data = PCA_set,
                start = initial_values
)

summary(WL_model)

test <- -0.067 * PCA_set$GPP + 178.85049 / (1 + exp(0.022269 * PCA_set$GWL)) * exp(0.039848 * PCA_set$Tair)

ggplot(data = PCA_set, aes(x = GWL, y = test)) + geom_point() + geom_smooth(method = "loess", col = "red")

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
initial_values <- list(
  alpha = 0.178,
  beta = 136.28,
  gamma = 0.022,
  omega = 0.053
)


AFPS_model <- nls(NEE_CO2_MDS_small ~ alpha * GPP + beta / (1 + exp(-gamma * SENTEK1)) * exp(omega * Tair),
                  data = train,
                  start = initial_values
)

summary(AFPS_model)

test3 <- -0.046133 * PCA_set$GPP + 222.587933 / (1 + exp(-0.006830 * PCA_set$SENTEK1)) * exp(0.044859 * PCA_set$Tair)
test3 <- 222.587933 / (1 + exp(0.006830 * PCA_set$SENTEK1)) * exp(0.044859 * 15)

ggplot(data = PCA_set, aes(x = SENTEK1, y = test3)) + geom_point() + geom_smooth(method = "loess", col = "red")

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
test_values <- list(
  alpha = 0.178,
  beta = 136.25,
  gamma = 10,
  omega = 0.053
)

test_model <- nls(NEE_CO2_MDS_small ~ alpha * GPP + ((beta * TENSIO2) / (gamma + TENSIO2)) * exp(omega * Tair),
                  data = PCA_set,
                  start = test_values)

summary(test_model)

test4 <- -0.008656 * PCA_set$GPP + ((138.538559 * PCA_set$SENTEK1) / (0.404 + PCA_set$SENTEK1)) * exp(0.040339 * PCA_set$Tair)

ggplot(data = PCA_set, aes(x = TENSIO2, y = test4)) + geom_point() + geom_smooth(method = "loess", col = "red")






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
linear1 <- lm(NEE_CO2_MDS_small ~ GWL + GPP + Tair, 
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
