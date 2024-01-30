library(tidyverse)
library(AICcmodavg)
library(nlstools)

PCA_set <- readRDS("Langeweide/Statistics_file.rds")

initial_values <- list(
  alpha = 0.178,
  beta = 136.28,
  gamma = 0.022,
  omega = 0.053
)

initial_values2 <- list(
  alpha = -0.06,
  oli = - 0.39,
  beta = 78.28,
  gamma = 0.039,
  omega = 0.023
)

test_values <- list(
  alpha = 0.178,
  beta = 136.25,
  gamma = 10,
  omega = 0.053
)

WL_model <- nls(NEE_CO2_MDS_small ~ alpha * GPP + beta / (1 + exp(gamma * GWL)) * exp(omega * Tair),
            data = PCA_set,
            start = initial_values
                )

extended_model <- nls(NEE_CO2_MDS_small ~ alpha * GPP * exp(oli * Tair) + beta / (1 + exp(gamma * GWL)) * exp(omega * Tsoil_1_015),
                data = PCA_set,
                start = initial_values2
)

AFPS_model <- nls(NEE_CO2_MDS_small ~ alpha * GPP + beta / (1 + exp(gamma * -SENTEK1)) * exp(omega * Tair),
                data = PCA_set,
                start = initial_values
)

test_model <- nls(NEE_CO2_MDS_small ~ alpha * GPP + ((beta * TENSIO2) / (gamma + TENSIO2)) * exp(omega * Tair),
                  data = PCA_set,
                  start = test_values)


test <- -0.067 * PCA_set$GPP + 178.85049 / (1 + exp(0.022269 * PCA_set$GWL)) * exp(0.039848 * PCA_set$Tair)

ggplot(data = PCA_set, aes(x = GWL, y = test)) + geom_point() + geom_smooth(method = "loess", col = "red")

test2 <- -1.18291 * PCA_set$GPP * exp(-0.36501 * PCA_set$Tair) + 162.76352 / (1 + exp(0.01094 * PCA_set$GWL)) * exp(0.05148 * PCA_set$Tsoil_1_015)

ggplot(data = PCA_set, aes(x = GWL, y = test2)) + geom_point() + geom_smooth(method = "loess", col = "red")

test3 <- -0.046133 * PCA_set$GPP + 222.587933 / (1 + exp(0.006830 * PCA_set$SENTEK1)) * exp(0.044859 * PCA_set$Tair)

ggplot(data = PCA_set, aes(x = SENTEK1, y = test3)) + geom_point() + geom_smooth(method = "loess", col = "red")

test4 <- -0.008656 * PCA_set$GPP + ((138.538559 * PCA_set$SENTEK1) / (0.404 + PCA_set$SENTEK1)) * exp(0.040339 * PCA_set$Tair)

ggplot(data = PCA_set, aes(x = TENSIO2, y = test4)) + geom_point() + geom_smooth(method = "loess", col = "red")


summary(WL_model)
summary(extended_model)
summary(AFPS_model)
summary(test_model)

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
