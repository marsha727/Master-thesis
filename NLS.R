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
  alpha = 0.26,
  oli = -0.39,
  beta = 78.28,
  gamma = 0.039,
  omega = 0.023
)

WL_model <- nls(NEE_CO2_MDS_small ~ alpha * GPP + beta / (1 + exp(gamma * GWL)) * exp(omega * Tair),
            data = PCA_set,
            start = initial_values
                )

extended_model <- nls(NEE_CO2_MDS_small ~ alpha * GPP * exp(oli * Tair) + beta / (1 + exp(gamma * GWL)) * exp(omega * Tsoil_1_015),
                data = PCA_set,
                start = initial_values2
)

AFPS_model <- nls(NEE_CO2_MDS_small ~ alpha * GPP + beta / (1 + exp(gamma * SENTEK1)) * exp(omega * Tair),
                data = PCA_set,
                start = initial_values
)

summary(WL_model)
summary(extended_model)
summary(AFPS_model)

AIC(WL_model, extended_model)

predicted_values <- predict(WL_model)

cor(PCA_set$NEE_CO2_MDS_small, predict(WL_model))

plot(PCA_set$NEE_CO2_MDS_small, predicted_values, 
     xlab = "Observed NEE", ylab = "Predicted NEE",
     main = "Observed vs. Predicted NEE")
abline(0, 1, col = "red")

sse <- WL_model$m$deviance()
null <- lm(NEE_CO2_MDS_small~1, PCA_set)
sst <- data.frame(summary.aov(null)[[1]])$Sum.Sq
percent_variation_explained = 100*(sst-sse)/sst


mse <- mean((PCA_set$NEE_CO2_MDS_small - predicted_values)^2)
rsquared <- cor(PCA_set$NEE_CO2_MDS_small, predicted_values)^2

AIC(WL_model)
AIC(extended_model)
