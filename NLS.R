library(tidyverse)

PCA_set <- readRDS("Langeweide/Statistics_file.rds")

initial_values <- list(
  alpha = 0.26,
  beta = 78.28,
  gamma = 0.039,
  omega = 0.023
)

WL_model <- nls(NEE_CO2_MDS_small ~ alpha * GPP + beta / (1 + exp(gamma * GWL)) * exp(omega * Tair),
            data = PCA_set,
            start = initial_values
                )

AFPS_model <- nls(NEE_CO2_MDS_small ~ alpha * GPP + beta / (1 + exp(gamma * SENTEK1)) * exp(omega * Tair),
                data = PCA_set,
                start = initial_values
)

summary(WL_model)
summary(AFPS_model)





