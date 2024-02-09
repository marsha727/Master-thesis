library(tidyverse)
library(nls.multstart)
library(AICcmodavg)

# Define your model function
AFPS_model_function <- function(alpha, beta, gamma, omega, GPP, SENTEK1, Tair) {
  alpha * GPP + beta / (1 + exp(-gamma * SENTEK1)) * exp(omega * Tair)
}

# Fit the model using nls_multstart
fits <- nls_multstart(NEE_CO2_MDS_small ~ AFPS_model_function(alpha, beta, gamma, omega, GPP, SENTEK1, Tair),
                      data = PCA_set,
                      iter = 500,
                      start_lower = c(alpha = 0, beta = 0, gamma = 0, omega = 0),
                      start_upper = c(alpha = 1, beta = 170, gamma = 1, omega = 1),
                      lower = c(alpha = -Inf, beta = -Inf, gamma = -Inf, omega = -Inf),
                      supp_errors = 'Y')

# Define your model function
GWL_model_function <- function(alpha, beta, gamma, omega, GPP, GWL, Tair) {
  alpha * GPP + beta / (1 + exp(-gamma * GWL)) * exp(omega * Tair)
}

GWL_model_function <- function(beta, gamma, omega, GPP, GWL, Tair, c) {
  GPP + beta / (1 + exp(-gamma * (GWL + c))) * exp(omega * Tair)
}

# Fit the model using nls_multstart
fits_GWL <- nls_multstart(NEE_CO2_MDS_small ~ GWL_model_function(alpha, beta, gamma, omega, GPP, GWL, Tair, c),
                      data = PCA_set,
                      iter = 500,
                      start_lower = c(alpha = 0, beta = 0, gamma = 0, omega = 0, c = 0),
                      start_upper = c(alpha = 1, beta = 170, gamma = 1, omega = 1, c = 100),
                      lower = c(alpha = -Inf, beta = -Inf, gamma = -Inf, omega = -Inf, c = -Inf),
                      supp_errors = 'Y')


limited_model <- function(alpha, beta, gamma, omega, GPP, SENTEK1, Tair) {
  alpha * GPP + ((beta * SENTEK1) / (gamma + SENTEK1)) * exp(omega * Tair)
}

fits_limited <- nls_multstart(NEE_CO2_MDS_small ~ limited_model(alpha, beta, gamma, omega, GPP, SENTEK1, Tair),
                              data = PCA_set,
                              iter = 500,
                              start_lower = c(alpha = 0, beta = 0, gamma = 0, omega = 0),
                              start_upper = c(alpha = 1, beta = 170, gamma = 1, omega = 1),
                              lower = c(alpha = -Inf, beta = -Inf, gamma = -Inf, omega = -Inf),
                              supp_errors = 'Y')
                              
models <- list(fits, fits_GWL, fits_limited)
model_names <- c("fits", "fits_GWL", "fits_limited")

aictab(cand.set = models, modnames = model_names)

