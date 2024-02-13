library(tidyverse)
library(nls.multstart)
library(AICcmodavg)

PCA_set <- readRDS("Langeweide/Statistics_file.rds")
#remove empty first row
PCA_set <- PCA_set[-1,]

#AFPS sigmoid#################################################################

# Define model function
AFPS_model_function_S3 <- function(alpha, beta, gamma, omega, GPP, SENTEK3, Tair) {
  alpha * GPP + beta / (1 + exp(-gamma * SENTEK3)) * exp(omega * Tair)
}

# Fit the model using nls_multstart
fits_S3 <- nls_multstart(NEE_CO2_MDS_small ~ AFPS_model_function_S3(alpha, beta, gamma, omega, GPP, SENTEK3, Tair),
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

limited_model_S3 <- function(alpha, beta, gamma, omega, GPP, SENTEK3, Tair) {
  alpha * GPP + ((beta * SENTEK3) / (gamma + SENTEK3)) * exp(omega * Tair)
}

fits_limited_S3 <- nls_multstart(NEE_CO2_MDS_small ~ limited_model_S3(alpha, beta, gamma, omega, GPP, SENTEK3, Tair),
                              data = PCA_set,
                              iter = 500,
                              start_lower = c(alpha = 0, beta = 0, gamma = 0, omega = 0),
                              start_upper = c(alpha = 1, beta = 170, gamma = 1, omega = 1),
                              lower = c(alpha = -Inf, beta = -Inf, gamma = -Inf, omega = -Inf),
                              supp_errors = 'Y')


#Parabolic model##########################################################
parabolic_model_S3 <- function(alpha, beta, gamma, omega, epsilon, GPP, SENTEK3, Tair) {
  alpha * GPP + beta * exp(-0.5 * ((SENTEK3 - gamma) / omega)^2) * exp(epsilon * Tair)
}

fits_parabolic_S3 <- nls_multstart(NEE_CO2_MDS_small ~ parabolic_model_S3(alpha, beta, gamma, omega, epsilon, GPP, SENTEK3, Tair),
                                data = PCA_set,
                                iter = 500,
                                start_lower = c(alpha = 0, beta = 0, gamma = 0, omega = 0, epsilon = 0),
                                start_upper = c(alpha = 1, beta = 130, gamma = 20, omega = 10, epsilon = 1),
                                lower = c(alpha = -Inf, beta = -Inf, gamma = -Inf, omega = -Inf, epsilon = -Inf),
                                supp_errors = 'Y')