library(tidyverse)
library(pls)
library(mixKernel)

LAW_ICOS <- readRDS("Langeweide/LAW_MS_ICOS.rds")
AFPS_NEE_WL_Tair <- readRDS("Datasets/Extracted/AFPS_NEE_WL_Tair.rds")
PCA_set <- readRDS("Langeweide/Statistics_file.rds")

#extracted <- AFPS_NEE_WL_Tair[[1]]$df

#extracted <- extracted %>% 
  #rename(SENTEK1 = y.LAW_MS_ICOS.SENTEK1.mean,
        # SENTEK3 = y.LAW_MS_ICOS.SENTEK3.mean,
         #TENSIO2 = y.LAW_MS_ICOS.TENSIO2.mean,
         #TENSIO3 = y.LAW_MS_ICOS.TENSIO3.mean,
         #OWASIS = y.LAW_MS_ICOS.OWASIS.mean,
         #NEE_CO2_MDS_small = w.LAW_MS_ICOS.NEE_CO2_MDS_small.mean,
         #GPP = v.LAW_MS_ICOS.GPP.mean,
         #GWL = x.LAW_MS_ICOS.GWL_mean.mean,
         #Tair = u.LAW_MS_ICOS.Tair_f.mean)

#extracted <- na.omit(extracted)
num_intervals <- ceiling(nrow(PCA_set) / 7)

# Create indices for train and test intervals
train_intervals <- seq(1, num_intervals, by = 2)  # Use every other 7-day interval for training
test_intervals <- seq(2, num_intervals, by = 2)   # Use the remaining 7-day intervals for testing

# Create train and test datasets
train_indices <- unlist(sapply(train_intervals, function(i) seq((i-1)*7 + 1, min(i*7, nrow(PCA_set)))))
test_indices <- unlist(sapply(test_intervals, function(i) seq((i-1)*7 + 1, min(i*7, nrow(PCA_set)))))

train <- PCA_set[train_indices, c("NEE_CO2_MDS_small", "SENTEK1", "SENTEK3", "TENSIO2", "TENSIO3", "OWASIS", "GWL", "GPP", "Tair", "Tsoil_1_015")]
y_test <- PCA_set[test_indices, "NEE_CO2_MDS_small"]
test <- PCA_set[test_indices, c("NEE_CO2_MDS_small", "SENTEK1", "SENTEK3", "TENSIO2", "TENSIO3", "OWASIS", "GWL", "GPP", "Tair", "Tsoil_1_015")]

# Verify the dimensions of the resulting datasets
dim(train)
length(y_test)
dim(test)


model <- plsr(NEE_CO2_MDS_small ~ SENTEK1 + SENTEK3 + TENSIO2 + TENSIO3 + OWASIS +
                GWL + GPP + Tair, data = PCA_set, scale = TRUE, validation = "CV")

summary(model)

validationplot(model)
validationplot(model, val.type = "MSEP")
validationplot(model, val.type =  "R2")

train <- PCA_set[1:105, c("NEE_CO2_MDS_small", "SENTEK1" , "SENTEK3" , "TENSIO2" , "TENSIO3" , "OWASIS" ,
                              "GWL" , "GPP" , "Tair")]
y_test <- PCA_set[106:211, c("NEE_CO2_MDS_small")]
test <- PCA_set[106:211, c("NEE_CO2_MDS_small", "SENTEK1" , "SENTEK3" , "TENSIO2" , "TENSIO3" , "OWASIS" ,
                             "GWL" , "GPP" , "Tair")]

model <- plsr(NEE_CO2_MDS_small ~ SENTEK1 + SENTEK3 + TENSIO2 + TENSIO3 + OWASIS + Tsoil_1_015 +
                GWL + GPP + Tair, data = train, scale = TRUE, validation = "CV")

pcr_pred <- predict(model, test, ncomp = 2)

sqrt(mean((pcr_pred - y_test)^2))

coefficients <- coef(model)
sum.coef = sum(sapply(coefficients, abs))
coefficients = coefficients * 100 / sum.coef
coefficients = sort(coefficients[,1,1])
barplot(tail(coefficients, 8))

model$loadings


