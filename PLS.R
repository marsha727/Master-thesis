library(tidyverse)
library(pls)

LAW_ICOS <- readRDS("Langeweide/LAW_MS_ICOS.rds")
AFPS_NEE_WL_Tair <- readRDS("Datasets/Extracted/AFPS_NEE_WL_Tair.rds")

extracted <- AFPS_NEE_WL_Tair[[1]]$df

extracted <- extracted %>% 
  rename(SENTEK1 = y.LAW_MS_ICOS.SENTEK1.mean,
         SENTEK3 = y.LAW_MS_ICOS.SENTEK3.mean,
         TENSIO2 = y.LAW_MS_ICOS.TENSIO2.mean,
         TENSIO3 = y.LAW_MS_ICOS.TENSIO3.mean,
         OWASIS = y.LAW_MS_ICOS.OWASIS.mean,
         NEE_CO2_MDS_small = w.LAW_MS_ICOS.NEE_CO2_MDS_small.mean,
         GPP = v.LAW_MS_ICOS.GPP.mean,
         GWL = x.LAW_MS_ICOS.GWL_mean.mean,
         Tair = u.LAW_MS_ICOS.Tair_f.mean)

extracted <- na.omit(extracted)


model <- plsr(NEE_CO2_MDS_small ~ SENTEK1 + SENTEK3 + TENSIO2 + TENSIO3 + OWASIS +
                GWL + GPP + Tair, data = extracted, scale = TRUE, validation = "CV")

summary(model)

validationplot(model)
validationplot(model, val.type = "MSEP")
validationplot(model, val.type =  "R2")

train <- extracted[1:105, c("SENTEK1" , "SENTEK3" , "TENSIO2" , "TENSIO3" , "OWASIS" ,
                              "GWL" , "GPP" , "Tair")]
y_test <- extracted[106:211, c("NEE_CO2_MDS_small")]
test <- extracted[106:211, c("SENTEK1" , "SENTEK3" , "TENSIO2" , "TENSIO3" , "OWASIS" ,
                             "GWL" , "GPP" , "Tair")]

model <- plsr(NEE_CO2_MDS_small ~ SENTEK1 + SENTEK3 + TENSIO2 + TENSIO3 + OWASIS +
                GWL + GPP + Tair, data = extracted, scale = TRUE, validation = "CV")

pcr_pred <- predict(model, test, ncomp = 2)

sqrt(mean((pcr_pred - y_test)^2))

coefficients <- coef(model)
sum.coef = sum(sapply(coefficients, abs))
coefficients = coefficients * 100 / sum.coef
coefficients = sort(coefficients[,1,1])
barplot(tail(coefficients, 8))

