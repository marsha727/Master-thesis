#principle component analysis
library(tidyverse)
library(kernlab)
library(missMDA)

Langeweide_PCA <- Langeweide_full %>% 
  select(-c(NEE_H, SWIN, SWOUT, LWIN, LWOUT, bowen_ratio, Tdew_EP, datetime, ET, EF3, LE,
            Tsoil_1_005, Tsoil_1_015, Tsoil_1_025, Tsoil_1_035,
            Tsoil_1_045, Tsoil_1_055, Tsoil_1_065, Tsoil_1_075, Tsoil_1_085,
            Tsoil_1_095, Tsoil_1_105, Tsoil_3_005, Tsoil_3_015, Tsoil_3_025, Tsoil_3_035,
            Tsoil_3_045, Tsoil_3_055, Tsoil_3_065, Tsoil_3_075, Tsoil_3_085,
            Tsoil_3_095, Tsoil_3_105, NEE_CO2_MDS, NEE_CO2))

Langeweide_missing <- Langeweide_full %>%
  select(-c(NEE_H, bowen_ratio, Tdew_EP, datetime, EF3, LE,
            Tsoil_1_005, Tsoil_1_025, Tsoil_1_035,
            Tsoil_1_045, Tsoil_1_055, Tsoil_1_065, Tsoil_1_075, Tsoil_1_085,
            Tsoil_1_095, Tsoil_1_105, Tsoil_3_005, Tsoil_3_015, Tsoil_3_025, Tsoil_3_035,
            Tsoil_3_045, Tsoil_3_055, Tsoil_3_065, Tsoil_3_075, Tsoil_3_085,
            Tsoil_3_095, Tsoil_3_105, NEE_CO2_MDS, NEE_CO2))

Langeweide_night$datetime <- as.POSIXct(Langeweide_night$datetime, format = "%Y-%m-%d %H:%M:%S")

Langeweide_date <- Langeweide_night$datetime

Langeweide_night <- Langeweide_night[complete.cases(Langeweide_night$SENTEK1), ] %>% 
  group_by(datetime = format(datetime, "%Y-%m-%d")) %>% 
  summarise(SENTEK1 = mean(SENTEK1, na.rm = TRUE),
            SENTEK3 = mean(SENTEK3, na.rm = TRUE),
            TENSIO2 = mean(TENSIO2, na.rm = TRUE),
            TENSIO3 = mean(TENSIO3, na.rm = TRUE),
            OWASIS = mean(OWASIS, na.rm = TRUE),
            GWL_mean = mean(GWL_mean, na.rm = TRUE),
            Tsoil_1_015 = mean(Tsoil_1_015, na.rm = TRUE),
            Tair = mean(Tair, na.rm = TRUE),
            ET = sum(ET, na.rm = TRUE),
            VPD = mean(VPD, na.rm = TRUE),
            RAIN = sum(RAIN, na.rm = TRUE),
            NEE_CO2_MDS2 = sum(NEE_CO2_MDS2, na.rm = TRUE)
  ) %>% 
  select(-c(datetime, SENTEK3, TENSIO2, TENSIO3))

cycle4 <- Langeweide_night %>% 
  filter(datetime >= "2022-08-19" & datetime <= "2022-09-07") %>% 
  select(-c(datetime))

cycle3 <- Langeweide_night %>% 
  filter(datetime >= "2022-08-01" & datetime <= "2022-08-18") %>% 
  select(-c(datetime))




Langeweide_PCA <- na.omit(Langeweide_PCA)
Langeweide_PCA <- na.omit(Langeweide_night)
Langeweide_PCA <- na.omit(cycle3)

pca_result <- prcomp(Langeweide, scale = TRUE)

names(pca_result)

pca_result$center #mean
pca_result$scale #stdev

pca_result$rotation

pca_result$rotation <- -pca_result$rotation
pca_result$rotation

pca_result$x <- -pca_result$x
head(pca_result$x)

biplot(pca_result, scale = 0)

plot(pca_result$sdev^2, type = "b", pch = 16, main = "Scree Plot", xlab = "Principal Component", ylab = "Variance Explained")


nb <- estim_ncpPCA(Langeweide, scale = TRUE)

comp <- imputePCA(Langeweide_missing, ncp = 5, scale = TRUE)

pca_result_fill <- prcomp(comp$completeObs, scale = TRUE)

pca_result_fill$rotation <- -pca_result_fill$rotation
pca_result_fill$rotation

biplot(pca_result_fill, scale = 0)




numeric_columns <- sapply(LAW_MS_ICOS, is.numeric)

# Remove numeric columns
Langeweide <- LAW_MS_ICOS[, numeric_columns]
str


