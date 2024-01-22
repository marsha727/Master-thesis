#principle component analysis
library(tidyverse)
library(kernlab)
library(missMDA)

AFPS_NEE_WL_Tair <- readRDS("Datasets/Extracted/AFPS_NEE_WL_Tair.rds")
PCA_set <- readRDS("Langeweide/Statistics_file.rds")

cycle4 <- PCA_set %>% 
  filter(datetime >= "2022-08-19" & datetime <= "2022-09-07") %>% 
  select(-c(datetime))

cycle3 <- PCA_set %>% 
  filter(datetime >= "2022-08-01" & datetime <= "2022-08-18") %>% 
  select(-c(datetime))

PCA_set <- PCA_set %>% 
  select(-c(datetime, AFPS_mean, ET, EF, OWASIS))

Langeweide_PCA <- na.omit(PCA_set)

pca_result <- prcomp(Langeweide_PCA, scale = TRUE)

names(pca_result)

pca_result$center #mean
pca_result$scale #stdev

pca_result$rotation

pca_result$rotation <- -pca_result$rotation
pca_result$rotation

sumpca_result$x <- -pca_result$x
head(pca_result$x)

biplot(pca_result, var.axes = TRUE)

plot(pca_result$sdev^2, type = "b", pch = 16, main = "Scree Plot", xlab = "Principal Component", ylab = "Variance Explained")



#Missing values
nb <- estim_ncpPCA(PCA_set, scale = TRUE)

comp <- imputePCA(PCA_set, ncp = 0, scale = TRUE)

pca_result_fill <- prcomp(comp$completeObs, scale = TRUE)

pca_result_fill$rotation <- -pca_result_fill$rotation
pca_result_fill$rotation

biplot(pca_result_fill, var.axes = TRUE)

eigenvalues <- pca_result$sdev^2

cumulative_variance <- cumsum(eigenvalues) / sum(eigenvalues)
print(cumulative_variance)

# Print or inspect the eigenvalues
print(eigenvalues)




