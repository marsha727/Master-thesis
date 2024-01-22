library(kernlab)
library(caTools)

AFPS_NEE_WL_Tair <- readRDS("Datasets/Extracted/AFPS_NEE_WL_Tair.rds")
PCA_set <- readRDS("Langeweide/Statistics_file.rds")

PCA_set <- PCA_set %>% 
  select(-c(AFPS_mean, ET, EF, OWASIS))

Langeweide_PCA <- na.omit(PCA_set)

set.seed(123)

split <- sample.split(Langeweide_PCA$datetime, SplitRatio = 0.7)
training_set <- subset(Langeweide_PCA, split == TRUE)
test_set <- subset(Langeweide_PCA, split == FALSE)
nrow(training_set)/nrow(Langeweide_PCA)
nrow(test_set)/nrow(Langeweide_PCA)

training_set = data.frame(scale(training_set))
test_set = data.frame(scale(test_set))

kcpa_result <- kpca(~., data = training_set, kernel = "rbfdot", features = 10)

pcv(kpca_result)

loadings <- kpca_result@rotated

training_set_pca <- as.data.frame(predict(kpca_result, training_set))
head(training_set_pca)
training_set_pca$NEE_CO2_MDS_small <- training_set$NEE_CO2_MDS_small

test_set_pca <- as.data.frame(predict(kpca_result, test_set))
test_set_pca$NEE_CO2_MDS_small <- test_set$NEE_CO2_MDS_small



kcpa <- kcpa(~., data=)