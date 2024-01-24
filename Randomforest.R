#Random forest
library(randomForest)
library(caret)
library(e1071)
library(caTools)

Langeweide <- readRDS("Langeweide/Statistics_file.rds")

Langeweide <- Langeweide %>% 
  select(-c(ET, EF, AFPS_mean, datetime, OWASIS)) %>% 
  na.omit()

model <- randomForest(NEE_CO2_MDS_small ~., data = Langeweide, ntree = 50, do.trace = T)
print(model)
plot(model)

which.min(model$mse)
sqrt(model$mse[which.min(model$mse)]) 

mtry <- tuneRF(Langeweide[-1], Langeweide$NEE_CO2_MDS_small, stepFactor = 1.5, improve = 0.01, trace = T, plot = T, ntreeTry = 50)

best.m <- mtry[mtry[,2] == min(mtry[,2]), 1]

print(mtry)
print(best.m)


model2 <- randomForest(NEE_CO2_MDS_small~., data = Langeweide, ntree = 100, mtry = 11, importance = TRUE)

print(model2)

importance(model)
varImpPlot(model)

pred1 = predict(model2, type = "prob")
