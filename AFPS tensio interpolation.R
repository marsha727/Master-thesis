#Subscript for tensiometer interpolation

library(tidyverse)

tensio <- read.csv("Datasets/LAW_TENS_2020-2023_clean.csv")

tensio$TIMESTAMP <- as.POSIXct(tensio$TIMESTAMP, format = "%Y-%m-%d %H:%M:%S")

tensio <- tensio %>% 
  select(TIMESTAMP, MS_TMAP_4_D_020, MS_TMAP_5_D_040, MS_TMAP_6_D_060,
         MS_TMAP_7_D_020, MS_TMAP_8_D_040, MS_TMAP_9_D_060)

#Dealing with missing values
cor <- cor(tensio$MS_TMAP_4_D_020, tensio$MS_TMAP_7_D_020, method = "pearson", use = "complete.obs")
cor2 <- cor(tensio$MS_TMAP_5_D_040, tensio$MS_TMAP_8_D_040, method = "pearson", use = "complete.obs")

per <- mean(tensio$MS_TMAP_4_D_020 / tensio$MS_TMAP_7_D_020, na.rm = TRUE)
per2 <- mean(tensio$MS_TMAP_5_D_040 / tensio$MS_TMAP_8_D_040, na.rm = TRUE)

missing_values_indices <- which(is.na(tensio$MS_TMAP_4_D_020))
missing_values_indices2 <- which(is.na(tensio$MS_TMAP_8_D_040))

# Automatically find the start and end indices of the gap
start_index <- min(missing_values_indices)
end_index <- max(missing_values_indices)

start_index2 <- min(missing_values_indices2)
end_index2 <- max(missing_values_indices2)

# Use the correlation to estimate missing values of MS_TMAP_4_D_020 based on MS_TMAP_7_D_020
tensio$MS_TMAP_4_D_020[start_index:end_index] <-
  tensio$MS_TMAP_7_D_020[start_index:end_index] * per

tensio$MS_TMAP_8_D_040[start_index2:end_index2] <-
  tensio$MS_TMAP_5_D_040[start_index2:end_index2] * per2

#Interpolation of tensiometer
{Depths_to_interpolate <- seq(20, 60, by = 1)

model_data <- AFPS_TENSIO %>% 
  select(TIMESTAMP, MS_TMAP_4_D_020, MS_TMAP_5_D_040, MS_TMAP_6_D_060) %>% 
  rename(Depth_20 = MS_TMAP_4_D_020, Depth_40 = MS_TMAP_5_D_040, Depth_60 = MS_TMAP_6_D_060)

model_data_long <- model_data %>% 
  gather(key = "Depths", value = "Value", -TIMESTAMP) %>%
  mutate(Depth = as.numeric(gsub("Depth_", "", Depths)),
         InteractionTerm = interaction(Depth, TIMESTAMP))

model <- lm(Value ~ InteractionTerm, data = model_data_long)

interp_data <- expand.grid(Depth = Depths_to_interpolate, Time = unique(model_data$TIMESTAMP))
interp_data$InteractionTerm <- interaction(interp_data$Depth, interp_data$Time)

interp_data$Value <- predict(model, newdata = interp_data)
} 

#new options
tensio_2 <- tensio %>% 
  select(TIMESTAMP, MS_TMAP_4_D_020, MS_TMAP_5_D_040, MS_TMAP_6_D_060) %>% 
  rename(Depth_20 = MS_TMAP_4_D_020, Depth_40 = MS_TMAP_5_D_040, Depth_60 = MS_TMAP_6_D_060)

#create a new tensio file that sorts all SMP values in one column and a depth
tensio_long <- tensio_2 %>% 
  pivot_longer(cols = -TIMESTAMP,
               names_to = "Column",
               values_to = "SoilMatrixPotential") %>% 
  mutate(Depth = str_extract(Column, "\\d+$")) %>% 
  select(-Column)

tensio_long$Depth <- as.numeric(tensio_long$Depth)

#linear model for soil matrix potential and depth
lm_depth_MP <- lm(SoilMatrixPotential ~ Depth + TIMESTAMP, data = tensio_long )

Depths_to_interpolate <- seq(20, 60, by = 1)
Times_to_interpolate <- unique(tensio_long$TIMESTAMP)

new_data <- expand.grid(Depth = Depths_to_interpolate, TIMESTAMP = Times_to_interpolate)

predict_values <- predict(lm_depth_MP, newdata = new_data)

predicted_tensio <- data.frame(
  Depth = rep(Depths_to_interpolate, each = length(Times_to_interpolate)),
  TIMESTAMP = rep(Times_to_interpolate, each = length(Depths_to_interpolate)),
  SoilMatrixPotential = predict_values
)

predicted_tensio$TIMESTAMP <- as.POSIXct(predicted_tensio$TIMESTAMP, format = "%Y-%m-%d %H:%M:%S")
