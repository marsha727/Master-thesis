#Subscript for tensiometer interpolation

library(tidyverse)
library(purrr)

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

{#new options
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
tensio_long$TIMESTAMP <- as.POSIXct(tensio_long$TIMESTAMP)

#linear model for soil matrix potential and depth
lm_depth_MP <- lm(SoilMatrixPotential ~ TIMESTAMP + Depth, data = tensio_long )

Depths_to_interpolate <- seq(20, 60, by = 1)
Times_to_interpolate <- unique(tensio_long$TIMESTAMP)

new_data <- data.frame(Depth = rep(Depths_to_interpolate, each = length(Times_to_interpolate)))

new_data$TIMESTAMP <- rep(seq(min(tensio_long$TIMESTAMP), max(tensio_long$TIMESTAMP),
                              length.out = length(unique(tensio_long$TIMESTAMP))),
                              times = length(Depths_to_interpolate))

#new_data <- expand.grid(Depth = Depths_to_interpolate, TIMESTAMP = Times_to_interpolate)

predict_values <- predict(lm_depth_MP, newdata = new_data)

predicted_tensio <- data.frame(
  Depth = rep(Depths_to_interpolate, each = length(unique(tensio_long$TIMESTAMP))),
  TIMESTAMP = rep(unique(tensio_long$TIMESTAMP), times = length(Depths_to_interpolate)),
  SoilMatrixPotential = predict_values
)
}

{#new options
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

tensio_long$SoilMatrixPotential <- ave(tensio_long$SoilMatrixPotential, tensio_long$Depth, FUN = function(x) cumsum(!is.na(x)))

# Function to fit linear model for each time step
fit_lm_by_time <- function(data) {
  lm(SoilMatrixPotential ~ Depth, data = data)
}

# Fit separate linear models for each time step
lm_models <- tensio_long %>%
  group_split(TIMESTAMP) %>%
  map(fit_lm_by_time)

# Create a data frame for prediction with all combinations of Depth and TIMESTAMP
Depths_to_interpolate <- as.numeric(seq(20, 60, by = 2))
Times_to_interpolate <- unique(tensio_long$TIMESTAMP)
new_data <- expand.grid(Depth = Depths_to_interpolate, TIMESTAMP = Times_to_interpolate)

# Predict values using the fitted linear models
predict_values <- map(lm_models, ~ predict(.x, newdata = new_data))

# Combine the predicted values into a data frame
predicted_tensio <- data.frame(
  Depth = rep(Depths_to_interpolate, each = length(Times_to_interpolate)),
  TIMESTAMP = rep(Times_to_interpolate, times = length(Depths_to_interpolate)),
  SoilMatrixPotential = predict_values
)
}

#Interpolation method

#make subselection for tensio point 2 and 3
tensio_2 <- tensio %>% 
  select(TIMESTAMP, MS_TMAP_4_D_020, MS_TMAP_5_D_040, MS_TMAP_6_D_060)

#replace 1 NA with previous value to prevent error
tensio_2$MS_TMAP_4_D_020 <- ifelse(is.na(tensio_2$MS_TMAP_4_D_020), lag(tensio_2$MS_TMAP_4_D_020), tensio_2$MS_TMAP_4_D_020)
tensio_2$MS_TMAP_5_D_040 <- ifelse(is.na(tensio_2$MS_TMAP_5_D_040), lag(tensio_2$MS_TMAP_5_D_040), tensio_2$MS_TMAP_5_D_040)
tensio_2$MS_TMAP_6_D_060 <- ifelse(is.na(tensio_2$MS_TMAP_6_D_060), lag(tensio_2$MS_TMAP_6_D_060), tensio_2$MS_TMAP_6_D_060)

tensio_long <- tibble(date = seq(as.POSIXct("2022-04-02 00:00:00"),
                                 as.POSIXct("2023-02-28 22:00:00"),
                                 by = "30 mins"),
                      Depth_20 = tensio_2$MS_TMAP_4_D_020,
                      Depth_40 = tensio_2$MS_TMAP_5_D_040,
                      Depth6_0 = tensio_2$MS_TMAP_6_D_060)

#First i say which depths to interpolate (= every 1 cm) then i say do each depth at every date
Depths_to_interpolate <- sort(unique(c(c(20, 40, 60), seq(ceiling(20), floor(60), 1))))
Depths_to_interpolate <- crossing(date = unique(tensio_long$date), depth = Depths_to_interpolate)
  
tensio_interp <- tensio_long %>% 
  gather(depth, value, -date) %>% 
  mutate(depth = as.numeric(gsub("\\D", "", depth))) %>% 
  full_join(Depths_to_interpolate) %>% 
  arrange(date, depth) %>% 
  group_by(date) %>% 
  mutate(value.interp = if(length(na.omit(value)) > 1) { 
    approx(depth, value, xout = depth)$y
  } else{
    value
  })









  
#check out these missing row later on!!
full_sequence <- seq(from = as.POSIXct("2022-04-02 00:00:00"), 
                     to = as.POSIXct("2023-02-28 23:00:00"), 
                     by = "30 mins")     

missing_datetimes <- as.POSIXct(setdiff(full_sequence, tensio_2$TIMESTAMP))



