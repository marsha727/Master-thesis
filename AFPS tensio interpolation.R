#Subscript for tensiometer interpolation

AFPS_TENSIO <- read.csv2("Transformed/Langeweide_Tensio_AFPS_mm_200.csv")

AFPS_TENSIO$TIMESTAMP <- as.POSIXct(AFPS_TENSIO$TIMESTAMP, format = "%Y-%m-%d %H:%M:%S")

#Dealing with missing values
cor <- cor(AFPS_TENSIO$MS_TMAP_4_D_020, AFPS_TENSIO$MS_TMAP_7_D_020, method = "pearson", use = "complete.obs")
cor2 <- cor(AFPS_TENSIO$MS_TMAP_5_D_040, AFPS_TENSIO$MS_TMAP_8_D_040, method = "pearson", use = "complete.obs")

missing_values_indices <- which(is.na(AFPS_TENSIO$MS_TMAP_4_D_020))
missing_values_indices2 <- which(is.na(AFPS_TENSIO$MS_TMAP_8_D_040))

# Automatically find the start and end indices of the gap
start_index <- min(missing_values_indices)
end_index <- max(missing_values_indices)

start_index2 <- min(missing_values_indices2)
end_index2 <- max(missing_values_indices2)

# Use the correlation to estimate missing values of MS_TMAP_4_D_020 based on MS_TMAP_7_D_020
AFPS_TENSIO$MS_TMAP_4_D_020[start_index:end_index] <-
  AFPS_TENSIO$MS_TMAP_7_D_020[start_index:end_index] * cor

AFPS_TENSIO$MS_TMAP_8_D_040[start_index2:end_index2] <-
  AFPS_TENSIO$MS_TMAP_5_D_040[start_index2:end_index2] * cor2

#Interpolation of tensiometer
Depths_to_interpolate <- seq(20, 60, by = 1)

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
  

