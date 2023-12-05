#Groundwater analysis

library(tidyverse)

GWL <- read.csv("Datasets/LAW_MS.csv")
Langeweide_data <- readRDS("Datasets/LAW_MS_ICOS.rds")

GWL_old <- Langeweide_data %>% 
  select(datetime, WL_1, WL_2, WL_3, WL_4, WL_5, WL_cor)

GWL$datetime <- as.POSIXct(GWL$datetime, format = "%Y-%m-%d %H:%M:%S")

#somehow the 00:00:00 times are removed so need to fix
GWL <- GWL %>% 
  mutate(datetime = as.POSIXct(ifelse(is.na(datetime), lag(datetime) + 3600, datetime)))

start_time <- min(GWL$datetime)
end_time <- max(GWL_old$datetime)

#make sure dates are correct
GWL_old <- GWL_old %>% 
  filter(datetime >= start_time & datetime <= end_time)

#order according to date
GWL <- GWL[order(GWL$datetime), ]

GWL_mmv <- GWL %>% 
  select(datetime, WL_1, WL_1_cal2, WL_2, WL_2_cal2, WL_3, WL_3_cal2, WL_4, WL_4_cal2) %>% 
  mutate(WL_1 = WL_1 + WL_1_cal2) %>% 
  mutate(WL_2 = WL_2 + WL_2_cal2) %>% 
  mutate(WL_3 = WL_3 + WL_3_cal2) %>% 
  mutate(WL_4 = WL_4 + WL_4_cal2) %>% 
  mutate(GWL_mean = rowMeans(select(., matches("^WL_[0-9]+$")), na.rm = TRUE))

GWL_mmv <- GWL_mmv %>% 
  filter(datetime >= start_time & datetime <= end_time)

GWL_half_hourly <- seq(from = start_time, to = end_time, by = "30 min")

GWL_half_hourly <- data.frame(datetime = GWL_half_hourly)

GWL_mmv <- GWL_half_hourly %>% 
  left_join(GWL_mmv, by = "datetime")

compare <- bind_cols(GWL_mmv$GWL_mean, GWL_old$WL_cor)
