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
end_time <- max(GWL$datetime)

#make sure dates are correct
GWL_old <- GWL_old %>% 
  filter(datetime >= start_time & datetime <= end_time)


