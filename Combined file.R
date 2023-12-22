#hello
library(tidyverse)

ET <- readRDS("App/Langeweide_ET.rds")
AFPS <- readRDS("App/AFPS_int_TS.rds")
Langeweide_data <- readRDS("Datasets/LAW_MS_ICOS.RDS")
WL <- readRDS("Transformed/Langeweide_groundwater_old.rds")

start_date <- min(AFPS$datetime)
end_date <- max(AFPS$datetime)

ET <- ET %>% 
  filter(datetime >= start_date & datetime <= end_date) %>% 
  rename(datetime2 = datetime)

WL <- WL %>% 
  group_by(datetime = format(datetime, "%Y-%m-%d")) %>% 
  summarise(WL = mean(GWL_mean, na.rm = TRUE)) %>% 
  filter(datetime >= start_date & datetime <= end_date) %>% 
  rename(datetime3 = datetime)

Langeweide <- cbind(AFPS, WL, ET) 

Langeweide <- Langeweide %>% 
  select(-c(EF1, EF2, datetime2, datetime3, T))
