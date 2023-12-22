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

Tsoil <- Langeweide_data %>% 
  select(datetime, Tsoil_1_005, Tsoil_1_015, Tsoil_1_025, Tsoil_1_035,
         Tsoil_1_045, Tsoil_1_055, Tsoil_1_065, Tsoil_1_075, Tsoil_1_085,
         Tsoil_1_095, Tsoil_1_105, Tsoil_3_005, Tsoil_3_015, Tsoil_3_025, Tsoil_3_035,
         Tsoil_3_045, Tsoil_3_055, Tsoil_3_065, Tsoil_3_075, Tsoil_3_085,
         Tsoil_3_095, Tsoil_3_105) %>% 
  group_by(datetime = format(datetime, "%Y-%m-%d")) %>% 
  summarise(across(everything(), ~mean(., na.rm = TRUE))) %>% 
  filter(datetime >= start_date & datetime <= end_date) %>% 
  rename(datetime4 = datetime)
  

Langeweide <- cbind(AFPS, WL, ET, Tsoil) 

Langeweide <- Langeweide %>% 
  select(-c(EF1, EF2, datetime2, datetime3, T, datetime4))


