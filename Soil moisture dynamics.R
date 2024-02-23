library(tidyverse)

AFPS <- readRDS("App/AFPS_int_TS.rds")
Langeweide_data <- readRDS("Datasets/LAW_MS_ICOS.rds")

Langeweide_sub <- Langeweide_data %>% 
  select(datetime, NEE_CO2, WL_cor, Tair, RAIN, VPD)

Langeweide_data_day <- Langeweide_data %>% 
  group_by(datetime = format(datetime, "%Y-%m-%d")) %>% 
  summarize(
    NEE_CO2 = sum(NEE_CO2, na.rm = T),
    WL_cor = mean(WL_cor, na.rm = T),
    Tair = mean(Tair, na.rm = T),
    RAIN = sum(RAIN, na.rm = T),
    VPD = mean(VPD, na.rm = T)
  )

start_date <- as.POSIXct("2022-04-02")
end_date <- as.POSIXct("2022-10-31")

Langeweide_data_day <- Langeweide_data_day %>% 
  filter(datetime >= start_date & datetime <= end_date)

Langeweide_data_day <- bind_cols(Langeweide_data_day, AFPS[ ,2:6])

Langeweide_data_day$datetime <- as.POSIXct(Langeweide_data_day$datetime, "%Y-%m-%d")

write_rds(Langeweide_data_day, "App/Langeweide_extended.rds")


