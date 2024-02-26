library(tidyverse)
library(EnvStats)
library(data.table)

Langeweide_data <- readRDS("Datasets/LAW_MS_ICOS.rds")

ET <- Langeweide_data %>% 
  select(datetime, sunrise, sunset, Tair, ET, VPD, RH, bowen_ratio, Tdew_EP, RAIN, WIND, LE_flag, SWIN, SWOUT, LWIN, LWOUT, NEE_H)
           
#filters the LE flag conditions 2, as ET based on LE
ET$ET[ET$LE_flag == 2] <- NA

#ET$ET[ET$LE_flag == 1] <- NA

#filter for wind conditions that are from SW (least water influence)
#ET_wind <- ET %>% 
  #filter(!WIND >= 210 & WIND <= 240)

#matching_row <- which(ET$datetime %in% ET_wind$datetime) 

#ET[matching_row, "ET"] <- NA

#Percentiles filter for outliers
lower_bound <- quantile(ET$ET, na.rm = TRUE, 0.01)
upper_bound <- quantile(ET$ET, na.rm = TRUE, 0.99)

outlier_p <- which(ET$ET < lower_bound | ET$ET > upper_bound)

outlier_p_ET <- data.frame(ET[outlier_p, ])

#ET_p_filtered <- ET

ET[outlier_p, "ET"] <- NA

#after filtering daily values can be computed

ET$sunset <- as.POSIXct(ET$sunset, format = "%Y-%m-%d %H:%M:%S")
ET$sunrise <- as.POSIXct(ET$sunrise, format = "%Y-%m-%d %H:%M:%S")

ET$within_range <- ET$datetime >= ET$sunrise & ET$datetime <= ET$sunset

#daily values but ensure to exclude days with too few data points
ET_d <- ET %>%
  group_by(datetime = format(datetime, "%Y-%m-%d")) %>%
  summarise(
    Tair = ifelse(sum(!is.na(Tair)) >= 36, mean(Tair, na.rm = TRUE), NA),
    ET = ifelse(sum(!is.na(ET)) >= 36, sum(ET, na.rm = TRUE), NA),
    VPD = ifelse(sum(!is.na(VPD)) >= 36, mean(VPD, na.rm = TRUE), NA),
    RH = ifelse(sum(!is.na(RH)) >= 36, mean(RH, na.rm = TRUE), NA),
    Tdew_EP = ifelse(sum(!is.na(Tdew_EP)) >= 36, mean(Tdew_EP, na.rm = TRUE), NA),
    RAIN = ifelse(sum(!is.na(RAIN)) >= 36, sum(RAIN, na.rm = TRUE), NA),
    WIND = ifelse(sum(!is.na(WIND)) >= 36, mean(WIND, na.rm = TRUE), NA),
    bowen_ratio = ifelse(sum(!is.na(bowen_ratio)) >= 36, mean(bowen_ratio, na.rm = TRUE), NA),
    SWIN = ifelse(sum(!is.na(SWIN)) >= 36, mean(SWIN, na.rm = TRUE), NA),
    SWOUT = ifelse(sum(!is.na(SWOUT)) >= 36, mean(SWOUT, na.rm = TRUE), NA),
    LWIN = ifelse(sum(!is.na(LWIN)) >= 36, mean(LWIN, na.rm = TRUE), NA),
    LWOUT = ifelse(sum(!is.na(LWOUT)) >= 36, mean(LWOUT, na.rm = TRUE), NA),
    NEE_H = ifelse(sum(!is.na(NEE_H)) >= 36, mean(NEE_H, na.rm = TRUE), NA)
  )

#Filter for the negative values
ET_neg <- ET_d %>% 
  filter(ET < 0)

#lets test for Tair = Tdew conditions and P > 0 conditions
RH_check <- ET_neg %>% 
  filter(RH > 95) %>% 
  filter(abs(Tdew_EP - Tair) > 1 | RAIN > 0) 

matching_row <- which(ET$datetime %in% RH_check$datetime) 

ET[matching_row, "ET"] <- NA

#filter for wind conditions that are from SW (least water influence)
ET_wind <- ET_d %>% 
  filter(!WIND >= 210 & WIND <= 240)

matching_row <- which(ET_d$datetime %in% ET_wind$datetime) 

ET_d[matching_row, "ET"] <- NA

#calculate EF by converting back to LE and using sensible heat flux
ET_d <- ET_d %>%   
  mutate(LE = (ET * 2.45) / 0.0864) %>% #LE of vapor and conversion factor W/m2
  mutate(EF3 = LE / (LE + (NEE_H / 0.0864)))


#For convienience of plotting in the App i do one that is already
#aggregated to daily values (for EF) and one halfhourly

#make sure new datetime is in correct formatting
ET_d$datetime <- as.POSIXct(ET_d$datetime, format = "%Y-%m-%d")
ET$datetime <- as.POSIXct(ET$datetime, format = "%Y-%m-%d %H:%M:%S")

#format for writing
ET_d$datetime <- format(ET_d$datetime, "%Y-%m-%d")
ET$datetime <- format(ET$datetime, "%Y-%m-%d %H:%M:%S")

write_rds(ET, file = "App/Langeweide_ET_halfhour.rds")
write.csv2(ET, file = "App/Langeweide_ET_halfhour.csv", row.names = FALSE)

#file writing
write.csv2(ET_d, file = "App/Langeweide_ET_day.csv", row.names = FALSE)

write_rds(ET_d, file = "App/Langeweide_ET.rds")
write_rds(ET_d, file = "App/Langeweide_ET_noWindCor.rds")

test <- read.csv2("App/Langeweide_ET_halfhour.csv")
test <- read.csv2("App/Langeweide_ET_day.csv")
