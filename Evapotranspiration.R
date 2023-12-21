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

#Percentiles = closer to the boxplot quantiles
lower_bound <- quantile(ET$ET, na.rm = TRUE, 0.01)
upper_bound <- quantile(ET$ET, na.rm = TRUE, 0.99)

outlier_p <- which(ET$ET < lower_bound | ET$ET > upper_bound)

outlier_p_ET <- data.frame(ET[outlier_p, ])

#ET_p_filtered <- ET

ET[outlier_p, "ET"] <- NA

#Percentiles for ET LE flag 2
#lower_bound_f2 <- quantile(ET_filter_f2$ET, na.rm = TRUE, 0.01)
#upper_bound_f2 <- quantile(ET_filter_f2$ET, na.rm = TRUE, 0.99)

#outlier_p_f2 <- which(ET_filter_f2$ET < lower_bound | ET_filter_f2$ET > upper_bound)

#outlier_p_ET_f2 <- data.frame(ET_filter_f2[outlier_p_f2, ])

#ET_p_filtered_f2 <- ET_filter_f2

#ET_p_filtered_f2[outlier_p_f2, "ET"] <- NA

#after filtering daily values can be computed

ET$sunset <- as.POSIXct(ET$sunset, format = "%Y-%m-%d %H:%M:%S")
ET$sunrise <- as.POSIXct(ET$sunrise, format = "%Y-%m-%d %H:%M:%S")

ET$within_range <- ET$datetime >= ET$sunrise & ET$datetime <= ET$sunset


# this gives SWIN a 0 (for night) but only if its within 3 hours sunset/sunrise
ET$SWIN <- ifelse(
  is.na(ET$SWIN) & (
    !ET$within_range |
      shift(!ET$within_range, 1) |
      shift(!ET$within_range, 2) |
      shift(!ET$within_range, 3) |
      shift(!ET$within_range, -1) |  
      shift(!ET$within_range, -2) |
      shift(!ET$within_range, -3) 
  ),
  0,
  ET$SWIN
)

ET$SWOUT <- ifelse(
  is.na(ET$SWOUT) & (
    !ET$within_range |
      shift(!ET$within_range, 1) |
      shift(!ET$within_range, 2) |
      shift(!ET$within_range, 3) |
      shift(!ET$within_range, -1) |  # Include the previous row
      shift(!ET$within_range, -2) |
      shift(!ET$within_range, -3) # Include two rows before
  ),
  0,
  ET$SWOUT
)

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

#test if these high RH are also present in other RH measurements
{Sat_dates <- ET_neg %>% 
  select(datetime)

RH_measurements <- Langeweide_data %>% 
  select(datetime, RH, RH_NOBV1, RH_NOBV2, RH_KNMI)

RH_measurements_d <- RH_measurements %>% 
  group_by(datetime = format(datetime, "%Y-%m-%d")) %>% 
  summarise(
    RH = ifelse(sum(!is.na(RH)) >= 36, mean(RH, na.rm = TRUE), NA),
    RH_NOBV1 = ifelse(sum(!is.na(RH_NOBV1)) >= 36, mean(RH_NOBV1, na.rm = TRUE), NA),
    RH_NOBV2 = ifelse(sum(!is.na(RH_NOBV2)) >= 36, mean(RH_NOBV2, na.rm = TRUE), NA),
    RH_KNMI = ifelse(sum(!is.na(RH_KNMI)) >= 18, mean(RH_KNMI, na.rm = TRUE), NA),
  )

RH_measurements_d$datetime <- as.POSIXct(RH_measurements_d$datetime, format = "%Y-%m-%d")

RH_measurements_neg <- RH_measurements_d %>% 
  filter(datetime %in% Sat_dates$datetime)
}

#lets test for Tair = Tdew conditions and P > 0 conditions
RH_check <- ET_neg %>% 
  filter(RH > 95) %>% 
  filter(abs(Tdew_EP - Tair) > 1 | RAIN > 0) 

matching_row <- which(ET_d$datetime %in% RH_check$datetime) 

ET_d[matching_row, "ET"] <- NA

#filter for wind conditions that are from SW (least water influence)
ET_wind <- ET_d %>% 
  filter(!WIND >= 210 & WIND <= 240)

matching_row <- which(ET_d$datetime %in% ET_wind$datetime) 

ET_d[matching_row, "ET"] <- NA

#yearly sum of ET
#ET_y <- ET %>% 
  #summarise(Tair = mean(Tair, na.rm = T), ET = sum(ET, na.rm = T), VPD = mean(VPD, na.rm = T), RH = mean(RH, na.rm = T))

#calculate EF by converting back to LE and using sensible heat flux
ET_d <- ET_d %>%   
  mutate(LE = (ET * 2.45) / 0.0864) %>% #LE of vapor and conversion factor W/m2
  mutate(Rn = SWIN - SWOUT + LWIN - LWOUT) %>% 
  mutate(EF1 = LE / Rn) %>% 
  mutate(EF2 = 1 / (1 + bowen_ratio)) %>% 
  mutate(EF3 = LE / (LE + (NEE_H / 0.0864))) %>% 
  mutate(T = LE + (NEE_H / 0.0864))

ET_d1 <- ET_d %>% 
  filter(EF1 < 1 & EF1 > -1)

ET_d1$datetime <- as.POSIXct(ET_d1$datetime, format = "%Y-%m-%d")

#make sure new datetime is in correct formatting
ET_d$datetime <- as.POSIXct(ET_d$datetime, format = "%Y-%m-%d")

#format for writing
ET_d$datetime <- format(ET_d$datetime, "%Y-%m-%d")

#file writing
write.csv2(ET_d, file = "Transformed/ET_langeweide.csv")

write_rds(ET_d, file = "App/Langeweide_ET.rds")

test.read <- read.csv2("Transformed/ET_langeweide.csv")



#Checking relationships

#filtered ET overtime
ggplot(ET_p_filtered_f2) +
  geom_point(aes(x = datetime, y = ET, color = "ET")) +
  labs(
    title = "half hourly ET",
    x = "datetime",
    y = "ET (mm)"
  ) +
  scale_color_manual(
    values = c("ET" = "darkblue"),
    name = "Legend"
  ) +
  theme(
    plot.title = element_text(size = 18, hjust = 0.5),
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 9),
  )

#daily sum ET filtered
ggplot(ET_d) +
  geom_point(aes(x = datetime, y = LE, color = "LE"), size = 1 ) +
  geom_point(aes(x = datetime, y = H, color = "H"), size = 1) +
  geom_point(aes(x = datetime, y = ))
  labs(
    title = "Net radiation (Rn) and Latent heat (LE)",
    x = "datetime",
    y = "RN/LE (W/m2)"
  ) +
  scale_color_manual(
    values = c("LE" = "skyblue", "H" = "tomato"),
    name = "Legend"
  ) +
  theme(
    plot.title = element_text(size = 18, hjust = 0.5),
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 9),
  )

  #EF
  ggplot(ET_d) +
    geom_point(aes(x = datetime, y = EF3, color = "EF"), size = 1.5 ) +
  labs(
    title = "The evaporative fraction (EF)",
    x = "datetime",
    y = "EF (0-1)"
  ) +
    scale_color_manual(
      values = c("EF" = "skyblue"),
      name = "Legend"
    ) +
    theme(
      plot.title = element_text(size = 18, hjust = 0.5),
      legend.title = element_text(size = 10),
      legend.text = element_text(size = 9),
    )  


ggplot(ET) +
  geom_point(aes(x = RH, y = VPD))

ggplot(outliers_ET) +
  geom_point(aes(x = Tair, y = ET))

ggplot(RH_VPD) +
  geom_point(aes(x = VPD, y = ET))


Rad <- Langeweide_data %>% 
  select(datetime, sunrise, sunset, SWIN_KNMI, SWIN, SWOUT, LWIN, LWOUT, bowen_ratio, ET, NEE_H)

Rad$sunset <- as.POSIXct(Rad$sunset, format = "%Y-%m-%d %H:%M:%S")
Rad$sunrise <- as.POSIXct(Rad$sunrise, format = "%Y-%m-%d %H:%M:%S")

Rad$within_range <- Rad$datetime >= Rad$sunrise & Rad$datetime <= Rad$sunset

Rad$SWIN <- ifelse(is.na(Rad$SWIN), 0, Rad$SWIN)
Rad$SWOUT <- ifelse(is.na(Rad$SWOUT), 0, Rad$SWOUT)

Rad <- Rad %>% 
  mutate(Rn = SWIN - SWOUT + LWIN - LWOUT) #%>% 
#group_by(Hour = format(datetime, "%H")) %>% 
#summarise(Rn = mean(Rn, na.rm = T))

Rad <- Rad %>% 
  mutate(EF_b = 1 / (bowen_ratio + 1))

Rad <- Rad %>% 
  group_by(datetime = format(datetime, "%Y-%m-%d")) %>% 
  summarise(ET = )

Rad <- Rad %>% 
  mutate(LE = (ET * 2.45) / 0.0864) %>%  #LE of vapor en omreken factor W/m2
  mutate(EF = LE / (LE - NEE_H))
