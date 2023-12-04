library(tidyverse)
library(EnvStats)

Langeweide_data <- readRDS("Datasets/LAW_MS_ICOS.rds")

ET <- Langeweide_data %>% 
  select(datetime, sunrise, sunset, Tair, ET, VPD, RH, bowen_ratio, Tdew_EP, RAIN, WIND, SWIN, Ustar, CO2_flag, NEE_CO2, LE_flag, SWIN_KNMI, SWOUT, LWIN, LWOUT)

ET$sunset <- as.POSIXct(ET$sunset, format = "%Y-%m-%d %H:%M:%S")
ET$sunrise <- as.POSIXct(ET$sunrise, format = "%Y-%m-%d %H:%M:%S")

#filters the LE flag conditions 2, as ET based on LE
ET$ET[ET$LE_flag == 2] <- NA

ET_filter_f2 <- ET 
ET_filter_f2$ET[ET_filter_f2$LE_flag == 1] <- NA

#Percentiles = closer to the boxplot quantiles
lower_bound <- quantile(ET$ET, na.rm = TRUE, 0.01)
upper_bound <- quantile(ET$ET, na.rm = TRUE, 0.99)

outlier_p <- which(ET$ET < lower_bound | ET$ET > upper_bound)

outlier_p_ET <- data.frame(ET[outlier_p, ])

ET_p_filtered <- ET

ET_p_filtered[outlier_p, "ET"] <- NA

#Percentiles for ET LE flag 2
lower_bound_f2 <- quantile(ET_filter_f2$ET, na.rm = TRUE, 0.01)
upper_bound_f2 <- quantile(ET_filter_f2$ET, na.rm = TRUE, 0.99)

outlier_p_f2 <- which(ET_filter_f2$ET < lower_bound | ET_filter_f2$ET > upper_bound)

outlier_p_ET_f2 <- data.frame(ET_filter_f2[outlier_p_f2, ])

ET_p_filtered_f2 <- ET_filter_f2

ET_p_filtered_f2[outlier_p_f2, "ET"] <- NA

#after filtering daily values can be computed

#daily values but ensure to exclude days with too few data points
ET_d <- ET_p_filtered %>%
  group_by(datetime = format(datetime, "%Y-%m-%d")) %>%
  summarise(
    Tair = ifelse(sum(!is.na(Tair)) >= 36, mean(Tair, na.rm = TRUE), NA),
    ET = ifelse(sum(!is.na(ET)) >= 36, sum(ET, na.rm = TRUE), NA),
    VPD = ifelse(sum(!is.na(VPD)) >= 36, mean(VPD, na.rm = TRUE), NA),
    RH = ifelse(sum(!is.na(RH)) >= 36, mean(RH, na.rm = TRUE), NA),
    Tdew_EP = ifelse(sum(!is.na(Tdew_EP)) >= 36, mean(Tdew_EP, na.rm = TRUE), NA),
    RAIN = ifelse(sum(!is.na(RAIN)) >= 36, sum(RAIN, na.rm = TRUE), NA),
    WIND = ifelse(sum(!is.na(WIND)) >= 36, mean(WIND, na.rm = TRUE), NA),
    SWIN = ifelse(sum(!is.na(SWIN)) >= 36, mean(SWIN, na.rm = TRUE), NA),
    Ustar = ifelse(sum(!is.na(Ustar)) >= 36, mean(Ustar, na.rm = TRUE), NA),
    bowen_ratio = ifelse(sum(!is.na(bowen_ratio)) >= 36, mean(bowen_ratio, na.rm = TRUE), NA),
    SWIN = ifelse(sum(!is.na(SWIN_KNMI)) >= 1, mean(SWIN_KNMI, na.rm = TRUE), NA),
    SWOUT = ifelse(sum(!is.na(SWOUT)) >= 1, mean(SWOUT, na.rm = TRUE), NA),
    LWIN = ifelse(sum(!is.na(LWIN)) >= 1, mean(LWIN, na.rm = TRUE), NA),
    LWOUT = ifelse(sum(!is.na(LWOUT)) >= 1, mean(LWOUT, na.rm = TRUE), NA)
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

{#lets test for Tair = Tdew conditions and P > 0 conditions
RH_check <- ET_neg %>% 
  filter(RH > 95) %>% 
  filter(abs(Tdew_EP - Tair) > 1 & RAIN == 0 | abs(Tdew_EP - Tair) > 2)

matching_row <- which(ET_d$datetime %in% RH_check$datetime) 

ET_d[matching_row, "ET"] <- NA
}

{#yearly sum of ET
ET_y <- ET %>% 
  summarise(Tair = mean(Tair, na.rm = T), ET = sum(ET, na.rm = T), VPD = mean(VPD, na.rm = T), RH = mean(RH, na.rm = T))
}
#make sure new datetime is in correct formatting
ET_d$datetime <- as.POSIXct(ET_d$datetime, format = "%Y-%m-%d")
ET_n$datetime <- as.POSIXct(ET_n$datetime, format = "%Y-%m-%d")

#format for writing
ET_d$datetime <- format(ET_d$datetime, "%Y-%m-%d")

#file writing
write.csv2(ET_d, file = "Transformed/ET_langeweide.csv")

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
  geom_line(aes(x = datetime, y = ET, color = "ET"), size = 0.55) +
  labs(
    title = "daily ET",
    x = "datetime",
    y = "ET (mm/d)"
  ) +
  scale_color_manual(
    values = c("ET" = "tomato"),
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

check <- ET[ET$CO2_flag == 2 & is.na(ET$NEE_CO2), ]





#experiment to convert to LE... forget this for now
LE_of_vapor <- 2.45

ET_d <- ET_d %>% 
  mutate(LE = (ET * 2.45) / 0.0864) %>% 
  mutate(Rn = (SWIN + LWIN - SWOUT - LWOUT)) %>% 
  mutate(EF = LE / Rn)
