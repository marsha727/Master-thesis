library(tidyverse)
library(EnvStats)

Langeweide_data <- readRDS("Datasets/LAW_MS_ICOS.rds")

ET <- Langeweide_data %>% 
  select(datetime, sunrise, sunset, Tair, ET, VPD, RH, bowen_ratio, Tdew_EP, RAIN, WIND, SWIN, Ustar, CO2_flag, NEE_CO2)

ET$sunset <- as.POSIXct(ET$sunset, format = "%Y-%m-%d %H:%M:%S")
ET$sunrise <- as.POSIXct(ET$sunrise, format = "%Y-%m-%d %H:%M:%S")

#histogram and boxplot for distribution
bin_width <- 0.5

breaks <- seq(from = min(ET$ET, na.rm = T), to = max(ET$ET, na.rm = T) + bin_width, by = bin_width)

print(breaks)

hist(ET$ET, main = "Histogram ET", xlab = "Values", ylab = "Frequency", col = "lightblue", border = "black", breaks = breaks, ylim = c(0,12))

#boxplots for outliers and identify with which variable
boxplot(ET$ET)

boxplot(ET$ET ~ ET$Ustar, ET = ET)

#Tukey's method
{qnt <- quantile(ET$ET, na.rm = T)
iqr <- IQR(ET$ET, na.rm = T)

lower <- qnt[2] - 1.5 * iqr
upper <- qnt[4] + 1.5 * iqr

outliers <- ET$ET < lower | ET$ET > upper

outliers_ET <- data.frame(ET[outliers, ])

ET_filtered <- ET[!outliers, ]
}

#Percentiles = closer to the boxplot quantiles
lower_bound <- quantile(ET$ET, na.rm = TRUE, 0.01)
upper_bound <- quantile(ET$ET, na.rm = TRUE, 0.99)

outlier_p <- which(ET$ET < lower_bound | ET$ET > upper_bound)

outlier_p_ET <- data.frame(ET[outlier_p, ])

ET_p_filtered <- ET

ET_p_filtered[outlier_p, "ET"] <- NA

#Hampel filter dont like this one
{lower_bound_h <- median(ET$ET, na.rm = TRUE) - 3 * mad(ET$ET, constant = 1, na.rm = TRUE)
upper_bound_h <- median(ET$ET, na.rm = TRUE) + 3 * mad(ET$ET, constant = 1, na.rm = TRUE)
}

#Rosner test, this is actually nice but not good for non normal distrubution
{Rosners_test <- rosnerTest(ET$ET, k = 308)

results_rosner <- Rosners_test$all.stats

outliers_rosner_row <- results_rosner$Obs.Num[results_rosner$Outlier]

outliers_rosner <- data.frame(ET[outliers_rosner_row, ])

ET_filtered_rosner <- ET[-outliers_rosner_row, ]
}

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
    bowen_ratio = ifelse(sum(!is.na(bowen_ratio)) >= 36, mean(bowen_ratio, na.rm = TRUE), NA)
  )

#Filter for the negative values
ET_neg <- ET_d %>% 
  filter(ET < 0)

#test if these high RH are also present in other RH measurements
Sat_dates <- ET_neg %>% 
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


#lets test for Tair = Tdew conditions and P > 0 conditions
RH_check <- ET_neg %>% 
  filter(RH > 95) %>% 
  filter(abs(Tdew_EP - Tair) > 1 & RAIN == 0 | abs(Tdew_EP - Tair) > 2)

matching_row <- which(ET_d$datetime %in% RH_check$datetime) 

ET_d[matching_row, "ET"] <- NA


#yearly sum of ET
ET_y <- ET %>% 
  summarise(Tair = mean(Tair, na.rm = T), ET = sum(ET, na.rm = T), VPD = mean(VPD, na.rm = T), RH = mean(RH, na.rm = T))

#make sure new datetime is in correct formatting
ET_d$datetime <- as.POSIXct(ET_d$datetime, format = "%Y-%m-%d")
ET_n$datetime <- as.POSIXct(ET_n$datetime, format = "%Y-%m-%d")


#Checking relationships

#filtered ET overtime
ggplot(ET_filtered_rosner) +
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

