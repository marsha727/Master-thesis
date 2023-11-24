library(tidyverse)

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
qnt <- quantile(ET$ET, na.rm = T)
iqr <- IQR(ET$ET, na.rm = T)

lower <- qnt[2] - 1.5 * iqr
upper <- qnt[4] + 1.5 * iqr

outliers <- ET$ET < lower | ET$ET > upper

outliers_ET <- data.frame(ET[outliers, ])

ET_filtered <- ET[!outliers, ]

#Percentiles = closer to the boxplot quantiles
lower_bound <- quantile(ET$ET, na.rm = TRUE, 0.01)
upper_bound <- quantile(ET$ET, na.rm = TRUE, 0.99)

outlier_p <- which(ET$ET < lower_bound | ET$ET > upper_bound)

outlier_p_ET <- data.frame(ET[outlier_p, ])

#Z scores???
ET_z <- scale(ET$ET)

hist(ET_z$V1)

#Hampel filter dont like this one
lower_bound_h <- median(ET$ET, na.rm = TRUE) - 3 * mad(ET$ET, constant = 1, na.rm = TRUE)
upper_bound_h <- median(ET$ET, na.rm = TRUE) + 3 * mad(ET$ET, constant = 1, na.rm = TRUE)

#Rosner test, this is actually nice
test <- rosnerTest(ET$ET, k = 308)

results_rosner <- test$all.stats

outliers_rosner_row <- results_rosner$Obs.Num[results_rosner$Outlier]

outliers_rosner <- data.frame(ET[outliers_rosner, ])

ET_filtered_rosner <- ET[-outliers_rosner_row, ]


ggplot(ET) +
  geom_point(aes(x = datetime, y = Tair, color = "Tair"), size = 0.5) +
  geom_line(aes(x = datetime, y = Tdew_EP, color = "Tdew"), size = 0.5) +
  scale_color_manual(
    values = c("Tair" = "tomato", "Tdew" = "skyblue")
  )

Rain_data <- ET %>% 
  filter(RAIN > 0)

rain_start_times <- Rain_data$datetime
rain_end_times <- Rain_data$datetime + as.difftime(6, format = "%H", units = "hours")

rain_start_times <- format(rain_start_times, "%Y-%m-%d %H:%M:%S")
rain_end_times <- format(rain_end_times, "%Y-%m-%d %H:%M:%S")

ET_test <- ET %>% 
  filter(!(datetime >= rain_start_times & datetime <= rain_end_times))

ET_neg <- ET %>% 
  filter(ET < 0)


ggplot(ET_d) +
  geom_line(aes(x = datetime, y = ET))

ET_d <- ET %>%  
  group_by(datetime = format(datetime, "%Y-%m-%d")) %>% 
  summarise(Tair = mean(Tair, na.rm = T), 
            ET = sum(ET, na.rm = T), 
            VPD = mean(VPD, na.rm = T), 
            RH = mean(RH, na.rm = T),
            Tdew_EP = mean(Tdew_EP, na.rm= T),
            RAIN = mean(RAIN, na.rm = T),
            WIND = mean(WIND, na.rm = T),
            SWIN = mean(SWIN, na.rm = T),
            Ustar = mean(Ustar, na.rm = T),
            bowen_ratio = mean(bowen_ratio, na.rm = T))

ET_outliers_d <- outliers_ET %>%  
  group_by(datetime = format(datetime, "%Y-%m-%d")) %>% 
  summarise(Tair = mean(Tair, na.rm = T), 
            ET = sum(ET, na.rm = T), 
            VPD = mean(VPD, na.rm = T), 
            RH = mean(RH, na.rm = T),
            Tdew_EP = mean(Tdew_EP, na.rm= T),
            RAIN = mean(RAIN, na.rm = T),
            WIND = mean(WIND, na.rm = T),
            SWIN = mean(SWIN, na.rm = T))

ET_n <- ET %>% 
  filter(datetime >= sunset | datetime <= sunrise) %>% 
  group_by(datetime = format(datetime, "%Y-%m-%d")) %>% 
  summarise(Tair = mean(Tair, na.rm = T), 
            ET = sum(ET, na.rm = T), 
            VPD = mean(VPD, na.rm = T), 
            RH = mean(RH, na.rm = T),
            Tdew_EP = mean(Tdew_EP, na.rm= T),
            SWIN = mean(SWIN, na.rm = T))

ET_y <- ET %>% 
  summarise(Tair = mean(Tair, na.rm = T), ET = sum(ET, na.rm = T), VPD = mean(VPD, na.rm = T), RH = mean(RH, na.rm = T))

ET_d$datetime <- as.POSIXct(ET_d$datetime, format = "%Y-%m-%d")
ET_n$datetime <- as.POSIXct(ET_n$datetime, format = "%Y-%m-%d")

ggplot(ET) +
  geom_point(aes(x = RH, y = VPD))

ggplot(outliers_ET) +
  geom_point(aes(x = Tair, y = ET))

ggplot(RH_VPD) +
  geom_point(aes(x = VPD, y = ET))

check <- ET[ET$CO2_flag == 2 & is.na(ET$NEE_CO2), ]

