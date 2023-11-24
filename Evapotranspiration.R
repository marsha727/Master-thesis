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

#getting all negative values
ET_neg <- ET %>% 
  filter(ET < 0)


#after filtering daily values can be computed

#daily sum of ET
ET_d <- ET_filtered_rosner %>%  
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
    values = c("ET" = "darkblue"),
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

