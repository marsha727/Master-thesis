library(tidyverse)

Langeweide_data <- readRDS("Datasets/LAW_MS_ICOS.rds")

ET <- Langeweide_data %>% 
  select(datetime, sunrise, sunset, Tair, ET, VPD, RH, bowen_ratio, Tdew_EP, RAIN)

ET$sunset <- as.POSIXct(ET$sunset, format = "%Y-%m-%d %H:%M:%S")
ET$sunrise <- as.POSIXct(ET$sunrise, format = "%Y-%m-%d %H:%M:%S")

#histogram and boxplot for distribution
bin_width <- 0.5

breaks <- seq(from = min(ET$ET, na.rm = T), to = max(ET$ET, na.rm = T) + bin_width, by = bin_width)

print(breaks)

hist(ET$ET, main = "Histogram ET", xlab = "Values", ylab = "Frequency", col = "lightblue", border = "black", breaks = breaks, ylim = c(0,12))

boxplot(ET$ET)

boxplot(ET$ET ~ ET$RAIN, ET = ET)

ggplot(ET) +
  geom_point(aes(x = datetime, y = Tair, color = "Tair"), size = 0.5) +
  geom_line(aes(x = datetime, y = Tdew_EP, color = "Tdew"), size = 0.5) +
  scale_color_manual(
    values = c("Tair" = "tomato", "Tdew" = "skyblue")
  )



ggplot(ET) +
  geom_point(aes(x = datetime, y = ET))









ET_d <- ET %>%  
  group_by(datetime = format(datetime, "%Y-%m-%d")) %>% 
  summarise(Tair = mean(Tair, na.rm = T), 
            ET = sum(ET, na.rm = T), 
            VPD = mean(VPD, na.rm = T), 
            RH = mean(RH, na.rm = T),
            Tdew_EP = mean(Tdew_EP, na.rm= T))

ET_n <- ET %>% 
  filter(datetime >= sunset | datetime <= sunrise) %>% 
  group_by(datetime = format(datetime, "%Y-%m-%d")) %>% 
  summarise(Tair = mean(Tair, na.rm = T), 
            ET = sum(ET, na.rm = T), 
            VPD = mean(VPD, na.rm = T), 
            RH = mean(RH, na.rm = T),
            Tdew_EP = mean(Tdew_EP, na.rm= T))

ET_y <- ET %>% 
  summarise(Tair = mean(Tair, na.rm = T), ET = sum(ET, na.rm = T), VPD = mean(VPD, na.rm = T), RH = mean(RH, na.rm = T))

  

ET_d$datetime <- as.POSIXct(ET_d$datetime, format = "%Y-%m-%d")
ET_n$datetime <- as.POSIXct(ET_d$datetime, format = "%Y-%m-%d")

ggplot(ET_d) +
  geom_line(aes(x = datetime, y = ET))

ggplot(ET_d) +
  geom_point(aes(x = ET, y = Tair))

