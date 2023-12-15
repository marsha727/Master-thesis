#For the profile over depth

library(tidyverse)
library(lubridate)
library(viridis)

TENSIO <- readRDS("Transformed/Langeweide_tensio_interpolated.rds")
SENTEK <- read.csv2("Transformed/Langeweide_Sentek_AFPS.csv")

#I need to change the structure of the SENTEK file
#Make a long tibble basically just rename the to the depth it represents
SENTEK_long1 <- data.frame(date = SENTEK$datetime,
                       Depth_005 = SENTEK$SWC_1_005,
                       Depth_015 = SENTEK$SWC_1_015,
                       Depth_025 = SENTEK$SWC_1_025,
                       Depth_035 = SENTEK$SWC_1_035,
                       Depth_045 = SENTEK$SWC_1_045,
                       Depth_055 = SENTEK$SWC_1_055,
                       Depth_065 = SENTEK$SWC_1_065,
                       Depth_075 = SENTEK$SWC_1_075,
                       Depth_085 = SENTEK$SWC_1_085,
                       Depth_095 = SENTEK$SWC_1_095,
                       Depth_105 = SENTEK$SWC_1_105,
                       Depth_115 = SENTEK$SWC_1_115)


SENTEK_long3 <- data.frame(date = SENTEK$datetime,
                       Depth_005 = SENTEK$SWC_3_005,
                       Depth_015 = SENTEK$SWC_3_015,
                       Depth_025 = SENTEK$SWC_3_025,
                       Depth_035 = SENTEK$SWC_3_035,
                       Depth_045 = SENTEK$SWC_3_045,
                       Depth_055 = SENTEK$SWC_3_055,
                       Depth_065 = SENTEK$SWC_3_065,
                       Depth_075 = SENTEK$SWC_3_075,
                       Depth_085 = SENTEK$SWC_3_085,
                       Depth_095 = SENTEK$SWC_3_095,
                       Depth_105 = SENTEK$SWC_3_105,
                       Depth_115 = SENTEK$SWC_3_115)

#get tensio values measured
TENSIO_real <- TENSIO %>% 
  filter(depth == 20 | depth == 40 | depth == 60)

#Creates the sequence for depths sort for each date
Depths <- sort(unique(c(c(5, 115), seq(ceiling(5), floor(115), 10))))
Depths <- crossing(date = unique(SENTEK_long1$date), depth = Depths)

#This basically attaches the values from long tibble to the depths per date
SENTEK_profile1 <- SENTEK_long1 %>% 
  gather(depth, value, -date) %>% 
  mutate(depth = as.numeric(gsub("\\D", "", depth))) %>% 
  full_join(Depths) %>% 
  arrange(date, depth) %>% 
  group_by(date)

SENTEK_profile3 <- SENTEK_long3 %>% 
  gather(depth, value, -date) %>% 
  mutate(depth = as.numeric(gsub("\\D", "", depth))) %>% 
  full_join(Depths) %>% 
  arrange(date, depth) %>% 
  group_by(date)

#calculate an average over each month
SENTEK_profile1_month <- SENTEK_profile1 %>% 
  na.omit(date) %>% 
  mutate(month = month(date)) %>%  # Extract month as a label use lubricate package
  group_by(depth, month) %>%
  summarise(AFPS = mean(value, na.rm = TRUE))
  

SENTEK_profile3_month <- SENTEK_profile3 %>% 
  na.omit(date) %>%
  mutate(month = month(date)) %>%  # Extract month as a label use lubricate package
  group_by(depth, month) %>%
  summarise(AFPS = mean(value, na.rm = TRUE)) 
  
TENSIO_month <- TENSIO_real %>% 
  group_by(depth, month = format(datetime, "%m")) %>% 
  summarise(AFPS2 = mean(AFPS2), AFPS3 = mean(AFPS3))
  
ggplot(SENTEK_profile1_month, aes(x = -depth, y = AFPS/100, color = month)) +
  geom_line() +
  labs(
    title = "AFPS Over Depth (SENTEK3)",
       x = "Depth (cm)",
       y = "AFPS (mm)") +
  scale_color_viridis_d(option = "turbo") +
  theme_minimal()

#with monotonic polynominal plotting line
ggplot(TENSIO_month, aes(x = -depth, y = AFPS3, color = month)) +
  #geom_line()+
  #geom_smooth(method = "gam", formula = y ~ s(x, k = 3, bs = "cr", m = 1), se = FALSE) +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), se = FALSE) +
  labs(
    title = "AFPS over depth (TENSIO2)",
    x = "Depth (cm)",
    y = "AFPS (mm)"
  ) +
  scale_color_viridis_d(option = "turbo") +
  theme_minimal()

#seasonal

SENTEK_profile3_season <- SENTEK_profile3 %>% 
  na.omit(date) %>% 
  mutate(Season = case_when(
    month(date) %in% c(1, 2, 3) ~ "1",
    month(date) %in% c(4, 5, 6) ~ "2",
    month(date) %in% c(7, 8, 9) ~ "3",
    month(date) %in% c(10, 11, 12) ~ "4")) %>% 
  group_by(depth, Season) %>% 
  summarise(AFPS = mean(value, na.rm = T))

TENSIO_season <- TENSIO %>% 
  mutate(Season = case_when(
    month(datetime) %in% c(1, 2, 3) ~ "1",
    month(datetime) %in% c(4, 5, 6) ~ "2",
    month(datetime) %in% c(7, 8, 9) ~ "3",
    month(datetime) %in% c(10, 11, 12) ~ "4")) %>% 
  group_by(depth, Season) %>% 
  summarise(AFPS2 = mean(AFPS2, na.rm = T), AFPS3 = mean(AFPS3, na.rm = T))

custom_colors <- c("royalblue", "gold", "lawngreen", "tomato")

ggplot(SENTEK_profile1_season, aes(x = -depth, y = AFPS, color = Season)) +
  geom_line() +
  labs(
    title = "AFPS Over Depth (SENTEK1)",
    x = "Depth (cm)",
    y = "AFPS (mm)") +
  scale_color_manual(
    values = custom_colors,
    breaks = c(1, 2, 3, 4),
    labels = c("JFM", "AMJ", "JAS", "OND")
  ) +
  theme_minimal()

ggplot(TENSIO_season, aes(x = -depth, y = AFPS2, color = Season)) +
  geom_line() +
  #geom_smooth(method = "lm", formula = y ~ poly(x, 2), se = FALSE) +
  labs(
    title = "AFPS Over Depth (TENSIO)",
    x = "Depth (cm)",
    y = "AFPS (mm)") +
  scale_color_manual(
    values = custom_colors,
    breaks = c(1, 2, 3, 4),
    labels = c("JFM", "AMJ", "JAS", "OND")
  ) +
  theme_minimal()

#SENTEK_profile1_month <- data.frame(SENTEK_profile1_month)
#SENTEK_profile3_month <- data.frame(SENTEK_profile3_month)

#SENTEK_profile1_month <- SENTEK_profile1_month %>% 
  #mutate(datetime = as.POSIXct(month, format = "%m"))

#SENTEK_profile3_month <- SENTEK_profile3_month %>% 
  #mutate(datetime = as.POSIXct(month, format = "%m"))

#TENSIO_month <- TENSIO_month %>% 
  #mutate(datetime = as.POSIXct())

write_rds(SENTEK_profile1_month, "App/Langeweide_SENTEK1_profile.rds")
write_rds(SENTEK_profile3_month, "App/Langeweide_SENTEK3_profile.rds")
write_rds(TENSIO_month, "App/Langeweide_TENSIO_profile.rds")

test <- readRDS("App/Langeweide_SENTEK1_profile.rds")

