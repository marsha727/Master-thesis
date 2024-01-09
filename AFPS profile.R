#For the profile over depth

library(tidyverse)
library(lubridate)
library(viridis)

TENSIO <- readRDS("Transformed/Langeweide_tensio_interpolated.rds")
SENTEK <- read.csv2("Transformed/Langeweide_Sentek_AFPS.csv")
GWL <- readRDS("Transformed/Langeweide_groundwater.rds")

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

#Make sure the dates are overlapping for study period
SENTEK_profile1 <- SENTEK_profile1 %>% 
  filter(date >= "2022-04-02" & date <= "2022-11-01")

SENTEK_profile3 <- SENTEK_profile3 %>% 
  filter(date >= "2022-04-02" & date <= "2022-11-01")

TENSIO_real <- TENSIO_real %>% 
  filter(datetime >= "2022-04-02" & datetime <= "2022-10-31")

GWL <- GWL %>% 
  filter(datetime >= "2022-04-02" & datetime <= "2022-10-31")

#calculate an average over each month
SENTEK_profile1_month <- SENTEK_profile1 %>% 
  na.omit(date) %>% 
  mutate(month = as.character(month(date))) %>%  # Extract month as a label use lubricate package
  group_by(depth, month) %>%
  summarise(AFPS = mean(value, na.rm = TRUE)) %>% 
  rename(AFPS1 = AFPS)

SENTEK_profile3_month <- SENTEK_profile3 %>% 
  na.omit(date) %>%
  mutate(month = as.character(month(date))) %>%  # Extract month as a label use lubricate package
  group_by(depth, month) %>%
  summarise(AFPS = mean(value, na.rm = TRUE)) %>% 
  rename(AFPS3 = AFPS)

#bind into one dataframe
SENTEK_month <- cbind(SENTEK_profile1_month, SENTEK_profile3_month[,3], GWL_month[])

GWL_month <- GWL %>% 
  mutate(month = as.character(month(datetime))) %>% 
  group_by(month) %>% 
  summarise(GWL = mean(GWL_mean, na.rm = TRUE))

TENSIO_month <- TENSIO_real %>% 
  group_by(depth, month = as.character(month(datetime))) %>% 
  summarise(AFPS2 = mean(AFPS2), AFPS3 = mean(AFPS3))

#seasonal averages !!!!be careful with interpetation

SENTEK_profile1_season <- SENTEK_profile1 %>% 
  na.omit(date) %>% 
  mutate(Season = case_when(
    month(date) %in% c(3, 4, 5) ~ "Spring",
    month(date) %in% c(6, 7, 8) ~ "Summer",
    month(date) %in% c(9, 10, 11) ~ "Autumn",
    month(date) %in% c(12, 1, 2) ~ "Winter")) %>% 
  group_by(depth, Season) %>% 
  summarise(AFPS = mean(value, na.rm = T)) %>% 
  rename(AFPS1 = AFPS)

SENTEK_profile3_season <- SENTEK_profile3 %>% 
  na.omit(date) %>% 
  mutate(Season = case_when(
    month(date) %in% c(3, 4, 5) ~ "Spring",
    month(date) %in% c(6, 7, 8) ~ "Summer",
    month(date) %in% c(9, 10, 11) ~ "Autumn",
    month(date) %in% c(12, 1, 2) ~ "Winter")) %>% 
  group_by(depth, Season) %>% 
  summarise(AFPS = mean(value, na.rm = T)) %>% 
  rename(AFPS3 = AFPS)


SENTEK_season <- cbind(SENTEK_profile1_season, SENTEK_profile3_season[,3])

TENSIO_season <- TENSIO_real %>% 
  mutate(Season = case_when(
    month(datetime) %in% c(1, 2, 3) ~ "1",
    month(datetime) %in% c(4, 5, 6) ~ "2",
    month(datetime) %in% c(7, 8, 9) ~ "3",
    month(datetime) %in% c(10, 11, 12) ~ "4")) %>% 
  group_by(depth, Season) %>% 
  summarise(AFPS2 = mean(AFPS2, na.rm = T), AFPS3 = mean(AFPS3, na.rm = T))



#PLOTTING

#seasonal

custom_colors <- c("royalblue", "gold", "lawngreen", "tomato")

ggplot(SENTEK_season) +
  geom_line(aes(x = -depth, y = AFPS1, color = Season)) +
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


#DEPTHS on Y

ggplot(SENTEK_season) +
  geom_path(aes(x = AFPS1, y = -depth, color = Season, linetype = "AFPS1")) +
  geom_path(aes(x = AFPS3, y = -depth, color = Season, linetype = "AFPS3")) +
  labs(
    title = "AFPS Over Depth (SENTEK1)",
    x = "AFPS (mm)",
    y = "Depth (cm)",
    linetype = "Probe"
  ) +
  scale_color_manual(
    values = custom_colors,
    breaks = c("Spring", "Summer", "Autumn", "Winter"),
    labels = c("Spring", "Summer", "Autumn", "Winter")
  ) +
  scale_linetype_manual(
    values = c(AFPS1 = "solid", AFPS3 = "dotdash"),
    breaks = c("AFPS1", "AFPS3"),
    labels = c("AFPS1", "AFPS3")
  ) +
  theme_minimal()

ggplot(TENSIO_season, aes(x = -depth, y = AFPS2, color = Season)) +
  #geom_path() +
  stat_smooth(method = "lm", formula = y ~ poly(x, 2), se = FALSE) +
  coord_flip() +
  #geom_smooth(method = "lm", formula = y ~ poly(x, 2), se = FALSE) +
  labs(
    title = "AFPS Over Depth (TENSIO)",
    x = "AFPS (mm)",
    y = "Depth (cm)") +
  scale_color_manual(
    values = custom_colors,
    breaks = c(1, 2, 3, 4),
    labels = c("JFM", "AMJ", "JAS", "OND")
  ) +
  theme_minimal()


# Your months data in the GWL_month data frame
months_data <- as.numeric(GWL_month$month)

# Min-Max normalization function for months
normalize_months <- function(x) {
  (x - min(x)) / (max(x) - min(x)) * 0.4
}

normalize_months_T <- function(x) {
  (x - min(x)) / (max(x) - min(x)) * 0.94
}

# Applying normalization to the months data in the GWL_month data frame
GWL_month$normalized_month <- normalize_months(months_data)
GWL_month$normalized_month_T <- normalize_months_T(months_data)


#MONTH
ggplot() +
  geom_path(data = SENTEK_month, aes(x = AFPS1/100, y = -depth, color = month, linetype = "AFPS1")) +
  geom_path(data = SENTEK_month, aes(x = AFPS3/100, y = -depth, color = month, linetype = "AFPS3")) +
  geom_point(data = GWL_month, aes(x = as.numeric(normalized_month), y = GWL, color = month), shape = 16 ) +
  geom_text(data = GWL_month, aes(x = as.numeric(normalized_month), y = GWL, label = "GWL"), vjust = -0.5, size = 2) +
labs(
    title = "AFPS Over Depth compared to GWL",
    x = "AFPS (mm)",  
    y = "Depth (cm)",
    linetype = "Probe",
  ) +
  scale_color_manual(
    values = c("#440154", "#3B528B", "#21918C", "#5EC962", "#FDE725", "#FFAC00", "#D73027", "black"),
    breaks = c(4, 5, 6, 7, 8, 9, 10),
    labels = c("Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct")
  ) +
  scale_linetype_manual(
    values = c(AFPS1 = "solid", AFPS3 = "dotdash"),
    breaks = c("AFPS1", "AFPS3"),
    labels = c("AFPS1", "AFPS3")
  ) +
  #scale_color_viridis_d(option = "turbo") +
  theme_minimal()

ggplot() +
  geom_path(data = TENSIO_month, aes(x = AFPS2, y = -depth, color = month, linetype = "AFPS2")) +
  geom_path(data = TENSIO_month, aes(x = AFPS3, y = -depth, color = month, linetype = "AFPS3")) +
  geom_point(data = GWL_month, aes(x = as.numeric(normalized_month_T), y = GWL, color = month), shape = 16 ) +
  geom_text(data = GWL_month, aes(x = as.numeric(normalized_month_T), y = GWL, label = "GWL"), vjust = -0.5, size = 2) +
  labs(
    title = "AFPS Over Depth compared to GWL",
    x = "AFPS (mm)",  
    y = "Depth (cm)",
    linetype = "Probe",
  ) +
  scale_color_manual(
    values = c("#440154", "#3B528B", "#21918C", "#5EC962", "#FDE725", "#FFAC00", "#D73027", "black
               "),
    breaks = c(04, 05, 06, 07, 08, 09, 10),
    labels = c("Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct")
  ) +
  scale_linetype_manual(
    values = c(AFPS2 = "solid", AFPS3 = "dotdash"),
    breaks = c("AFPS2", "AFPS3"),
    labels = c("AFPS2", "AFPS3")
  ) +
  #scale_color_viridis_d(option = "turbo") +
  theme_minimal()



write_rds(SENTEK_profile1_month, "App/Langeweide_SENTEK1_profile.rds")
write_rds(SENTEK_profile3_month, "App/Langeweide_SENTEK3_profile.rds")
write_rds(TENSIO_month, "App/Langeweide_TENSIO_profile.rds")

test <- readRDS("App/Langeweide_SENTEK1_profile.rds")


merged <- merge(SENTEK_month, GWL_month, by = "month", all.x = TRUE)





