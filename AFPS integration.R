library(tidyverse)

AFPS_SENTEK <- read.csv2("Transformed/Langeweide_Sentek_AFPS.csv")
AFPS_TENSIO <- read.csv2("Transformed/Langeweide_Tensio_AFPS_mm_200.csv")
OWASIS_BBB <- read.csv2("Transformed/Langeweide_OWASIS_BBB.csv")
Langeweide_data <- readRDS("Datasets/LAW_MS_ICOS.RDS")

AFPS_SENTEK$datetime <- as.POSIXct(AFPS_SENTEK$datetime, format = "%Y-%m-%d %H:%M:%S")
AFPS_TENSIO$TIMESTAMP <- as.POSIXct(AFPS_TENSIO$TIMESTAMP, format = "%Y-%m-%d %H:%M:%S")
OWASIS_BBB$Date <- as.POSIXct(OWASIS_BBB$Date, format = "%Y-%m-%d")
Langeweide_data$datetime <- as.POSIXct(Langeweide_data$datetime, format = "%Y-%m-%d %H:%M:%S")

#Fist check max depth of OWASIS_BBB in cm
Max_GW_OWASIS <- abs(min(OWASIS_BBB$MedianGW_mmv)*100)

#Dealing with missing values
cor <- cor(AFPS_TENSIO$MS_TMAP_4_D_020, AFPS_TENSIO$MS_TMAP_7_D_020, method = "pearson", use = "complete.obs")
cor2 <- cor(AFPS_TENSIO$MS_TMAP_5_D_040, AFPS_TENSIO$MS_TMAP_8_D_040, method = "pearson", use = "complete.obs")

missing_values_indices <- which(is.na(AFPS_TENSIO$MS_TMAP_4_D_020))
missing_values_indices2 <- which(is.na(AFPS_TENSIO$MS_TMAP_8_D_040))

# Automatically find the start and end indices of the gap
start_index <- min(missing_values_indices)
end_index <- max(missing_values_indices)

start_index2 <- min(missing_values_indices2)
end_index2 <- max(missing_values_indices2)

# Use the correlation to estimate missing values of MS_TMAP_4_D_020 based on MS_TMAP_7_D_020
AFPS_TENSIO$MS_TMAP_4_D_020[start_index:end_index] <-
  AFPS_TENSIO$MS_TMAP_7_D_020[start_index:end_index] * cor

AFPS_TENSIO$MS_TMAP_8_D_040[start_index2:end_index2] <-
  AFPS_TENSIO$MS_TMAP_5_D_040[start_index2:end_index2] * cor2

#Integrate SENTEK
AFPS_int_SENTEK <- AFPS_SENTEK %>% 
  mutate(Probe1 = SWC_1_015 + SWC_1_025 + SWC_1_035 + SWC_1_045 + SWC_1_055 + SWC_1_065 + SWC_1_075) %>% 
  mutate(Probe3= SWC_3_015 + SWC_3_025 + SWC_3_035 + SWC_3_045 + SWC_3_055 + SWC_1_065 + SWC_1_075)

AFPS_int_TENSIO <- AFPS_TENSIO %>% 
  mutate(Probe456 = MS_TMAP_4_D_020 + MS_TMAP_5_D_040 + MS_TMAP_6_D_060) %>% 
  mutate(Probe789 = MS_TMAP_7_D_020 + MS_TMAP_8_D_040 + MS_TMAP_9_D_060)

#Period of analysis
start_date <- "2022-04-02"
end_date <- "2022-11-01"

#filter for the dates
AFPS_int_SENTEK <- AFPS_int_SENTEK %>% 
  filter(datetime >= start_date & datetime <= end_date)

AFPS_int_TENSIO <- AFPS_int_TENSIO %>% 
  filter(TIMESTAMP >= start_date & TIMESTAMP <= end_date)

AFPS_SENTEK <- AFPS_SENTEK %>% 
  filter(datetime >= start_date & datetime <= end_date)

AFPS_TENSIO <- AFPS_TENSIO %>% 
  filter(TIMESTAMP >= start_date & TIMESTAMP <= end_date)

Langeweide_data <- Langeweide_data %>% 
  filter(day >= start_date & day <= end_date)

#Extract GWL
GWL <- Langeweide_data %>% 
  select(datetime, WL_1, WL_2, WL_3, WL_4, WL_5, WL, WL_cor)


#need to do an estimation on the conversion to NAP
columns_to_mean <- c("WL_1", "WL_2", "WL_3", "WL_4", "WL_5")
GWL_mmv <- GWL %>% 
  mutate(WL_1 = WL_1 + 204) %>% 
  mutate(WL_2 = WL_2 + 204) %>% 
  mutate(WL_3 = WL_3 + 204) %>% 
  mutate(WL_4 = WL_4 + 204) %>% 
  mutate(WL_5 = WL_5 + 204)

GWL_mmv <- GWL_mmv %>% 
  mutate(WL_mean = rowMeans(GWL_mmv[ , columns_to_mean], na.rm = TRUE))

#need to aggregate by day for OWASIS
AFPS_int_SENTEK <- AFPS_int_SENTEK %>% 
  group_by(datetime = format(datetime, "%Y-%m-%d")) %>% 
  summarise(across(everything(), mean, na.rm = TRUE))

#need to aggregate by day for OWASIS
AFPS_int_TENSIO <- AFPS_int_TENSIO %>% 
  group_by(datetime = format(TIMESTAMP, "%Y-%m-%d")) %>% 
  summarise(across(everything(), mean, na.rm = TRUE))

AFPS_TENSIO$TIMESTAMP <- as.POSIXct(AFPS_TENSIO$TIMESTAMP, format = "%Y-%m-%d")
AFPS_int_SENTEK$datetime <- as.POSIXct(AFPS_int_SENTEK$datetime, format = "%Y-%m-%d")
AFPS_int_TENSIO$TIMESTAMP <- as.POSIXct(AFPS_int_TENSIO$TIMESTAMP, format = "%Y-%m-%d")
OWASIS_BBB$Date <- as.POSIXct(OWASIS_BBB$Date, format = "%Y-%m-%d")

ggplot(AFPS_int_SENTEK) +
  geom_line(aes(x = datetime, y = Probe1))

ggplot(AFPS_int_TENSIO) +
  geom_line(aes(x = TIMESTAMP, y = Probe789))

ggplot(OWASIS_BBB) +
  geom_line(aes(x = Date, y = MedianBBB))

ggplot(AFPS_TENSIO) +
  geom_point(aes(x = TIMESTAMP, y = MS_TMAP_2_D_030))





Depth20 <- cbind(Depth20_14, Depth20_17)

ggplot(AFPS_TENSIO) +
  geom_line(aes(x = TIMESTAMP, y = MS_TMAP_5_D_040, color = "4"), size = 0.5) +
  geom_line(aes(x = TIMESTAMP, y = MS_TMAP_8_D_040, color = "7"), size = 0.5) +
  scale_color_manual(
    values = c("1" = "tomato", "4" = "skyblue", "7" = "orange"),
    name = "Measurement device"
  ) +
  theme(
    legend.title = element_text(size = 8),
    legend.text = element_text(size = 7),
    legend.key.width = unit(0.5, "cm")
  )

ggplot(AFPS_int_TENSIO) +
  geom_point(aes(x = TIMESTAMP, y = Probe456, color = "Probe456")) +
  geom_point(aes(x = TIMESTAMP, y = Probe789, color = "Probe789")) +
  scale_color_manual(
    values = c("Probe456" = "tomato", "Probe789" = "skyblue"),
    name = "Probes"
  ) +
  theme(
    legend.title = element_text(size = 8),
    legend.text = element_text(size = 7),
    legend.key.width = unit(0.5, "cm")
  )

#just some experiments for quality check
AFPS <- bind_cols(AFPS_SENTEK[1], AFPS_SENTEK[ , 3:26], AFPS_TENSIO[, 2:10], GWL_mmv[, 8:9] )

AFPS$datetime <- as.POSIXct(AFPS$datetime, format = "%Y-%m-%d %H:%M:%S")

ggplot(AFPS) +
  geom_line(aes(x = datetime, y = SWC_1_025, color = "Sentek1"), linewidth = 0.3) +
  geom_line(aes(x = datetime, y = MS_TMAP_7_D_020, color = "Tensio7"), linewidth = 0.3) +
  geom_path(aes(x = datetime, y = WL_cor, color = "GWL"), na.rm = TRUE, linewidth = 0.3) +
  scale_color_manual(
    values = c("Sentek1" = "tomato", "Tensio7" = "skyblue", "GWL" = "yellowgreen"),
    name = "Probes"
  ) +
  scale_y_continuous(
    name = "AFPS (mm)",
    sec.axis = sec_axis(~., name = "WL (cm-mv)")
  ) +
  theme(
    legend.title = element_text(size = 8),
    legend.text = element_text(size = 7),
    legend.key.width = unit(0.5, "cm")
  )


