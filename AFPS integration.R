library(tidyverse)

AFPS_SENTEK <- read.csv2("Transformed/Langeweide_Sentek_AFPS.csv")
AFPS_TENSIO <- read.csv2("Transformed/Langeweide_Tensio_AFPS_mm_200.csv")
OWASIS_BBB <- read.csv2("Transformed/Langeweide_OWASIS_BBB.csv")

AFPS_SENTEK$datetime <- as.POSIXct(AFPS_SENTEK$datetime, format = "%Y-%m-%d %H:%M:%S")
AFPS_TENSIO$TIMESTAMP <- as.POSIXct(AFPS_TENSIO$TIMESTAMP, format = "%Y-%m-%d %H:%M:%S")
OWASIS_BBB$Date <- as.POSIXct(OWASIS_BBB$Date, format = "%Y-%m-%d")

#Fist check max depth of OWASIS_BBB in cm
Max_GW_OWASIS <- abs(min(OWASIS_BBB$MedianGW_mmv)*100)

#integrate AFPS over profile depth

#Integrate SENTEK
AFPS_int_SENTEK <- AFPS_SENTEK %>% 
  mutate(Probe1 = SWC_1_015 + SWC_1_025 + SWC_1_035 + SWC_1_045 + SWC_1_055) %>% 
  mutate(Probe3= SWC_3_015 + SWC_3_025 + SWC_3_035 + SWC_3_045 + SWC_3_055)

AFPS_int_TENSIO <- AFPS_TENSIO %>% 
  mutate(Probe123 = MS_TMAP_1_D_020 + MS_TMAP_2_D_030 + MS_TMAP_3_D_050) %>% 
  mutate(Probe456 = MS_TMAP_4_D_020 + MS_TMAP_5_D_040 + MS_TMAP_6_D_060) %>% 
  mutate(Probe789 = MS_TMAP_7_D_020 + MS_TMAP_8_D_040 + MS_TMAP_9_D_060)

start_date <- "2022-04-02"
end_date <- "2022-11-02"

#filter for the dates
AFPS_int_SENTEK <- AFPS_int_SENTEK %>% 
  filter(datetime >= start_date & datetime <= end_date)

AFPS_int_TENSIO <- AFPS_int_TENSIO %>% 
  filter(TIMESTAMP >= start_date & TIMESTAMP <= end_date)

#need to aggregate by day for OWASIS
AFPS_int_SENTEK <- AFPS_int_SENTEK %>% 
  group_by(datetime = format(datetime, "%Y-%m-%d")) %>% 
  summarise(across(everything(), mean, na.rm = TRUE))

#need to aggregate by day for OWASIS
AFPS_int_TENSIO <- AFPS_int_TENSIO %>% 
  group_by(datetime = format(TIMESTAMP, "%Y-%m-%d")) %>% 
  summarise(across(everything(), mean, na.rm = TRUE))

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
  geom_point(aes(x = TIMESTAMP, y = MS_TMAP_1_D_020, color = "1"), size = 0.5) +
  geom_point(aes(x = TIMESTAMP, y = MS_TMAP_4_D_020, color = "4"), size = 0.5) +
  geom_point(aes(x = TIMESTAMP, y = MS_TMAP_7_D_020, color = "7"), size = 0.5) +
  scale_color_manual(
    values = c("1" = "tomato", "4" = "skyblue", "7" = "orange"),
    name = "Measurement device"
  ) +
  theme(
    legend.title = element_text(size = 8),
    legend.text = element_text(size = 7),
    legend.key.width = unit(0.5, "cm")
  )

ggplot(AFPS_TENSIO) +
  geom_point(aes(x = TIMESTAMP, y = MS_TMAP_1_D_020, color = "1"), size = 0.5) +
  geom_point(aes(x = TIMESTAMP, y = MS_TMAP_2_D_030, color = "2"), size = 0.5) +
  geom_point(aes(x = TIMESTAMP, y = MS_TMAP_3_D_050, color = "3"), size = 0.5) +
  scale_color_manual(
    values = c("1" = "tomato", "2" = "skyblue", "3" = "orange"),
    name = "Measurement device"
  ) +
  theme(
    legend.title = element_text(size = 8),
    legend.text = element_text(size = 7),
    legend.key.width = unit(0.5, "cm")
  )



