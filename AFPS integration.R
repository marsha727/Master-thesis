library(tidyverse)

AFPS_SENTEK <- read.csv2("Transformed/Langeweide_Sentek_AFPS.csv")
AFPS_TENSIO <- read.csv2("Transformed/Langeweide_tensio_interpolated.csv")
OWASIS_BBB <- read.csv2("Transformed/Langeweide_OWASIS_BBB.csv")
Langeweide_data <- readRDS("Datasets/LAW_MS_ICOS.RDS")

AFPS_SENTEK$datetime <- as.POSIXct(AFPS_SENTEK$datetime, format = "%Y-%m-%d %H:%M:%S")
AFPS_TENSIO$TIMESTAMP <- as.POSIXct(AFPS_TENSIO$datetime, format = "%Y-%m-%d %H:%M:%S")
OWASIS_BBB$Date <- as.POSIXct(OWASIS_BBB$Date, format = "%Y-%m-%d")
Langeweide_data$datetime <- as.POSIXct(Langeweide_data$datetime, format = "%Y-%m-%d %H:%M:%S")

#Fist check max depth of OWASIS_BBB in cm
Max_GW_OWASIS <- abs(min(OWASIS_BBB$MedianGW_mmv)*100)

#Integrate SENTEK + tensio over 60 cm ~ not perfect allignment
AFPS_int_SENTEK <- AFPS_SENTEK %>% 
  mutate(Probe1 = SWC_1_025 + SWC_1_035 + SWC_1_045 + SWC_1_055 + SWC_1_065) %>% 
  mutate(Probe3= SWC_3_025 + SWC_3_035 + SWC_3_045 + SWC_3_055 + SWC_1_065)

AFPS_int_TENSIO <- AFPS_TENSIO %>% 
  group_by(datetime) %>% 
  summarise(AFPS_int2 = sum(AFPS2_mm), AFPS_int3 = sum(AFPS3_mm))

#Second selection for OWASIS
AFPS_int_SENTEK_O <- AFPS_SENTEK %>% 
  mutate(Probe1 = SWC_1_005 + SWC_1_015 + SWC_1_025) %>% 
  mutate(Probe3 = SWC_3_005 + SWC_3_015 + SWC_3_025)


#Period of analysis
start_date <- "2022-04-02"
end_date1 <- "2022-11-02"
end_date2 <- "2022-11-01" #do it ask me why but the other cut doesnt work for tensio
end_date3 <- "2022-10-31"

#filter for the dates
AFPS_int_SENTEK <- AFPS_int_SENTEK %>% 
  filter(datetime >= start_date & datetime <= end_date3)

AFPS_int_TENSIO <- AFPS_int_TENSIO %>% 
  filter(datetime >= start_date & datetime <= end_date2)

AFPS_int_SENTEK_O <- AFPS_int_SENTEK_O %>% 
  filter(datetime >= start_date & datetime <= end_date3)

AFPS_SENTEK <- AFPS_SENTEK %>% 
  filter(datetime >= start_date & datetime <= end_date3)

AFPS_TENSIO <- AFPS_TENSIO %>% 
  filter(datetime >= start_date & datetime <= end_date2)

Langeweide_data <- Langeweide_data %>% 
  filter(day >= start_date & day <= end_date3)

OWASIS_BBB <- OWASIS_BBB %>% 
  filter(Date >= start_date & Date <= end_date3)

#GWL side project
{#Extract GWL
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
}

#need to aggregate by day for OWASIS
AFPS_int_SENTEK <- AFPS_int_SENTEK %>% 
  group_by(datetime = format(datetime, "%Y-%m-%d")) %>% 
  summarise(across(everything(), ~mean(., na.rm = TRUE)))

#need to aggregate by day for OWASIS
AFPS_int_TENSIO<- AFPS_int_TENSIO %>% 
  group_by(datetime = as.Date(datetime)) %>% 
  summarise(across(everything(), ~mean(., na.rm = TRUE)))


#Second aggregation for OWASIS
AFPS_int_SENTEK_O <- AFPS_int_SENTEK_O %>% 
  group_by(datetime = format(datetime, "%Y-%m-%d")) %>% 
  summarise(across(everything(), ~mean(., na.rm = TRUE)))  

AFPS_TENSIO$datetime <- as.POSIXct(AFPS_TENSIO$datetime, format = "%Y-%m-%d")
AFPS_int_SENTEK$datetime <- as.POSIXct(AFPS_int_SENTEK$datetime, format = "%Y-%m-%d")
AFPS_int_SO$datetime <- as.POSIXct(AFPS_int_SO$datetime, format = "%Y-%m-%d") 
AFPS_int_TS$datetime <- as.POSIXct(AFPS_int_TS$datetime, format = "%Y-%m-%d")
AFPS_int_TENSIO$datetime <- as.POSIXct(AFPS_int_TENSIO$datetime, format = "%Y-%m-%d")
OWASIS_BBB$Date <- as.POSIXct(OWASIS_BBB$Date, format = "%Y-%m-%d")



#Combined AFPS TENSIO AND SENTEK
AFPS_int_TS <- bind_cols(AFPS_int_SENTEK[1], AFPS_int_SENTEK[ ,27:28], AFPS_int_TENSIO[ ,2:3], OWASIS_BBB[4])

AFPS_int_SO <- bind_cols(AFPS_int_SENTEK_O[1], AFPS_int_SENTEK_O[ ,27:28], OWASIS_BBB[4])

AFPS_int_TS_30min <- bind_cols(AFPS_int_SENTEK[1], AFPS_int_SENTEK[ ,27:28], AFPS_int_TENSIO[ ,2:3])

write_rds(AFPS_int_TS, file = "App/AFPS_int_TS.rds")

#PLOTTING

#TENSIO AND SENTEK WITH VOLUME = 60 CM
ggplot(AFPS_int_TS) +
  geom_line(aes(x = datetime, y = Probe1, color = "SENTEK1"), linewidth = 0.3) +
  geom_line(aes(x = datetime, y = AFPS_int2, color = "TENSIO2"), linewidth = 0.3) +
  geom_line(aes(x = datetime, y = MedianBBB, color = "OWASIS"), linewidth = 0.3) +
  labs(
    title = "AFPS int (SENTEK  VS TENSIO VS OWASIS) depth: 20-60 and unknown OWASIS",
    x = "Date",
    y = "AFPS int (mm)"
  ) +
  scale_color_manual(
    values = c("SENTEK1" = 'red', "SENTEK3" = "tomato", "TENSIO2" = "blue", "TENSIO3" = "skyblue", "OWASIS" = "darkgreen"),
    name = "Device"
  ) +
  theme(
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 9),
    legend.key.width = unit(0.5, "cm")
  )

#SENTEK and OWASIS with volume = ~20
ggplot(AFPS_int_SO) +
  geom_line(aes(x = datetime, y = Probe1, color = "SENTEK1"), linewidth = 0.3) +
  geom_line(aes(x = datetime, y = Probe3, color = "SENTEK3"), linewidth = 0.3) +
  geom_line(aes(x = datetime, y = MedianBBB, color = "OWASIS"), linewidth = 0.3) +
  labs(
    title = "AFPS int (SENTEK VS OWASIS) depth = ~0-25",
    x = "Date",
    y = "AFPS int (mm)"
  ) +
  scale_color_manual(
    values = c("SENTEK1" = 'red', "SENTEK3" = "tomato", "OWASIS" = "blue"),
    name = "Device"
  ) +
  theme(
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 9),
    legend.key.width = unit(0.5, "cm")
  )





#This was to check the GWL with TENSIO values
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


