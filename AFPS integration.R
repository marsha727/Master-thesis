library(tidyverse)

AFPS_SENTEK <- read.csv2("Transformed/Langeweide_Sentek_AFPS.csv")
AFPS_TENSIO <- read.csv2("Transformed/Langeweide_tensio_interpolated.csv")
OWASIS_BBB <- read.csv2("Transformed/Langeweide_OWASIS_BBB.csv")
Langeweide_data <- readRDS("Datasets/LAW_MS_ICOS.RDS")
ET <- readRDS("App/Langeweide_ET.rds")

AFPS_SENTEK$datetime <- as.POSIXct(AFPS_SENTEK$datetime, format = "%Y-%m-%d %H:%M:%S")
AFPS_TENSIO$TIMESTAMP <- as.POSIXct(AFPS_TENSIO$datetime, format = "%Y-%m-%d %H:%M:%S")
OWASIS_BBB$Date <- as.POSIXct(OWASIS_BBB$Date, format = "%Y-%m-%d")
Langeweide_data$datetime <- as.POSIXct(Langeweide_data$datetime, format = "%Y-%m-%d %H:%M:%S")
ET$datetime <- as.POSIXct(ET$datetime, format = "%Y-%m-%d")

#Fist check max depth of OWASIS_BBB in cm
Max_GW_OWASIS <- abs(min(OWASIS_BBB$MedianGW_mmv)*100)

#Integrate SENTEK + tensio over 60 cm ~ not perfect allignment
AFPS_int_SENTEK <- AFPS_SENTEK %>% 
  mutate(Probe1 = SWC_1_025 + SWC_1_035 + SWC_1_045 + SWC_1_055 + SWC_1_065) %>% 
  mutate(Probe3= SWC_3_025 + SWC_3_035 + SWC_3_045 + SWC_3_055 + SWC_1_065)

AFPS_int_TENSIO <- AFPS_TENSIO %>% 
  group_by(datetime) %>% 
  summarise(AFPS2 = sum(AFPS2), AFPS3 = sum(AFPS3))

#write a RDS just for APP
write_rds(AFPS_int_SENTEK, "App/Langeweide_sentek_int.rds")
write_rds(AFPS_int_TENSIO, "App/Langeweide_tensio_int.rds")

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

AFPS_SENTEK <- AFPS_SENTEK %>% 
  filter(datetime >= start_date & datetime <= end_date3)

AFPS_TENSIO <- AFPS_TENSIO %>% 
  filter(datetime >= start_date & datetime <= end_date2)

Langeweide_data <- Langeweide_data %>% 
  filter(day >= start_date & day <= end_date3)

OWASIS_BBB <- OWASIS_BBB %>% 
  filter(Date >= start_date & Date <= end_date3)

ET <- ET %>% 
  filter(datetime >= start_date & datetime <= end_date3)

#make subselection of langeweide before using
Langeweide_data <- Langeweide_data %>% 
  select(datetime, WL_cor, BBB,
         NEE_CO2, NEE_CO2_MDS, NEE_CO2_MDS2, 
         Tsoil_1_005, Tsoil_1_015, Tsoil_1_025, Tsoil_1_035, Tsoil_1_045, Tsoil_1_055,
         Tsoil_3_065, Tsoil_1_075, Tsoil_1_085, Tsoil_1_095, Tsoil_1_105, Tsoil_3_005, Tsoil_3_015, Tsoil_3_025, Tsoil_3_035, Tsoil_3_045, Tsoil_3_055,
         Tsoil_3_065, Tsoil_3_075, Tsoil_3_085, Tsoil_3_095, Tsoil_3_105)

#need to aggregate by day for OWASIS
AFPS_int_SENTEK <- AFPS_int_SENTEK %>% 
  group_by(datetime = format(datetime, "%Y-%m-%d")) %>% 
  summarise(across(everything(), ~mean(., na.rm = TRUE)))

#need to aggregate by day for OWASIS
AFPS_int_TENSIO <- AFPS_int_TENSIO %>% 
  group_by(datetime = as.Date(datetime)) %>% 
  summarise(across(everything(), ~mean(., na.rm = TRUE)))


#Correct datetime format
AFPS_int_SENTEK$datetime <- as.POSIXct(AFPS_int_SENTEK$datetime, format = "%Y-%m-%d")

#Combined AFPS TENSIO AND SENTEK
AFPS_int_TS <- bind_cols(AFPS_int_SENTEK[1], AFPS_int_SENTEK[ ,27:28], AFPS_int_TENSIO[ ,2:3], OWASIS_BBB[4])

#correct datetime format
AFPS_int_TS$datetime <- as.POSIXct(AFPS_int_TS$datetime, format = "%Y-%m-%d")

AFPS_int_TS <- AFPS_int_TS %>% 
  rename(SENTEK1 = Probe1, SENTEK3 = Probe3, TENSIO2 = AFPS2, TENSIO3 = AFPS3, OWASIS = MedianBBB)

Langeweide <- Langeweide_data %>% 
  select(day, )


#write file for App
write_rds(AFPS_int_TS, file = "App/AFPS_int_TS.rds")

#PLOTTING

#TENSIO AND SENTEK WITH VOLUME = 60 CM
ggplot(AFPS_int_TS) +
  geom_line(aes(x = datetime, y = Probe1, color = "SENTEK1"), linewidth = 0.3) +
  geom_line(aes(x = datetime, y = AFPS3, color = "TENSIO2"), linewidth = 0.3) +
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


