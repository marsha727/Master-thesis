#simplified function of the tensiometer conversions
library(tidyverse)
library(readxl)
library(splines)

Tensiometer <- read.csv("Datasets/LAW_TENS_2020-2023_clean.csv")
Bodem_fysische_metingen <- read_xlsx("Datasets/BodemFysischeMetingen_LAW.xlsx", sheet = "Evap+MvG")

#Conversion formula from kPa to cmH2O only applied to TMAP (matrix potential)
kPa_to_cmH2O <- function(x){
  ifelse(is.na(x), NA, x*10.1971623)
}

Tensiometer_cmH20 <- Tensiometer %>% 
  mutate(across(.cols = 2:10, ~kPa_to_cmH2O(.))) #here filter for TMAP


#non linear interpolation now dividing the use of end and begin depths
#make a subset of the values i require (I remove NA row manually for now)
Subset_Bodem_fysische_metingen <- Bodem_fysische_metingen %>% 
  filter(row_number() %in% c(3, 4, 7))

depth_to_interpolate <- 30 #depth that is missing

#Some are in character so convert to numeric
Subset_Bodem_fysische_metingen$a <- as.numeric(Subset_Bodem_fysische_metingen$a)
Subset_Bodem_fysische_metingen$begindiepte <- as.numeric(Subset_Bodem_fysische_metingen$begindiepte)
Subset_Bodem_fysische_metingen$einddiepte <- as.numeric(Subset_Bodem_fysische_metingen$einddiepte)

#calling MvG values manually because i couldnt get my loop to function
WCS1 <- Subset_Bodem_fysische_metingen$WCS[1]
WCR1 <- Subset_Bodem_fysische_metingen$WCR[1]
a1 <- Subset_Bodem_fysische_metingen$a[1]
n1 <- Subset_Bodem_fysische_metingen$n[1]
m1 <- Subset_Bodem_fysische_metingen$m[1]

WCS2 <- Subset_Bodem_fysische_metingen$WCS[2]
WCR2 <- Subset_Bodem_fysische_metingen$WCR[2]
a2 <- Subset_Bodem_fysische_metingen$a[2]
n2 <- Subset_Bodem_fysische_metingen$n[2]
m2 <- Subset_Bodem_fysische_metingen$m[2]

WCS3 <- Subset_Bodem_fysische_metingen$WCS[3]
WCR3 <- Subset_Bodem_fysische_metingen$WCR[3]
a3 <- Subset_Bodem_fysische_metingen$a[3]
n3 <- Subset_Bodem_fysische_metingen$n[3]
m3 <- Subset_Bodem_fysische_metingen$m[3]

#doing the MvG calculation manually because i couldnt make the loop work
#after discussion use 30 cm = 20 cm in probe 1-3 (above the drain = wet)
Tensiometer_SWC <- Tensiometer_cmH20 %>% 
  mutate(MS_TMAP_1_D_020 = WCR1 + (WCS1 - WCR1)/((1 + abs(a1 * MS_TMAP_1_D_020)^n1)^m1)) %>% 
  mutate(MS_TMAP_2_D_030 = WCR1 + ((WCS1 - WCR1)/((1+abs(a1 * MS_TMAP_2_D_030)^n1)^m1))) %>% 
  mutate(MS_TMAP_3_D_050 = WCR2 + ((WCS2 - WCR2)/((1+abs(a2 * MS_TMAP_3_D_050)^n2)^m2))) %>% 
  
  mutate(MS_TMAP_4_D_020 = WCR1 + ((WCS1 - WCR1)/((1+abs(a1 * MS_TMAP_4_D_020)^n1)^m1))) %>% 
  mutate(MS_TMAP_5_D_040 = WCR2 + ((WCS2 - WCR2)/((1+abs(a2 * MS_TMAP_5_D_040)^n2)^m2))) %>%
  mutate(MS_TMAP_6_D_060 = WCR3 + ((WCS3 - WCR3)/((1+abs(a3 * MS_TMAP_6_D_060)^n3)^m3))) %>% 
  
  mutate(MS_TMAP_7_D_020 = WCR1 + ((WCS1 - WCR1)/((1+abs(a1 * MS_TMAP_7_D_020)^n1)^m1))) %>% 
  mutate(MS_TMAP_8_D_040 = WCR2 + ((WCS2 - WCR2)/((1+abs(a2 * MS_TMAP_8_D_040)^n2)^m2))) %>%
  mutate(MS_TMAP_9_D_060 = WCR3 + ((WCS3 - WCR3)/((1+abs(a3 * MS_TMAP_9_D_060)^n3)^m3)))

#AFPS in percentages as a percentage of the total pore space
AFPS_tensiometer <- Tensiometer_SWC %>% 
  mutate(MS_TMAP_1_D_020 = (1 - (MS_TMAP_1_D_020 / WCS1))) %>% 
  mutate(MS_TMAP_2_D_030 = 1 - (MS_TMAP_2_D_030 / WCS1)) %>% 
  mutate(MS_TMAP_3_D_050 = 1 - (MS_TMAP_3_D_050 / WCS2)) %>% 
  
  mutate(MS_TMAP_4_D_020 = 1 - (MS_TMAP_4_D_020 / WCS1)) %>% 
  mutate(MS_TMAP_5_D_040 = 1 - (MS_TMAP_5_D_040 / WCS2)) %>% 
  mutate(MS_TMAP_6_D_060 = 1 - (MS_TMAP_6_D_060 / WCS3)) %>% 
  
  mutate(MS_TMAP_7_D_020 = 1 - (MS_TMAP_7_D_020 / WCS1)) %>% 
  mutate(MS_TMAP_8_D_040 = 1 - (MS_TMAP_8_D_040 / WCS2)) %>% 
  mutate(MS_TMAP_9_D_060 = 1 - (MS_TMAP_9_D_060 / WCS3))

#Calculate AFPS in mm
AFPS_mm_tensiometer <- AFPS_tensiometer %>% 
  mutate(MS_TMAP_1_D_020 = MS_TMAP_1_D_020 * WCS1 * 100) %>% 
  mutate(MS_TMAP_2_D_030 = MS_TMAP_2_D_030 * WCS1 * 100) %>% 
  mutate(MS_TMAP_3_D_050 = MS_TMAP_3_D_050 * WCS2 * 100) %>% 
  
  mutate(MS_TMAP_4_D_020 = MS_TMAP_4_D_020 * WCS1 * 100) %>% 
  mutate(MS_TMAP_5_D_040 = MS_TMAP_5_D_040 * WCS2 * 100) %>% 
  mutate(MS_TMAP_6_D_060 = MS_TMAP_6_D_060 * WCS3 * 100) %>% 
  
  mutate(MS_TMAP_7_D_020 = MS_TMAP_7_D_020 * WCS1 * 100) %>% 
  mutate(MS_TMAP_8_D_040 = MS_TMAP_8_D_040 * WCS2 * 100) %>% 
  mutate(MS_TMAP_9_D_060 = MS_TMAP_9_D_060 * WCS3 * 100)

#normalization function
min_max_normalize <- function(x){
  (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
}

#normalization
Normalization_Tensio <- Tensiometer_SWC %>% 
  mutate(MS_TMAP_1_D_020 = min_max_normalize(MS_TMAP_2_D_030)) %>% 
  mutate(MS_TMAP_2_D_030 = min_max_normalize(MS_TMAP_2_D_030)) %>% 
  mutate(MS_TMAP_3_D_050 = min_max_normalize(MS_TMAP_3_D_050)) %>% 
  
  mutate(MS_TMAP_4_D_020 = min_max_normalize(MS_TMAP_4_D_020)) %>% 
  mutate(MS_TMAP_5_D_040 = min_max_normalize(MS_TMAP_5_D_040)) %>% 
  mutate(MS_TMAP_6_D_060 = min_max_normalize(MS_TMAP_6_D_060)) %>% 
  
  mutate(MS_TMAP_7_D_020 = min_max_normalize(MS_TMAP_7_D_020)) %>% 
  mutate(MS_TMAP_8_D_040 = min_max_normalize(MS_TMAP_8_D_040)) %>% 
  mutate(MS_TMAP_9_D_060 = min_max_normalize(MS_TMAP_9_D_060))

#normalization
Normalization_Tensio_kPa <- Tensiometer %>% 
  mutate(MS_TMAP_1_D_020 = min_max_normalize(MS_TMAP_2_D_030)) %>% 
  mutate(MS_TMAP_2_D_030 = min_max_normalize(MS_TMAP_2_D_030)) %>% 
  mutate(MS_TMAP_3_D_050 = min_max_normalize(MS_TMAP_3_D_050)) %>% 
  
  mutate(MS_TMAP_4_D_020 = min_max_normalize(MS_TMAP_4_D_020)) %>% 
  mutate(MS_TMAP_5_D_040 = min_max_normalize(MS_TMAP_5_D_040)) %>% 
  mutate(MS_TMAP_6_D_060 = min_max_normalize(MS_TMAP_6_D_060)) %>% 
  
  mutate(MS_TMAP_7_D_020 = min_max_normalize(MS_TMAP_7_D_020)) %>% 
  mutate(MS_TMAP_8_D_040 = min_max_normalize(MS_TMAP_8_D_040)) %>% 
  mutate(MS_TMAP_9_D_060 = min_max_normalize(MS_TMAP_9_D_060))

#Make sure datetime is correct format
Tensiometer_SWC$TIMESTAMP <- format(Tensiometer_SWC$TIMESTAMP, format = "%Y:%m:%d %H:%M:%S")
WFPS_tensiometer$TIMESTAMP <- format(Tensiometer_SWC$TIMESTAMP, format = "%Y:%m:%d %H:%M:%S")
Normalization_Tensio$TIMESTAMP <- format(Normalization_Tensio$TIMESTAMP, format = "%Y:%m:%d %H:%M:%S")
Normalization_Tensio_kPa$TIMESTAMP <- format(Normalization_Tensio_kPa$TIMESTAMP, format = "%Y:%m:%d %H:%M:%S")
AFPS_tensiometer$TIMESTAMP <- format(AFPS_tensiometer$TIMESTAMP, format = "%Y-%m-%d %H:%M:%S")
AFPS_mm_tensiometer$TIMESTAMP <- format(AFPS_tensiometer$TIMESTAMP, format = "%Y-%m-%d %H:%M:%S")

#write to a new csv file
#Extracting dataset to CSV
write.csv2(AFPS_tensiometer, file = "Transformed/Langeweide_Tensio_AFPS.csv", row.names = FALSE)
write.csv2(AFPS_mm_tensiometer, file = "Transformed/Langeweide_Tensio_AFPS_mm.csv", row.names = FALSE)
write.csv2(Tensiometer_SWC, file = "Transformed/Langeweide_Tensio.csv", row.names = FALSE)
write.csv2(Subset_Bodem_fysische_metingen, file = "Datasets/MvG_Bodem_fysische_metingen.csv", row.names = FALSE)
write.csv2(Normalization_Tensio, file = "Transformed/Langeweide_Tensio_norm.csv", row.names = FALSE)
write.csv2(Normalization_Tensio_kPa, file = "Transformed/Langeweide_Tensio_norm_kPa.csv", row.names = FALSE)


test.read2 <- read.csv2("Transformed/Langeweide_Tensio_AFPS_mm.csv")

ggplot(Tensiometer_SWC, aes(x = TIMESTAMP)) +
  geom_point(aes(y = MS_TMAP_1_D_020, color = "20 cm"), size = 0.3) +
  geom_point(aes(y = MS_TMAP_2_D_030, color = "30 cm"), size = 0.3) +
  geom_point(aes(y = MS_TMAP_3_D_050, color = "50 cm"), size = 0.3) +
  labs(
    title = "Volumetric water content at given pressure head ψ (P1-3)",
    x = "Datetime",
    y = "θ(ψ)"
  ) +
  scale_color_manual(
    values = c("20 cm" = "red", "30 cm" = "purple", "50 cm" = "blue"),
    labels = c("20 cm", "30 cm", "50 cm"),
    name = "Depth"
  ) +
  theme(
    plot.title = element_text(size = 10)
  )+
  scale_x_datetime(
    labels = scales::date_format("%B")
  )

ggplot(Tensiometer_SWC, aes(x = TIMESTAMP)) +
  geom_point(aes(y = MS_TMAP_4_D_020, color = "20 cm"), size = 0.3) +
  geom_point(aes(y = MS_TMAP_5_D_040, color = "40 cm"), size = 0.3) +
  geom_point(aes(y = MS_TMAP_6_D_060, color = "60 cm"), size = 0.3) +
  labs(
    title = "Volumetric water content at given pressure head ψ (P4-6)",
    x = "Datetime",
    y = "θ(ψ)"
  ) +
  scale_color_manual(
    values = c("20 cm" = "red", "40 cm" = "purple", "60 cm" = "blue"),
    labels = c("20 cm", "40 cm", "60 cm"),
    name = "Depth"
  ) +
  theme(
    plot.title = element_text(size = 10)
  )+
  scale_x_datetime(
    labels = scales::date_format("%B")
  )

ggplot(Tensiometer_SWC, aes(x = TIMESTAMP)) +
  geom_point(aes(y = MS_TMAP_7_D_020, color = "20 cm"), size = 0.3) +
  geom_point(aes(y = MS_TMAP_8_D_040, color = "40 cm"), size = 0.3) +
  geom_point(aes(y = MS_TMAP_9_D_060, color = "60 cm"), size = 0.3) +
  labs(
    title = "Volumetric water content at given pressure head ψ (P7-9)",
    x = "Datetime",
    y = "θ(ψ)"
  ) +
  scale_color_manual(
    values = c("20 cm" = "red", "40 cm" = "purple", "60 cm" = "blue"),
    labels = c("20 cm", "40 cm", "60 cm"),
    name = "Depth"
  ) +
  theme(
    plot.title = element_text(size = 10)
  ) +
  scale_x_datetime(
    labels = scales::date_format("%B")
  )

ggplot(Tensiometer_SWC, aes(x = TIMESTAMP)) +
  geom_line(aes(y = MS_TMAP_1_D_020, color = "Probe 1"), size = 0.3) +
  geom_line(aes(y = MS_TMAP_4_D_020, color = "Probe 4"), size = 0.3) +
  geom_line(aes(y = MS_TMAP_7_D_020, color = "Probe 7"), size = 0.3) +
  labs(
    title = "θ(ψ) at 20 cm depth",
    x = "Datetime",
    y = "θ(ψ)"
  ) +
  scale_color_manual(
    values = c("Probe 1" = "red", "Probe 4" = "green", "Probe 7" = "blue"),
    labels = c("Probe 1", "Probe 4", "Probe 7"),
    name = "Probes"
  )  +
  theme(
    plot.title = element_text(size = 12)
  ) +
  scale_x_datetime(
    labels = scales::date_format("%B")
  )

ggplot(Tensiometer_SWC, aes(x = TIMESTAMP)) +
  geom_line(aes(y = MS_TMAP_5_D_040, color = "Probe 5"), size = 0.3) +
  geom_line(aes(y = MS_TMAP_8_D_040, color = "Probe 8"), size = 0.3) +
  labs(
    title = "θ(ψ) at 40 cm depth",
    x = "Datetime",
    y = "θ(ψ)"
  ) +
  scale_color_manual(
    values = c("Probe 5" = "red", "Probe 8" = "blue"),
    labels = c("Probe 5", "Probe 8"),
    name = "Probes"
  )  +
  theme(
    plot.title = element_text(size = 12)
  ) +
  scale_x_datetime(
    labels = scales::date_format("%B")
  )

ggplot(Tensiometer_SWC, aes(x = TIMESTAMP)) +
  geom_line(aes(y = MS_TMAP_6_D_060, color = "Probe 6"), size = 0.3) +
  geom_line(aes(y = MS_TMAP_9_D_060, color = "Probe 9"), size = 0.3) +
  labs(
    title = "θ(ψ) at 60 cm depth",
    x = "Datetime",
    y = "θ(ψ)"
  ) +
  scale_color_manual(
    values = c("Probe 6" = "red", "Probe 9" = "blue"),
    labels = c("Probe 6", "Probe 9"),
    name = "Probes"
  )  +
  theme(
    plot.title = element_text(size = 12)
  ) +
  scale_x_datetime(
    labels = scales::date_format("%B")
  )

ggplot(Tensiometer_SWC, aes(x = TIMESTAMP)) +
  geom_line(aes(y = MS_TMAP_1_D_020, color = "1_20 cm"), size = 0.3) +
  geom_line(aes(y = MS_TMAP_2_D_030, color = "2_30 cm"), size = 0.3) +
  geom_line(aes(y = MS_TMAP_3_D_050, color = "3_50 cm"), size = 0.3) +
  geom_line(aes(y = MS_TMAP_4_D_020, color = "4_20 cm"), size = 0.3) +
  geom_line(aes(y = MS_TMAP_5_D_040, color = "5_40 cm"), size = 0.3) +
  geom_line(aes(y = MS_TMAP_6_D_060, color = "6_60 cm"), size = 0.3) +
  geom_line(aes(y = MS_TMAP_7_D_020, color = "7_20 cm"), size = 0.3) +
  geom_line(aes(y = MS_TMAP_8_D_040, color = "8_40 cm"), size = 0.3) +
  geom_line(aes(y = MS_TMAP_9_D_060, color = "9_60 cm"), size = 0.3) +
  labs(
    title = "Volumetric water content at given pressure head ψ",
    x = "Datetime",
    y = "θ(ψ)"
  )+
  theme(
    plot.title = element_text(size = 10)
  )+
  scale_x_datetime(
    labels = scales::date_format("%B")
  )

#WFPS
ggplot(WFPS_tensiometer, aes(x = TIMESTAMP)) +
  geom_line(aes(y = MS_TMAP_1_D_020, color = "20 cm"), size = 0.3) +
  geom_line(aes(y = MS_TMAP_2_D_030, color = "30 cm"), size = 0.3) +
  geom_line(aes(y = MS_TMAP_3_D_050, color = "50 cm"), size = 0.3) +
  labs(
    title = "WFPS (%) from θ(ψ) (P1-3)",
    x = "Datetime",
    y = "WFPS (%)"
  ) +
  scale_color_manual(
    values = c("20 cm" = "red", "30 cm" = "purple", "50 cm" = "blue"),
    labels = c("20 cm", "30 cm", "50 cm"),
    name = "Depth"
  ) +
  theme(
    plot.title = element_text(size = 10)
  )+
  scale_x_datetime(
    labels = scales::date_format("%B")
  )

ggplot(WFPS_tensiometer, aes(x = TIMESTAMP)) +
  geom_line(aes(y = MS_TMAP_4_D_020, color = "20 cm"), size = 0.3) +
  geom_line(aes(y = MS_TMAP_5_D_040, color = "40 cm"), size = 0.3) +
  geom_line(aes(y = MS_TMAP_6_D_060, color = "60 cm"), size = 0.3) +
  labs(
    title = "WFPS (%) from θ(ψ) (P4-6)",
    x = "Datetime",
    y = "WFPS (%)"
  ) +
  scale_color_manual(
    values = c("20 cm" = "red", "40 cm" = "purple", "60 cm" = "blue"),
    labels = c("20 cm", "40 cm", "60 cm"),
    name = "Depth"
  ) +
  theme(
    plot.title = element_text(size = 10)
  )+
  scale_x_datetime(
    labels = scales::date_format("%B")
  )

ggplot(WFPS_tensiometer, aes(x = TIMESTAMP)) +
  geom_line(aes(y = MS_TMAP_7_D_020, color = "20 cm"), size = 0.3) +
  geom_line(aes(y = MS_TMAP_8_D_040, color = "40 cm"), size = 0.3) +
  geom_line(aes(y = MS_TMAP_9_D_060, color = "60 cm"), size = 0.3) +
  labs(
    title = "WFPS (%) from θ(ψ) (P7-9)",
    x = "Datetime",
    y = "WFPS (%)"
  ) +
  scale_color_manual(
    values = c("20 cm" = "red", "40 cm" = "purple", "60 cm" = "blue"),
    labels = c("20 cm", "40 cm", "60 cm"),
    name = "Depth"
  ) +
  theme(
    plot.title = element_text(size = 10)
  ) +
  scale_x_datetime(
    labels = scales::date_format("%B")
  )

ggplot(WFPS_tensiometer, aes(x = TIMESTAMP)) +
  geom_line(aes(y = MS_TMAP_1_D_020, color = "Probe 1"), size = 0.3) +
  geom_line(aes(y = MS_TMAP_4_D_020, color = "Probe 4"), size = 0.3) +
  geom_line(aes(y = MS_TMAP_7_D_020, color = "Probe 7"), size = 0.3) +
  labs(
    title = "WFPS (%) at 20 cm depth",
    x = "Datetime",
    y = "WFPS (%)"
  ) +
  scale_color_manual(
    values = c("Probe 1" = "red", "Probe 4" = "green", "Probe 7" = "blue"),
    labels = c("Probe 1", "Probe 4", "Probe 7"),
    name = "Probes"
  )  +
  theme(
    plot.title = element_text(size = 12)
  ) +
  scale_x_datetime(
    labels = scales::date_format("%B")
  )

ggplot(WFPS_tensiometer, aes(x = TIMESTAMP)) +
  geom_line(aes(y = MS_TMAP_5_D_040, color = "Probe 5"), size = 0.3) +
  geom_line(aes(y = MS_TMAP_8_D_040, color = "Probe 8"), size = 0.3) +
  labs(
    title = "WFPS (%) at 40 cm depth",
    x = "Datetime",
    y = "WFPS (%)"
  ) +
  scale_color_manual(
    values = c("Probe 5" = "red", "Probe 8" = "blue"),
    labels = c("Probe 5", "Probe 8"),
    name = "Probes"
  )  +
  theme(
    plot.title = element_text(size = 12)
  ) +
  scale_x_datetime(
    labels = scales::date_format("%B")
  )

ggplot(WFPS_tensiometer, aes(x = TIMESTAMP)) +
  geom_line(aes(y = MS_TMAP_6_D_060, color = "Probe 6"), size = 0.3) +
  geom_line(aes(y = MS_TMAP_9_D_060, color = "Probe 9"), size = 0.3) +
  labs(
    title = "WFPS (%) at 60 cm depth",
    x = "Datetime",
    y = "WFPS(%)"
  ) +
  scale_color_manual(
    values = c("Probe 6" = "red", "Probe 9" = "blue"),
    labels = c("Probe 6", "Probe 9"),
    name = "Probes"
  )  +
  theme(
    plot.title = element_text(size = 12)
  ) +
  scale_x_datetime(
    labels = scales::date_format("%B")
  )
