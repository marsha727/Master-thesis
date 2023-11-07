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

#Interpolate first on begin then end depth and take the average value as it is a range
#WCS
interpolated_WCS_b <- spline(Subset_Bodem_fysische_metingen$begindiepte, Subset_Bodem_fysische_metingen$WCS, xout = depth_to_interpolate)$y
interpolated_WCS_e <- spline(Subset_Bodem_fysische_metingen$einddiepte, Subset_Bodem_fysische_metingen$WCS, xout = depth_to_interpolate)$y
interpolated_WCS = mean(c(interpolated_WCS_b, interpolated_WCS_e))
#a
interpolated_a_b <- spline(Subset_Bodem_fysische_metingen$begindiepte, Subset_Bodem_fysische_metingen$a, xout = depth_to_interpolate)$y
interpolated_a_e <- spline(Subset_Bodem_fysische_metingen$einddiepte, Subset_Bodem_fysische_metingen$a, xout = depth_to_interpolate)$y
interpolated_a = mean(c(interpolated_a_b, interpolated_a_e))
#n
interpolated_n_b <- spline(Subset_Bodem_fysische_metingen$begindiepte, Subset_Bodem_fysische_metingen$n, xout = depth_to_interpolate)$y
interpolated_n_e <- spline(Subset_Bodem_fysische_metingen$einddiepte, Subset_Bodem_fysische_metingen$n, xout = depth_to_interpolate)$y
interpolated_n = mean(c(interpolated_n_b, interpolated_n_e))
#m
interpolated_m_b <- spline(Subset_Bodem_fysische_metingen$begindiepte, Subset_Bodem_fysische_metingen$m, xout = depth_to_interpolate)$y
interpolated_m_e <- spline(Subset_Bodem_fysische_metingen$einddiepte, Subset_Bodem_fysische_metingen$m, xout = depth_to_interpolate)$y
interpolated_m = mean(c(interpolated_m_b, interpolated_m_e))

#Make a new row of the interpolated values to the subset in similar format
new_row <- Subset_Bodem_fysische_metingen %>% 
  summarize(
    Locatie = "INT",
    begindiepte = depth_to_interpolate,
    einddiepte = depth_to_interpolate,
    WCS = interpolated_WCS,
    WCR = 0,
    a = interpolated_a,
    n = interpolated_n,
    m = interpolated_m,
    across(.cols = -c(Locatie, begindiepte, einddiepte, WCS, WCR, a, n, m), .fns = ~NA)
  )

#attach row to the subset
Subset_Bodem_fysische_metingen <- rbind(Subset_Bodem_fysische_metingen, new_row)

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

#Make sure datetime is correct format
Tensiometer_SWC$TIMESTAMP <- as.POSIXct(Tensiometer_SWC$TIMESTAMP, format = "%Y-%m-%d %H:%M:%S")

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
