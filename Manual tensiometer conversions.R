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
  mutate(across(.cols = 2:10, ~kPa_to_cmH2O(.)))



#non linear interpolation using both start and end depths
#make a subset of the values i require (I remove NA row manually for now)
Subset_Bodem_fysische_metingen <- Bodem_fysische_metingen %>% 
  filter(row_number() %in% c(3, 4, 7))

depth_to_interpolate <- 30

Subset_Bodem_fysische_metingen$a <- as.numeric(Subset_Bodem_fysische_metingen$a)
Subset_Bodem_fysische_metingen$begindiepte <- as.numeric(Subset_Bodem_fysische_metingen$begindiepte)
Subset_Bodem_fysische_metingen$einddiepte <- as.numeric(Subset_Bodem_fysische_metingen$einddiepte)

#makes a dataframe so the y value is attached to end and begin depth for all MvG parameters
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

Subset_Bodem_fysische_metingen <- rbind(Subset_Bodem_fysische_metingen, new_row)

ggplot(Subset_Bodem_fysische_metingen) +
  geom_point(aes(x = begindiepte, y = WCS)) +
  geom_point(aes(x = einddiepte, y = WCS)) 

  


#calling values manually because i couldnt get my loop to function
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

#doing calculation manually because i couldnt make the loop work
Tensiometer_cmH20 <- Tensiometer_cmH20 %>% 
  mutate(MS_TMAP_1_D_020 = WCR1 + (WCS1 - WCR1)/((1 + abs(a1 * MS_TMAP_1_D_020)^n1)^m1)) %>% 
  mutate(MS_TMAP_2_D_030 = WCR1 + ((interpolated_WCS - WCR1)/((1+abs(interpolated_a * MS_TMAP_2_D_030)^interpolated_n)^interpolated_m))) %>% 
  mutate(MS_TMAP_3_D_050 = WCR2 + ((WCS2 - WCR2)/((1+abs(a2 * MS_TMAP_3_D_050)^n2)^m2))) %>% 
  
  mutate(MS_TMAP_4_D_020 = WCR1 + ((WCS1 - WCR1)/((1+abs(a1 * MS_TMAP_4_D_020)^n1)^m1))) %>% 
  mutate(MS_TMAP_5_D_040 = WCR2 + ((WCS2 - WCR2)/((1+abs(a2 * MS_TMAP_5_D_040)^n2)^m2))) %>%
  mutate(MS_TMAP_6_D_060 = WCR3 + ((WCS3 - WCR3)/((1+abs(a3 * MS_TMAP_6_D_060)^n3)^m3))) %>% 
  
  mutate(MS_TMAP_7_D_020 = WCR1 + ((WCS1 - WCR1)/((1+abs(a1 * MS_TMAP_7_D_020)^n1)^m1))) %>% 
  mutate(MS_TMAP_8_D_040 = WCR2 + ((WCS2 - WCR2)/((1+abs(a2 * MS_TMAP_8_D_040)^n2)^m2))) %>%
  mutate(MS_TMAP_9_D_060 = WCR3 + ((WCS3 - WCR3)/((1+abs(a3 * MS_TMAP_9_D_060)^n3)^m3)))
