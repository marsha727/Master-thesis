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

#makes a dataframe so the y value is attached to end and begin depth for all MvG parameters
#WCS
Interpolation_WCS <- data.frame(x = c(Subset_Bodem_fysische_metingen$begindiepte, Subset_Bodem_fysische_metingen$einddiepte), y = c(Subset_Bodem_fysische_metingen$WCS, Subset_Bodem_fysische_metingen$WCS))
interpolated_WCS <- spline(Interpolation_WCS$x, Interpolation_WCS$y, xout = depth_to_interpolate)$y
#a
Interpolation_a <- data.frame(x = c(Subset_Bodem_fysische_metingen$begindiepte, Subset_Bodem_fysische_metingen$einddiepte), y = c(Subset_Bodem_fysische_metingen$a, Subset_Bodem_fysische_metingen$a))
interpolated_a <- spline(Interpolation_a$x, Interpolation_a$y, xout = depth_to_interpolate)$y
#n
Interpolation_n <- data.frame(x = c(Subset_Bodem_fysische_metingen$begindiepte, Subset_Bodem_fysische_metingen$einddiepte), y = c(Subset_Bodem_fysische_metingen$n, Subset_Bodem_fysische_metingen$n))
interpolated_n <- spline(Interpolation_n$x, Interpolation_n$y, xout = depth_to_interpolate)$y
#m
Interpolation_m <- data.frame(x = c(Subset_Bodem_fysische_metingen$begindiepte, Subset_Bodem_fysische_metingen$einddiepte), y = c(Subset_Bodem_fysische_metingen$m, Subset_Bodem_fysische_metingen$m))
interpolated_m <- spline(Interpolation_m$x, Interpolation_m$y, xout = depth_to_interpolate)$y

#make new row to attach to dataset for INT = interpolated values
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


#Create the MvG equation for SWC
Subset_Bodem_fysische_metingen$a <- as.numeric(Subset_Bodem_fysische_metingen$a)
MvG <- function(x, WCS, WCR, a, m ,n){
  col_name <- colnames(Tensiometer_cmH20)
  depth <- as.numeric(sub(".*_(\\d{3})$", "\\1", col_name[-1])) #extracts the number from colname
  
  print(col_name)
  print(depth)
  
matching_rows <- numeric(length(depth))

for(i in 1:length(depth)){
  depth_value <- depth[i]
  print(depth_value)
  matching_row <- which(depth_value >= Subset_Bodem_fysische_metingen$begindiepte
                        & depth_value <= Subset_Bodem_fysische_metingen$einddiepte)
  matching_rows[i] <- ifelse(length(matching_row) > 0, matching_row, NA)
  print(matching_row)


  WCS <- Subset_Bodem_fysische_metingen$WCS[matching_row]#extract the right WCS for depth
  print(WCS)
  WCR <- Subset_Bodem_fysische_metingen$WCR[matching_row]
  a <- Subset_Bodem_fysische_metingen$a[matching_row]
  print(a)
  n <- Subset_Bodem_fysische_metingen$n[matching_row]
  print(n)
  m <- Subset_Bodem_fysische_metingen$m[matching_row]
  print(m)
}
  ifelse(is.na(x), NA, WCS + ((WCS - WCR)/((1+abs(a * x^n)^m))))
}

SWC_Tensiometer <- Tensiometer %>% 
  mutate(across(-c(1,11:19), ~MvG(.)))

  

