#Subscript for tensiometer interpolation
library(tidyverse)

tensio <- read.csv("Datasets/LAW_TENS_2020-2023_clean.csv")
Subset_Bodem_fysische_metingen <- read.csv2("MvG_Bodem_fysische_metingen.csv")


tensio$TIMESTAMP <- as.POSIXct(tensio$TIMESTAMP, format = "%Y-%m-%d %H:%M:%S")

tensio <- tensio %>% 
  select(TIMESTAMP, MS_TMAP_4_D_020, MS_TMAP_5_D_040, MS_TMAP_6_D_060,
         MS_TMAP_7_D_020, MS_TMAP_8_D_040, MS_TMAP_9_D_060)

#Dealing with missing values

#check if the datasets have a high correlation so i can interpolate based on its dynamics
cor <- cor(tensio$MS_TMAP_4_D_020, tensio$MS_TMAP_7_D_020, method = "pearson", use = "complete.obs")
cor2 <- cor(tensio$MS_TMAP_5_D_040, tensio$MS_TMAP_8_D_040, method = "pearson", use = "complete.obs")

#Cor was above 98, the second set will be drier due to distance from drain so use the average difference for calc
per <- mean(tensio$MS_TMAP_4_D_020 / tensio$MS_TMAP_7_D_020, na.rm = TRUE)
per2 <- mean(tensio$MS_TMAP_5_D_040 / tensio$MS_TMAP_8_D_040, na.rm = TRUE)

#return rows with missing values
missing_values_indices <- which(is.na(tensio$MS_TMAP_4_D_020))
missing_values_indices2 <- which(is.na(tensio$MS_TMAP_8_D_040))

#Find the start and end indices/rows of the gap
start_index <- min(missing_values_indices)
end_index <- max(missing_values_indices)

start_index2 <- min(missing_values_indices2)
end_index2 <- max(missing_values_indices2)

# Use the percentage difference to estimate missing values of MS_TMAP_4_D_020 based on MS_TMAP_7_D_020
tensio$MS_TMAP_4_D_020[start_index:end_index] <-
  tensio$MS_TMAP_7_D_020[start_index:end_index] * per

tensio$MS_TMAP_8_D_040[start_index2:end_index2] <-
  tensio$MS_TMAP_5_D_040[start_index2:end_index2] * per2

#Interpolation method

#make subselection for tensio point 2 and 3
tensio_2 <- tensio %>% 
  select(TIMESTAMP, MS_TMAP_4_D_020, MS_TMAP_5_D_040, MS_TMAP_6_D_060)

tensio_3 <- tensio %>% 
  select(TIMESTAMP, MS_TMAP_7_D_020, MS_TMAP_8_D_040, MS_TMAP_9_D_060)

#replace 1 NA with previous value to prevent error
tensio_2$MS_TMAP_4_D_020 <- ifelse(is.na(tensio_2$MS_TMAP_4_D_020), lag(tensio_2$MS_TMAP_4_D_020), tensio_2$MS_TMAP_4_D_020)
tensio_2$MS_TMAP_5_D_040 <- ifelse(is.na(tensio_2$MS_TMAP_5_D_040), lag(tensio_2$MS_TMAP_5_D_040), tensio_2$MS_TMAP_5_D_040)
tensio_2$MS_TMAP_6_D_060 <- ifelse(is.na(tensio_2$MS_TMAP_6_D_060), lag(tensio_2$MS_TMAP_6_D_060), tensio_2$MS_TMAP_6_D_060)

tensio_3$MS_TMAP_7_D_020 <- ifelse(is.na(tensio_3$MS_TMAP_7_D_020), lag(tensio_3$MS_TMAP_7_D_020), tensio_3$MS_TMAP_7_D_020)
tensio_3$MS_TMAP_8_D_040 <- ifelse(is.na(tensio_3$MS_TMAP_8_D_040), lag(tensio_3$MS_TMAP_8_D_040), tensio_3$MS_TMAP_8_D_040)
tensio_3$MS_TMAP_9_D_060 <- ifelse(is.na(tensio_3$MS_TMAP_9_D_060), lag(tensio_3$MS_TMAP_9_D_060), tensio_3$MS_TMAP_9_D_060)

#Make a long tibble so that the depths and the known SMP are in a seperate column
tensio_long2 <- tibble(date = as.POSIXct(tensio_2$TIMESTAMP),
                      Depth_20 = tensio_2$MS_TMAP_4_D_020,
                      Depth_40 = tensio_2$MS_TMAP_5_D_040,
                      Depth6_0 = tensio_2$MS_TMAP_6_D_060)

tensio_long3 <- tibble(date = as.POSIXct(tensio_3$TIMESTAMP),
                       Depth_20 = tensio_3$MS_TMAP_7_D_020,
                       Depth_40 = tensio_3$MS_TMAP_8_D_040,
                       Depth6_0 = tensio_3$MS_TMAP_9_D_060)

#First i say which depths to interpolate (= every 1 cm) then i say do each depth at every date
Depths_to_interpolate2 <- sort(unique(c(c(20, 40, 60), seq(ceiling(20), floor(60), 1))))
Depths_to_interpolate2 <- crossing(date = unique(tensio_long2$date), depth = Depths_to_interpolate2)

Depths_to_interpolate3 <- sort(unique(c(c(20, 40, 60), seq(ceiling(20), floor(60), 1))))
Depths_to_interpolate3 <- crossing(date = unique(tensio_long3$date), depth = Depths_to_interpolate3)


#linear interpolation
#tensio_interp2 <- tensio_long2 %>% 
  #gather(depth, value, -date) %>% 
  #mutate(depth = as.numeric(gsub("\\D", "", depth))) %>% 
  #full_join(Depths_to_interpolate2) %>% 
  #arrange(date, depth) %>% 
  #group_by(date) %>% 
  #mutate(value.interp = if(length(na.omit(value)) > 1) { 
    #approx(depth, value, xout = depth)$y
  #} else{
    #value
  #})

#linear interpolation
#tensio_interp3 <- tensio_long3 %>% 
  #gather(depth, value, -date) %>% 
  #mutate(depth = as.numeric(gsub("\\D", "", depth))) %>% 
  #full_join(Depths_to_interpolate3) %>% 
  #arrange(date, depth) %>% 
  #group_by(date) %>% 
  #mutate(value.interp = if(length(na.omit(value)) > 1) { 
    #approx(depth, value, xout = depth)$y
  #} else{
    #value
  #})

#non-linear interpolation with splinefun and monoH.FC
tensio_interp2 <- tensio_long2 %>% 
  gather(depth, value, -date) %>% 
  mutate(depth = as.numeric(gsub("\\D", "", depth))) %>% 
  full_join(Depths_to_interpolate2) %>% 
  arrange(date, depth) %>% 
  group_by(date) %>% #this first part is to create the table with column depth and value
  mutate(value.interp = if (length(na.omit(value)) > 1) { 
    # Non-linear interpolation using splinefun for each date group
    spline_fit1 <- splinefun(depth, value, method = "monoH.FC")
    interpolated_values <- spline_fit1(seq(20, 60, by = 1))
    
    # Clip the interpolated values to the range of the original depths
    pmin(pmax(interpolated_values, min(value, na.rm = TRUE)), max(value, na.rm = TRUE))
  } else {
    value
  })

#non-linear interpolation with splinefun and monoH.FC (monotonicity of increase)
tensio_interp3 <- tensio_long3 %>% 
  gather(depth, value, -date) %>% 
  mutate(depth = as.numeric(gsub("\\D", "", depth))) %>% 
  full_join(Depths_to_interpolate3) %>% 
  arrange(date, depth) %>% 
  group_by(date) %>% 
  mutate(value.interp = if (length(na.omit(value)) > 1) { 
    # Non-linear interpolation using splinefun for each date group
    spline_fit2 <- splinefun(depth, value, method = "monoH.FC")
    interpolated_values <- spline_fit2(seq(20, 60, by = 1))
    # Clip the interpolated values to the range of the original depths
    pmin(pmax(interpolated_values, min(value, na.rm = TRUE)), max(value, na.rm = TRUE))
  } else {
    value
  })


#filter for combination of datasets rename columns
tensio_interp2 <- tensio_interp2 %>% 
  select(date, depth, value.interp) %>% 
  rename(date_2 = date, depth_2 = depth, SMP_2 = value.interp)

tensio_interp3 <- tensio_interp3 %>% 
  select(date, depth, value.interp) %>% 
  rename(date_3 = date, depth_3 = depth, SMP_3 = value.interp)


#combine datasets
combine_tensio <- bind_cols(tensio_interp2, tensio_interp3)
combine_tensio <- as.data.frame(combine_tensio)

#Now perform the conversions 

#Matric potential from kPa to cmH20
kPa_to_cmH2O <- function(x){
  ifelse(is.na(x), NA, x*10.1971623)
}

Tensiometer_cmH20 <- combine_tensio %>% 
  mutate(across(contains("SMP"), ~kPa_to_cmH2O(.)))

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

#apply MvG function adjusted for interpolations depths
#note that MvG paramaters is per larger layer
SWC_TENSIO <- Tensiometer_cmH20 %>% 
  mutate(
    SWC2 = case_when(
      between(depth_2, 15, 35) ~ WCR1 + (WCS1 - WCR1)/((1 + abs(a1 * SMP_2)^n1)^m1),
      between(depth_2, 36, 55) ~ WCR2 + (WCS2 - WCR2)/((1 + abs(a2 * SMP_2)^n2)^m2),
      between(depth_2, 56, 65) ~ WCR3 + (WCS3 - WCR3)/((1 + abs(a3 * SMP_2)^n3)^m3),
      TRUE ~ NA_real_
      ),
    SWC3 = case_when(
      between(depth_3, 15, 35) ~ WCR1 + (WCS1 - WCR1)/((1 + abs(a1 * SMP_3)^n1)^m1),
      between(depth_3, 36, 55) ~ WCR2 + (WCS2 - WCR2)/((1 + abs(a2 * SMP_3)^n2)^m2),
      between(depth_3, 56, 65) ~ WCR3 + (WCS3 - WCR3)/((1 + abs(a3 * SMP_3)^n3)^m3),
      TRUE ~ NA_real_
    )
  )

#Now convert to AFPS (%) using the saturated water content
AFPS_tensio <- SWC_TENSIO %>% 
  mutate(
    AFPS2 = case_when(
      between(depth_2, 15, 35) ~ 1 - (SWC2 / WCS1),
      between(depth_2, 36, 55) ~ 1 - (SWC2 / WCS2),
      between(depth_2, 56, 65) ~ 1 - (SWC2 / WCS3),
      TRUE ~ NA_real_
    ),
    AFPS3 = case_when(
      between(depth_3, 15, 35) ~ 1 - (SWC3 / WCS1),
      between(depth_3, 36, 55) ~ 1 - (SWC3 / WCS2),
      between(depth_3, 56, 65) ~ 1 - (SWC3 / WCS3),
      TRUE ~ NA_real_
    )
  )

#Convert AFPS to mm by multiplying with total volume and max porosity
AFPS_mm_tensio <- AFPS_tensio %>% 
  mutate(
    AFPS2_mm = case_when(
      between(depth_2, 15, 35) ~ AFPS2 * WCS1 * 10,
      between(depth_2, 36, 55) ~ AFPS2 * WCS2 * 10,
      between(depth_2, 56, 65) ~ AFPS2 * WCS3 * 10,
      TRUE ~ NA_real_
    ),
    AFPS3_mm = case_when(
      between(depth_3, 15, 35) ~ AFPS3 * WCS1 * 10,
      between(depth_3, 36, 55) ~ AFPS3 * WCS2 * 10,
      between(depth_3, 56, 65) ~ AFPS3 * WCS3 * 10,
      TRUE ~ NA_real_   
    )
  )

#Write to csv for analysis

#clean some collumns out
AFPS_mm_tensio <- AFPS_mm_tensio %>% 
  select(date_2, depth_2, AFPS2_mm, AFPS3_mm) %>% 
  rename(datetime = date_2, depth = depth_2, AFPS2 = AFPS2_mm, AFPS3 = AFPS3_mm)


#write in RDS and CSV
write_rds(AFPS_mm_tensio, file = "App/Langeweide_tensio_interpolated.rds")

AFPS_mm_tensio$datetime <- format(AFPS_mm_tensio$datetime, "%Y-%m-%d %H:%M:%S")

write.csv2(AFPS_mm_tensio, file = "Transformed/Langeweide_tensio_interpolated.csv", row.names = FALSE)

testread <- read.csv2("Transformed/Langeweide_tensio_interpolated.csv")

