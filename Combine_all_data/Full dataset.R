library(tidyverse)

AFPS_SENTEK <- read.csv2("Transformed/Langeweide_Sentek_AFPS.csv")
AFPS_TENSIO <- read.csv2("Transformed/Langeweide_tensio_interpolated.csv")

ET <- readRDS("App/Langeweide_ET_noWindCor.rds") #for daily EF
ET_halfhour <- readRDS("App/Langeweide_ET_halfhour.RDS")
OWASIS <- read.csv2("Transformed/Langeweide_OWASIS_BBB.csv")
Langeweide_data <- readRDS("Datasets/LAW_MS_ICOS.RDS")
WL <- readRDS("Transformed/Langeweide_groundwater.rds")

#Formating of datetime
AFPS_SENTEK$datetime <- as.POSIXct(AFPS_SENTEK$datetime, format = "%Y-%m-%d %H:%M:%S")
AFPS_TENSIO$TIMESTAMP <- as.POSIXct(AFPS_TENSIO$datetime, format = "%Y-%m-%d %H:%M:%S")

#Integrate SENTEK + tensio over 60 cm ~ not perfect allignment
AFPS_int_SENTEK <- AFPS_SENTEK %>% 
  mutate(SENTEK1 = SWC_1_025 + SWC_1_035 + SWC_1_045 + SWC_1_055 + SWC_1_065) %>% 
  mutate(SENTEK3 = SWC_3_025 + SWC_3_035 + SWC_3_045 + SWC_3_055 + SWC_1_065) %>% 
  select(datetime, SENTEK1, SENTEK3)

#This integrate TENSIO per halfhour
AFPS_int_TENSIO <- AFPS_TENSIO %>% 
  group_by(datetime) %>% 
  summarise(TENSIO2 = sum(AFPS2), TENSIO3 = sum(AFPS3)) %>% 
  select(datetime, TENSIO2, TENSIO3)

#Extract only ET (mm) and EF (ratio)
EF <- ET %>% 
  select(datetime, EF3)

ET_halfhour <- ET_halfhour %>% 
  select(datetime, ET)

#Extract OWASIS and set time to 1am for merging later
OWASIS <- OWASIS %>% 
  select(Date, MedianBBB) %>% 
  rename(datetime = Date,
         OWASIS = MedianBBB)

OWASIS$datetime <- as.POSIXct(OWASIS$datetime, format = "%Y-%m-%d")

OWASIS$datetime <- as.POSIXct(format(OWASIS$datetime, "%Y-%m-%d 01:00:00"))

#remove columns that i wanted to replace in langeweide file
Langeweide_subset <- Langeweide_data %>% 
  select(-c(ET, WL_1, WL_2, WL_3, WL_4, WL_5))

#Correct datetime format
AFPS_int_SENTEK$datetime <- as.POSIXct(AFPS_int_SENTEK$datetime, format = "%Y-%m-%d")

#select dateframes for merging
data_frames <- list(Langeweide_subset, EF, ET_halfhour, AFPS_int_SENTEK, AFPS_int_TENSIO, OWASIS, WL)

#merge dataframes
LAW_MS_ICOS <- Reduce(function(x, y) merge(x, y, by = "datetime", all.x = TRUE), data_frames)

LAW_MS_ICOS$datetime <- as.POSIXct(LAW_MS_ICOS$datetime, format = "%Y-%m-%d %H:%M:%S")
LAW_MS_ICOS$datetime <- format(LAW_MS_ICOS$datetime, "%Y-%m-%d %H:%M:%S")


write_rds(LAW_MS_ICOS, "Langeweide/LAW_MS_ICOS.RDS")
write.csv2(LAW_MS_ICOS, "Langeweide/LAW_MS_ICOS.csv", row.names = FALSE)

test <- readRDS("Langeweide/LAW_MS_ICOS.RDS")
test <- read.csv2("Langeweide/LAW_MS_ICOS.csv")
