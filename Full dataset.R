#combine with old LAW_ICOS file

ET <- readRDS("App/Langeweide_ET_noWindCor.rds")
ET_halfhour <- readRDS("App/Langeweide_ET_halfhour.rds")
AFPS <- readRDS("App/AFPS_int_TS.rds") #for OWASIS
AFPS_halfhour <- readRDS("App/AFPS_int_TS_halfhour.rds")
Langeweide_data <- readRDS("Datasets/LAW_MS_ICOS.RDS")
WL <- readRDS("Transformed/Langeweide_groundwater.rds")

#Extract only ET and EF
EF <- ET %>% 
  select(datetime, EF3)

ET_halfhour <- ET_halfhour %>% 
  select(datetime, ET)

#Extract OWASIS and set time to 1am
OWASIS <- AFPS %>% 
  select(datetime, OWASIS)

OWASIS$datetime <- as.POSIXct(OWASIS$datetime, format = "%Y-%m-%d")

OWASIS$datetime <- as.POSIXct(format(OWASIS$datetime, "%Y-%m-%d 01:00:00"))

#remove columns that i wanted to replace in langeweide file
Langeweide_subset <- Langeweide_data %>% 
  select(-c(ET, WL_1, WL_2, WL_3, WL_4, WL_5))


#select dateframes for merging
data_frames <- list(Langeweide_subset, EF, ET_halfhour, AFPS_halfhour, OWASIS, WL)

#merge dataframes
LAW_MS_ICOS <- Reduce(function(x, y) merge(x, y, by = "datetime", all.x = TRUE), data_frames)

write_rds(LAW_MS_ICOS, "Langeweide/LAW_MS_ICOS.RDS")

test <- readRDS("Langeweide/LAW_MS_ICOS.RDS")

