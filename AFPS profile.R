#For the profile over depth

library(tidyverse)

TENSIO <- readRDS("Transformed/Langeweide_tensio_interpolated.rds")
SENTEK <- read.csv2("Transformed/Langeweide_Sentek_AFPS.csv")

#I need to change the structure of the SENTEK file
#Make a long tibble basically just rename the to the depth it represents
SENTEK_long1 <- tibble(date = as.POSIXct(SENTEK$datetime, format = "%Y-%m-%d %H:%M:%S"),
                       Depth_005 = SENTEK$SWC_1_005,
                       Depth_015 = SENTEK$SWC_1_015,
                       Depth_025 = SENTEK$SWC_1_025,
                       Depth_035 = SENTEK$SWC_1_035,
                       Depth_045 = SENTEK$SWC_1_045,
                       Depth_055 = SENTEK$SWC_1_055,
                       Depth_065 = SENTEK$SWC_1_065,
                       Depth_075 = SENTEK$SWC_1_075,
                       Depth_085 = SENTEK$SWC_1_085,
                       Depth_095 = SENTEK$SWC_1_095,
                       Depth_105 = SENTEK$SWC_1_105,
                       Depth_115 = SENTEK$SWC_1_115)


SENTEK_long3 <- tibble(date = as.POSIXct(SENTEK$datetime, format = "%Y-%m-%d %H:%M:%S"),
                       Depth_005 = SENTEK$SWC_3_005,
                       Depth_015 = SENTEK$SWC_3_015,
                       Depth_025 = SENTEK$SWC_3_025,
                       Depth_035 = SENTEK$SWC_3_035,
                       Depth_045 = SENTEK$SWC_3_045,
                       Depth_055 = SENTEK$SWC_3_055,
                       Depth_065 = SENTEK$SWC_3_065,
                       Depth_075 = SENTEK$SWC_3_075,
                       Depth_085 = SENTEK$SWC_3_085,
                       Depth_095 = SENTEK$SWC_3_095,
                       Depth_105 = SENTEK$SWC_3_105,
                       Depth_115 = SENTEK$SWC_3_115)

#Creates the sequence for depths sort for each date
Depths <- sort(unique(c(c(5, 115), seq(ceiling(5), floor(115), 10))))
Depths <- crossing(date = unique(SENTEK_long1$date), depth = Depths)

#This basically attaches the values from long tibble to the depths per date
SENTEK_profile1 <- SENTEK_long1 %>% 
  gather(depth, value, -date) %>% 
  mutate(depth = as.numeric(gsub("\\D", "", depth))) %>% 
  full_join(Depths) %>% 
  arrange(date, depth) %>% 
  group_by(date)

SENTEK_profile3 <- SENTEK_long3 %>% 
  gather(depth, value, -date) %>% 
  mutate(depth = as.numeric(gsub("\\D", "", depth))) %>% 
  full_join(Depths) %>% 
  arrange(date, depth) %>% 
  group_by(date)


