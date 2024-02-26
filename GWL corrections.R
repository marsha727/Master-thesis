#Groundwater analysis

library(tidyverse)
library(readxl)

GWL <- read.csv("Datasets/LAW_MS.csv") #this one does not cover full study period
Langeweide_data <- readRDS("Datasets/LAW_MS_ICOS.rds") #full GWL data but older data
RFH <- read_xlsx("Datasets/Peilbuizen_nap_langeweide.xlsx") #reference heights

#select old data for comparison and estimation for earlier data
GWL_old <- Langeweide_data %>% 
  select(datetime, WL_1, WL_2, WL_3, WL_4, WL_5, WL_cor)

#ensure the date formatting
GWL$datetime <- as.POSIXct(GWL$datetime, format = "%Y-%m-%d %H:%M:%S")

#somehow the 00:00:00 times are removed so need to fix
GWL <- GWL %>% 
  mutate(datetime = as.POSIXct(ifelse(is.na(datetime), lag(datetime) + 3600, datetime)))

#order according to date
GWL <- GWL[order(GWL$datetime), ]

start_time <- min(GWL$datetime)
end_time <- max(GWL_old$datetime)

#substract the surface height/maaiveldhoogte
GWL_mmv <- GWL %>%
  select(datetime, WL_1, WL_2, WL_3, WL_4, WL_5) %>%
  mutate(WL_1 = (WL_1/100 - RFH$NAP_2021_mv[1])*100,
         WL_2 = (WL_2/100 - RFH$NAP_2021_mv[2])*100,
         WL_3 = (WL_3/100 - RFH$NAP_2021_mv[3])*100,
         WL_4 = (WL_4/100 - RFH$NAP_2021_mv[3])*100,
         WL_5 = (WL_5/100 - RFH$NAP_2021_mv[5])*100) %>%
  mutate(GWL_mean = as.numeric(rowMeans(select(., starts_with("WL")), na.rm = TRUE)))

#GWL new is hourly other 30 min so add a NA row
GWL_half_hourly <- seq(from = start_time, to = end_time, by = "30 min")

GWL_half_hourly <- data.frame(datetime = GWL_half_hourly)

GWL_mmv <- GWL_half_hourly %>% 
  left_join(GWL_mmv, by = "datetime")

#Do same calculations for old GWL data
GWL_LAW <- GWL_old %>%
  select(datetime, WL_1, WL_2, WL_3, WL_4, WL_5) %>% 
  mutate(WL_1 = (WL_1/100 - RFH$NAP_2021_mv[1])*100,
         WL_2 = (WL_2/100 - RFH$NAP_2021_mv[2])*100,
         WL_3 = (WL_3/100 - RFH$NAP_2021_mv[3])*100,
         WL_4 = (WL_4/100 - RFH$NAP_2021_mv[3])*100,
         WL_5 = (WL_5/100 - RFH$NAP_2021_mv[5])*100) %>%
  mutate(GWL_mean = rowMeans(select(., starts_with("WL")), na.rm = TRUE))

#removes NA rows for ggplot comparison
#GWL_LAW$GWL_mean <- ifelse(is.nan(GWL_LAW$GWL_mean), NA, GWL_LAW$GWL_mean)
#GWL_LAW <- GWL_LAW[!is.na(GWL_LAW$GWL_mean), ]

#!!!!!The old data is the same, but mean was computed differently
#First a mean of all GWL points, then substract AHN
#Reference heights makes ~5cm difference
#Raw GWL data is same, so i use GWL_LAW (old) for longer study period

GWL_LAW$datetime <- format(GWL_LAW$datetime, "%Y-%m-%d %H:%M:%S")

write_rds(GWL_LAW, file = "Transformed/Langeweide_groundwater.rds")
write.csv(GWL_LAW, file = "Transformed/Langeweide_groundwater.csv", row.names = FALSE)

test <- readRDS("Transformed/Langeweide_groundwater.rds")
test2 <- read.csv("Transformed/Langeweide_groundwater.csv")
test3 <- read.csv("Transformed/Langeweide_groundwater_2023.csv")
