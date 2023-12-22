#Groundwater analysis

library(tidyverse)
library(readxl)

GWL <- read.csv("Datasets/LAW_MS.csv")
Langeweide_data <- readRDS("Datasets/LAW_MS_ICOS.rds")
RFH <- read_xlsx("Datasets/Peilbuizen_nap_langeweide.xlsx")

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
  select(WL_1, WL_2, WL_3, WL_4, WL_5, datetime) %>%
  mutate(WL_1 = (WL_1/100 - RFH$NAP_2021_mv[1])*100,
         WL_2 = (WL_2/100 - RFH$NAP_2021_mv[2])*100,
         WL_3 = (WL_3/100 - RFH$NAP_2021_mv[3])*100,
         WL_4 = (WL_4/100 - RFH$NAP_2021_mv[3])*100,
         WL_5 = (WL_5/100 - RFH$NAP_2021_mv[5])*100) %>%
  mutate(GWL_mean = rowMeans(select(., starts_with("WL")), na.rm = TRUE))

#order for datetime again, idk why it shifts
GWL_mmv <- GWL_mmv[order(GWL_mmv$datetime), ]

#GWL new is hourly other 30 min so add a NA row
GWL_half_hourly <- seq(from = start_time, to = end_time, by = "30 min")

GWL_half_hourly <- data.frame(datetime = GWL_half_hourly)

GWL_mmv <- GWL_half_hourly %>% 
  left_join(GWL_mmv, by = "datetime")

#Now i want to estimate start of april based on old GWL data 
GWL_old <- GWL_old %>%
  mutate(WL_1 = (WL_1/100 - RFH$NAP_2021_mv[1])*100,
         WL_2 = (WL_2/100 - RFH$NAP_2021_mv[2])*100,
         WL_3 = (WL_3/100 - RFH$NAP_2021_mv[3])*100,
         WL_4 = (WL_4/100 - RFH$NAP_2021_mv[3])*100,
         WL_5 = (WL_5/100 - RFH$NAP_2021_mv[5])*100) %>%
  mutate(GWL_mean = rowMeans(select(., starts_with("WL")), na.rm = TRUE))

start_date <- as.POSIXct("2022-04-02 01:00:00", format = "%Y-%m-%d %H:%M")
end_date <- as.POSIXct("2022-04-21 07:30:00")

GWL_subset <- GWL_old %>% 
  filter(datetime >= start_date & datetime <= end_date) %>% 
  select(-WL_cor)

GWL_corrected <- rbind(GWL_subset, GWL_mmv)

compare <- bind_cols(GWL_mmv$GWL_mean, GWL_old$WL_cor, GWL_old$datetime)

write_rds(GWL_mmv, file = "Transformed/Langeweide_groundwater.rds")
write_rds(GWL_old, file = "Transformed/Langeweide_groundwater_old.rds")

test <- readRDS("Transformed/Langeweide_groundwater.rds")
