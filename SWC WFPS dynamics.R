library(tidyverse)

SENTEK_all <- read.csv2("Transformed/Langeweide_Sentek.csv")
TENSIO_SWC <- read.csv2("Transformed/Langeweide_Tensio.csv")
OWASIS_BBB <- read.csv2("Transformed/Langeweide_OWASIS_BBB.csv")

#converting datatime to POSIXct
SENTEK_all$datetime <- as.POSIXct(SENTEK_all$datetime, format = "%Y-%m-%d %H:%M:%S")
TENSIO_SWC$TIMESTAMP <- as.POSIXct(TENSIO_SWC$TIMESTAMP, format = "%Y-%m-%d %H:%M:%S")
OWASIS_BBB$Date <- as.POSIXct(OWASIS_BBB$Date, format = "%Y-%m-%d")

#start and end date to make overlap correct
start_date <- "2022-04-02"
end_date <- "2022-11-02"

#filter for the dates
SENTEK_all <- SENTEK_all %>% 
  filter(datetime >= start_date & datetime <= end_date)
TENSIO_SWC <- TENSIO_SWC %>% 
  filter(TIMESTAMP >= start_date & TIMESTAMP <= end_date)

#need to aggregate by day for OWASIS
SENTEK_all <- SENTEK_all %>% 
  group_by(datetime = format(datetime, "%Y-%m-%d")) %>% 
  summarise(across(everything(), mean, na.rm = TRUE))

TENSIO_SWC <- TENSIO_SWC %>% 
  group_by(TIMESTAMP, format(TIMESTAMP, "%Y-%m-%d")) %>% 
  summarise(across(everything(), mean, na.rm = TRUE))
  

#I want to bind them to have one data file for analysis
SWC_all <- bind_cols(SENTEK_all[,2:30], TENSIO_SWC[,2:11], OWASIS_BBB)