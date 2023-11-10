library(tidyverse)

SENTEK_all <- read.csv2("Transformed/Langeweide_Sentek.csv")
TENSIO_SWC <- read.csv2("Transformed/Langeweide_Tensio.csv")
OWASIS_BBB <- read.csv2("Transformed/Langeweide_OWASIS_BBB.csv")

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
  group_by(Day = format(datetime, "%d"))


#I want to bind them to have one data file for analysis
SWC_all <- bind_cols(SENTEK_all[,2:30], TENSIO_SWC[,2:11], OWASIS_BBB)