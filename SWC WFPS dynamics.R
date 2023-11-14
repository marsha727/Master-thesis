library(tidyverse)

SENTEK_all <- read.csv2("Transformed/Langeweide_Sentek.csv")
TENSIO_SWC <- read.csv2("Transformed/Langeweide_Tensio.csv")
OWASIS_BBB <- read.csv2("Transformed/Langeweide_OWASIS_BBB.csv")
TENSIO_WFPS <- read.csv2("Transformed/Langeweide_Tensio_WFPS.csv")

#converting datatime to POSIXct
SENTEK_all$datetime <- as.POSIXct(SENTEK_all$datetime, format = "%Y-%m-%d %H:%M:%S")
TENSIO_SWC$TIMESTAMP <- as.POSIXct(TENSIO_SWC$TIMESTAMP, format = "%Y-%m-%d %H:%M:%S")
OWASIS_BBB$Date <- as.POSIXct(OWASIS_BBB$Date, format = "%Y-%m-%d")
TENSIO_WFPS$TIMESTAMP <- as.POSIXct(TENSIO_WFPS$TIMESTAMP, format = "%Y-%m-%d %H:%M:%S")

#start and end date to make overlap correct
start_date <- "2022-04-02"
end_date <- "2022-11-01"

#filter for the dates
SENTEK_all <- SENTEK_all %>% 
  filter(datetime >= start_date & datetime <= end_date)
TENSIO_SWC <- TENSIO_SWC %>% 
  filter(TIMESTAMP >= start_date & TIMESTAMP <= end_date)
TENSIO_WFPS <- TENSIO_WFPS %>% 
  filter(TIMESTAMP >= start_date & TIMESTAMP <= end_date)

#need to aggregate by day for OWASIS
SENTEK_all <- SENTEK_all %>% 
  group_by(datetime = format(datetime, "%Y-%m-%d")) %>% 
  summarise(across(everything(), mean, na.rm = TRUE))

TENSIO_SWC <- TENSIO_SWC %>% 
  group_by(TIMESTAMP = format(TIMESTAMP, "%Y-%m-%d")) %>% 
  summarise(across(everything(), mean, na.rm = TRUE))

TENSIO_WFPS <- TENSIO_WFPS %>% 
  group_by(TIMESTAMP = format(TIMESTAMP, "%Y-%m-%d")) %>% 
  summarize(across(everything(), mean, na.rm = TRUE))

#Bind all SWC measurements note: BBB is AFPS
SWC_all <- bind_cols(SENTEK_all[1], SENTEK_all[,4:27], TENSIO_SWC[,2:11], OWASIS_BBB[,2:5])
WFPS_all <- bind_cols(SENTEK_all[1], SENTEK_all[,52:75], TENSIO_WFPS[,2:11], OWASIS_BBB[,2:5])

SWC_all$datetime <- as.POSIXct(SWC_all$datetime, format = "%Y-%m-%d")
WFPS_all$datetime <- as.POSIXct(WFPS_all$datetime, format = "%Y-%m-%d")

ggplot(SWC_all, aes(x = datetime)) +
  geom_line(aes(y = SWC_1_025), size = 0.5) +
  geom_line(aes(y = MS_TMAP_1_D_020*100), size = 0.5)

ggplot(WFPS_all, aes(x = datetime)) +
  geom_line(aes(y = SWC_3_025_WFPS, color = "SENTEK"), size = 0.5, alpha = 3) +
  geom_line(aes(y = MS_TMAP_7_D_020, color = "TENSIO"), size = 0.5, alpha = 3) +
  labs(
    title = "WFPS: SENTEK VS TENSIO",
    x = "datetime",
    y = "WFPS (%)"
  ) +
  scale_color_manual(
    values = c("SENTEK" = "red", "TENSIO" = "blue"),
    labels = c("SENTEK", "TENSIO"),
    name = "Measurement device"
    ) +
  theme(
    legend.title = element_text(size = 8),
    legend.text = element_text(size = 7),
    legend.key.width = unit(0.5, "cm")
  )
