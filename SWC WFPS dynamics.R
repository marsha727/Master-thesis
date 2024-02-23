library(tidyverse)

SENTEK_all <- read.csv2("Transformed/Langeweide_Sentek.csv")
SENTEK_norm <- read.csv2("Transformed/Langeweide_normalized_SWC.csv")

TENSIO_SWC <- read.csv2("Transformed/Langeweide_Tensio.csv")
TENSIO_WFPS <- read.csv2("Transformed/Langeweide_Tensio_WFPS.csv")
TENSIO_SWC_norm <- read.csv2("Transformed/Langeweide_Tensio_norm.csv")
TENSIO_kPa_norm <- read.csv2("Transformed/Langeweide_Tensio_norm_kPa.csv")

OWASIS_BBB <- read.csv2("Transformed/Langeweide_OWASIS_BBB.csv")


#converting datatime to POSIXct
SENTEK_all$datetime <- as.POSIXct(SENTEK_all$datetime, format = "%Y-%m-%d %H:%M:%S")
SENTEK_norm$datetime <- as.POSIXct(SENTEK_norm$datetime, format = "%Y-%m-%d %H:%M:%S")

TENSIO_SWC$TIMESTAMP <- as.POSIXct(TENSIO_SWC$TIMESTAMP, format = "%Y-%m-%d %H:%M:%S")
TENSIO_WFPS$TIMESTAMP <- as.POSIXct(TENSIO_WFPS$TIMESTAMP, format = "%Y-%m-%d %H:%M:%S")
TENSIO_SWC_norm$TIMESTAMP <- as.POSIXct(TENSIO_SWC_norm$TIMESTAMP, format = "%Y-%m-%d %H:%M:%S")
TENSIO_kPa_norm$TIMESTAMP <- as.POSIXct(TENSIO_kPa_norm$TIMESTAMP, format = "%Y-%m-%d %H:%M:%S")

OWASIS_BBB$Date <- as.POSIXct(OWASIS_BBB$Date, format = "%Y-%m-%d")



#start and end date to make overlap correct
start_date <- "2022-04-02"
end_date <- "2022-11-01"

#filter for the dates
SENTEK_all <- SENTEK_all %>% 
  filter(datetime >= start_date & datetime <= end_date)
SENTEK_norm <- SENTEK_norm %>% 
  filter(datetime >= start_date & datetime <= end_date)

TENSIO_SWC <- TENSIO_SWC %>% 
  filter(TIMESTAMP >= start_date & TIMESTAMP <= end_date)
TENSIO_WFPS <- TENSIO_WFPS %>% 
  filter(TIMESTAMP >= start_date & TIMESTAMP <= end_date)
TENSIO_SWC_norm <- TENSIO_SWC_norm %>% 
  filter(TIMESTAMP >= start_date & TIMESTAMP <= end_date)
TENSIO_kPa_norm <- TENSIO_kPa_norm %>% 
  filter(TIMESTAMP >= start_date & TIMESTAMP <= end_date)

#need to aggregate by day for OWASIS
SENTEK_all <- SENTEK_all %>% 
  group_by(datetime = format(datetime, "%Y-%m-%d")) %>% 
  summarise(across(everything(), mean, na.rm = TRUE))
SENTEK_norm <- SENTEK_norm %>% 
  group_by(datetime = format(datetime, "%Y-%m-%d")) %>% 
  summarise(across(everything(), mean, na.rm = TRUE))

TENSIO_SWC <- TENSIO_SWC %>% 
  group_by(TIMESTAMP = format(TIMESTAMP, "%Y-%m-%d")) %>% 
  summarise(across(everything(), mean, na.rm = TRUE))
TENSIO_WFPS <- TENSIO_WFPS %>% 
  group_by(TIMESTAMP = format(TIMESTAMP, "%Y-%m-%d")) %>% 
  summarize(across(everything(), mean, na.rm = TRUE))
TENSIO_SWC_norm <- TENSIO_SWC_norm %>% 
  group_by(TIMESTAMP = format(TIMESTAMP, "%Y-%m-%d")) %>% 
  summarise(across(everything(), mean, na.rm = TRUE))
TENSIO_kPa_norm <- TENSIO_kPa_norm %>% 
  group_by(TIMESTAMP = format(TIMESTAMP, "%Y-%m-%d")) %>% 
  summarise(across(everything(), mean, na.rm = TRUE))

#Bind all SWC measurements note: BBB is AFPS
SWC_all <- bind_cols(SENTEK_all[1], SENTEK_all[,4:27], TENSIO_SWC[,2:11], OWASIS_BBB[,2:5])
WFPS_all <- bind_cols(SENTEK_all[1], SENTEK_all[,52:75], TENSIO_WFPS[,2:11], OWASIS_BBB[,2:11])
SWC_norm <- bind_cols(SENTEK_norm[1], SENTEK_norm[,3:26], TENSIO_SWC_norm[,2:10], OWASIS_BBB[,13:14])

SWC_all$datetime <- as.POSIXct(SWC_all$datetime, format = "%Y-%m-%d")
WFPS_all$datetime <- as.POSIXct(WFPS_all$datetime, format = "%Y-%m-%d")
SWC_norm$datetime <- as.POSIXct(SWC_norm$datetime, format = "%Y-%m-%d")

#PLOTS
#WFPS of all
ggplot(WFPS_all, aes(x = datetime)) +
  geom_line(aes(y = SWC_3_025_WFPS, color = "SENTEK"), size = 0.5, alpha = 3) +
  geom_line(aes(y = MS_TMAP_7_D_020, color = "TENSIO"), size = 0.5, alpha = 3) +
  geom_line(aes(y = MedianWFPS/100, color = "OWASIS (mm)"), size = 0.5, alpha = 3) +
  labs(
    title = "WFPS: SENTEK, TENSIO & OWASIS",
    x = "datetime",
    y = "WFPS (%)"
  ) +
  scale_color_manual(
    values = c("SENTEK" = "red", "TENSIO" = "blue", "OWASIS (mm)" = "darkgreen"),
    name = "Measurement device"
    ) +
  theme(
    legend.title = element_text(size = 8),
    legend.text = element_text(size = 7),
    legend.key.width = unit(0.5, "cm")
  )

#WFPS TENSIO AND SENTEK
ggplot(WFPS_all, aes(x = datetime)) +
  geom_line(aes(y = SWC_1_025_WFPS, color = "SENTEK1"), size = 0.5, alpha = 3) +
  geom_line(aes(y = SWC_3_025_WFPS, color = "SENTEK3"), size = 0.5, alpha = 3) +
  geom_line(aes(y = MS_TMAP_1_D_020, color = "TENSIO1"), size = 0.5, alpha = 3) +
  geom_line(aes(y = MS_TMAP_7_D_020, color = "TENSIO4"), size = 0.5, alpha = 3) +
  labs(
    title = "WFPS: SENTEK VS TENSIO",
    x = "datetime",
    y = "WFPS (%)"
  ) +
  scale_color_manual(
    values = c("SENTEK1" = "tomato", "SENTEK3" = "red", "TENSIO1" = "deepskyblue", "TENSIO4" = "blue"),
    labels = c("SENTEK1", "SENTEK3", "TENSIO1", "TENSIO4"),
    name = "Measurement device"
  ) +
  theme(
    legend.title = element_text(size = 8),
    legend.text = element_text(size = 7),
    legend.key.width = unit(0.5, "cm")
  )

#SWC of TENSIO AND SENTEK
ggplot(SWC_all, aes(x = datetime)) +
  geom_line(aes(y = SWC_1_025/100, color = "SENTEK1"), size = 0.5, alpha = 3) +
  geom_line(aes(y = SWC_3_025/100, color = "SENTEK3"), size = 0.5, alpha = 3) +
  geom_line(aes(y = MS_TMAP_1_D_020, color = "TENSIO1"), size = 0.5, alpha = 3) +
  geom_line(aes(y = MS_TMAP_7_D_020, color = "TENSIO7"), size = 0.5, alpha = 3) +
  labs(
    title = "SWC: SENTEK VS TENSIO",
    x = "datetime",
    y = "SWC (%)"
  ) +
  scale_color_manual(
    values = c("SENTEK1" = "tomato", "SENTEK3" = "red", "TENSIO1" = "deepskyblue", "TENSIO7" = "blue"),
    labels = c("SENTEK1", "SENTEK3", "TENSIO1", "TENSIO7"),
    name = "Measurement device"
  ) +
  theme(
    legend.title = element_text(size = 8),
    legend.text = element_text(size = 7),
    legend.key.width = unit(0.5, "cm")
  )

#SWC normalized of TENSIO AND SENTEK
ggplot(SWC_norm, aes(x = datetime)) +
  geom_line(aes(y = SWC_3_025, color = "SENTEK"), size = 0.5, alpha = 3) +
  geom_line(aes(y = MS_TMAP_7_D_020, color = "TENSIO"), size = 0.5, alpha = 3) +
  geom_line(aes(y = (-MedianBBB_norm), color = "OWASIS"), size = 0.5, alpha = 3) +
  labs(
    title = "SWC norm: SENTEK, TENSIO & OWASIS",
    x = "datetime",
    y = "SWC normalized (%)"
  ) +
  scale_color_manual(
    values = c("SENTEK" = "red", "TENSIO" = "blue", "OWASIS" = "darkgreen"),
    name = "Measurement device"
  ) +
  theme(
    legend.title = element_text(size = 8),
    legend.text = element_text(size = 7),
    legend.key.width = unit(0.5, "cm")
  )

ggplot(SWC_all, aes(x = SWC_1_015/100, y = MS_TMAP_1_D_020, color = "tomato")) +
  geom_point(size = 1.5) +
  geom_smooth(method = "nsl", se = FALSE, color = "red")
  labs(
    title = "correlations plot",
    x = "SENTEK",
    y = "TENSIO"
  )+
  theme(
    legend.title = element_text(size = 8),
    legend.text = element_text(size = 7),
    legend.key.width = unit(0.5, "cm")
  )
  
ggplot(SWC_norm, aes(x = SWC_1_025, y = MS_TMAP_1_D_020, color = "tomato")) +
    geom_point(size = 1.5) +
  labs(
    title = "correlations plot",
    x = "SENTEK",
    y = "TENSIO"
  )+
    theme(
      legend.title = element_text(size = 8),
      legend.text = element_text(size = 7),
      legend.key.width = unit(0.5, "cm")
    )

ggplot(SWC_norm, aes(x = SWC_3_025, y = MedianBBB_norm, color = "tomato")) +
  geom_point(size = 1.5) +
  labs(
    title = "correlations plot",
    x = "SENTEK",
    y = "OWASIS"
  )+
  theme(
    legend.title = element_text(size = 8),
    legend.text = element_text(size = 7),
    legend.key.width = unit(0.5, "cm")
  )

ggplot(SWC_norm, aes(x = MS_TMAP_7_D_020, y = MedianBBB_norm, color = "tomato")) +
  geom_point(size = 1.5) +
  labs(
    title = "correlations plot",
    x = "SENTEK",
    y = "OWASIS"
  )+
  theme(
    legend.title = element_text(size = 8),
    legend.text = element_text(size = 7),
    legend.key.width = unit(0.5, "cm")
  )

ggplot(WFPS_all, aes(x = SWC_3_025_WFPS, y = MS_TMAP_7_D_020, color = "tomato")) +
  geom_point(size = 1.5) +
  labs(
    title = "correlations plot",
    x = "SENTEK",
    y = "TENSIO"
  )+
  theme(
    legend.title = element_text(size = 8),
    legend.text = element_text(size = 7),
    legend.key.width = unit(0.5, "cm")
  )

 SWC_all_cor <- cor(SWC_all$SWC_3_025, SWC_all$MS_TMAP_7_D_020, method = "spearman", use = "complete.obs")
 SWC_all_cor <- cor(SWC_all$SWC_1_025, SWC_all$MS_TMAP_1_D_020, method = "spearman", use = "complete.obs")
 SWC_all_cor <- cor(SWC_all$MS_TMAP_7_D_020, SWC_all$MedianBBB, method = "spearman", use = "complete.obs")
 SWC_all_cor <- cor(WFPS_all$SWC_3_025_WFPS, WFPS_all$MS_TMAP_7_D_020, method = "spearman", use = "complete.obs")
 SWC_all_cor <- cor(SENTEK_all$SWC_3_025_SF, SWC_all$MS_TMAP_7_D_020, method = "spearman", use = "complete.obs") 
 