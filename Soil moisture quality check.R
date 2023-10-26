library(tidyverse)
library(readr)

All_Soil_moisture <- read.csv2("Transformed/Langeweide_Sentek1.csv")
All_Soil_moisture$datetime <- as.POSIXct(All_Soil_moisture$datetime, format = "%Y-%m-%d %H:%M:%S")

P_Soil_moisture <- read.csv2("Transformed/Langeweide_P_SWC.csv")
P_Soil_moisture$datetime <- as.POSIXct(P_Soil_moisture$datetime, format = "%Y-%m-%d %H:%M:%S")

Normalized_Soil_moisture <- read.csv2("Transformed/Langeweide_Sentek_normalized.csv")
Normalized_Soil_moisture$datetime <- as.POSIXct(Normalized_Soil_moisture$datetime, format = "%Y-%m-%d %H:%M:%S")

#Dynamics of WFPS
ggplot(All_Soil_moisture) +
  geom_point(mapping = aes(x = datetime, y = SWC_1_005_WFPS, color = "5 cm"), size = 0.2) +
  geom_point(mapping = aes(x = datetime, y = SWC_1_015_WFPS, color = "15 cm"), size = 0.2) +
  geom_point(mapping = aes(x = datetime, y = SWC_1_025_WFPS, color = "25 cm"), size = 0.2) +
  geom_point(mapping = aes(x = datetime, y = SWC_1_055_WFPS, color = "55 cm"), size = 0.2) +
  geom_point(mapping = aes(x = datetime, y = SWC_1_085_WFPS, color = "85 cm"), size = 0.2) +
  geom_point(mapping = aes(x = datetime, y = SWC_1_105_WFPS, color = "105 cm"), size = 0.2) +
  geom_point(mapping = aes(x = datetime, y = SWC_1_115_WFPS, color = "115 cm"), size = 0.2) +
  
  labs(
    title = "soil moisture content",
    x = "date",
    y = "WFPS (%)",
  ) +
  scale_color_manual(
    values = c("5 cm" = "blue", "15 cm" = "yellow", "25 cm" = "red", "55 cm" = "green", "85 cm" = "orange", "105 cm" = "purple", "115 cm" = "cadetblue"),
    name = "Depth" )

#Similar plot but for SWC
ggplot(All_Soil_moisture) +
  geom_point(mapping = aes(x = datetime, y = SWC_1_005, color = "5 cm"), size = 0.2) +
  geom_point(mapping = aes(x = datetime, y = SWC_1_015, color = "15 cm"), size = 0.2) +
  geom_point(mapping = aes(x = datetime, y = SWC_1_025, color = "25 cm"), size = 0.2) +
  geom_point(mapping = aes(x = datetime, y = SWC_1_055, color = "55 cm"), size = 0.2) +
  geom_point(mapping = aes(x = datetime, y = SWC_1_085, color = "85 cm"), size = 0.2) +
  geom_point(mapping = aes(x = datetime, y = SWC_1_105, color = "105 cm"), size = 0.2) +
  geom_point(mapping = aes(x = datetime, y = SWC_1_115, color = "115 cm"), size = 0.2) +
  
  labs(
    title = "soil moisture content",
    x = "date",
    y = "Soil moisture (%)",
  ) +
  scale_color_manual(
    values = c("5 cm" = "blue", "15 cm" = "yellow", "25 cm" = "red", "55 cm" = "green", "85 cm" = "orange", "105 cm" = "purple", "115 cm" = "cadetblue"),
    name = "Depth" )


#checking the soil moisture patterns against WFPS
ggplot(All_Soil_moisture) +
  geom_point(mapping = aes(x = datetime, y = SWC_1_005, color = "Soil moisture"), size = 0.3) +
  geom_point(mapping = aes(x = datetime, y = SWC_1_005_WFPS*100, color = "WFPS"), size = 0.3) +
  labs(
    title = "Soil moisture vs WFPS at 005 cm",
    x = "date",
    y = "Soil moisture or WFPS in %"
  )

#correlation between SWC and WFPS
ggplot(All_Soil_moisture, aes(x = SWC_1_005, y = SWC_1_005_WFPS*100), size = 0.2) +
  geom_jitter() +
  geom_smooth(method = "loess", se = TRUE) +
  labs(
    title = "correlation between soil moisture and WFPS",
    x = "Soil moisture",
    y = "WFPS"
  )

#Checking normalized values
ggplot(Normalized_Soil_moisture) +
  geom_point(aes(x = datetime, y = SWC_1_005_SF_N, color = "Soil moisture"), size = 0.2) +
  geom_point(aes(x = datetime, y = SWC_1_005_WFPS, color = "WFPS"), size = 0.2) +
  labs(
    title = "Dynamics soil moisture normalized and WFPS",
    x = "Normalizeed soil moisture",
    y = "WFPS"
  )

ggplot(Normalized_Soil_moisture) +
  geom_point(aes(x = SWC_1_005_SF_N, y = SWC_1_005_WFPS), size = 0.2) +
  labs(
    title = "Correlation normalized SF and WFPS",
    x = "Normalized SF (%)",
    y = "WFPS (%)"
  )

ggplot(P_Soil_moisture) +
  geom_line(aes(x = datetime, y = SWC_1_005_WFPS)) +
  geom_point(aes(x = datetime, y = RAIN)) +
  labs(
    title = "SWC and P",
    x = "datetime",
    y = "SWC and P"
  )
