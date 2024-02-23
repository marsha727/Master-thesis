library(tidyverse)
library(readr)

All_Soil_moisture <- read.csv2("Transformed/Langeweide_Sentek.csv")
All_Soil_moisture$datetime <- as.POSIXct(All_Soil_moisture$datetime, format = "%Y-%m-%d %H:%M:%S")

P_Soil_moisture <- read.csv2("Transformed/Langeweide_P_SWC.csv")
P_Soil_moisture$datetime <- as.POSIXct(P_Soil_moisture$datetime, format = "%Y-%m-%d %H:%M:%S")

Normalized_Soil_moisture <- read.csv2("Transformed/Langeweide_Sentek_normalized.csv")
Normalized_Soil_moisture$datetime <- as.POSIXct(Normalized_Soil_moisture$datetime, format = "%Y-%m-%d %H:%M:%S")

#The following is just for the precipitation part.
#Grouping so i can calculate the sum of daily P
P_Soil_moisture_D <- P_Soil_moisture %>% 
  group_by(Year = format(datetime, "%Y"), Month = format(datetime, "%m"), Day = format(datetime, "%d")) %>% 
  summarize(Daily_P = sum(RAIN), Daily_SWC_1_005_WFPS = mean(SWC_1_005_WFPS)) 

#reattach the date  
P_Soil_moisture_D$Date <- as.Date(paste(P_Soil_moisture_D$Year, P_Soil_moisture_D$Month, P_Soil_moisture_D$Day, sep = "-"))

#Remove the 0 mm as i am interested in change in SWC to P
filtered_P_Soil_moisture_D <- P_Soil_moisture_D %>%
  filter(Daily_P != 0)

#THis calculated the change in SWC every day
filtered_P_Soil_moisture_D <- filtered_P_Soil_moisture_D %>%
  mutate(SoilMoistureChange = c(0, diff(Daily_SWC_1_005_WFPS)))

#make a subset of WFPS
WFPS_Subset <- All_Soil_moisture %>% 
  select(datetime, SWC_1_005_WFPS, SWC_1_015_WFPS, SWC_1_025_WFPS, SWC_1_035_WFPS, SWC_1_045_WFPS, SWC_1_055_WFPS,
         SWC_1_065_WFPS, SWC_1_075_WFPS, SWC_1_085_WFPS, SWC_1_095_WFPS, SWC_1_105_WFPS, SWC_1_115_WFPS,
         SWC_3_005_WFPS, SWC_3_015_WFPS, SWC_3_025_WFPS, SWC_3_035_WFPS, SWC_3_045_WFPS, SWC_3_055_WFPS,
         SWC_3_065_WFPS, SWC_3_075_WFPS, SWC_3_085_WFPS, SWC_3_095_WFPS, SWC_3_105_WFPS, SWC_3_115_WFPS,)

#############################################################
#Dynamics of WFPS
ggplot(All_Soil_moisture) +
  geom_line(mapping = aes(x = datetime, y = SWC_1_005_WFPS, color = "5 cm"), size = 0.2) +
  geom_line(mapping = aes(x = datetime, y = SWC_1_015_WFPS, color = "25 cm"), size = 0.2) +
  geom_line(mapping = aes(x = datetime, y = SWC_1_025_WFPS, color = "45 cm"), size = 0.2) +
  geom_line(mapping = aes(x = datetime, y = SWC_1_035_WFPS, color = "65 cm"), size = 0.2) +
  geom_line(mapping = aes(x = datetime, y = SWC_1_045_WFPS, color = "85 cm"), size = 0.2) +
  geom_line(mapping = aes(x = datetime, y = SWC_1_065_WFPS, color = "65 cm"), size = 0.2) +
  geom_line(mapping = aes(x = datetime, y = SWC_1_085_WFPS, color = "85 cm"), size = 0.2) +
  geom_line(mapping = aes(x = datetime, y = SWC_1_105_WFPS, color = "105 cm"), size = 0.2) +
  geom_line(mapping = aes(x = datetime, y = SWC_1_115_WFPS, color = "115 cm"), size = 0.2) +
  labs(
    title = "Water-filled pore space",
    x = "date",
    y = "WFPS (%)",
  ) +
  scale_color_manual(
    values = c("5 cm" = "blue", "15 cm" = "yellow", "25 cm" = "red", "35 cm" = "darkgreen", "45 cm" = "orange", "65 cm" = "purple", "85 cm" = "cadetblue", "105 cm" = "pink", "115 cm" = "lightgreen"),
    labels = c("5 cm", "15 cm", "25 cm", "35 cm", "45 cm", "65 cm", "85 cm", "105 cm", "115 cm"),
    name = "Depth" 
  ) +
  theme(
    plot.title = element_text(size = 16)
  )

#Similar plot but for SWC
ggplot(All_Soil_moisture) +
  geom_line(mapping = aes(x = datetime, y = SWC_1_005, color = "5 cm"), size = 0.2) +
  geom_line(mapping = aes(x = datetime, y = SWC_1_015, color = "25 cm"), size = 0.2) +
  geom_line(mapping = aes(x = datetime, y = SWC_1_025, color = "45 cm"), size = 0.2) +
  geom_line(mapping = aes(x = datetime, y = SWC_1_035, color = "65 cm"), size = 0.2) +
  geom_line(mapping = aes(x = datetime, y = SWC_1_045, color = "85 cm"), size = 0.2) +
  geom_line(mapping = aes(x = datetime, y = SWC_1_065, color = "65 cm"), size = 0.2) +
  geom_line(mapping = aes(x = datetime, y = SWC_1_085, color = "85 cm"), size = 0.2) +
  geom_line(mapping = aes(x = datetime, y = SWC_1_105, color = "105 cm"), size = 0.2) +
  geom_line(mapping = aes(x = datetime, y = SWC_1_115, color = "115 cm"), size = 0.2) +
  labs(
    title = "Soil moisture content",
    x = "date",
    y = "SWC (%)",
  ) +
  scale_color_manual(
    values = c("5 cm" = "blue", "15 cm" = "yellow", "25 cm" = "red", "35 cm" = "darkgreen", "45 cm" = "orange", "65 cm" = "purple", "85 cm" = "cadetblue", "105 cm" = "pink", "115 cm" = "lightgreen"),
    labels = c("5 cm", "15 cm", "25 cm", "35 cm", "45 cm", "65 cm", "85 cm", "105 cm", "115 cm"),
    name = "Depth" 
  ) +
  theme(
    plot.title = element_text(size = 16)
  )


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

#cor plot between normalized values showing linear relationship
ggplot(Normalized_Soil_moisture) +
  geom_point(aes(x = SWC_1_005_SF_N, y = SWC_1_005_WFPS), size = 0.2) +
  labs(
    title = "Correlation normalized SF and WFPS",
    x = "Normalized SF (%)",
    y = "WFPS (%)"
  )

#Checking how probe 1 and 3 compare
ggplot(All_Soil_moisture) +
  geom_point(aes(x = datetime, y = SWC_1_025_WFPS, color = "Probe 1"), size = 0.1) +
  geom_point(aes(x = datetime, y = SWC_3_025_WFPS, color = "Probe 3"), size = 0.1) +
  labs(
    title = "Probe 1 vs 3",
    x = "datetime",
    y = "WFPS (%)"
  ) +
  scale_color_manual(values = c("Probe 1" = "red", "Probe 3" = "blue"))



#Scatterplot generally linear but hysteresis
ggplot(All_Soil_moisture) +
  geom_point(aes(x = SWC_1_015_WFPS, y = SWC_3_015_WFPS), size = 0.1)+
  labs(
    title = "Cor probe 1 & probe 3",
    x = "Probe 1",
    y = "Probe 3",
  )


#Just some graph to check out P and WFPS
ggplot(P_Soil_moisture_D, aes(x = Date)) +
  geom_line(aes(y = Daily_SWC_1_005_WFPS)) +
  geom_col(aes(y = Daily_P, fill = "red", size = 1)) +
  scale_y_continuous(
    name = "WFPS (%)",
    limits = c(0, 1),
    sec.axis = sec_axis(~., name = "Precipitation (mm)", breaks = seq(0, 35, by = 5))
  ) +
  labs(
    title = "WFPS and P",
    x = "datetime",
  )

#Correlation plot WFPS and P
ggplot(P_Soil_moisture_D) +
  geom_jitter(aes(x = SoilMoistureChange, y = Daily_P)) +
  labs(
    title = "Scatterplot WFPS vs P",
    x = "WFPS (%)",
    y = "P (mm)"
  )
