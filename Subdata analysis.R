library(tidyverse)
library(viridis)
library(astsa)

AFPS_NEE_WL_Tair <- readRDS("Datasets/Extracted/AFPS_NEE_WL_Tair.rds")
SENTEK <- read.csv2("Transformed/Langeweide_Sentek_AFPS.csv")
TENSIO <- readRDS("Transformed/Langeweide_tensio_interpolated.rds")
  
TENSIO <- TENSIO %>% 
  filter(depth == 20)

TENSIO_40 <- TENSIO %>% 
  filter(depth == 40)

TENSIO_60 <- TENSIO %>% 
  filter(depth == 60)

extracted <- AFPS_NEE_WL_Tair[[1]]$df

extracted <- extracted %>% 
  rename(SENTEK1 = y.LAW_MS_ICOS.SENTEK1.mean,
         SENTEK3 = y.LAW_MS_ICOS.SENTEK3.mean,
         TENSIO2 = y.LAW_MS_ICOS.TENSIO2.mean,
         TENSIO3 = y.LAW_MS_ICOS.TENSIO3.mean,
         OWASIS = y.LAW_MS_ICOS.OWASIS.mean,
         NEE_CO2_MDS_small = w.LAW_MS_ICOS.NEE_CO2_MDS_small.mean,
         GPP = v.LAW_MS_ICOS.GPP.mean,
         GWL = x.LAW_MS_ICOS.GWL_mean.mean,
         Tair = u.LAW_MS_ICOS.Tair_f.mean) %>% 
  rowwise() %>%
  mutate(
    AFPS_mean = mean(c_across(c(SENTEK1, SENTEK3, TENSIO2, TENSIO3, OWASIS)))
  )

ggplot(extracted) +
  geom_point(aes(x = SENTEK1, y = NEE_CO2_MDS_small, color = "SENTEK1")) +
  geom_point(aes(x = SENTEK3, y = NEE_CO2_MDS_small, color = "SENTEK3")) +
  geom_point(aes(x = TENSIO2, y = NEE_CO2_MDS_small, color = "TENSIO2")) +
  geom_point(aes(x = TENSIO3, y = NEE_CO2_MDS_small, color = "TENSIO3")) +
  geom_point(aes(x = OWASIS, y = NEE_CO2_MDS_small, color = "OWASIS")) +
  scale_shape_manual(values = c(16, 17, 25, 15, 22)) +
  scale_color_manual(
    values = c("SENTEK1" = "red", "SENTEK3" = "tomato", 
               "TENSIO2" = "darkblue", "TENSIO3" = "skyblue",
               "OWASIS" = "darkgreen")
  ) +
  labs(
    x = "AFPS [mm]",
    y = "NEE CO2 [kg day-1 ha-1]",
    color = " "
  ) +
  theme(
    panel.border = element_rect(color = "black", fill = NA, size = 1),
    panel.background = element_rect(fill = "white", color = "black")
  )


ggplot(extracted) +
  geom_point(aes(x = SENTEK1, y = NEE_CO2_MDS_small, shape = "SENTEK1", color = Tair)) +
  geom_point(aes(x = SENTEK3, y = NEE_CO2_MDS_small, shape = "SENTEK3", color = Tair)) +
  geom_point(aes(x = TENSIO2, y = NEE_CO2_MDS_small, shape = "TENSIO2", color = Tair)) +
  geom_point(aes(x = TENSIO3, y = NEE_CO2_MDS_small, shape = "TENSIO3", color = Tair)) +
  geom_point(aes(x = OWASIS, y = NEE_CO2_MDS_small, shape = "OWASIS", color = Tair)) +
  scale_shape_manual(values = c(16, 17, 25, 15, 22)) +
  scale_color_viridis(option = "turbo") +
  labs(
    x = "AFPS [mm]",
    y = "NEE CO2 [kg day-1 ha-1]"
  ) +
  theme(
    panel.border = element_rect(color = "black", fill = NA, size = 0.5),
    panel.background = element_rect(fill = "white", color = "black")
  )


ggplot(extracted) +
  geom_point(aes(x = SENTEK1, y = NEE_CO2_MDS_small, shape = "SENTEK1", color = GWL)) +
  geom_point(aes(x = SENTEK3, y = NEE_CO2_MDS_small, shape = "SENTEK3", color = GWL)) +
  geom_point(aes(x = TENSIO2, y = NEE_CO2_MDS_small, shape = "TENSIO2", color = GWL)) +
  geom_point(aes(x = TENSIO3, y = NEE_CO2_MDS_small, shape = "TENSIO3", color = GWL)) +
  #geom_point(aes(x = OWASIS, y = NEE_CO2_MDS_small, shape = "OWASIS", color = GWL)) +
  scale_shape_manual(values = c(17, 25, 15, 22)) +
  scale_color_viridis(option = "turbo", trans = "reverse") +
  labs(
    x = "AFPS [mm]",
    y = "NEE CO2 [kg day-1 ha-1]"
  ) +
  theme(
    panel.border = element_rect(color = "black", fill = NA, size = 0.5),
    panel.background = element_rect(fill = "white", color = "black")
  )

ggplot(extracted) +
  geom_point(aes(x = Tair, y = NEE_CO2_MDS_small, color = SENTEK1)) +
  scale_shape_manual(values = c(16, 17, 25, 15, 22)) +
  scale_color_viridis(option = "turbo") +
  labs(
    x = "Tair [Celcius]",
    y = "NEE CO2 [kg day-1 ha-1]",
    color = "AFPS [mm]"
  ) +
  theme(
    panel.border = element_rect(color = "black", fill = NA, size = 0.5),
    panel.background = element_rect(fill = "white", color = "black")
  )

ggplot(extracted) +
  geom_line(aes(x = datetime, y = TENSIO2, color = NEE_CO2_MDS_small)) +
  scale_shape_manual(values = c(16, 17, 25, 15, 22)) +
  scale_color_viridis(option = "turbo") +
  labs(
    x = "Tair [Celcius]",
    y = "NEE CO2 [kg day-1 ha-1]",
    color = "AFPS [mm]"
  ) +
  theme(
    panel.border = element_rect(color = "black", fill = NA, size = 0.5),
    panel.background = element_rect(fill = "white", color = "black")
  )

SENTEK$datetime <- as.POSIXct(SENTEK$datetime, format = "%Y-%m-%d %H:%M:%S")
TENSIO$datetime <- as.POSIXct(TENSIO$datetime, format = "%Y-%m-%d %H:%M:%S")
SENTEK_sub <- SENTEK %>%  
  group_by(datetime = as.Date(datetime)) %>% 
  summarise(SWC_1_005 = mean(SWC_1_005, na.rm = TRUE),
            SWC_1_015 = mean(SWC_1_015, na.rm = TRUE),
            SWC_1_025 = mean(SWC_1_025, na.rm = TRUE),
            SWC_3_005 = mean(SWC_3_005, na.rm = TRUE),
            SWC_3_015 = mean(SWC_1_015, na.rm = TRUE),
            SWC_3_025 = mean(SWC_3_025, na.rm = TRUE)
            )
TENSIO_sub <- TENSIO %>% 
  group_by(datetime = as.Date(datetime)) %>% 
  summarise(TENSIO2_020 = mean(AFPS2, na.rm = TRUE),
            TENSIO3_020 = mean(AFPS3, na.rm = TRUE)
            )
Tensio_sub_40 <- TENSIO_40 %>% 
  group_by(datetime = as.Date(datetime)) %>% 
  summarise(TENSIO2_040 = (mean(AFPS2, na.rm = TRUE)),
            TENSIO3_040 = (mean(AFPS3, na.rm = TRUE))
  )
Tensio_sub_60 <- TENSIO_60 %>% 
  group_by(datetime = as.Date(datetime)) %>% 
  summarise(TENSIO2_060 = (mean(AFPS2, na.rm = TRUE)),
            TENSIO3_060 = (mean(AFPS3, na.rm = TRUE))
  )

data_frames <- list(extracted, SENTEK_sub, TENSIO_sub, Tensio_sub_40, Tensio_sub_60)

#merge dataframes
extracted_TS <- Reduce(function(x, y) merge(x, y, by = "datetime", all.x = TRUE), data_frames)


ggplot(extracted_TS) +
  geom_point(aes(x = SWC_1_005, y = NEE_CO2_MDS_small, color = "tomato")) +
  geom_point(aes(x = TENSIO2, y = NEE_CO2_MDS_small, color = "skyblue")) +
  scale_shape_manual(values = c(16, 17, 25, 15, 22)) + 
  labs(
    x = "AFPS [mm]",
    y = "NEE CO2 [kg day-1 ha-1]",
    color = "AFPS [mm]"
  ) +
  theme(
    panel.border = element_rect(color = "black", fill = NA, size = 0.5),
    panel.background = element_rect(fill = "white", color = "black")
  )



extracted_sub <- extracted %>% 
  filter(#between(datetime, as.Date("2022-05-12"), as.Date("2022-05-20")) |
          #between(datetime, as.Date("2022-07-03"), as.Date("2022-07-22")) |
           between(datetime, as.Date("2022-08-01"), as.Date("2022-09-07")))

ccf(extracted$TENSIO2, extracted$NEE_CO2_MDS_small, lag_max = 5, na.action = na.pass)

lag2.plot(na.omit(extracted$TENSIO2), na.omit(extracted$NEE_CO2_MDS_small), max.lag = 12)


###########################################################################
#STATISTICS

extracted <- extracted[-1,] #to remove NA row

#Top layer correlations
correlation_data <- extracted_TS %>%
  summarise(correlation = cor(SWC_1_005, NEE_CO2_MDS_small, method = "spearman", use = "complete.obs"))

# Create a scatter plot with a single correlation coefficient for each cycle
correlation_plot <- ggplot(extracted_TS, aes(x = SWC_1_005, y = NEE_CO2_MDS_small)) +
  stat_smooth(method = "glm", col = "red") +
  geom_point() +
  labs(
    x = "AFPS [mm] (Depth = 5 cm) ",
    y = "NEE CO2 [kg day-1 ha-1]"
  )
  geom_text(data = correlation_data, aes(x = Inf, y = -Inf, label = sprintf("R = %.2f", correlation)), vjust = 1, hjust = 1, size = 3)

library(ggpmisc)

#stat correlation for plotting and computing stats
correlation_plot + stat_correlation(mapping = use_label(c("R", "P")), 
                                    method = "spearman",
                                    size = 4, 
                                    label.x = "right", 
                                    label.y = "bottom"
)

#DEPTH 15cm

correlation_data <- extracted_TS %>%
  summarise(correlation = cor(SWC_1_015, NEE_CO2_MDS_small, method = "spearman", use = "complete.obs"))

# Create a scatter plot with a single correlation coefficient for each cycle
correlation_plot <- ggplot(extracted_TS, aes(x = SWC_1_015, y = NEE_CO2_MDS_small)) +
  stat_smooth(method = "lm", col = "red", span = 0.75) +
  geom_point() +
  labs(
    x = "AFPS [mm] (Depth = 15 cm) ",
    y = "NEE CO2 [kg day-1 ha-1]"
  )
  geom_text(data = correlation_data, aes(x = Inf, y = -Inf, label = sprintf("R = %.2f", correlation)), vjust = 1, hjust = 1, size = 3)

library(ggpmisc)

#stat correlation for plotting and computing stats
correlation_plot + stat_correlation(mapping = use_label(c("R", "P")),
                                    method = "pearson",
                                    size = 4, 
                                    label.x = "right", 
                                    label.y = "bottom"
)

#DEPTH 15cm

correlation_data <- extracted_TS %>%
  summarise(correlation = cor(SWC_3_025, NEE_CO2_MDS_small, method = "spearman", use = "complete.obs"))

# Create a scatter plot with a single correlation coefficient for each cycle
correlation_plot <- ggplot(extracted_TS, aes(x = SWC_1_025, y = NEE_CO2_MDS_small)) +
  stat_smooth(method = "gam", col = "red", span = 0.75) +
  geom_point() +
  labs(
    x = "AFPS [mm] (Depth = 15 cm) ",
    y = "NEE CO2 [kg day-1 ha-1]"
  )
geom_text(data = correlation_data, aes(x = Inf, y = -Inf, label = sprintf("R = %.2f", correlation)), vjust = 1, hjust = 1, size = 3)

library(ggpmisc)

#stat correlation for plotting and computing stats
correlation_plot + stat_correlation(mapping = use_label(c("R", "P")),
                                    method = "spearman",
                                    size = 4, 
                                    label.x = "right", 
                                    label.y = "bottom"
)

#Correlation for the TENSIO
correlation_data <- extracted_TS %>%
  summarise(correlation = cor(TENSIO2_020, NEE_CO2_MDS_small, method = "spearman", use = "complete.obs"))

# Create a scatter plot with a single correlation coefficient for each cycle
correlation_plot <- ggplot(extracted_TS, aes(x = TENSIO2_020, y = NEE_CO2_MDS_small)) +
  stat_smooth(method = "gam", col = "red", span = 0.75) +
  geom_point() +
  labs(
    x = "AFPS [mm] (Depth = 15 cm) ",
    y = "NEE CO2 [kg day-1 ha-1]"
  )
geom_text(data = correlation_data, aes(x = Inf, y = -Inf, label = sprintf("R = %.2f", correlation)), vjust = 1, hjust = 1, size = 3)

library(ggpmisc)

#stat correlation for plotting and computing stats
correlation_plot + stat_correlation(mapping = use_label(c("R", "P")),
                                    method = "spearman",
                                    size = 4, 
                                    label.x = "right", 
                                    label.y = "bottom"
)

#Correlation for the TENSIO 40 CM
correlation_data <- extracted_TS %>%
  summarise(correlation = cor(TENSIO2_040, NEE_CO2_MDS_small, method = "spearman", use = "complete.obs"))

# Create a scatter plot with a single correlation coefficient for each cycle
correlation_plot <- ggplot(extracted_TS, aes(x = TENSIO3_060, y = NEE_CO2_MDS_small)) +
  stat_smooth(method = "gam", col = "red", span = 0.75) +
  geom_point() +
  labs(
    x = "AFPS [mm] (Depth = 15 cm) ",
    y = "NEE CO2 [kg day-1 ha-1]"
  )
geom_text(data = correlation_data, aes(x = Inf, y = -Inf, label = sprintf("R = %.2f", correlation)), vjust = 1, hjust = 1, size = 3)

library(ggpmisc)

#stat correlation for plotting and computing stats
correlation_plot + stat_correlation(mapping = use_label(c("R", "P")),
                                    method = "spearman",
                                    size = 4, 
                                    label.x = "right", 
                                    label.y = "bottom"
)



####################################################################################

#Correlation for integrated AFPS
extracted <- extracted[-1,]

correlation_data <- extracted %>%
  summarise(correlation = cor(SENTEK1, NEE_CO2_MDS_small, method = "spearman", use = "complete.obs"))

# Create a scatter plot with a single correlation coefficient for each cycle
correlation_plot <- ggplot(extracted, aes(x = SENTEK1, y = NEE_CO2_MDS_small)) +
  stat_smooth(method = "gam", col = "red", span = 5) +
  geom_point() +
  labs(
  x = "AFPS [mm]",
  y = "NEE CO2 [kg day-1 ha-1]"
    )
  geom_text(data = correlation_data, aes(x = Inf, y = -Inf, label = sprintf("R = %.2f", correlation)), vjust = 1, hjust = 1, size = 3)

library(ggpmisc)

#stat correlation for plotting and computing stats
correlation_plot + stat_correlation(mapping = use_label(c("R", "P")),
                                    method = "spearman",
                                    size = 4, 
                                    label.x = "right", 
                                    label.y = "bottom"
)

# Create a scatter plot with a single correlation coefficient for each cycle
correlation_plot <- ggplot(extracted, aes(x = SENTEK3, y = NEE_CO2_MDS_small)) +
  stat_smooth(method = "gam", col = "red", span = 5) +
  geom_point() +
  labs(
    x = "AFPS [mm]",
    y = "NEE CO2 [kg day-1 ha-1]"
  )
  geom_text(data = correlation_data, aes(x = Inf, y = -Inf, label = sprintf("R = %.2f", correlation)), vjust = 1, hjust = 1, size = 3)

library(ggpmisc)

#stat correlation for plotting and computing stats
correlation_plot + stat_correlation(mapping = use_label(c("R", "P")),
                                    method = "spearman",
                                    size = 4, 
                                    label.x = "right", 
                                    label.y = "bottom"
)

# Create a scatter plot with a single correlation coefficient for each cycle
correlation_plot <- ggplot(extracted, aes(x = TENSIO2, y = NEE_CO2_MDS_small)) +
  stat_smooth(method = "gam", col = "red", span = 5) +
  geom_point() +
  labs(
    x = "AFPS [mm]",
    y = "NEE CO2 [kg day-1 ha-1]"
  )
  geom_text(data = correlation_data, aes(x = Inf, y = -Inf, label = sprintf("R = %.2f", correlation)), vjust = 1, hjust = 1, size = 3)

library(ggpmisc)

#stat correlation for plotting and computing stats
correlation_plot + stat_correlation(mapping = use_label(c("R", "P")),
                                    method = "spearman",
                                    size = 4, 
                                    label.x = "right", 
                                    label.y = "bottom"
)

# Create a scatter plot with a single correlation coefficient for each cycle
correlation_plot <- ggplot(extracted, aes(x = TENSIO3, y = NEE_CO2_MDS_small)) +
  stat_smooth(method = "gam", col = "red", span = 5) +
  geom_point() +
  labs(
    x = "AFPS [mm]",
    y = "NEE CO2 [kg day-1 ha-1]"
  )
geom_text(data = correlation_data, aes(x = Inf, y = -Inf, label = sprintf("R = %.2f", correlation)), vjust = 1, hjust = 1, size = 3)

library(ggpmisc)

#stat correlation for plotting and computing stats
correlation_plot + stat_correlation(mapping = use_label(c("R", "P")),
                                    method = "spearman",
                                    size = 4, 
                                    label.x = "right", 
                                    label.y = "bottom"
)

# Create a scatter plot with a single correlation coefficient for each cycle
correlation_plot <- ggplot(extracted, aes(x = OWASIS, y = NEE_CO2_MDS_small)) +
  stat_smooth(method = "gam", col = "red", span = 5) +
  geom_point() +
  labs(
    x = "AFPS [mm]",
    y = "NEE CO2 [kg day-1 ha-1]"
  )
geom_text(data = correlation_data, aes(x = Inf, y = -Inf, label = sprintf("R = %.2f", correlation)), vjust = 1, hjust = 1, size = 3)

library(ggpmisc)

#stat correlation for plotting and computing stats
correlation_plot + stat_correlation(mapping = use_label(c("R", "P")),
                                    method = "spearman",
                                    size = 4, 
                                    label.x = "right", 
                                    label.y = "bottom"
)
