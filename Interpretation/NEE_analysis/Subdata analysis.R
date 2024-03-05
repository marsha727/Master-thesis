library(tidyverse)
library(viridis)
library(astsa)

AFPS_NEE_WL_Tair <- readRDS("Datasets/Extracted/AFPS_NEE_WL_Tair.rds")
Stats <- readRDS("Langeweide/Statistics_file.rds")
SENTEK <- read.csv2("Transformed/Langeweide_Sentek_AFPS.csv")
TENSIO <- readRDS("Transformed/Langeweide_tensio_interpolated.rds")

#I use the extracted file from Jan Bierman's App output
#The reason is because the App aggregates the night-time NEE
#All other data is the same as my altered LAW_ICOS_MS (which is source file)
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


#Relation between AFPS and night-time NEE
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

#Relation between AFPS and night-time NEE with Tair
ggplot(extracted) +
  geom_point(aes(x = SENTEK1, y = NEE_CO2_MDS_small, shape = "SENTEK1", color = Tair)) +
  geom_point(aes(x = SENTEK3, y = NEE_CO2_MDS_small, shape = "SENTEK3", color = Tair)) +
  geom_point(aes(x = TENSIO2, y = NEE_CO2_MDS_small, shape = "TENSIO2", color = Tair)) +
  geom_point(aes(x = TENSIO3, y = NEE_CO2_MDS_small, shape = "TENSIO3", color = Tair)) +
  #geom_point(aes(x = OWASIS, y = NEE_CO2_MDS_small, shape = "OWASIS", color = Tair)) +
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

#Relation AFPS and night-time NEE and GWL
ggplot(extracted) +
  #geom_point(aes(x = SENTEK1, y = NEE_CO2_MDS_small, shape = "SENTEK1", color = GWL)) +
  #geom_point(aes(x = SENTEK3, y = NEE_CO2_MDS_small, shape = "SENTEK3", color = GWL)) +
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

#Night-time NEE and GWL with AFPS color
ggplot(extracted) +
  geom_point(aes(x = GWL, y = NEE_CO2_MDS_small, color = TENSIO3)) +
  #geom_point(aes(x = OWASIS, y = NEE_CO2_MDS_small, shape = "OWASIS", color = GWL)) +
  scale_color_viridis(option = "turbo", trans = "reverse") +
  labs(
    x = "GWL [cm]",
    y = "NEE CO2 [kg day-1 ha-1]"
  ) +
  theme(
    panel.border = element_rect(color = "black", fill = NA, size = 0.5),
    panel.background = element_rect(fill = "white", color = "black")
  )

# Define the custom color gradient
custom_colors <- c("black", "red", "yellow", "green")

#AFPS and night-time NEE with GPP
ggplot(Stats) +
  #geom_point(aes(x = SENTEK1, y = NEE_CO2_MDS_small, shape = "SENTEK1", color = GPP)) +
  #geom_point(aes(x = SENTEK3, y = NEE_CO2_MDS_small, shape = "SENTEK3", color = GPP)) +
  geom_point(aes(x = TENSIO2, y = NEE_CO2_MDS_small, shape = "TENSIO2", color = GPP)) +
  geom_point(aes(x = TENSIO3, y = NEE_CO2_MDS_small, shape = "TENSIO3", color = GPP)) +
  scale_shape_manual(values = c(17, 25, 15, 22)) +
  scale_color_gradientn(colors = custom_colors) +
  labs(
    x = "AFPS [mm]",
    y = "NEE CO2 [kg day-1 ha-1]"
  ) +
  theme(
    panel.border = element_rect(color = "black", fill = NA, size = 0.5),
    panel.background = element_rect(fill = "white", color = "black")
  )

#Tair and NEE with AFPS (mean)
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

#Extra plots not used
ggplot(Stats) +
  geom_point(aes(x = SENTEK1, y = GPP, shape = "SENTEK1", color = "tomato")) +
  geom_point(aes(x = SENTEK3, y = GPP, shape = "SENTEK3", color = "skyblue")) +
  #geom_point(aes(x = TENSIO2, y = NEE_CO2_MDS_small, shape = "TENSIO2", color = GPP)) +
  #geom_point(aes(x = TENSIO3, y = NEE_CO2_MDS_small, shape = "TENSIO3", color = GPP)) +
  #geom_point(aes(x = OWASIS, y = NEE_CO2_MDS_small, shape = "OWASIS", color = GWL)) +
  scale_shape_manual(values = c(17, 25, 15, 22)) +
  labs(
    x = "AFPS [mm]",
    y = "GPP [kg day-1 ha-1]"
  ) +
  theme(
    panel.border = element_rect(color = "black", fill = NA, size = 0.5),
    panel.background = element_rect(fill = "white", color = "black")
  )

###########################################################################
#STATISTICS
#Correlation for integrated AFPS
extracted <- extracted[-1,]

#Correlation matrix
Correlation_values <- Stats %>% 
  select(c(SENTEK1, SENTEK3, TENSIO2, TENSIO3, GWL, NEE_CO2_MDS_small,
           Tair, GPP))

cor <- cor(Correlation_values, use = "complete.obs", method = "spearman")


# Create correlation plot for AFPS against NEE
#The following plots are all correlation plots with R2 and P value
#For different methods (SENTEK + TENSIO etc.)

correlation_data <- extracted %>%
  summarise(correlation = cor(SENTEK1, NEE_CO2_MDS_small, method = "spearman", use = "complete.obs"))


correlation_plot <- ggplot(extracted, aes(x = SENTEK1, y = NEE_CO2_MDS_small)) +
  #stat_smooth(method = "gam", col = "red", span = 5) +
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

# Create a scatter plot with a single correlation coefficient for each cycle
correlation_plot <- ggplot(extracted, aes(x = GWL, y = NEE_CO2_MDS_small)) +
  stat_smooth(method = "loess", col = "red", span = 5) +
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
