#hello
library(tidyverse)

ET <- readRDS("App/Langeweide_ET_noWindCor.rds")
AFPS <- readRDS("App/AFPS_int_TS.rds")
Langeweide_data <- readRDS("Datasets/LAW_MS_ICOS.RDS")
WL <- readRDS("Transformed/Langeweide_groundwater.rds")

#daily aggregation

start_date <- min(AFPS$datetime)
end_date <- max(AFPS$datetime)

ET <- ET %>% 
  filter(datetime >= start_date & datetime <= end_date) %>% 
  rename(datetime2 = datetime)

WL <- WL %>% 
  group_by(datetime = format(datetime, "%Y-%m-%d")) %>% 
  summarise(WL = mean(GWL_mean, na.rm = TRUE)) %>% 
  filter(datetime >= start_date & datetime <= end_date) %>% 
  rename(datetime3 = datetime)

Tsoil <- Langeweide_data %>% 
  select(datetime, Tsoil_1_005, Tsoil_1_015, Tsoil_1_025, Tsoil_1_035,
         Tsoil_1_045, Tsoil_1_055, Tsoil_1_065, Tsoil_1_075, Tsoil_1_085,
         Tsoil_1_095, Tsoil_1_105, Tsoil_3_005, Tsoil_3_015, Tsoil_3_025, Tsoil_3_035,
         Tsoil_3_045, Tsoil_3_055, Tsoil_3_065, Tsoil_3_075, Tsoil_3_085,
         Tsoil_3_095, Tsoil_3_105) %>% 
  group_by(datetime = format(datetime, "%Y-%m-%d")) %>% 
  summarise(across(everything(), ~mean(., na.rm = TRUE))) %>% 
  filter(datetime >= start_date & datetime <= end_date) %>% 
  rename(datetime4 = datetime)

NEE <- Langeweide_data %>% 
  select(datetime, NEE_CO2_MDS2, NEE_CO2_MDS, NEE_CO2) %>% 
  group_by(datetime = format(datetime, "%Y-%m-%d")) %>% 
  summarise(NEE_CO2_MDS2 = sum(NEE_CO2_MDS2, na.rm = TRUE),
            NEE_CO2_MDS = sum(NEE_CO2_MDS, na.rm = TRUE),
            NEE_CO2 = sum(NEE_CO2, na.rm = TRUE)) %>% 
  filter(datetime >= start_date & datetime <= end_date) %>% 
  rename(datetime5 = datetime)

  

Langeweide <- cbind(AFPS, WL, ET, Tsoil, NEE) 

Langeweide <- Langeweide %>% 
  select(-c(EF1, EF2, datetime2, datetime3, T, datetime4, datetime5))

write_rds(Langeweide, "App/Langeweide_full.rds")

test <- readRDS("App/Langeweide_full.rds")

ggplot(Langeweide) +
  geom_point(aes(x = Tair, y = NEE_CO2_MDS2, color = WL)) +
  scale_color_viridis(option = "turbo", trans = "reverse")

ggplot(Langeweide) +
  geom_point(aes(x = SENTEK1, y = NEE_CO2_MDS2, shape = "SENTEK1", color = WL)) +
  geom_point(aes(x = SENTEK3, y = NEE_CO2_MDS2, shape = "SENTEK3", color = WL)) +
  geom_point(aes(x = TENSIO2, y = NEE_CO2_MDS2, shape = "TENSIO2", color = WL)) +
  geom_point(aes(x = TENSIO3, y = NEE_CO2_MDS2, shape = "TENSIO3", color = WL)) +
  geom_point(aes(x = OWASIS, y = NEE_CO2_MDS2, shape = "OWASIS", color = WL)) +
  scale_shape_manual(values = c(16, 17, 25, 15, 22)) +
  scale_color_viridis(option = "turbo", trans = "reverse") +
  labs(
    x = "AFPS [mm]",
    y = "NEE CO2 [kg day-1 ha-1]"
  ) +
  theme(
    panel.border = element_rect(color = "black", fill = NA, size = 1),
    panel.background = element_rect(fill = "white", color = "black")
  )

ggplot(Langeweide) +
  geom_point(aes(x = SENTEK1, y = NEE_CO2_MDS2, color = "SENTEK1")) +
  geom_point(aes(x = SENTEK3, y = NEE_CO2_MDS2, color = "SENTEK3")) +
  geom_point(aes(x = TENSIO2, y = NEE_CO2_MDS2, color = "TENSIO2")) +
  geom_point(aes(x = TENSIO3, y = NEE_CO2_MDS2, color = "TENSIO3")) +
  geom_point(aes(x = OWASIS, y = NEE_CO2_MDS2, color = "OWASIS")) +
  scale_shape_manual(values = c(16, 17, 25, 15, 22)) +
  scale_color_manual(
    values = c("SENTEK1" = "darkred", "SENTEK3" = "tomato", 
               "TENSIO2" = "darkblue", "TENSIO3" = "skyblue",
               "OWASIS" = "darkgreen")
  ) +
  labs(
    x = "AFPS [mm]",
    y = "NEE CO2 [kg day-1 ha-1]",
    color = "Legend"
  ) +
  theme(
    panel.border = element_rect(color = "black", fill = NA, size = 1),
    panel.background = element_rect(fill = "white", color = "black")
  )


ggplot(Langeweide) +
  geom_point(aes(x = SENTEK1, y = NEE_CO2_MDS2, shape = "SENTEK1", color = Tair)) +
  geom_point(aes(x = SENTEK3, y = NEE_CO2_MDS2, shape = "SENTEK3", color = Tair)) +
  geom_point(aes(x = TENSIO2, y = NEE_CO2_MDS2, shape = "TENSIO2", color = Tair)) +
  geom_point(aes(x = TENSIO3, y = NEE_CO2_MDS2, shape = "TENSIO3", color = Tair)) +
  geom_point(aes(x = OWASIS, y = NEE_CO2_MDS2, shape = "OWASIS", color = Tair)) +
  scale_shape_manual(values = c(16, 17, 25, 15, 22)) +
  scale_color_viridis(option = "turbo", trans = "reverse") +
  labs(
    x = "AFPS [mm]",
    y = "NEE CO2 [kg day-1 ha-1]"
  ) +
  theme(
    panel.border = element_rect(color = "black", fill = NA, size = 1),
    panel.background = element_rect(fill = "white", color = "black")
  )

ggplot(Langeweide) +
  geom_point(aes(x = SENTEK1, y = NEE_CO2_MDS2, shape = "SENTEK1", color = EF3)) +
  geom_point(aes(x = SENTEK3, y = NEE_CO2_MDS2, shape = "SENTEK3", color = EF3)) +
  geom_point(aes(x = TENSIO2, y = NEE_CO2_MDS2, shape = "TENSIO2", color = EF3)) +
  geom_point(aes(x = TENSIO3, y = NEE_CO2_MDS2, shape = "TENSIO3", color = EF3)) +
  geom_point(aes(x = OWASIS, y = NEE_CO2_MDS2, shape = "OWASIS", color = EF3)) +
  scale_shape_manual(values = c(16, 17, 25, 15, 22)) +
  scale_color_viridis(option = "turbo", trans = "reverse") +
  labs(
    x = "AFPS [mm]",
    y = "NEE CO2 [kg day-1 ha-1]"
  ) +
  theme(
    panel.border = element_rect(color = "black", fill = NA, size = 1),
    panel.background = element_rect(fill = "white", color = "black")
  )

