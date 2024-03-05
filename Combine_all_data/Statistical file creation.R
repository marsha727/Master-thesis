#In this code i combined two datasets extracted from 
#Jan Bierman's app output, in which i did night-time NEE aggregation

AFPS_NEE_WL_Tair <- readRDS("Datasets/Extracted/AFPS_NEE_WL_Tair.rds")
RAIN_EF_VPD_WIND <- readRDS("Datasets/Extracted/RAIN_ET_EF_VPD_WIND.rds")
LAW_MS_ICOS <- readRDS("Langeweide/LAW_MS_ICOS.rds")

#I add ET separately due to aggregation error with ET in the app
#App only allowed mean, but not sum of ET
ET <- LAW_MS_ICOS %>% 
  select(c(datetime, ET)) %>% 
  group_by(datetime = format(datetime, "%Y-%m-%d")) %>% 
  summarise(ET = ifelse(sum(!is.na(ET)) >= 36, sum(ET, na.rm = TRUE), NA)
  )

extracted <- AFPS_NEE_WL_Tair[[1]]$df
extracted_part2 <- RAIN_EF_VPD_WIND[[1]]$df  #this one has rain etc.

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
  ) %>% 
  select(-c(week, month, season.meteo, season.year))

extracted_part2 <- extracted_part2 %>% 
  rename(RAIN = x.LAW_MS_ICOS.RAIN.mean,
         Tsoil_1_015 = y.LAW_MS_ICOS.Tsoil_1_015.mean,
         EF = w.LAW_MS_ICOS.EF3.mean,
         VPD = v.LAW_MS_ICOS.VPD.mean,
         WIND = u.LAW_MS_ICOS.WIND.mean
  ) %>% 
  select(-c(week, month, season.meteo, season.year, ))

data_frames_dayav <- list(extracted, extracted_part2, ET)

#merge dataframes
extracted <- Reduce(function(x, y) merge(x, y, by = "datetime", all.x = TRUE), data_frames_dayav)

write_rds(extracted, file = "Langeweide/Statistics_file.rds")
