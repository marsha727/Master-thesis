library(tidyverse)

AFPS_SENTEK <- read.csv2("Transformed/Langeweide_Sentek_AFPS.csv")
AFPS_TENSIO <- read.csv2("Transformed/Langeweide_Tensio_AFPS_mm.csv")
OWASIS_BBB <- read.csv2("Transformed/Langeweide_OWASIS_BBB.csv")

AFPS_SENTEK$datetime <- as.POSIXct(AFPS_SENTEK$datetime, format = "%Y-%m-%d %H:%M:%S")
AFPS_TENSIO$TIMESTAMP <- as.POSIXct(AFPS_TENSIO$TIMESTAMP, format = "%Y-%m-%d %H:%M:%S")
OWASIS_BBB$Date <- as.POSIXct(OWASIS_BBB$Date, format = "%Y-%m-%d %H:%M:%S")

#Fist check max depth of OWASIS_BBB in cm
Max_GW_OWASIS <- abs(min(OWASIS_BBB$MedianGW_mmv)*100)

#integrate AFPS over profile depth

#Integrate SENTEK
AFPS_int_SENTEK <- AFPS_SENTEK %>% 
  mutate(Probe1 = SWC_1_005 + SWC_1_015 + SWC_1_025 + SWC_1_035 + SWC_1_045 + SWC_1_055 + SWC_1_065 + SWC_1_075 + SWC_1_085 + SWC_1_095 +
           SWC_1_105 + SWC_1_115) %>% 
  mutate(Probe3= SWC_3_005 + SWC_3_015 + SWC_3_025 + SWC_3_035 + SWC_3_045 + SWC_3_055 + SWC_3_065 + SWC_3_075 + SWC_3_085 + SWC_3_095 +
           SWC_3_105 + SWC_3_115)

AFPS_int_TENSIO <- AFPS_TENSIO %>% 
  mutate(Probe)
