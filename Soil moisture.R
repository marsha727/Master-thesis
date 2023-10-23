library(tidyverse)

Langeweide_data <- read_csv("Datasets/LAW_MS_ICOS.csv")

#extract the soil moisture data from the Langeweide data set seperately
#currently only access to 2/3 propes
Soil_moisture <- Langeweide_data %>% 
  select(datetime, Tair, SWC_1_005, SWC_1_015, SWC_1_025, SWC_1_035, SWC_1_045, SWC_1_055, SWC_1_065, SWC_1_075, SWC_1_085, SWC_1_095,
         SWC_1_105, SWC_1_115, SWC_3_005, SWC_3_015,SWC_3_015, SWC_3_025, SWC_3_035, SWC_3_045, SWC_3_055, SWC_3_065,
         SWC_3_075, SWC_3_085, SWC_3_095, SWC_3_105, SWC_3_115)

#defines the function for the SF correction
#include an ifelse for NA values
SF_value <- function(x){
  ifelse(is.na(x), NA, 0.232 * (x^0.410)-0.021)
}

#This applies the formula to all the column except the first one that is the datetime
#Must use ~ to say apply to all columns and (.) says to apply in the column value
SF_Soil_moisture <- Soil_moisture %>% 
  mutate(across(-1:-2, ~SF_value(.)))

#Next step would be to do Daniel's calculations of calibration curve

#SF corrected for temperature will be calculated first
#Make a new formula for SF corrected
SF_cor <- SF_Soil_moisture %>% 
  mutate(across(-1:-2, ~ifelse(is.na(.) | is.na(Tair), NA, . / (-0.00149 * (Tair - 4) + 1.014))))

#Now normalize the corrected SF values
SF_norm <- function(x){
  ifelse(is.na(x), NA, (x/max(x)))
}

SF_n <- SF_cor %>% 
  mutate(across(-1:-2, ~SF_norm(.)))

#Transform to WFPS
WFPS_cali <- function(x){
  ifelse(is.na(x), NA, 0.7033*x+0.2647)
}

WFPS_Sentek <- SF_n %>% 
  mutate(across(-1:-2, ~WFPS_cali(.)))


  