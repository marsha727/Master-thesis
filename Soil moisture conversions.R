library(tidyverse)

#reading RDS file (until november data)
Langeweide_data <- readRDS("Datasets/LAW_MS_ICOS.rds")

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
#first get the max values for each collumn using sapply
Max_values <- sapply(SF_cor, function(x) max(x, na.rm = TRUE))
SF_norm <- function(x, max_value){
  ifelse(is.na(x), NA, (x/ max_value))
}

#To prevent issues with length of the columns i remove the first two rows already
cols_to_normalize <- setdiff(names(SF_cor), c("datetime", "Tair"))

#For the loop to work i already make a new dataframe for SF normalized
SF_n <- SF_cor

#this loop apply the formula to each column that i specified earlier
#The max value is taken for each column and specified here what value it should take in the formula
#I added a check point to make sure the max values are differing
for(col in cols_to_normalize){
  max_value <- Max_values[col] 
  print(max_value)
  SF_n[[col]] <- SF_norm(SF_cor[[col]], max_value)
}

#Transform to WFPS
WFPS_cali <- function(x){
  ifelse(is.na(x), NA, 0.7033*x+0.2647)
}

WFPS_Sentek <- SF_n %>% 
  mutate(across(-1:-2, ~WFPS_cali(.)))


#Normalizing the soil moisture
Max_SM <- sapply(Soil_moisture, function(x) max(x, na.rm = TRUE))
SM_norm <- function(x, max_SM){
  ifelse(is.na(x), NA, (x/ max_SM))
}

cols_to_normalize <- setdiff(names(SF_cor), c("datetime", "Tair"))

SM_n <- Soil_moisture

for(col in cols_to_normalize){
  max_SM <- Max_SM[col] 
  print(max_value)
  SM_n[[col]] <- SM_norm(SM_n[[col]], max_SM)
}

#Make a new dataframe that contains SWC, SF and WFPS
new_column_names_WFPS <- paste(names(WFPS_Sentek), "WFPS", sep = "_") #Give new colnames to differentiate
colnames(WFPS_Sentek) <- new_column_names_WFPS
new_column_names_SF <- paste(names(SF_Soil_moisture), "SF", sep = "_")
colnames(SF_Soil_moisture) <- new_column_names_SF

Sentek <- bind_cols(Soil_moisture, SF_Soil_moisture, WFPS_Sentek)

Sentek <- Sentek %>% #remove double columns
  select(-datetime_WFPS, -datetime_SF, -Tair_WFPS, -Tair_SF)

#A second version with also the normalized values
new_column_names_SMn <- paste(names(SM_n), "N", sep = "_")
colnames(SM_n) <- new_column_names_SMn
new_column_names_SFn <- paste(names(SF_n), "SF_N", sep = "_")
colnames(SF_n) <- new_column_names_SFn

Sentek_Norm <- bind_cols(Sentek, SM_n, SF_n)

Sentek_Norm <- Sentek_Norm %>% #remove double columns
  select(-datetime_N, -datetime_SF_N, -Tair_N, -Tair_SF_N)

#Ensures the datetime is in correct formating for writing csv
Sentek$datetime <- format(Sentek$datetime, format = "%Y-%m-%d %H:%M:%S")
Sentek_Norm$datetime <- format(Sentek_Norm$datetime, format = "%Y-%m-%d %H:%M:%S")

#Extracting dataset to CSV
write.csv2(Sentek, file = "Langeweide_Sentek.csv", row.names = TRUE)
write.csv2(Sentek_Norm, file = "Langeweide_Sentek_normalized.csv", row.names = TRUE)

#test for Precipitation
Precipitation <- Langeweide_data %>% 
  select(RAIN, RAIN_NOBV1, RAIN_NOBV2)

P_Soil_moisture <- bind_cols(Sentek, Precipitation)

P_Soil_moisture$datetime <- format(P_Soil_moisture$datetime, format = "%Y-%m-%d %H:%M:%S")
write.csv2(P_Soil_moisture, file = "Transformed/Langeweide_P_SWC.csv", row.names = TRUE)

