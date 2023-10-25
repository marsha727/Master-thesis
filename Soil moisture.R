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
  SM_n[[col]] <- SM_norm(Soil_moisture[[col]], max_SM)
}

#binding dateset can be done with the following function
new_column_names <- paste(names(WFPS_Sentek), "WFPS", sep = "_")
colnames(WFPS_Sentek) <- new_column_names
WFPS_preSM <- bind_cols(Soil_moisture, WFPS_Sentek)
WFPS_SM_n <- bind_cols(SM_n, WFPS_Sentek)
WFPS_SF_n <- bind_cols(SF_n, WFPS_Sentek)

#MAke a new dataset that contains SWC, SF and WFPS
new_column_names_WFPS <- paste(names(WFPS_Sentek), "WFPS", sep = "_") #Give new colnames to differentiate
colnames(WFPS_Sentek) <- new_column_names_WFPS
new_column_names_SWC <- paste(names(Soil_moisture), "SWC", sep = "_")
colnames(WFPS_Sentek) <- new_column_names_SWC
new_column_names_SF <- paste(names(SF_Soil_moisture), "SF", sep = "_")

Sentek <- bind_cols(WFPS_Sentek, Soil_moisture, SF_Soil_moisture)



#######################################################################
#some try out plots
ggplot(WFPS_Sentek) +
  geom_point(mapping = aes(x = datetime, y = SWC_1_005, color = "5 cm"), size = 0.3) +
  geom_point(mapping = aes(x = datetime, y = SWC_1_015, color = "15 cm"), size = 0.3) +
  geom_point(mapping = aes(x = datetime, y = SWC_1_025, color = "25 cm"), size = 0.3) +
  geom_point(mapping = aes(x = datetime, y = SWC_1_055, color = "55 cm"), size = 0.3) +
  geom_point(mapping = aes(x = datetime, y = SWC_1_085, color = "85 cm"), size = 0.3) +
  geom_point(mapping = aes(x = datetime, y = SWC_1_105, color = "105 cm"), size = 0.3) +
  geom_point(mapping = aes(x = datetime, y = SWC_1_115, color = "115 cm"), size = 0.3) +
  
  labs(
    title = "soil moisture content",
    x = "date",
    y = "WFPS (%)",
  ) +
  scale_color_manual(
    values = c("5 cm" = "blue", "15 cm" = "yellow", "25 cm" = "red", "55 cm" = "green", "85 cm" = "orange", "105 cm" = "purple", "115 cm" = "cadetblue"),
    name = "Depth" )

ggplot(Langeweide_data) +
  geom_point(mapping = aes(x = datetime, y = SWC_1_005, color = "5 cm"), size = 0.3) +
  geom_point(mapping = aes(x = datetime, y = SWC_1_015, color = "15 cm"), size = 0.3) +
  geom_point(mapping = aes(x = datetime, y = SWC_1_025, color = "25 cm"), size = 0.3) +
  geom_point(mapping = aes(x = datetime, y = SWC_1_055, color = "55 cm"), size = 0.3) +
  geom_point(mapping = aes(x = datetime, y = SWC_1_085, color = "85 cm"), size = 0.3) +
  geom_point(mapping = aes(x = datetime, y = SWC_1_105, color = "105 cm"), size = 0.3) +
  geom_point(mapping = aes(x = datetime, y = SWC_1_115, color = "115 cm"), size = 0.3) +
  
  labs(
    title = "soil moisture content",
    x = "date",
    y = "Soil moisture (%)",
  ) +
  scale_color_manual(
    values = c("5 cm" = "blue", "15 cm" = "yellow", "25 cm" = "red", "55 cm" = "green", "85 cm" = "orange", "105 cm" = "purple", "115 cm" = "cadetblue"),
    name = "Depth" )


#checking the soil moisture patterns against WFPS
ggplot(WFPS_preSM) +
  geom_point(mapping = aes(x = datetime, y = SWC_1_005, color = "Soil moisture"), size = 0.3) +
  geom_point(mapping = aes(x = datetime, y = SWC_1_005_WFPS*100, color = "WFPS"), size = 0.3) +
  labs(
    title = "Soil moisture vs WFPS at 005 cm",
    x = "date",
    y = "Soil moisture or WFPS in %"
  )

ggplot(WFPS_preSM, aes(x = SWC_1_025, y = SWC_1_025_WFPS), size = 0.2) +
  geom_jitter() +
  geom_smooth(method = "loess", se = TRUE) +
  labs(
    title = "correlation between soil moisture and WFPS",
    x = "Soil moisture",
    y = "WFPS"
  )

ggplot(WFPS_SF_n) +
  geom_point(aes(x = datetime, y = SWC_1_005, color = "Soil moisture"), size = 0.2) +
  geom_point(aes(x = datetime, y = SWC_1_005_WFPS, color = "WFPS"), size = 0.2) +
  labs(
    title = "Dynamics soil moisture normalized and WFPS",
    x = "Normalizeed soil moisture",
    y = "WFPS"
  )
  
ggplot(WFPS_SF_n) +
  geom_point(aes(x = SWC_1_005, y = SWC_1_005_WFPS), size = 0.2) +
  labs(
    title = "Correlation normalized SWC and WFPS",
    x = "Normalized SWC",
    y = "WFPS"
  )


  