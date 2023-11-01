library(tidyverse)
library(readxl)
library(splines)

Tensiometer <- read.csv("Datasets/LAW_TENS_2020-2023_clean.csv")
Bodem_fysische_metingen <- read_xlsx("Datasets/BodemFysischeMetingen_LAW.xlsx", sheet = "Evap+MvG")

#Conversion formula from kPa to cmH2O only applied to TMAP (matrix potential)
kPa_to_cmH2O <- function(x){
  ifelse(is.na(x), NA, x*10.1971623)
}

Tensiometer_cmH20 <- Tensiometer %>% 
  mutate(across(.cols = 2:10, ~kPa_to_cmH2O(.)))

#Checking the relationship between depth and WCS
ggplot(Bodem_fysische_metingen) +
  geom_point(aes(x = begindiepte, y = WCS))

#Shows non-linear behaviour, interpolation

#make a subset of the values i require (I remove NA row manually for now)
Subset_Bodem_fysische_metingen <- Bodem_fysische_metingen %>% 
  filter(row_number() %in% c(3, 4, 7))

depth_to_interpolate <- 30

Interpolate_values <- data.frame(x = c(Subset_Bodem_fysische_metingen$begindiepte, Subset_Bodem_fysische_metingen$einddiepte), y = c(Subset_Bodem_fysische_metingen$WCS, Subset_Bodem_fysische_metingen$WCS))

interpolated_y <- spline(Interpolate_values$x, Interpolate_values$y, xout = depth_to_interpolate)$y











#create a range of x_values for each row
x_ranges <- list()

for (i in 1:nrow(Subset_Bodem_fysische_metingen)) {
  x_ranges[[i]] <- c(Subset_Bodem_fysische_metingen$begindiepte[i], Subset_Bodem_fysische_metingen$einddiepte[i])
}

