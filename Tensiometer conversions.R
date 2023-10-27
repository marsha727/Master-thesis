library(tidyverse)
library(readxl)

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
interp <- smooth.spline(Bodem_fysische_metingen$begindiepte, Bodem_fysische_metingen$WCS)
new_WCS <- predict(interp, xout = new_x, setLAB = FALSE)$Bodem_fysische_metingen$WCS

#I will have to apply a range to the all the x values of y and then interpolat
#for those outside the range --> the above would be incorrect because
#It only accounts for the begindiepte
  