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

#create a range of x_values for each row
x_ranges <- list()

for (i in 1:nrow(Subset_Bodem_fysische_metingen)) {
  x_ranges[[i]] <- c(Subset_Bodem_fysische_metingen$begindiepte[i], Subset_Bodem_fysische_metingen$einddiepte[i])
}

WCS_values <- list(c(Subset_Bodem_fysische_metingen$WCS))

interpolate_within_range <- function(x_ranges, WCS_values, x_profile){
  for(i in 1:length(x_ranges)){
    if(x_profile >= x_ranges[[i]][1] && x_profile <= x_ranges[[i]][2]){
      return(WCS_values[i])
    }
  }
 return(NA)
}


interp_function <- splinefun(WCS_values, xout = unlist(x_ranges))

x_profile <- c(20, 30, 50, 60)

interpolated_values <- interp_function(x_profile)

interpolated_values <- sapply(x_profile, function(x_profile) interpolate_within_range(x_ranges, WCS_values, x_profile))