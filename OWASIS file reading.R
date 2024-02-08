library(terra)
library(stringr)
library(dplyr)
#do not load the tidyverse packages because that has conflicting functions with terra package

Subset_Bodem_fysische_metingen <- read.csv2("Datasets/MvG_Bodem_fysische_metingen.csv")

#This sets the paths where the data should be retrieved. The first one is just a folder for raster files
#from OWASIS and the second one is a shapefile of footprint of EC towers
pathOWASISRast <-  "D:/R_master_thesis/Github/Master-thesis/Datasets/OWASIS/TIFF/groenehart/"
pathTowers <- "D:/R_master_thesis/OWASIS/Shapefiles/LAW_parc.gpkg"
pathPolders <- "D:/R_master_thesis/Github/Master-thesis/Datasets/OWASIS/Shapefiles/LAW_MS_ICOS_Tiwer_Path.shp"

#It creates list of file paths to .tif files in a new list
#recursive is used to it also takes any files from subdirectories from the pathway
#pattern determines that only files with .tif are taken
lOWASIS <- list.files(pathOWASISRast,recursive = T,pattern=".tif")

#Added this to prevent the auxilary files of interupting the iteration
#grepl checks for the pattern and gives TRUE if found, then it only keeps elements not having this pattern
lOWASIS <- lOWASIS[!grepl(".tif.aux.xml", lOWASIS)]

#An empty list
OWASISAb <- list()

#Adds elements BBB to OWASISAb list, and then sets its value from a different list
#This means that OWASISAb contains BBB, and BBB contains its own list
OWASISAb[["Beschikbare.Bodemberging"]] <- list(Abbreviation="BBB",Units= "mm")
OWASISAb[["Grondwaterstand"]] <- list(Abbreviation="GW",Units= "mm")
OWASISAb[["Bodemvocht"]] <- list(Abbreviation="BV",Units= "mm")

#I think this vect reads the shapefile and stores it for pTowers
pTowers <- vect(pathTowers)
pPolders <- vect(pathPolders)
#empty list
lTowi <- list()
#create a sequence of dates
lDay <- seq.Date(as.Date("2022-04-02","%Y-%m-%d"),as.Date("2022-11-01","%Y-%m-%d"),by = "1 day")
#A new data frame with day as variable
df.owasis <- data.frame(day=lDay)

#This taken the towers polygon (of polder) and than creates a buffer around this with 250 m
#cropped to pPolders so it will exclude the canal etc. that is outside the SSI
polTowers1 <- buffer(pTowers,width=250)
polTowers1 <- crop(polTowers1,pPolders)

#i think this might be outside the loop as well to make sure the namesPix is generated for later
ri0 <- rast(paste0(pathOWASISRast,lOWASIS[1])) #contains all the rasters for BBB
ri01 <- crop(ri0,polTowers1) #Cuts the raster to the buffer
ri01 <- mask(ri01,polTowers1) 
ptsri01 <- as.points(ri01) #makes points of the pixels?
dfri01 <- data.frame(values(ptsri01),geom(ptsri01)) #contains the coordinates of the points
namesPix <- paste0("x_",dfri01[["x"]],"_y",dfri01[["y"]]) #creates the names for each pixel
for(NTi in pTowers[["Name"]]){ #creates new columns based on names in pTowers but which idk
  df.owasis[,NTi] <- NA
}

#It makes a new list
l.df.owasis <- list()
nVar <- c("BBB","GW","BV")

for(nVari in nVar){ #create a list with BBB etc.
  l.df.owasisi <- list()
  for(nPi in namesPix){     #for each pixel it creates a list within the BBB, GW and BV

    l.df.owasisi[[nPi]] <- df.owasis
  }  
  l.df.owasis[[nVari]] <- l.df.owasisi
}

for(owi in lOWASIS){
  
  #the str_split splil text whenever a underscore is present. [[1]] then selects the first element from this split
  #The tail(...., 3) part extracts the last 3 elements from list created????
  #Then the [[1]] again selects the first part of that list
  #Remove Owasis from the string that is created
  nvi <- str_remove(tail(str_split(owi,"_")[[1]],3)[[1]],"Owasis.")
  #???
  nvi1 <- OWASISAb[[nvi]][["Abbreviation"]]
  #This part extracts the date from owi and assures its format
  #Owi is from lOWASIS and this is the filepath?
  dayi <- as.Date(str_split(tail(str_split(owi,"_")[[1]],2)[[1]],"T")[[1]][[1]],"%Y-%m-%d")
  print(dayi)
  
#Check if any day is equal to date list created earlier
  if(any(lDay==dayi)){
    #this reads the raster file, in this case it seems that owi is part of a filename that i want to read
    #whereas the pathOWASISRast is the directory
    ri <- rast(paste0(pathOWASISRast,owi))
    
    #similar as befor but in loop
    ri1 <- crop(ri,polTowers1)
    ri1 <- mask(ri1,polTowers1)
    ptsri <- as.points(ri1)
    dfri <- data.frame(values(ptsri),geom(ptsri))
    #dfi <-extract(ri,pTowers,fun=function(x)median(x,na.rm=T)) 
    
    #I think this part attaches the values of dfi for each day to the the list that contains BBB etc.
    if(nrow(dfri)>0){
      for(i in seq.int(nrow(dfri))){
        if(is.finite(dfri[i,1])){
          nPixi <- paste0("x_",dfri[i,"x"],"_y",dfri01[i,"y"])
          l.df.owasis[[nvi1]][[nPixi]][which(lDay==dayi),pTowers[["Name"]][1,"Name"]] <- dfri[i,1]            
        }
      }      
    }
    
  }
} 

#Create empty list for mean and stdev values
mean_values_BBB <- list()
stdev_values_BBB <- list()
median_values_BBB <- list()
mad_values_BBB <- list()

#loop to extract values for each pixel for date FOR BBB
for (date in lDay) {
  # Extract values for each pixel for the given date each pixel being its own df
  valueForDate <- lapply(l.df.owasis$BBB, function(Pixel) {
    subset_data <- Pixel[Pixel$day %in% as.Date(date), "LAW_MS_ICOS"]
    if (length(subset_data) > 0) {
      return(subset_data)
      print(subset_data)
    } else {
      return(NA)
    }
  })
  
  # Check for NA
  valueForDate <- unlist(valueForDate)
  #valueForDate <- valueForDate[!is.na(valueForDate)]
  valueForDate <- valueForDate[!is.na(valueForDate) & valueForDate != 0]
  
  # Calculate the mean for the current date ignoring NA in calc
  #gives values that are NA, a NaN, otherwise calculate mean or sd
  mean_values_BBB[[as.character(date)]] <- ifelse(length(valueForDate) > 0, mean(valueForDate, na.rm = TRUE), NaN)
  stdev_values_BBB[[as.character(date)]] <- ifelse(length(valueForDate) > 0, sd(valueForDate, na.rm = TRUE), NaN)
  median_values_BBB[[as.character(date)]] <- ifelse(length(valueForDate) > 0, median(valueForDate, na.rm = TRUE), NaN)
  mad_values_BBB[[as.character(date)]] <- ifelse(length(valueForDate) > 0, mad(valueForDate, na.rm = TRUE), NaN)
}

mean_values_GW <- list()
stdev_values_GW <- list()
median_values_GW <- list()
mad_values_GW <- list()

#Second loop for groundwater
for (date in lDay) {
  # Extract values for each pixel for the given date each pixel being its own df
  valueForDate_GW <- lapply(l.df.owasis$GW, function(Pixel_GW) {
    subset_data_GW <- Pixel_GW[Pixel_GW$day %in% as.Date(date), "LAW_MS_ICOS"]
    if (length(subset_data_GW) > 0) {
      return(subset_data_GW)
    } else {
      return(NA)
    }
  })
  
  # Check for NA
  valueForDate_GW <- unlist(valueForDate_GW)
  #valueForDate_GW <- valueForDate_GW[!is.na(valueForDate_GW)]
  valueForDate_GW <- valueForDate_GW[!is.na(valueForDate_GW) & valueForDate_GW != 0]
  
  # Calculate the mean for the current date ignoring NA in calc
  #gives values that are NA, a NaN, otherwise calculate mean or sd
  mean_values_GW[[as.character(date)]] <- ifelse(length(valueForDate_GW) > 0, mean(valueForDate_GW, na.rm = TRUE), NaN)
  stdev_values_GW[[as.character(date)]] <- ifelse(length(valueForDate_GW) > 0, sd(valueForDate_GW, na.rm = TRUE), NaN)
  median_values_GW[[as.character(date)]] <- ifelse(length(valueForDate_GW) > 0, median(valueForDate_GW, na.rm = TRUE), NaN)
  mad_values_GW[[as.character(date)]] <- ifelse(length(valueForDate_GW) > 0, mad(valueForDate_GW, na.rm = TRUE), NaN)
}

# Create a data frame with the calculated mean and standard deviation values
#here again a fail safe for the NA/NaN values otherwise errors
OWASIS_BBB_GW <- data.frame(
  Date = as.Date(lDay),
  
  MeanBBB = sapply(mean_values_BBB, function(x) if (all(is.na(x))) NaN else mean(x)),
  StdevBBB = sapply(stdev_values_BBB, function(x) if (all(is.na(x))) NaN else x),
  MedianBBB = sapply(median_values_BBB, function(x) if (all(is.na(x))) NaN else x),
  MadBBB = sapply(mad_values_BBB, function(x) if(all(is.na(x))) NaN else x),
  
  MeanGW = sapply(mean_values_GW, function(x) if (all(is.na(x))) NaN else mean(x)),
  StdevGW = sapply(stdev_values_GW, function(x) if (all(is.na(x))) NaN else x),
  MedianGW = sapply(median_values_GW, function(x) if (all(is.na(x))) NaN else x),
  MadGW = sapply(mad_values_GW, function(x) if(all(is.na(x))) NaN else x)
)

TPS_av = 0.695
AHN_mNAP_mmv <- -1.97

#calculation to WFPS
OWASIS_BBB_GW <- OWASIS_BBB_GW %>% 
  mutate(MedianGW_mmv = (MedianGW - (AHN_mNAP_mmv))) %>% 
  mutate(MedianWFPS = (TPS_av * abs(MedianGW_mmv*1000)) - MedianBBB) %>% 
  mutate(TPS_check = MedianBBB + MedianWFPS) %>% 
  mutate(TPS = TPS_av * abs(MedianGW_mmv*1000))
  #mutate(MedianBBB_p = MedianBBB / (TPS_av * abs(MedianGW_mmv*1000))) %>% 
  #mutate(MedianWFPS = TPS_av - MedianBBB_p)

#normalisation function excluding NA values
min_max_normalize <- function(x){
  (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
}

#Perform normalization function on MedianBBB
OWASIS_BBB_GW <- OWASIS_BBB_GW %>% 
  mutate(MedianBBB_norm = min_max_normalize(MedianBBB)) %>% 
  mutate(MedianWFPS_norm = min_max_normalize(MedianWFPS))

#Some test plots
ggplot(OWASIS_BBB_GW) +
  geom_line(aes(x = Date, y = MedianBBB_norm))

ggplot(OWASIS_BBB_GW) +
  geom_point(aes(x = MedianBBB, y = MedianWFPS))

ggplot(OWASIS_BBB_GW) +
  geom_line(aes(x = Date, y = TPS))
  
ggplot(OWASIS_BBB_GW) +
  geom_line(aes(x = Date, y = MedianGW))

ggplot(OWASIS_BBB_GW) +
  geom_line(aes(x = MedianGW_mmv, y = TPS))

#just to visually check the variation
ggplot(OWASIS_BBB_GW, aes(x = Date)) +
  #geom_line(aes(y = MeanBBB), color = "blue") +
  geom_line(aes(y = MedianBBB), color = "red") +
  #geom_ribbon(aes(ymin = MeanBBB - StdevBBB, ymax = MeanBBB + StdevBBB), fill = "lightblue", alpha = 0.5) +
  geom_ribbon(aes(ymin = MedianBBB - StdevBBB, ymax = MedianBBB + StdevBBB), fill = "pink", alpha = 0.5) +
  labs(
    title = "median pixel values with STDEV",
       x = "Date",
       y = "BBB (mm)"
    )

ggplot(OWASIS_BBB_GW, aes(x = Date)) +
  geom_line(aes(y = MedianBBB), color = "blue") +
  geom_line(aes(y = MedianWFPS), color = "red") +
  labs(title = "BBB and WFPS",
       x = "Date",
       y = "Filled pore space (mm)") +
  theme_minimal()

ggplot(LAW_MS_ICOS, aes(x = datetime)) +
  geom_point(aes(y = WL_1, color = "red"), size = 0.5) +
  geom_point(aes(y = WL_2, color = "blue"), size = 0.5) +
  geom_point(aes(y = WL_3, color = "green"), size = 0.5) 

ggplot(OWASIS_BBB_GW, aes(x = Date)) +
  geom_line(aes(y = MeanGW))


#Some threshold investigation compared to meean MAD
dates_with_large_MAD <- OWASIS_BBB$Date[OWASIS_BBB$MadBBB >= 2]
print(dates_with_large_MAD)
average_MAD <- mean(OWASIS_BBB$MadBBB, na.rm = TRUE)

# Calculate mean and standard deviation for each pixel across all dates
P_mean_values <- lapply(l.df.owasis$BBB, function(Pixel) {
  P_mean_value <- mean(Pixel$LAW_MS_ICOS, na.rm = TRUE)
  return(ifelse(is.nan(P_mean_value), NA, P_mean_value))
})

P_stdev_values <- lapply(l.df.owasis$BBB, function(Pixel) {
  P_stdev_value <- sd(Pixel$LAW_MS_ICOS, na.rm = TRUE)
  return(ifelse(is.nan(P_stdev_value), NA, P_stdev_value))
})

# Identify the name of the pixel with the maximum mean value
max_mean_pixel_name <- names(P_mean_values)[which.max(P_mean_values)]

# Identify the name of the pixel with the maximum standard deviation value
max_stdev_pixel_name <- names(P_stdev_values)[which.max(P_stdev_values)]

#extract buffer and pixel coordinates for analysis in GIS
writeVector(polTowers1, "Transformed/buffer.shp", overwrite = TRUE )
writeRaster(ri01, "Transformed/rbuffer.tif", overwrite = TRUE)

#formating the dates for converersions
OWASIS_BBB_GW$Date <- format(OWASIS_BBB_GW$Date, format = "%Y-%m-%d")

#write csv for the BBB files
write.csv2(OWASIS_BBB_GW, file = "Transformed/Langeweide_OWASIS_BBB_500.csv", row.names = FALSE)
test_read <- read.csv2("Transformed/Langeweide_OWASIS_BBB.csv")
