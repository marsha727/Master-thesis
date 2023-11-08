library(terra)
library(stringr)
#do not load the tidyverse packages because that has conflicting functions with terra package

#This sets the paths where the data should be retrieved. The first one is just a folder for raster files
#from OWASIS and the second one is a shapefile of footprint of EC towers
pathOWASISRast <-  "D:/R_master_thesis/Github/Master-thesis/Datasets/OWASIS/TIFF/groenehart/"
pathTowers <- "D:/R_master_thesis/Github/Master-thesis/Datasets/OWASIS/Shapefiles/LAW_MS_ICOS_Tiwer_Path.shp"

#It creates list of file paths to .tif files in a new list
#recursive is used to it also takes any files from subdirectories from the pathway
#pattern determines that only files with .tif are taken
lOWASIS <- list.files(pathOWASISRast,recursive = T,pattern=".tif")

#An empty list
OWASISAb <- list()

#Adds elements BBB to OWASISAb list, and then sets its value from a different list
#This means that OWASISAb contains BBB, and BBB contains its own list
OWASISAb[["Beschikbare.Bodemberging"]] <- list(Abbreviation="BBB",Units= "mm")
OWASISAb[["Grondwaterstand"]] <- list(Abbreviation="GW",Units= "mm")
OWASISAb[["Bodemvocht"]] <- list(Abbreviation="BV",Units= "mm")

#I think this vect reads the shapefile and stores it for pTowers
pTowers <- vect(pathTowers)
#empty list
lTowi <- list()
#create a sequence of dates
lDay <- seq.Date(as.Date("2022-04-02","%Y-%m-%d"),as.Date("2022-07-27","%Y-%m-%d"),by = "1 day")
#A new data frame with day as variable
df.owasis <- data.frame(day=lDay)

#I think this means that pTowers has a column called names, the names
#is temporatly taken by NTi, and for each NTi a new column is creates
#is used to create a new column in the new dataframe df.owasis, with all its cells having NA
for(NTi in pTowers[["Name"]]){
  df.owasis[,NTi] <- NA
}

#It makes a new list
# in this list we have three elements that all point to the same dataframe
l.df.owasis <- list()
l.df.owasis[["BBB"]] <- df.owasis
l.df.owasis[["GW"]] <- df.owasis
l.df.owasis[["BV"]] <- df.owasis

#Another loop i think the point is that it needs to remove some parts of the of the file directory to 
#find the directly later
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
    #ri contains values for locations, and pTowers are also spatial points
    #the function tries to match those locations and extracts those values for those locations
    #fun= function(x)meadian(x, na.rm=T) calculates the median of the values from each location
    dfi <-extract(ri,pTowers,fun=function(x)median(x,na.rm=T)) 
    
    #I think this part attaches the values of dfi for each day to the the list that contains BBB etc.
    for(i in seq.int(nrow(dfi))){
      if(is.finite(dfi[i,ncol(dfi)])){
        l.df.owasis[[nvi1]][which(lDay==dayi),pTowers[["Name"]][i,"Name"]] <- dfi[i,ncol(dfi)]            
      }
    }
    
  }
} 


#I would like to get my data in the list to a dataframe for camparison later
OWASIS_BBB <- data.frame(l.df.owasis[["BBB"]])

start_date <- "2022-04-02"
end_date <- "2022-07-26"

#make sure the selection aligns of the dates
OWASIS_BBB <- OWASIS_BBB %>% 
  filter(day >= start_date & day <= end_date)

