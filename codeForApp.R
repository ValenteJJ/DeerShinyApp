
rm(list=ls())

# library(raster)
library(sf)
# library(rgdal)
library(terra)
library(FedData)
library(tidyverse)
library(tigris)



#Create a dropdown menu where you select the state
stateName = 'AL'

#Not sure if year is important, but it should default to the most
#current year for which the county shape data are available
yearNum = NULL

#Download the shapefile of all Alabama counties
countyShape = counties(state = stateName, cb = FALSE, year = yearNum)

#Could create another dropdown menu here with all county names where they can
#select the value of the countyName variable below.
countyShape$NAME
countyName = 'Lee'

countyOfInterest = countyShape %>% filter(NAME==countyName)
# countyOfInterest = countyShape[which(countyShape$NAME==countyName),]

#Can now plot the county
ggplot()+
  geom_sf(data = countyOfInterest, color="black",
        fill="white", size=0.25)

#Can choose NLCD years of 2001, 2004, 2006, 2008, 2011, 2016, or 2019
nlcdYear = 2019

#Must identify for what spatial area you want NLCD data. Options are
#L48 (lower 48 US states),
#AK (Alaska, 2011 and 2016 only),
#HI (Hawaii, 2001 only), or
#PR (Puerto Rico, 2001 only)
landMass = 'L48'

#Set the directory where you want the nlcd data stored
directoryForStorage = getwd()

#Buffer the county by 1 km
countyBuffer = st_buffer(countyOfInterest, dist=2000)



#Download the nlcd data and crop to the county of interest
croppedNLCD = get_nlcd(countyBuffer,
                       label=countyName,
                       year = nlcdYear,
                       dataset = "landcover",
                       landmass = landMass,
                       extraction.dir = file.path(directoryForStorage, "FedData", "extractions", "nlcd"),
                       raster.options = c("COMPRESS=DEFLATE", "ZLEVEL=9"),
                       force.redo = TRUE
)

#Reproject the county so it is the same as the NLCD data
# countyShape = as_Spatial(countyShape)
countyShape = st_transform(countyOfInterest, crs(croppedNLCD))
countyBuffer = st_transform(countyBuffer, crs(croppedNLCD))

#Now we can plot it for funsies
plot(croppedNLCD)
plot(st_geometry(countyBuffer), add=T, lwd=5)



#Now we can reclassify the NLCD raster so that all forest has a value
#of 1 and everything else has a value of 0
reclassifyValues = matrix(c(1, 39, 0,
                            40, 49, 1,
                            50, 100, 0), nrow=3, ncol=3, byrow=T)
forestNon = classify(croppedNLCD, rcl=reclassifyValues)
forestNon = mask(forestNon, countyBuffer)

#Now we can look at the distribution of forest (1 values) in the county
plot(forestNon)
plot(st_geometry(countyShape), add=T)



#increase the grain and calculate the mean
aggForest <- aggregate(forestNon, fact=53, fun='mean')

#Is there a specific projection that you need this in?
crs(aggForest, proj=T)
aggForest = mask(aggForest, countyShape)
plot(aggForest)
plot(st_geometry(countyShape), add=T)


#Somehow you'll need folks to set the filename for where to output the ascii
#It should specify the file path and then end with asciiFileName.asc
fileName = NA

#You should probably double-check to make sure this output works in your system before we go too far.
writeRaster(aggForest, filename=fileName, overwrite=TRUE)



#What else do we need to add in here?