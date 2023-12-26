
library(raster)
library(sf)
library(rgdal)
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

countyOfInterest = countyShape[which(countyShape$NAME==countyName),]

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

#Download the nlcd data and crop to the county of interest
croppedNLCD = get_nlcd(countyOfInterest,
                       label=countyName,
                       year = nlcdYear,
                       dataset = "landcover",
                       landmass = landMass,
                       extraction.dir = file.path(directoryForStorage, "FedData", "extractions", "nlcd"),
                       raster.options = c("COMPRESS=DEFLATE", "ZLEVEL=9"),
                       force.redo = FALSE
)

#Reproject the county so it is the same as the NLCD data
countyShape = as_Spatial(countyShape)
countyShape = spTransform(countyShape, crs(croppedNLCD))

#Now we can plot it for funsies
plot(croppedNLCD)
plot(countyShape, add=T, lwd=5)


#Now we can reclassify the NLCD raster so that all forest has a value
#of 1 and everything else has a value of 0
reclassifyValues = matrix(c(1, 39, 0,
                            40, 49, 1,
                            50, 100, 0), nrow=3, ncol=3, byrow=T)
forestNon = classify(croppedNLCD, rcl=reclassifyValues)

#Now we can look at the distribution of forest (1 values) in the county
plot(forestNon)
