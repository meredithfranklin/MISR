###### MISR netcdf extract and processing (California)####

library(RNetCDF) # for reading netcdf file formats
library(date) # for converting julian dates
library(chron) # for converting julian dates
library(lubridate) # for date interval matching
library(plyr) # for easy merging/subsetting
library(dplyr) #for easy merging/subsetting
library(proj4) # for map projections

#### Geographic projection for California applied to all lat/lon ####
proj.albers<-"+proj=aea +lat_1=34.0 +lat_2=40.5 +lon_0=-120.0 +x_0=0 +y_0=-4000000 +ellps=GRS80 +datum=NAD83 +units=km"

###### MISR NetCDF ######
# Create list of filenames in directory
setwd("/Users/mf/Documents/MISR/Data")
misr.files <- list.files("./",pattern="*_LM_4p4km*",full.names=FALSE)

# Extract data from netcdf: use RegBestEstimate for AOD, use RegLowestResid for fractions and SS albedo
misr.list<-vector('list',length(misr.files))
for(i in 1:length(misr.files)) { 
  dat<-open.nc(misr.files[i])
  lat<-var.get.nc(dat, "Latitude")
  lon<-var.get.nc(dat, "Longitude")
  julian<-var.get.nc(dat, "Julian")
  AOD<-var.get.nc(dat,"RegBestEstimateSpectralOptDepth")
  AODsmallfrac<-var.get.nc(dat,"RegLowestResidSpectralOptDepthFraction_Small")
  AODmedfrac<-var.get.nc(dat,"RegLowestResidSpectralOptDepthFraction_Medium")
  AODlargefrac<-var.get.nc(dat,"RegLowestResidSpectralOptDepthFraction_Large")
  AODnonspher<-var.get.nc(dat,"RegLowestResidSpectralOptDepthFraction_Nonsphere")
  SSAlbedo<-var.get.nc(dat,"RegLowestResidSpectralSSA")
  land.water.mask<-var.get.nc(dat,"AlgTypeFlag")
  AOD.mixture.number<-var.get.nc(dat,"RegLowestResidMixture")#land=3 water=1
  AOD.dat<-data.frame(lat=lat,lon=lon,julian=julian,AOD=AOD,AODsmallfrac=AODsmallfrac,AODmedfrac=AODmedfrac,
                      AODlargefrac=AODlargefrac,AODnonspher=AODnonspher,SSAlbedo=SSAlbedo,
                      land.water.mask=land.water.mask, AOD.mixture.number=AOD.mixture.number)
  misr.list[[i]]<-AOD.dat
}

misr.all<-do.call("rbind", misr.list)
# Convert Julian dates, create month day year variables for matching with surface measures
misr.all$date<-dates(misr.all$julian, origin=c(month=11, day=24, year= -4713))
misr.all$date2<-mdy(misr.all$date)
date<-strsplit(as.character(misr.all$date),"/")
misr.all$month<-as.numeric(sapply(date, "[[", 1) )
misr.all$day<-as.numeric(sapply(date,"[[",2))
misr.all$year<-as.numeric(sapply(date,"[[",3))+2000

# multiply fraction by AOD to get AODsmall, AODmed, AODlarge
misr.all$AODsmall<-misr.all$AODsmallfrac*misr.all$AOD
misr.all$AODmed<-misr.all$AODmedfrac*misr.all$AOD
misr.all$AODlarge<-misr.all$AODlargefrac*misr.all$AOD

# Convert lat and lon into planar x and y (California projection)
newcoords.misr<-project(as.matrix(cbind(misr.all$lon, misr.all$lat)), proj=proj.albers)
misr.all$x<-newcoords.misr[,1]
misr.all$y<-newcoords.misr[,2]

# Write (read) csv
write.csv(misr.all,paste("misr_",min(misr.all$year),"_",max(misr.all$year),".csv", sep=""),row.names=FALSE)

