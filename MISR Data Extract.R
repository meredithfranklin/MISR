##############################################
# MISR 2008-2009 4.4km AOD and fractions 
# AQS Daily http://www.epa.gov/airdata/ad_data_daily.html
# Aeronet data http://aeronet.gsfc.nasa.gov/
# ICV data (CHS study)
# December 2014, February-June 2015
# Meredith Franklin
##############################################

library(ncdf) # for reading netcdf file formats
library(date) # for converting julian dates
library(chron) # for converting julian dates
library(lubridate) # for date interval matching
library(plyr) # for easy merging/subsetting
library(dplyr) #for easy merging/subsetting
library(fields) # for spatial functions
library(proj4) # for map projections
library(R.utils) # decompressing NCDC data

#### Geographic projection for California applied to all lat/lon ####
proj.albers<-"+proj=aea +lat_1=34.0 +lat_2=40.5 +lon_0=-120.0 +x_0=0 +y_0=-4000000 +ellps=GRS80 +datum=NAD83 +units=km"

###### MISR NetCDF ######
# Create list of filenames in directory
setwd("/Users/mf/Documents/MISR/Data")
misr.files <- list.files("./",pattern="*_LM_4p4km*",full.names=FALSE)

# Extract data from netcdf: use RegBestEstimate for AOD, use RegLowestResid for fractions and SS albedo
misr.list<-vector('list',length(misr.files))
  for(i in 1:length(misr.files)) { 
    dat<-open.ncdf(misr.files[i])
    lat<-get.var.ncdf(dat, "Latitude")
    lon<-get.var.ncdf(dat, "Longitude")
    julian<-get.var.ncdf(dat, "Julian")
    AOD<-get.var.ncdf(dat,"RegBestEstimateSpectralOptDepth")
    AODsmallfrac<-get.var.ncdf(dat,"RegLowestResidSpectralOptDepthFraction_Small")
    AODmedfrac<-get.var.ncdf(dat,"RegLowestResidSpectralOptDepthFraction_Medium")
    AODlargefrac<-get.var.ncdf(dat,"RegLowestResidSpectralOptDepthFraction_Large")
    AODnonspher<-get.var.ncdf(dat,"RegLowestResidSpectralOptDepthFraction_Nonsphere")
    SSAlbedo<-get.var.ncdf(dat,"RegLowestResidSpectralSSA")
    land.water.mask<-get.var.ncdf(dat,"AlgTypeFlag")
    AOD.mixture.number<-get.var.ncdf(dat,"RegLowestResidMixture")#land=3 water=1
    AOD.dat<-data.frame(lat=lat,lon=lon,julian=julian,AOD=AOD,AODsmallfrac=AODsmallfrac,AODmedfrac=AODmedfrac,
                        AODlargefrac=AODlargefrac,AODnonspher=AODnonspher,SSAlbedo=SSAlbedo,
                        land.water.mask=land.water.mask, AOD.mixture.number=AOD.mixture.number)
    misr.list[[i]]<-AOD.dat
}
misr.08.09<-do.call("rbind", misr.list)

# Convert Julian dates, create month day year variables for matching with surface measures
misr.08.09$date<-dates(misr.08.09$julian, origin=c(month=11, day=24, year= -4713))
misr.08.09$date2<-mdy(misr.08.09$date)
date<-strsplit(as.character(misr.08.09$date),"/")
misr.08.09$month<-as.numeric(sapply(date, "[[", 1) )
misr.08.09$day<-as.numeric(sapply(date,"[[",2))
misr.08.09$year<-as.numeric(sapply(date,"[[",3))+2000

# multiply fraction by AOD to get AODsmall, AODmed, AODlarge
misr.08.09$AODsmall<-misr.08.09$AODsmallfrac*misr.08.09$AOD
misr.08.09$AODmed<-misr.08.09$AODmedfrac*misr.08.09$AOD
misr.08.09$AODlarge<-misr.08.09$AODlargefrac*misr.08.09$AOD

# Convert lat and lon into planar x and y (California projection)
newcoords.misr<-project(as.matrix(cbind(misr.08.09$lon, misr.08.09$lat)), proj=proj.albers)
misr.08.09$x<-newcoords.misr[,1]
misr.08.09$y<-newcoords.misr[,2]

# Write (read) csv
#write.csv(misr.08.09,"misr_08_09.csv",row.names=FALSE)
misr.08.09<-read.csv("/Users/mf/Documents/MISR/Data/misr_08_09.csv")


##### Daily AQS PM25 and PM10 data ######

aqspm25.files <- list.files("/Users/mf/Documents/AQS/PM25",pattern="CA_PM25*",full.names=TRUE)
aqspm10.files <- list.files("/Users/mf/Documents/AQS/PM10",pattern="CA_PM10*",full.names=TRUE)

# Extract data then subset
aqspm25.list<-vector('list',length(aqspm25.files))
aqspm10.list<-vector('list',length(aqspm10.files))

for(i in 1:length(aqspm25.files)) { 
  # PM25
  dat.pm25<-read.csv(aqspm25.files[i],stringsAsFactors=FALSE)
# separate m/d/y from Date
  date.pm25<-strsplit(dat.pm25$Date,"/")
  dat.pm25$month<-as.numeric(sapply(date.pm25, "[[", 1) )
  dat.pm25$day<-as.numeric(sapply(date.pm25,"[[",2))
  dat.pm25$year<-as.numeric(sapply(date.pm25,"[[",3))+2000
# reading national .txt files (no geographic reference)  
  #header<-read.table(aqs.files[i],sep="|",header=TRUE,row.names=NULL,
  #                   check.names=FALSE,nrows=2,skip=1,comment.char="")
  #colnames(dat)<-names(header)
  #dat<-cbind(read.fwf(file = textConnection(as.character(dat$Year)), 
  #               widths = c(4, 2, 3), colClasses = "character", 
  #               col.names = c("year2", "month", "day")),dat)
  aqspm25.list[[i]]<-dat.pm25
  
  # PM10
  dat.pm10<-read.csv(aqspm10.files[i],stringsAsFactors=FALSE)
  # separate m/d/y from Date
  #dat.pm10$date2<-mdy(dat.pm10$Date) 
  date.pm10<-strsplit(dat.pm10$Date,"/")
  dat.pm10$month<-as.numeric(sapply(date.pm10, "[[", 1) )
  dat.pm10$day<-as.numeric(sapply(date.pm10,"[[",2))
  dat.pm10$year<-as.numeric(sapply(date.pm10,"[[",3))

  aqspm10.list[[i]]<-dat.pm10
}

aqspm25.08.09 <- do.call("rbind", aqspm25.list) 
aqspm10.08.09 <- do.call("rbind", aqspm10.list) 

# Convert lat and lon into planar x and y (California projection)
newcoords.aqspm25<-project(as.matrix(cbind(aqspm25.08.09$SITE_LONGITUDE, aqspm25.08.09$SITE_LATITUDE)), proj=proj.albers)
aqspm25.08.09$x<-newcoords.aqspm25[,1]
aqspm25.08.09$y<-newcoords.aqspm25[,2]

newcoords.aqspm10<-project(as.matrix(cbind(aqspm10.08.09$SITE_LONGITUDE, aqspm10.08.09$SITE_LATITUDE)), proj=proj.albers)
aqspm10.08.09$x<-newcoords.aqspm10[,1]
aqspm10.08.09$y<-newcoords.aqspm10[,2]

# Subset AQS data around LA area for this study
aqspm25.08.09.ss<-aqspm25.08.09[aqspm25.08.09$SITE_LONGITUDE>=-120 & aqspm25.08.09$SITE_LONGITUDE<= -117,]
aqspm25.08.09.ss<-aqspm25.08.09.ss[aqspm25.08.09.ss$SITE_LATITUDE>=33.2 & aqspm25.08.09.ss$SITE_LATITUDE<=35,]
aqspm25.08.09.ss<-aqspm25.08.09.ss[aqspm25.08.09.ss$SITE_LONGITUDE != -119.4869,] #Remove Catalina

aqspm10.08.09.ss<-aqspm10.08.09[aqspm10.08.09$SITE_LONGITUDE>=-120 & aqspm10.08.09$SITE_LONGITUDE<= -117,]
aqspm10.08.09.ss<-aqspm10.08.09.ss[aqspm10.08.09.ss$SITE_LATITUDE>=33.2 & aqspm10.08.09.ss$SITE_LATITUDE<=35,]
aqspm10.08.09.ss<-aqspm10.08.09.ss[aqspm10.08.09.ss$SITE_LONGITUDE != -119.4869,] #Remove Catalina

# Use only the POC=1 (FRM) monitors and POC=2 (colocated FRM) for PM25
PM25.POC2<-as.factor(aqspm25.08.09.ss$POC)
summary(PM25.POC2)
aqspm25.08.09.ss2<-aqspm25.08.09.ss[aqspm25.08.09.ss$POC %in% c(1,2),]
#aqs.08.09.ss2<-aqs.08.09.ss2[aqs.08.09.ss2$AQS_PARAMETER_CODE==88101,]
aqspm25.08.09.ss2$date2<- mdy(aqspm25.08.09.ss2$Date) #use lubridate function for date matching (ICV)
aqspm25.08.09.ss2<-aqspm25.08.09.ss2[,c(-5:-14)]

# average over all POCs
PM10.POC2<-as.factor(aqspm10.08.09.ss$POC)
summary(PM10.POC2)
aqspm10.08.09.ss2<-aqspm10.08.09.ss[aqspm10.08.09.ss$POC %in% c(1,2,3,4,5),]
aqspm10.08.09.ss3<- ddply(aqspm10.08.09.ss2, .(month, day, year,AQS_SITE_ID), summarise, Daily.Mean.PM10.Concentration=mean(Daily.Mean.PM10.Concentration,na.rm=TRUE),
                          x=mean(x),y=mean(y),SITE_LATITUDE=mean(SITE_LATITUDE),SITE_LONGITUDE=mean(SITE_LONGITUDE))
aqspm10.08.09.ss3$date<-as.Date(paste(aqspm10.08.09.ss3$month, aqspm10.08.09.ss3$day, aqspm10.08.09.ss3$year, sep = "/" )  , format = "%m/%d/%Y" )


# Write (read) .csv
write.csv(aqspm25.08.09.ss2,"/Users/mf/Documents/MISR/Data/aqspm25_08_09_ss_frm.csv",row.names=FALSE) # subset to CA and FRM
write.csv(aqspm25.08.09.ss,"/Users/mf/Documents/MISR/Data/aqspm25_08_09_ss.csv",row.names=FALSE) # subset to CA
write.csv(aqspm25.08.09,"/Users/mf/Documents/MISR/Data/aqsppm25_08_09.csv",row.names=FALSE) # not subset
aqspm25.08.09.ss2<-read.csv("/Users/mf/Documents/MISR/Data/aqsPM25_08_09_ss_frm.csv")

write.csv(aqspm10.08.09.ss2,"/Users/mf/Documents/MISR/Data/aqspm10_08_09_ss.csv",row.names=FALSE) # subset to CA
write.csv(aqspm10.08.09.ss3,"/Users/mf/Documents/MISR/Data/aqspm10_08_09_new.csv",row.names=FALSE) # subset to CA
write.csv(aqspm10.08.09,"/Users/mf/Documents/MISR/Data/aqspm10_08_09.csv",row.names=FALSE) # not subset
aqspm10.08.09.ss2<-read.csv("/Users/mf/Documents/MISR/Data/aqspm10_08_09_ss.csv",row.names=FALSE)

#### EPA STN data (1 in 3 or 1 in 6 days) #####
# Parameter codes for species EC=88307 OC=88305, sulfate=88403, nitrate=88306, PM25=88502
setwd("/Users/mf/Documents/AQS/STN")
stn.files <- list.files("./",pattern="RD_501_SPEC_.*.csv",full.names=FALSE)
# Extract data
stn.list<-vector('list',length(stn.files))
for(i in 1:length(stn.files)) { 
  dat<-read.csv(stn.files[i],stringsAsFactors=FALSE)
  dat$StateCode<-as.numeric(dat$StateCode)
  #dat$CountyCode<-as.numeric(dat$CountyCode)
  #dat$SiteID<-as.numeric(dat$SiteID)
  dat$POC<-as.numeric(dat$POC)
  dat$Concentration<-as.numeric(dat$Concentration)
  dat<-dat[dat$StateCode==6 & dat$POC==5 & dat$Parameter>88000,] #Subset to CA and STN sites (remove POC 6)

  # separate m/d/y from Date
  dat$date2<-parse_date_time(dat$Date,"ymd")
  dat$year<-as.numeric(substr(dat$Date,1,4))
  dat$month<-as.numeric(substr(dat$Date,5,6))
  dat$day<-as.numeric(substr(dat$Date,7,8))
  
  PM25stn<-dat[dat$Parameter==88502,c(3:4,12,28:31)]
  PM25stn<-rename(PM25stn, PM25=Concentration)
  #PM25stn<-PM25stn[with(PM25stn,order(CountyCode, SiteID, month, day, year)),]
  PM25stn<-PM25stn[!duplicated(PM25stn),]
  
  EC<-dat[dat$Parameter==88307,c(3:4,12,28:31)]
  EC<-rename(EC, EC=Concentration)
  EC<-EC[!duplicated(EC),]
  
  OC<-dat[dat$Parameter==88305,c(3:4,12,28:31)]
  OC<-rename(OC, OC=Concentration)
  OC<-OC[!duplicated(OC),]
  
  NH4<-dat[dat$Parameter==88306,c(3:4,12,28:31)]
  NH4<-rename(NH4, NH4=Concentration)
  NH4<-NH4[!duplicated(NH4),]
  
  SO4<-dat[dat$Parameter==88403,c(3:4,12,28:31)]
  SO4<-rename(SO4, SO4=Concentration)
  SO4<-SO4[!duplicated(SO4),]
  
  join1<-join(EC, OC, by=c("CountyCode","SiteID","date2"))
  join2<-join(join1,NH4, by=c('CountyCode','SiteID','date2'))
  join3<-join(join2,SO4, by=c('CountyCode','SiteID','date2'))
  join4<-join(join3,PM25stn, by=c('CountyCode','SiteID','date2'))
  # reading national .txt files (no geographic reference)  
 
  stn.list[[i]]<-join4
}

stn.08.09 <- do.call("rbind", stn.list) 

stn.08.09<-stn.08.09[with(stn.08.09,order(CountyCode, SiteID, date2)),]
# Find out where site 6-19-8 is located. Note used coordinates for Fresno 6-9-11 site (same county)
stn.08.09$SiteID<-ifelse(stn.08.09$CountyCode==19 & stn.08.09$SiteID==8, 11, stn.08.09$SiteID)

# Include STN site info
stn.site.info<-read.csv("STNSiteInfo.csv")
stn.site.CA<-stn.site.info[stn.site.info$StateCode==6,]
stn.08.09.all<-join(stn.08.09,stn.site.CA, by=c('CountyCode','SiteID'))

# Add projected coordinates
newcoords.stn<-project(as.matrix(cbind(stn.08.09.all$Longitude, stn.08.09.all$Latitude)), proj=proj.albers)
stn.08.09.all$x<-newcoords.stn[,1]
stn.08.09.all$y<-newcoords.stn[,2]

stn.08.09.ss<-stn.08.09.all[stn.08.09.all$Latitude>=33.599,]
stn.08.09.ss<-stn.08.09.ss[stn.08.09.ss$Latitude<=35,]
# remove duplicate columns
stn.08.09.ss<-stn.08.09.ss[,c(-9:-11,-13:-15,-17:-19,-21:-23)]

# Write (read) .csv
write.csv(stn.08.09.ss,"stn_08_09_ss.csv",row.names=FALSE)
stn.08.09.ss<-read.csv("/Users/mf/Documents/AQS/STN/stn_08_09_ss.csv")

##### Daily NCDC data #####
# Station Information
#file <- "ftp://ftp.ncdc.noaa.gov/pub/data/noaa/isd-history.csv"
#download.file(file, "isd-history.csv")
setwd("/Users/mf/Documents/NCDC/")
stations <- read.csv("isd-history.csv") 
st.ca<-stations[stations$CTRY=="US" & stations$STATE=="CA",]
st.ca$BEGIN <- as.numeric(substr(st.ca$BEGIN, 1, 4))
st.ca$END <- as.numeric(substr(st.ca$END, 1, 4))

st.so.ca<-st.ca[st.ca$LAT>=33.2 & st.ca$LAT<=35,]
st.so.ca<-st.so.ca[st.so.ca$LON>= -120 & st.so.ca$LON<= -117,]
# Remove Catalina and buoys
st.so.ca<-st.so.ca[-(grep(c("BUOY|CATALINA|ISLAND"),st.so.ca$STATION.NAME)),]
st.so.ca<-st.so.ca[complete.cases(st.so.ca),]
# write.csv(st.so.ca,"/Users/mf/Documents/NCDC/SoCalNCDCsites.csv",row.names = FALSE)
# st.so.ca<-read.csv("/Users/mf/Documents/NCDC/SoCalNCDCsites.csv")

for (y in 2008:2009){
# Download from NOAA ftp 
#   y.la.list<-st.so.ca[st.so.ca$BEGIN<=y & st.so.ca$END>=y,]
#  for (s in 1:dim(y.la.list)[1]){
#    filename<-paste(sprintf("%06d",y.la.list[s,1]),"-",sprintf("%05d",y.la.list[s,2]),"-",y,".gz",sep="")
#    download.file(paste("ftp://ftp.ncdc.noaa.gov/pub/data/noaa/", y,"/",filename,sep=""), paste("/Users/mf/Documents/NCDC/SoCal Met/",filename,sep=""), method='wget') 
#    }

#  files.gz <- list.files("./SoCal Met",full.names=TRUE,pattern=".gz")
#      for(i in 1:length(files.gz)){
#       gunzip(files.gz[[i]],overwrite=TRUE)
#    }
  
# Extract data from downloaded files
# Need to define column widths, see ftp://ftp.ncdc.noaa.gov/pub/data/noaa/ish-format-document.pdf
  column.widths <- c(4, 6, 5, 4, 2, 2, 2, 2, 1, 6, 7, 5, 5, 5, 4, 3, 1, 1, 4, 1, 5, 1, 1, 1, 6, 1, 1, 1, 5, 1, 5, 1, 5, 1)
#stations <- as.data.frame(matrix(NA, length(files.gz),))
#names(stations) <- c("USAFID", "WBAN", "YR", "LAT","LONG", "ELEV")

  met.files <- list.files("./SoCal Met",full.names=TRUE,include.dirs = FALSE, recursive=FALSE)
  met.list<-vector('list',length(met.files))
    for (i in 1:length(met.files)) {
      if (file.info(met.files[i])$size>0){
      met.data <- read.fwf(met.files[i], column.widths)
      names(met.data) <- c("ID","USAFID", "WBAN", "year", "month","day", "hour", "min","srcflag", "lat", "lon",
                    "typecode","elev","callid","qcname","wind.dir", "wind.dir.qc","wind.type.code","wind.sp","wind.sp.qc",
                        "ceiling.ht","ceiling.ht.qc","ceiling.ht.method","sky.cond","vis.dist","vis.dist.qc","vis.var","vis.var.qc",
                            "temp","temp.qc", "dew.point","dew.point.qc","atm.press","atm.press.qc")
  # change 9999 to missing
      met.data$wind.dir<-ifelse(met.data$wind.dir==999,NA,met.data$wind.dir)
      met.data$wind.sp<-ifelse(met.data$wind.sp==9999,NA,met.data$wind.sp)
      met.data$ceiling.ht<-ifelse(met.data$ceiling.ht==99999,NA,met.data$ceiling.ht)
      met.data$vis.dist<-ifelse(met.data$vis.dist==999999,NA,met.data$vis.dist)
      met.data$temp<-ifelse(met.data$temp==9999,NA,met.data$temp)
      met.data$dew.point<-ifelse(met.data$dew.point==9999,NA,met.data$dew.point)
      met.data$atm.press<-ifelse(met.data$atm.press==99999,NA,met.data$atm.press)
  
  # conversions and scaling factors
      met.data$lat <- met.data$lat/1000
      met.data$lon <- met.data$lon/1000
      met.data$wind.sp <- met.data$wind.sp/10
      met.data$temp <- met.data$temp/10
      met.data$dew.point <- met.data$dew.point/10
      met.data$atm.press<- met.data$atm.press/10
  #drop some variables
      met.data<-subset(met.data, select=-c(ID,srcflag,typecode,callid,qcname))
  # take average of hours matching MISR overpass time
      met.data.misr.hrs<-met.data[met.data$hour %in% c(10,11,12),]
      met.data.misr.avg<- ddply(met.data.misr.hrs, .(month, day, year,lat,lon,USAFID,elev), summarise, temp=mean(temp,na.rm=TRUE),
                            dew.point=mean(dew.point,rm=TRUE), ceiling.ht=mean(ceiling.ht,na.rm=TRUE), wind.dir=mean(wind.dir,na.rm=TRUE),
                              wind.sp=mean(wind.sp,na.rm=TRUE), atm.press=mean(atm.press,na.rm=TRUE))

      met.list[[i]]<-met.data.misr.avg
    }
  else{ print("Zero file")
    }
}
}
met.08.09 <- do.call("rbind", met.list) 
# add projected coordinates
newcoords.met<-project(as.matrix(cbind(met.08.09$lon, met.08.09$lat)), proj=proj.albers)
met.08.09$x<-newcoords.met[,1]
met.08.09$y<-newcoords.met[,2]

# read/write
write.csv(met.08.09,"/Users/mf/Documents/MISR/Data/met_08_09.csv",row.names=FALSE)
met.08.09<-read.csv("/Users/mf/Documents/MISR/Data/met_08_09.csv")

# match MISR, AQS and STN by date and distance
# Take unique dates from MISR file

misr.days<-misr.08.09 %>% distinct(date2) 
misr.aqspm25.match.all<-vector('list',length(misr.days$date))
misr.aqspm10.match.all<-vector('list',length(misr.days$date))
misr.stn.match.all<-vector('list',length(misr.days$date))
met.aqspm25.match.all<-vector('list',length(misr.days$date))
met.aqspm10.match.all<-vector('list',length(misr.days$date))   
met.stn.match.all<-vector('list',length(misr.days$date)) 

for (i in 1:length(misr.days$date)){
  # Select data on MISR days
#   aqspm25.daily<-aqspm25.08.09.ss2[aqspm25.08.09.ss2$day %in% misr.days[i,]$day &
#                                      aqspm25.08.09.ss2$month %in% misr.days[i,]$month &
#                                      aqspm25.08.09.ss2$year %in% misr.days[i,]$year ,] 
  
  aqspm10.daily<-aqspm10.08.09.ss3[aqspm10.08.09.ss3$day %in% misr.days[i,]$day & 
                                     aqspm10.08.09.ss3$month %in% misr.days[i,]$month &
                                     aqspm10.08.09.ss3$year %in% misr.days[i,]$year,] 
  
#   stn.daily<-stn.08.09.ss[stn.08.09.ss$day %in% misr.days[i,]$day &
#                             stn.08.09.ss$month %in% misr.days[i,]$month &
#                             stn.08.09.ss$year %in% misr.days[i,]$year,] 
#   
  misr.daily<-misr.08.09[misr.08.09$day %in% misr.days[i,]$day &
                           misr.08.09$month %in% misr.days[i,]$month &
                           misr.08.09$year %in% misr.days[i,]$year,] 
                       
  met.daily<-met.08.09[met.08.09$day %in% misr.days[i,]$day &
                         met.08.09$month %in% misr.days[i,]$month & 
                         met.08.09$year %in% misr.days[i,]$year,]
  
  #distance matrices for each dataset
  #dist.misr.pm25<-rdist(cbind(misr.daily$x,misr.daily$y),cbind(aqspm25.daily$x,aqspm25.daily$y))
  dist.misr.pm10<-rdist(cbind(misr.daily$x,misr.daily$y),cbind(aqspm10.daily$x,aqspm10.daily$y))
  #dist.misr.stn<-rdist(cbind(misr.daily$x,misr.daily$y),cbind(stn.daily$x,stn.daily$y))
  #dist.pm25.met<-rdist(cbind(met.daily$x,met.daily$y),cbind(aqspm25.daily$x,aqspm25.daily$y))
  dist.pm10.met<-rdist(cbind(met.daily$x,met.daily$y),cbind(aqspm10.daily$x,aqspm10.daily$y))
  #dist.stn.met<-rdist(cbind(met.daily$x,met.daily$y),cbind(stn.daily$x,stn.daily$y))
  
  # take pixel that is smallest distance from AQS site (but within 5km)
  # identify row of distance matrix (misr pixel id), with smallest column is aqs site
  
  #misr.aqspm25.match.list<-vector('list',length(dist.misr.pm25[1,]))
  misr.aqspm10.match.list<-vector('list',length(dist.misr.pm10[1,]))
 # misr.stn.match.list<-vector('list',length(dist.misr.stn[1,]))
  
  # Identify MISR pixel closest to AQS site
  # Match MISR and AQS PM25
#   for (j in 1:length(dist.misr.pm25[1,])){ 
#     if (min(dist.misr.pm25[,j])<=5){
#       misr.aqspm25.match.list[[j]]<-data.frame(misr.daily[which.min(dist.misr.pm25[,j]),],aqspm25.daily[j,]) 
#     } 
#   }
#  
  # Match MISR and AQS PM10 
  for (j in 1:length(dist.misr.pm10[1,])){ 
    if (min(dist.misr.pm10[,j])<=5){
      misr.aqspm10.match.list[[j]]<-data.frame(misr.daily[which.min(dist.misr.pm10[,j]),],aqspm10.daily[j,]) # identifies misr pixel close to AQS PM10 site
    } 
  }
  
  # Match MISR and STN  
#   if (length(dist.misr.stn[1,])>0){
#     misr.stn.match.list<-vector('list',length(dist.misr.stn[1,]))
#       for (j in 1:length(dist.misr.stn[1,])){ 
#       if (min(dist.misr.stn[,j])<=5){
#       misr.stn.match.list[[j]]<-data.frame(misr.daily[which.min(dist.misr.stn[,j]),],stn.daily[j,]) # identifies misr pixel close to STN site
#       }
#     } 
#   }
  
 # misr.aqspm25.match.all[[i]] <- do.call("rbind", misr.aqspm25.match.list) 
  misr.aqspm10.match.all[[i]] <- do.call("rbind", misr.aqspm10.match.list) 
 # misr.stn.match.all[[i]] <- do.call("rbind", misr.stn.match.list) 
  
  # now match AQS site with closest NOAA met site
 # met.pm25.match.list<-vector('list',length(dist.pm25.met[1,]))
  met.pm10.match.list<-vector('list',length(dist.pm10.met[1,]))
 # met.stn.match.list<-vector('list',length(dist.stn.met[1,]))
  
  # match AQS PM25 with met
#   for (j in 1:length(dist.pm25.met[1,])){ 
#     if (min(dist.pm25.met[,j])<=25){
#       met.pm25.match.list[[j]]<-data.frame(met.daily[which.min(dist.pm25.met[,j]),],aqspm25.daily[j,]) 
#     }
#   }
  # match AQS PM10 with met
  for (j in 1:length(dist.pm10.met[1,])){ 
    if (min(dist.pm10.met[,j])<=25){
      met.pm10.match.list[[j]]<-data.frame(met.daily[which.min(dist.pm10.met[,j]),],aqspm10.daily[j,]) 
    }
  }
  # match STN with met
#   if (length(dist.stn.met[1,])>0){
#     for (j in 1:length(dist.stn.met[1,])){ 
#       if (min(dist.stn.met[,j])<=25){
#       met.stn.match.list[[j]]<-data.frame(met.daily[which.min(dist.stn.met[,j]),],stn.daily[j,]) 
#       }
#     }
#   }
 # met.aqspm25.match.all[[i]] <- do.call("rbind", met.pm25.match.list)
  met.aqspm10.match.all[[i]] <- do.call("rbind", met.pm10.match.list)
 # met.stn.match.all[[i]] <- do.call("rbind", met.stn.match.list)
}

misr.aqspm25 <- do.call("rbind", misr.aqspm25.match.all)
write.csv(misr.aqspm25,"/Users/mf/Documents/MISR/Data/misr_aqspm25.csv",row.names=FALSE)

misr.aqspm10 <- do.call("rbind", misr.aqspm10.match.all)
write.csv(misr.aqspm10,"/Users/mf/Documents/MISR/Data/misr_aqspm10_new.csv",row.names=FALSE)

misr.stn <- do.call("rbind", misr.stn.match.all)
write.csv(misr.stn,"/Users/mf/Documents/MISR/Data/misr_stn.csv",row.names=FALSE)

aqspm25.met <- do.call("rbind", met.aqspm25.match.all)
write.csv(aqspm25.met, "/Users/mf/Documents/MISR/Data/aqspm25_met.csv",row.names=FALSE)

aqspm10.met <- do.call("rbind", met.aqspm10.match.all)
write.csv(aqspm10.met, "/Users/mf/Documents/MISR/Data/aqspm10_met_new.csv",row.names=FALSE)

aqsstn.met <- do.call("rbind", met.stn.match.all)
write.csv(aqsstn.met, "/Users/mf/Documents/MISR/Data/aqsstn_met.csv",row.names=FALSE)

# merge with met
misr.aqspm25.met<-join(misr.aqspm25, aqspm25.met, by=c('AQS_SITE_ID','month','day','year'))
write.csv(misr.aqspm25.met, "/Users/mf/Documents/MISR/Data/misr_aqspm25_met.csv")

misr.aqspm10.met<-join(misr.aqspm10, aqspm10.met, by=c('AQS_SITE_ID','month','day','year'))
write.csv(misr.aqspm10.met, "/Users/mf/Documents/MISR/Data/misr_aqspm10_met_new.csv")

misr.aqsstn.met<-join(misr.stn, aqsstn.met, by=c('AQS_site_ID','month','day','year'))
write.csv(misr.aqsstn.met, "/Users/mf/Documents/MISR/Data/misr_aqsstn_met.csv")



