##############################################
# MISR 2008-2009 4km data extraction 
# AQS Daily http://www.epa.gov/airdata/ad_data_daily.html
# Aeronet data http://aeronet.gsfc.nasa.gov/
# ICV data (CHS study)
# December 2014, February 2015
# Meredith Franklin
##############################################

library(ncdf) # for reading netcdf file formats
library(date) # for converting julian dates
library(chron) # for converting julian dates
#library(plyr) # for easy merging/subsetting
library(dplyr) #for easy merging/subsetting
library(sas7bdat) # for reading SAS file formats
library(fields) # for spatial functions
library(proj4) # for map projections
library(R.utils) # decompressing NCDC data

###### MISR NetCDF ######
# Create list of filenames in directory
setwd("/Users/mf/Documents/MISR/Data")
misr.files <- list.files("./",pattern="*_LM_4p4km*",full.names=FALSE)

# Extract data: use RegBestEstimate for AOD, use RegLowestResid for fractions and SS albedo
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
    land.water.mask<-get.var.ncdf(dat,"AlgTypeFlag")#land=3 water=1
    AOD.dat<-data.frame(lat=lat,lon=lon,julian=julian,AOD=AOD,AODsmallfrac=AODsmallfrac,AODmedfrac=AODmedfrac,AODlargefrac=AODlargefrac,AODnonspher=AODnonspher,SSAlbedo=SSAlbedo)
    misr.list[[i]]<-AOD.dat
}
misr.08.09<-do.call("rbind", misr.list)

# Convert Julian dates, create month day year variables for matching with surface measures
misr.08.09$date<- chron(misr.08.09$julian, origin=c(month=11, day=24, year= -4713))

misr.08.09$year<-years(misr.08.09$date)
misr.08.09$month<-as.numeric(months(misr.08.09$date))
misr.08.09$day<-as.numeric(days(misr.08.09$date))
# multiply fraction by AOD to get AODsmall, AODmed, AODlarge
misr.08.09$AODsmall<-misr.08.09$AODsmallfrac*misr.08.09$AOD
misr.08.09$AODmed<-misr.08.09$AODmedfrac*misr.08.09$AOD
misr.08.09$AODlarge<-misr.08.09$AODlargefrac*misr.08.09$AOD

# Convert lat and lon into planar x and y (California projection)
proj.albers<-"+proj=aea +lat_1=34.0 +lat_2=40.5 +lon_0=-120.0 +x_0=0 +y_0=-4000000 +ellps=GRS80 +datum=NAD83 +units=km"
newcoords.misr<-project(as.matrix(cbind(misr.08.09$lon, misr.08.09$lat)), proj=proj.albers)
misr.08.09$x<-newcoords.misr[,1]
misr.08.09$y<-newcoords.misr[,2]

#write.csv(misr.08.09,"misr.08.09.csv")

# Take monthly averages for matching with ICV surface data
misr.08.09.monthly <- ddply(misr.08.09, .(lat,lon,month), summarise, AOD.month=mean(AOD),
                        AODsmall.month=mean(AODsmall), AODmed.month=mean(AODmed), 
                        AODlarge.month=mean(AODlarge),AODnonsph.month=mean(AODnonspher))
#write.csv(misr.08.09.monthly,"misr.08.09.monthly.csv")


##### Daily AQS data ######
setwd("/Users/mf/Documents/AQS/PM25")
aqs.files <- list.files("./",pattern=".csv",full.names=FALSE)

# Extract data
aqs.list<-vector('list',length(aqs.files))
for(i in 1:length(aqs.files)) { 
  dat<-read.csv(aqs.files[i],stringsAsFactors=FALSE)
# separate m/d/y from Date
  date<-strsplit(dat$Date,"/")
  dat$month<-as.numeric(sapply(date, "[[", 1) )
  dat$day<-as.numeric(sapply(date,"[[",2))
  dat$year<-as.numeric(sapply(date,"[[",3))+2000
  dat<-dat[,c(-10:-13)]
# reading national .txt files (no geographic reference)  
  #header<-read.table(aqs.files[i],sep="|",header=TRUE,row.names=NULL,
  #                   check.names=FALSE,nrows=2,skip=1,comment.char="")
  #colnames(dat)<-names(header)
  #dat<-cbind(read.fwf(file = textConnection(as.character(dat$Year)), 
  #               widths = c(4, 2, 3), colClasses = "character", 
  #               col.names = c("year2", "month", "day")),dat)
  aqs.list[[i]]<-dat
}

AQS.08.09 <- do.call("rbind", aqs.list) 

# Convert lat and lon into planar x and y (California projection)
proj.albers<-"+proj=aea +lat_1=34.0 +lat_2=40.5 +lon_0=-120.0 +x_0=0 +y_0=-4000000 +ellps=GRS80 +datum=NAD83 +units=km"
newcoords.aqs<-project(as.matrix(cbind(AQS.08.09$SITE_LONGITUDE, AQS.08.09$SITE_LATITUDE)), proj=proj.albers)
AQS.08.09$x<-newcoords.aqs[,1]
AQS.08.09$y<-newcoords.aqs[,2]

# Subset AQS data around LA area
AQS.08.09.ss<-AQS.08.09[AQS.08.09$SITE_LONGITUDE>=-120 & AQS.08.09$SITE_LONGITUDE<= -117,]
AQS.08.09.ss<-AQS.08.09.ss[AQS.08.09.ss$SITE_LATITUDE>=33.2 & AQS.08.09.ss$SITE_LATITUDE<=35,]
AQS.08.09.ss<-AQS.08.09.ss[AQS.08.09.ss$SITE_LONGITUDE != -119.4869,] #Remove Catalina

#write.csv(AQS.08.09.ss,"/Users/mf/Documents/MISR/Data/AQS.08.09.ss.csv")
#write.csv(AQS.08.09,"/Users/mf/Documents/MISR/Data/AQS.08.09.csv")
AQS.08.09.ss<-read.csv("/Users/mf/Documents/MISR/Data/AQS.08.09.ss.csv")

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
  y.la.list<-st.so.ca[st.so.ca$BEGIN<=y & st.so.ca$END>=y,]
  for (s in 1:dim(y.la.list)[1]){
    filename<-paste(sprintf("%06d",y.la.list[s,1]),"-",sprintf("%05d",y.la.list[s,2]),"-",y,".gz",sep="")
    download.file(paste("ftp://ftp.ncdc.noaa.gov/pub/data/noaa/", y,"/",filename,sep=""), paste("/Users/mf/Documents/NCDC/SoCal Met/",filename,sep=""), method='wget') 
}

  files.gz <- list.files("./SoCal Met",full.names=TRUE,pattern=".gz")
      for(i in 1:length(files.gz)){
       gunzip(files.gz[[i]])
}

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
  #data <- data[, c(2:8, 10:11, 13, 16, 19, 29,31, 33)]
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

met.08.09 <- do.call("rbind", met.list) 
# add projected coordinates
proj.albers<-"+proj=aea +lat_1=34.0 +lat_2=40.5 +lon_0=-120.0 +x_0=0 +y_0=-4000000 +ellps=GRS80 +datum=NAD83 +units=km"
newcoords.met<-project(as.matrix(cbind(met.08.09$lon, met.08.09$lat)), proj=proj.albers)
met.08.09$x<-newcoords.met[,1]
met.08.09$y<-newcoords.met[,2]

write.csv(met.08.09,"/Users/mf/Documents/MISR/Data/met.08.09.csv")
met.08.09<-read.csv("/Users/mf/Documents/MISR/Data/met.08.09.csv")


# match MISR and AQS by date and distance
# Take unique dates from MISR file
misr.days<-misr.08.09 %>% distinct(round(julian,digits=0))
MISR.AQS.match.all<-vector('list',length(AQS.08.09.ss$Date))
met.AQS.match.all<-vector('list',length(AQS.08.09.ss$Date))
                          
for (i in 1:length(misr.days$date)){
  aqs.daily<-AQS.08.09.ss[AQS.08.09.ss$day %in% misr.days[i,]$day & 
                            AQS.08.09.ss$month %in% misr.days[i,]$month &
                              AQS.08.09.ss$year %in% misr.days[i,]$year,]
  misr.daily<-misr.08.09[misr.08.09$day %in% misr.days[i,]$day & 
                           misr.08.09$month %in% misr.days[i,]$month &
                           misr.08.09$year %in% misr.days[i,]$year,]
  met.daily<-met.08.09[met.08.09$day %in% misr.days[i,]$day &
                         met.08.09$month %in% misr.days[i,]$month & 
                         met.08.09$year %in% misr.days[i,]$year,]
  #distance matrix
  dist<-rdist(cbind(misr.daily$x,misr.daily$y),cbind(aqs.daily$x,aqs.daily$y))
  dist.met<-rdist(cbind(met.daily$x,met.daily$y),cbind(aqs.daily$x,aqs.daily$y))
  
  # take pixel which is smallest distance from AQS site
  # identify row of distance matrix (misr pixel id), with smallest column is aqs site
  
  MISR.AQS.match.list<-vector('list',length(dist[1,]))
  for (j in 1:length(dist[1,])){ 
    if (min(dist[,j])<=5){
      MISR.AQS.match.list[[j]]<-data.frame(misr.daily[which.min(dist[,j]),],aqs.daily[j,]) # identifies misr pixel
    } 
    #print(min(dist[,j]))
    #print(cor(match$AOD,match$Daily.Mean.PM2.5.Concentration))
  }
  MISR.AQS.match.all[[i]] <- do.call("rbind", MISR.AQS.match.list) 

  met.AQS.match.list<-vector('list',length(dist[1,]))
  for (j in 1:length(dist[1,])){ 
      met.AQS.match.list[[j]]<-data.frame(met.daily[which.min(dist.met[,j]),],aqs.daily[j,]) # identifies misr pixel
    } 
    #print(min(dist[,j]))
    #print(cor(match$AOD,match$Daily.Mean.PM2.5.Concentration))
  }
  met.AQS.match.all[[i]] <- do.call("rbind", met.AQS.match.list)

}


MISR.AQS <- do.call("rbind", MISR.AQS.match.all)
write.csv(MISR.AQS,"/Users/mf/Documents/MISR/Data/MISR.AQS.csv")

# Add in met data to matched MISR AQS. Take met site with minimum distance
misr.aqs.days<-MISR.AQS %>% distinct(round(julian,digits=0))
MISR.AQS.met.match.all<-vector('list',length(met.08.09$year))
for (i in 1:length(MISR.AQS$date)){
  met.daily<-met.08.09[met.08.09$day %in% MISR.AQS[i,]$day & 
                         met.08.09$month %in% MISR.AQS[i,]$month &
                         met.08.09$year %in% MISR.AQS[i,]$year,]

  #distance matrix
  dist<-rdist(cbind(met.daily$x,met.daily$y),cbind(MISR.AQS$x,MISR.AQS$y))
  # take pixel which is smallest distance from AQS site
  # identify row of distance matrix (misr pixel id), with smallest column is aqs site
  
  MISR.AQS.match.list<-vector('list',length(dist[1,]))
  for (j in 1:length(dist[1,])){ 
    if (min(dist[,j])<=5){
      MISR.AQS.match.list[[j]]<-data.frame(misr.daily[which.min(dist[,j]),],aqs.daily[j,]) # identifies misr pixel
    } 
    #print(min(dist[,j]))
    #print(cor(match$AOD,match$Daily.Mean.PM2.5.Concentration))
  }
  MISR.AQS.match.all[[i]] <- do.call("rbind", MISR.AQS.match.list)
  
}

# ICV data
# ICV<-read.sas7bdat("/Users/mf/Documents/AQS/STN/seasonal4wkdata_no2wk_aeavg_all.sas7bdat")
ICV<-read.sas7bdat("/Volumes/Projects/CHSICV/Temp/TempRima/ICV Comparisons Working Group/icv2_seasonal_27jan15.sas7bdat")

# Create variables and export to csv
ICV$date.start<- chron(ICV$startdate, origin=c(month=1, day=1, year= 1960))
ICV$date.end<- chron(ICV$enddate, origin=c(month=1, day=1, year= 1960))
ICV$duration<-ICV$date.end-ICV$date.start

counts.ICV <- ddply(ICV, .(ICV$date.start), nrow)
glendora<-ICVnew.ss[ICVnew.ss$town=="GL",]

# Convert lat and lon into planar x and y (California projection)
proj.albers<-"+proj=aea +lat_1=34.0 +lat_2=40.5 +lon_0=-120.0 +x_0=0 +y_0=-4000000 +ellps=GRS80 +datum=NAD83 +units=km"
newcoords.icv<-project(as.matrix(cbind(ICV$lat, ICV$lon)), proj=proj.albers)
ICV$x<-newcoords.icv[,1]
ICV$y<-newcoords.icv[,2]

# Match MISR with ICV within month first take average of MISR between dates for each ICV
misr.days<-misr.08.09 %>% distinct(round(julian,digits=0))
MISR.ICV.match.all<-vector('list',length(ICV$Date))
for (i in 1:length(misr.days$date)){
  aqs.daily<-AQS.08.09.ss[AQS.08.09.ss$day %in% misr.days[i,]$day & 
                            AQS.08.09.ss$month %in% misr.days[i,]$month &
                            AQS.08.09.ss$year %in% misr.days[i,]$year,]
  misr.daily<-misr.08.09[misr.08.09$day %in% misr.days[i,]$day & 
                           misr.08.09$month %in% misr.days[i,]$month &
                           misr.08.09$year %in% misr.days[i,]$year,]
  #distance matrix
  dist<-rdist(cbind(misr.daily$x,misr.daily$y),cbind(aqs.daily$x,aqs.daily$y))
  # take pixel which is smallest distance from AQS site
  # identify row of distance matrix (misr pixel id), with smallest column is aqs site
  
  MISR.AQS.match.list<-vector('list',length(dist[1,]))
  for (j in 1:length(dist[1,])){ 
    if (min(dist[,j])<=5){
      MISR.AQS.match.list[[j]]<-data.frame(misr.daily[which.min(dist[,j]),],aqs.daily[j,]) # identifies misr pixel
    } 
    #print(min(dist[,j]))
    #print(cor(match$AOD,match$Daily.Mean.PM2.5.Concentration))
  }
  MISR.AQS.match.all[[i]] <- do.call("rbind", MISR.AQS.match.list)
  
}


# AERONET MISR retrievals near aeronet Table mountain site
MISR.Aeronet.08.09<-AOD.dat.08.09[AOD.dat.08.09$lat>34.35&AOD.dat.08.09$lat<34.45,]
MISR.Aeronet.08.09<-MISR.Aeronet.08.09[MISR.Aeronet.08.09$lon>= -117.75&MISR.Aeronet.08.09$lon<= -117.6,]
MISR.Aeronet.08.09$year<-ifelse(MISR.Aeronet.08.09$year2==8690,2008,
                                ifelse(MISR.Aeronet.08.09$year2==8691,2009,
                                       ifelse(MISR.Aeronet.08.09$year2==8692,2010,0)))

# Aeronet Table Mountain coords 34.380, -117.680
aeronet<-read.csv("./Aeronet/Aeronet_TableMt_lev20.csv")
aeronet$date<-as.Date(aeronet$date, "%m/%d/%y")
as.numeric.factor <- function(x) {as.numeric(levels(x))[x]}
# 2008 and 2009 data
aeronet.08.09<-aeronet[aeronet$date>="2008-01-01" & aeronet$date<="2009-12-31",]
aeronet.08.09$month<-as.numeric(format(aeronet.08.09$date, "%m"))
aeronet.08.09$day<-as.numeric(format(aeronet.08.09$date, "%d"))
aeronet.08.09$year<-as.numeric(format(aeronet.08.09$date, "%Y"))
aeronet.08.09$AOT558<-apply(cbind(as.numeric.factor(aeronet.08.09$AOT675),as.numeric.factor(aeronet.08.09$AOT440)),1,mean)
aeronet.08.09 <- aeronet.08.09[with(aeronet.08.09,order(year, month,day,time)),]


# monthly means
aeronet.08.09.daily <- ddply(aeronet.08.09, .(day,month,year), summarise, 
                             AOT558.daily=mean(AOT558),
                             AOT675.daily=mean(as.numeric.factor(AOT675),na.rm=TRUE),
                             AOT500.daily=mean(as.numeric.factor(AOT500),na.rm=TRUE),
                             AOT440.daily=mean(as.numeric.factor(AOT440),na.rm=TRUE),
                             AOT870.daily=mean(as.numeric.factor(AOT870),na.rm=TRUE),
                             AOT558.sd=sd(AOT558),
                             AOT675.sd=sd(as.numeric.factor(AOT675),na.rm=TRUE),
                             AOT500.sd=sd(as.numeric.factor(AOT500),na.rm=TRUE),
                             AOT440.sd=sd(as.numeric.factor(AOT440),na.rm=TRUE),
                             AOT870.sd=sd(as.numeric.factor(AOT870),na.rm=TRUE))

aeronet.08.09.daily$date <- (paste(aeronet.08.09.daily$month , aeronet.08.09.daily$day, aeronet.08.09.daily$year, sep = "/" ))
aeronet.08.09.daily <- aeronet.08.09.daily[with(aeronet.08.09.daily,order(year, month,day)),]
plot(aeronet.08.09.daily$AOT500.daily,aeronet.08.09.daily$AOT500.sd)


# Merge MISR and Aeronet
MISR.Aeronet.08.09.merged<-merge(aeronet.08.09.daily,MISR.Aeronet.08.09,by=c("year","month","day"))
#Sepect closest point
#MISR.Aeronet.08.09.merged.close<-MISR.Aeronet.08.09.merged[MISR.Aeronet.08.09.merged$lat>34.37&MISR.Aeronet.08.09.merged$lat<34.39,]

#Compare
plot(MISR.Aeronet.08.09.merged$AOT558.daily,MISR.Aeronet.08.09.merged$AOD,ylab="MISR AOD",xlab="Aeronet AOT 558nm (interpolated)")
lm558<-lm(AOD~AOT558.daily,data=MISR.Aeronet.08.09.merged)
abline(lm558)

lm558small<-lm(AODsmall~AOT558.daily,data=MISR.Aeronet.08.09.merged)
lm558med<-lm(AODmed~AOT558.daily,data=MISR.Aeronet.08.09.merged)
lm558large<-lm(AODlarge~AOT558.daily,data=MISR.Aeronet.08.09.merged)

plot(MISR.Aeronet.08.09.merged$AOT400.daily,MISR.Aeronet.08.09.merged$AOD,ylab="MISR AOD",xlab="Aeronet AOD 400nm (interpolated)")
lm440<-lm(AOD~AOT440.daily,data=MISR.Aeronet.08.09.merged)

plot(MISR.Aeronet.08.09.merged$AOT500.daily,MISR.Aeronet.08.09.merged$AOD,ylab="MISR AOD",xlab="Aeronet AOD 500nm")
lm500<-lm(AOD~AOT500.daily,data=MISR.Aeronet.08.09.merged)

plot(MISR.Aeronet.08.09.merged$AOT675.daily,MISR.Aeronet.08.09.merged$AOD,ylab="MISR AOD",xlab="Aeronet AOD 675nm")
lm675<-lm(AOD~AOT675.daily,data=MISR.Aeronet.08.09.merged)

plot(MISR.Aeronet.08.09.merged$AOT870.daily,MISR.Aeronet.08.09.merged$AOD,ylab="MISR AOD",xlab="Aeronet AOD 870nm",xlim=c(0,0.1),ylim=c(0,0.25))
lm870<-lm(AOD~AOT870.daily,data=MISR.Aeronet.08.09.merged)

library(lattice)
xyplot(AOD ~ AOT558.daily | lat, data = MISR.Aeronet.08.09.merged, main="MISR AOD vs Aeronet 558nm (interpolated)",panel = function(x,  y, ...) {
  panel.xyplot(x, y, ...)
  panel.lmline(x, y, ...)})

xyplot(AOD ~ AOT675.daily | lat, data = MISR.Aeronet.08.09.merged, main="MISR AOD vs Aeronet 675nm",panel = function(x,  y, ...) {
  panel.xyplot(x, y, ...)
  panel.lmline(x, y, ...)})

xyplot(AOD ~ AOT500.daily | lat, data = MISR.Aeronet.08.09.merged, main="MISR AOD vs Aeronet 500nm",panel = function(x,  y, ...) {
  panel.xyplot(x, y, ...)
  panel.lmline(x, y, ...)})
xyplot(AOD ~ AOT870.daily | lat, data = MISR.Aeronet.08.09.merged, main="MISR AOD vs Aeronet 870nm",panel = function(x,  y, ...) {
  panel.xyplot(x, y, ...)
  panel.lmline(x, y, ...)})

# Correlation matrices

cor(MISR.Aeronet.08.09.merged[,c(4:8,17:21)])




