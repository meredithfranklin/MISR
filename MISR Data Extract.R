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
library(dplyr) # for easy merging
library(sas7bdat) # for reading SAS file formats
library(fields) # for spatial functions
library(proj4) # for map projections


###### MISR NetCDF ######
# Create list of filenames in directory
setwd("/Users/mf/Documents/MISR/Data")
misr.files <- list.files("./",pattern="*_LM_4p4km*",full.names=FALSE)

# Extract data
misr.list<-vector('list',length(misr.files))
  for(i in 1:length(misr.files)) { 
    dat<-open.ncdf(misr.files[i])
    lat<-get.var.ncdf(dat, "Latitude")
    lon<-get.var.ncdf(dat, "Longitude")
    julian<-get.var.ncdf(dat, "Julian")
    AOD<-get.var.ncdf(dat,"RegBestEstimateSpectralOptDepth")
    AODsmall<-get.var.ncdf(dat,"RegBestEstimateSpectralOptDepthFraction_Small")
    AODmed<-get.var.ncdf(dat,"RegBestEstimateSpectralOptDepthFraction_Medium")
    AODlarge<-get.var.ncdf(dat,"RegBestEstimateSpectralOptDepthFraction_Large")
    AODnonspher<-get.var.ncdf(dat,"RegBestEstimateSpectralOptDepthFraction_Nonsphere")
    AOD.dat<-data.frame(lat=lat,lon=lon,julian=julian,AOD=AOD,AODsmall=AODsmall,AODmed=AODmed,AODlarge=AODlarge,AODnonspher=AODnonspher)
    misr.list[[i]]<-AOD.dat
}
misr.08.09<-do.call("rbind", misr.list)

# Convert Julian dates, create month day year variables for matching with surface measures
misr.08.09$date<- chron(misr.08.09$julian, origin=c(month=11, day=24, year= -4713))
misr.08.09$year<-years(misr.08.09$date)
misr.08.09$month<-as.numeric(months(misr.08.09$date))
misr.08.09$day<-as.numeric(days(misr.08.09$date))

# Convert lat and lon into planar x and y (California projection)
proj.albers<-"+proj=aea +lat_1=34.0 +lat_2=40.5 +lon_0=-120.0 +x_0=0 +y_0=-4000000 +ellps=GRS80 +datum=NAD83 +units=km"
newcoords.misr<-project(as.matrix(cbind(misr.08.09$lon, misr.08.09$lat)), proj=proj.albers)
misr.08.09$x<-newcoords.misr[,1]
misr.08.09$y<-newcoords.misr[,2]

write.csv(misr.08.09,"misr.08.09.csv")

# Take monthly averages for matching with ICV surface data
misr.08.09.monthly <- ddply(misr.08.09, .(lat,lon,month), summarise, AOD.month=mean(AOD),
                        AODsmall.month=mean(AODsmall), AODmed.month=mean(AODmed), 
                        AODlarge.month=mean(AODlarge),AODnonsph.month=mean(AODnonspher))
write.csv(misr.08.09.monthly,"misr.08.09.monthly.csv")


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

write.csv(AQS.08.09.ss,"AQS.08.09.ss.csv")
write.csv(AQS.08.09,"AQS.08.09.csv")

##### Daily NCDC data #####
# Station Information
#file <- "ftp://ftp.ncdc.noaa.gov/pub/data/noaa/isd-history.csv"
#download.file(file, "isd-history.csv")
st <- read.csv("/Users/mf/Documents/NCDC/isd-history.csv") 
st.ca<-st[st$CTRY=="US" & st$STATE=="CA",]
st.ca$BEGIN <- as.numeric(substr(st.ca$BEGIN, 1, 4))
st.ca$END <- as.numeric(substr(st.ca$END, 1, 4))

st.so.ca<-st.ca[st.ca$LAT>=33.2 & st.ca$LAT<=35,]
st.so.ca<-st.so.ca[st.so.ca$LON<= -120 & st.so.ca$LON>= -117,]
st.so.ca<-st.so.ca[complete.cases(st.so.ca),]

# Pick WBAN 722880 (Burbank)
la.weather<-st.so.ca[st.so.ca$USAF %in% 722880,]
files.gz <- list.files("./Burbank Met",full.names=TRUE,pattern=".gz")
for (y in 2008:2009){
  y.la.list<-la.weather[la.weather$BEGIN<=y & la.weather$END>=y,]
  for (s in 1:dim(y.la.list)[1])
    filename<-paste(sprintf("%06d",y.la.list[s,1]),"-",sprintf("%05d",y.la.list[s,2]),"-",y,".gz",sep="")
  download.file(paste("ftp://ftp.ncdc.noaa.gov/pub/data/noaa/",y,"/",filename,sep=""), paste("/Users/mf/Documents/NCDC/Burbank Met/",filename,sep=""), method='wget') 
  for(i in 1:length(files)){
    gunzip(files.gz[[i]])
  }
}

column.widths <- c(4, 6, 5, 4, 2, 2, 2, 2, 1, 6, 7, 5, 5, 5, 4, 3, 1, 1, 4, 1, 5, 1, 1, 1, 6, 1, 1, 1, 5, 1, 5, 1, 5, 1)
stations <- as.data.frame(matrix(NA, length(files),6))
names(stations) <- c("USAFID", "WBAN", "YR", "LAT","LONG", "ELEV")

files <- list.files("./Burbank Met",full.names=TRUE)
for (i in 1:length(files)) {
  data <- read.fwf(files[i], column.widths)
  data <- data[, c(2:8, 10:11, 13, 16, 19, 29,31, 33)]
  names(data) <- c("USAFID", "WBAN", "YR", "M","D", "HR", "MIN", "LAT", "LONG", "ELEV","WIND.DIR", "WIND.SPD", "TEMP", "DEW.POINT","ATM.PRES")
  data$LAT <- data$LAT/1000
  data$LONG <- data$LONG/1000
  data$WIND.SPD <- data$WIND.SPD/10
  data$TEMP <- data$TEMP/10
  data$DEW.POINT <- data$DEW.POINT/10
  data$ATM.PRES <- data$ATM.PRES/10
  write.csv(data, file = paste(files[i],".csv", sep = ""), row.names = FALSE)
  stations[i, 1:3] <- data[1, 1:3]
  stations[i, 4:6] <- data[1, 8:10]
}

files.csv <- list.files("./Burbank Met/Processed",full.names=TRUE)
data.daily.all<-NULL
for (i in 1:length(files.csv)) {
  data<-read.csv(files.csv[i])
  data$TEMP<-ifelse(data$TEMP==999.9,NA,data$TEMP)
  data$DEW.POINT<-ifelse(data$DEW.POINT==999.9,NA,data$DEW.POINT)
  data$WIND.DIR<-ifelse(data$WIND.DIR==999.9,NA,data$WIND.DIR)
  data$WIND.SPD<-ifelse(data$WIND.SPD==999.9,NA,data$WIND.SPD)
  data$ATM.PRES<-ifelse(data$ATM.PRES>2000,NA,data$ATM.PRES)
  data.daily<-ddply(data,.(YR,M,D),summarize,temp=mean(TEMP,na.rm=TRUE),
                    dp.temp=mean(DEW.POINT,na.rm=TRUE),wd.sp=mean(WIND.SPD,na.rm=TRUE),
                    wd.dir=mean(WIND.DIR,na.rm=TRUE),atm.pres=mean(ATM.PRES,na.rm=TRUE))
  data.daily.all <- rbind(data.daily.all, data.daily)
}
data.daily.all$date<-as.Date(ISOdate(data.daily.all$YR,data.daily.all$M, data.daily.all$D))
write.csv(data.daily.all,"/Users/mf/Documents/NCDC/LAMet.csv") 



# match MISR and AQS by date and distance
# Take unique dates from MISR file
misr.days<-misr.08.09 %>% distinct(round(julian,digits=0))
MISR.AQS.match.all<-vector('list',length(AQS.08.09.ss$Date))
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

MISR.AQS <- do.call("rbind", MISR.AQS.match.all)
write.csv(MISR.AQS,"/Users/mf/Documents/MISR/Data/MISR.AQS.csv")

# MISR retrievals near aeronet Table mountain site
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


# ICV data
#ICV<-read.sas7bdat("/Users/mf/Documents/AQS/STN/seasonal4wkdata_no2wk_aeavg_all.sas7bdat")
ICVnew<-read.sas7bdat("/Volumes/Projects/CHSICV/MergeDatasets/mergeddata.sas7bdat")

# Keep relevant variables and export to csv
ICV.ss<-ICV[,c(1,5:9,302,317:318,358:361,365:373)]
ICVnew.ss<-ICVnew[,c(2:4,7,10:11,31:32,48:50,61:63,100,135,1262,1427,1435:1436,1438:1439,1451)]
ICVnew.ss$date.start<- chron(ICVnew.ss$startdate, origin=c(month=1, day=1, year= 1960))
ICVnew.ss$date.end<- chron(ICVnew.ss$enddate, origin=c(month=1, day=1, year= 1960))
ICVnew.ss$duration<-ICVnew.ss$date.end-ICVnew.ss$date.start
ICVnew.ss$startmonth<-month.day.year(unclass(ICVnew.ss$date.start))$month
ICVwarm<-ICVnew.ss[ICVnew.ss$season=="warm",]
ICVwarm.fine<-ICVwarm[!is.na(ICVwarm$Fine),]
ICVcool<-ICVnew.ss[ICVnew.ss$season=="cool",]
counts.ICV <- ddply(ICVnew.ss, .(ICVnew.ss$startmonth), nrow)
glendora<-ICVnew.ss[ICVnew.ss$town=="GL",]


