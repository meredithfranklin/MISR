##############################################
# MISR 08/09 4km data extraction and analysis
# Aeronet data from http://aeronet.gsfc.nasa.gov/
# AQS Sites Monthly
# AQS Daily http://www.epa.gov/ttn/airs/airsaqs/detaildata/downloadaqsdata.htm
# ICV Sites
# December 2014, February 2015
# Meredith Franklin
##############################################

library(ncdf) # for reading netcdf file formats
library(date) # for converting julian dates
library(chron) # for converting julian dates
library(plyr) # for easy merging
library(sas7bdat) # for reading SAS file formats
library(fields) # for spatial functions

setwd("/Users/mf/Documents/MISR/Data")

###### MISR NetCDF ######
# Create list of filenames in directory
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
AOD.dat.08.09<-do.call("rbind", misr.list)

# Convert Julian dates, create month day year variables for matching with ICV
AOD.dat.08.09$date<- chron(AOD.dat.08.09$julian, origin=c(month=11, day=24, year= -4713))
AOD.dat.08.09$year<-years(AOD.dat.08.09$date)
AOD.dat.08.09$month<-as.numeric(months(AOD.dat.08.09$date))
AOD.dat.08.09$day<-as.numeric(days(AOD.dat.08.09$date))

write.csv(AOD.dat.08.09,"AOD.dat.08.09.csv")

# Take monthly averages for matching with ICV data
AOD.monthly <- ddply(AOD.dat.08.09, .(lat,lon,month), summarise, AOD.month=mean(AOD),
                        AODsmall.month=mean(AODsmall), AODmed.month=mean(AODmed), 
                        AODlarge.month=mean(AODlarge),AODnonsph.month=mean(AODnonspher))
write.csv(AOD.monthly,"AOD.dat.08.09.monthly.csv")


# Daily AQS data
setwd("/Users/mf/Documents/AQS/PM25")
aqs.files <- list.files("./",pattern=".txt",full.names=FALSE)

# Extract data
aqs.list<-vector('list',length(aqs.files))
for(i in 1:length(aqs.files)) { 
  dat<-read.table(aqs.files[i], sep="|",header=FALSE,skip=2)
  header<-read.table(aqs.files[i],sep="|",header=TRUE,row.names=NULL,
                      check.names=FALSE,nrows=2,skip=1,comment.char="")
  colnames(dat)<-names(header)
  cbind(read.fwf(file = textConnection(as.character(dat$Year)), 
                 widths = c(4, 2, 3), colClasses = "character", 
                 col.names = c("Year2", "Month", "day")), 
        df[-1])
  aqs.PM25<-read.table("/Users/mf/Documents/Sunlight Cognition/Twins/Exposure Data/07a_PM25_hard_FINAL.txt",sep='\t',header=FALSE)
colnames(aqs.PM25)<-c("lon","lat","JulianDate","PM25")
dates<-read.csv("/Users/mf/Documents/Sunlight Cognition/Twins/Exposure Data/dates_SC.csv")
aqs.PM25<-merge(aqs.PM25,dates, by=c("JulianDate"),all=FALSE)
aqs.PM25.08.09<-aqs.PM25[aqs.PM25$Year>=2008&aqs.PM25$Year<=2009,]


aqs.PM25<-aqs.PM25[unique(c(aqs.PM25$lon, aqs.PM25$lat)),]
# Find unique subject locations and times for prediction
aqs.PM25.nodups<-aqs.PM25[!duplicated(aqs.PM25[c(1:2)]),]

aqs.PM25.09.09<-aqs.PM25[aqs.PM25$Year==2009&aqs.PM25$Month==9,]
aqs.PM25.09.09<-aqs.PM25.09.09[aqs.PM25.09.09$lon>=-120 & aqs.PM25.09.09$lon<= -117,]
aqs.PM25.09.09<-aqs.PM25.09.09[aqs.PM25.09.09$lat>=33.2 & aqs.PM25.09.09$lat<=35,]
aqs.PM25.09.09<-aqs.PM25.09.09[aqs.PM25.09.09$lon != -119.4869,] #Remove Catalina




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


