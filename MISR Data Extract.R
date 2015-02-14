##############################################
# MISR 08/09 4km data extraction and analysis
# Aeronet data from http://aeronet.gsfc.nasa.gov/
# AQS Sites
# ICV Sites
# December 2014
# Meredith Franklin
##############################################

library(ncdf) # for reading netcdf file formats
library(date) # for converting julian dates
library(chron) # for converting julian dates
library(plyr) 
#library(MASS)
library(RgoogleMaps) # mapping
library(RColorBrewer) 
library(sas7bdat) # for reading SAS file formats
library(fields) # for spatial functions

setwd("/Users/mf/Documents/MISR/Data")

###### MISR NetCDF ######
dat08 <- open.ncdf('MISR_AS_AEROSOL_LM_4p4km_Year_2008.nc')
dat09 <- open.ncdf('MISR_AS_AEROSOL_LM_4p4km_Year_2009.nc')


# Extract 2008 data
lat<-get.var.ncdf(dat08, "Latitude")
lon<-get.var.ncdf(dat08, "Longitude")
julian<-get.var.ncdf(dat08, "Julian")
AOD<-get.var.ncdf(dat08,"RegBestEstimateSpectralOptDepth")
AODsmall<-get.var.ncdf(dat08,"RegBestEstimateSpectralOptDepthFraction_Small")
AODmed<-get.var.ncdf(dat08,"RegBestEstimateSpectralOptDepthFraction_Medium")
AODlarge<-get.var.ncdf(dat08,"RegBestEstimateSpectralOptDepthFraction_Large")
AODnonspher<-get.var.ncdf(dat08,"RegBestEstimateSpectralOptDepthFraction_Nonsphere")
AOD.dat.08<-data.frame(lat=lat,lon=lon,julian=julian,AOD=AOD,AODsmall=AODsmall,AODmed=AODmed,AODlarge=AODlarge,AODnonspher=AODnonspher)
# Extract 2009 data
lat<-get.var.ncdf(dat09, "Latitude")
lon<-get.var.ncdf(dat09, "Longitude")
julian<-get.var.ncdf(dat09, "Julian")
AOD<-get.var.ncdf(dat09,"RegBestEstimateSpectralOptDepth")
AODsmall<-get.var.ncdf(dat09,"RegBestEstimateSpectralOptDepthFraction_Small")
AODmed<-get.var.ncdf(dat09,"RegBestEstimateSpectralOptDepthFraction_Medium")
AODlarge<-get.var.ncdf(dat09,"RegBestEstimateSpectralOptDepthFraction_Large")
AODnonspher<-get.var.ncdf(dat09,"RegBestEstimateSpectralOptDepthFraction_Nonsphere")
AOD.dat.09<-data.frame(lat=lat,lon=lon,julian=julian,AOD=AOD,AODsmall=AODsmall,AODmed=AODmed,AODlarge=AODlarge, AODnonspher=AODnonspher)
# Merge 2008-2009
AOD.dat.08.09<-rbind(AOD.dat.08,AOD.dat.09)

AOD.dat.08.09$date<- chron(AOD.dat.08.09$julian, origin=c(month=1, day=1, year= -4712))
AOD.dat.08.09$date<-dates(AOD.dat.08.09$date)
AOD.dat.08.09$month<-month.day.year(unclass(AOD.dat.08.09$date))$month
AOD.dat.08.09$day<-month.day.year(unclass(AOD.dat.08.09$date))$day
AOD.dat.08.09$year2<-month.day.year(unclass(AOD.dat.08.09$date))$year

AOD.dat.july<-AOD.08.monthly[AOD.08.monthly$month==7,]
AOD.dat.aug<-AOD.08.monthly[AOD.08.monthly$month==8,]

AOD.dat.09$date<- chron(AOD.dat.09$julian, origin=c(month=1, day=1, year= -4712))
AOD.dat.09$date<-dates(AOD.dat.09$date)
AOD.dat.09$month<-month.day.year(unclass(AOD.dat.09$date))$month
AOD.dat.09$day<-month.day.year(unclass(AOD.dat.09$date))$day

AOD.09.monthly <- ddply(AOD.dat.09, .(lat,lon,month), summarise, AOD.month=mean(AOD),
                        AODsmall.month=mean(AODsmall), AODmed.month=mean(AODmed), 
                        AODlarge.month=mean(AODlarge),AODnonsph.month=mean(AODnonspher))

AOD.dat.jul.09<-AOD.09.monthly[AOD.09.monthly$month==7,]
AOD.dat.aug.09<-AOD.dat.09[AOD.dat.09$month==8,]
AOD.dat.sep.09<-AOD.09.monthly[AOD.09.monthly$month==9,]
AOD.dat.oct.09<-AOD.09.monthly[AOD.09.monthly$month==10,]
AOD.dat.sep.05.09<-AOD.dat.09[AOD.dat.09$month==9 & AOD.dat.09$day==5,]
# remove outliers
# AOD.dat.aug.09<-AOD.dat.aug.09[AOD.dat.aug.09$AOD.month<0.2,]

# MISR retrievals near aeronet site
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

# AQS data
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

# Mapping MISR
#Create a function to generate a continuous color palette
rbPal <- colorRampPalette(tim.colors(32))
AOD.breaks <-quantile(AOD.dat.sep.09$AOD.month, seq(0,1,1/10))
col.ramp <-rbPal(length(AOD.breaks))  
AOD.dat.sep.09$Col <- rbPal(10)[as.numeric(cut(AOD.dat.sep.09$AOD.month,
                                breaks = quantile(AOD.dat.sep.09$AOD.month, seq(0,1,1/10))))]
zoom <- min(MaxZoom(range(AOD.dat.sep.09$lat), range(AOD.dat.sep.09$lon)))
map <- GetMap(center=c(34.42,  -117.8), maptype='satellite', zoom=7)
PlotOnStaticMap(map,AOD.dat.sep.09$lat,AOD.dat.sep.09$lon, cex=0.5,pch=15,col=AOD.dat.sep.09$Col)
PlotOnStaticMap(map,lat=34.380, lon= -117.680, cex=2,pch="+", col='purple',add=TRUE)
#PlotOnStaticMap(map,AOD.dat.jul.09$lat,AOD.dat.jul.09$lon, cex=2.5,pch=19,col='black',add=TRUE)
#PlotOnStaticMap(map,AOD.dat.aug.09$lat,AOD.dat.aug.09$lon, cex=2.5,pch=19,col='green',add=TRUE)
#PlotOnStaticMap(map,AOD.dat.sep.09$lat,AOD.dat.sep.09$lon, cex=2.5,pch=19,col='blue',add=TRUE)
#PlotOnStaticMap(map,AOD.dat.oct.09$lat,AOD.dat.oct.09$lon, cex=2.5,pch=19,col='yellow',add=TRUE)
#PlotOnStaticMap(map,aqs.PM25.nodups$lat,aqs.PM25.nodups$lon,pch=19,cex=0.7,add=TRUE)
PlotOnStaticMap(map,ICVwarm.fine$lat,ICVwarm.fine$lon, cex=0.9,pch=21,bg="green",add=TRUE)
PlotOnStaticMap(map,MISR.Aeronet$lat, MISR.Aeronet$lon, pch="+", col='green',add=TRUE)
PlotOnStaticMap(map,MISR.Aeronet.08.09.merged.close$lat,MISR.Aeronet.08.09.merged.close$lon,pch="+",col="black",add=TRUE)
legend('bottomleft',legend=(round(AOD.breaks,digits=2)),fill=col.ramp,bty='o',box.col='white',cex=0.9,title="AOD Sep 09")

# Mapping AQS
rbPal <- colorRampPalette(tim.colors(32))
PM.breaks <-quantile(aqs.PM25.09.09$PM25, seq(0,1,1/10))
col.ramp <-rbPal(length(PM.breaks))  
aqs.PM25.09.09$Col <- rbPal(10)[as.numeric(cut(aqs.PM25.09.09$PM25,breaks = quantile(aqs.PM25.09.09$PM25, seq(0,1,1/10))))]
zoom <- min(MaxZoom(range(aqs.PM25.09.09$lat), range(aqs.PM25.09.09$lon)))
map <- GetMap(center=c(34.30, -118.500), maptype='terrain', zoom=zoom)
#PlotOnStaticMap(map,aqs.PM25.09.09$lat,aqs.PM25.09.09$lon, cex=1.5,pch=21,bg=aqs.PM25.09.09$Col)
PlotOnStaticMap(map,ICVnew.ss$lat,ICVnew.ss$lon, cex=0.9,pch=21,bg="red")
#PlotOnStaticMap(map,lat=34.380, lon= -117.680, cex=1.6,pch="+", col='purple',add=TRUE)
#PlotOnStaticMap(map,aqs.PM25.nodups$lat,aqs.PM25.nodups$lon,pch=19,cex=0.7,add=TRUE)
legend('bottomleft',legend=(round(PM.breaks,digits=2)),fill=col.ramp,bty='o',box.col='white',cex=0.9,title="PM25 Sep 09")

# Mapping ICV
rbPal <- colorRampPalette(tim.colors(32))
ICV.breaks <-quantile(ICVwarm.fine$Fine, seq(0,1,1/10))
col.ramp <-rbPal(length(ICV.breaks))  
ICVwarm.fine$Col <- rbPal(10)[as.numeric(cut(ICVwarm.fine$Fine,breaks = quantile(ICVwarm.fine$Fine, seq(0,1,1/10))))]
zoom <- min(MaxZoom(range(ICVwarm.fine$lat), range(ICVwarm.fine$lon)))
map <- GetMap(center=c(34.12552,  -117.8), maptype='terrain', zoom=11)
#PlotOnStaticMap(map,aqs.PM25.09.09$lat,aqs.PM25.09.09$lon, cex=1.5,pch=21,bg=aqs.PM25.09.09$Col)
PlotOnStaticMap(map,ICVwarm.fine$lat,ICVwarm.fine$lon, cex=0.9,pch=21,bg=ICVwarm.fine$Col)
PlotOnStaticMap(map,AOD.dat.sep.09$lat,AOD.dat.sep.09$lon, cex=2.5,pch=15,col="grey",add=TRUE)
#PlotOnStaticMap(map,lat=34.380, lon= -117.680, cex=1.6,pch="+", col='purple',add=TRUE)
#PlotOnStaticMap(map,aqs.PM25.nodups$lat,aqs.PM25.nodups$lon,pch=19,cex=0.7,add=TRUE)
legend('bottomleft',legend=(round(ICV.breaks,digits=2)),fill=col.ramp,bty='o',box.col='white',cex=0.9,title="PM25 summer 2009")

# Define number of colours to be used in plot
nclr <- 10
# Define colour palette to be used
plotclr <- brewer.pal(nclr,YlOrRd)
# Define colour intervals and colour code variable for plotting
class <- classIntervals(aqs.PM25.09.09$PM25, nclr, style = "quantile")
colcode <- findColours(class, plotclr)
zoom <- min(MaxZoom(range(aqs.PM25.09.09$lat), range(aqs.PM25.09.09$lon)))
map <- GetMap(center=c(mean(aqs.PM25.09.09$lat), mean(aqs.PM25.09.09$lon)), maptype='hybrid', zoom=7)
PlotOnStaticMap(map,aqs.PM25.09.09$lat,aqs.PM25.09.09$lon, cex=3,pch=19,col=colcode)
legend("bottomleft", legend = names(attr(colcode, "table")), fill = attr(colcode, "palette"), cex = 1,bty='o',box.col='white',title="AOD Sep 5/09")


# Save Map
png("NYCGoogleMap.png",type="cairo-png")
# Add points
tmp <- PlotOnStaticMap(MyMap, lat = c(40.702147,40.711614,40.718217), lon = c(-74.015794,-74.012318,-73.998284), cex=1.5,pch=20, col=c('red', 'blue', 'green'), add=FALSE)
# Add lines:
PlotOnStaticMap(MyMap, lat = c(40.702147,40.711614,40.718217), lon = c(-74.015794,-74.012318,-73.998284), lwd=1.5, col=c('red', 'blue', 'green'), FUN = lines, add=TRUE)
dev.off()


