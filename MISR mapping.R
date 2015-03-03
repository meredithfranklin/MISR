##############################################
# MISR 08/09 4km data visualization
# December 2014, February 2015
# Meredith Franklin
##############################################
library(RgoogleMaps) 
library(RColorBrewer) 
AQS.08.09<-read.
proj.albers<-"+proj=aea +lat_1=34.0 +lat_2=40.5 +lon_0=-120.0 +x_0=0 +y_0=-4000000 +ellps=GRS80 +datum=NAD83 +units=km"
# Project the map
CA <- data.frame(map("state","california", plot=FALSE)[c("x","y")])
#project the map coordinates
newcoordsCA<-project(CA, proj=proj.albers)

# MISR data
misr.08.09<-read.csv("/Users/mf/Documents/MISR/Data/misr.08.09.csv")
misr.08.09.monthly<-read.csv("/Users/mf/Documents/MISR/Data/misr.08.09.monthly.csv")
# AQS data
AQS.08.09.ss<-read.csv("/Users/mf/Documents/AQS/PM25/AQS.08.09.ss.csv")


# pick a particular date
misr.05.07.08<-misr.08.09[misr.08.09$year==2008 & misr.08.09$month==5 & misr.08.09$day==7,]
aqs.05.07.08<-AQS.08.09.ss[AQS.08.09.ss$year==2008&AQS.08.09.ss$month==5&AQS.08.09.ss$day==7,]
#Create a function to generate a continuous color palette
rbPal <- colorRampPalette(tim.colors(32))
AOD.breaks <-quantile(misr.05.07.08$AOD, seq(0,1,1/10))
col.ramp <-rbPal(length(AOD.breaks))  
misr.05.07.08$Col <- rbPal(10)[as.numeric(cut(misr.05.07.08$AOD,
                                               breaks = quantile(misr.05.07.08$AOD, seq(0,1,1/10))))]
map <- GetMap(center=c(34.42,  -117.8), maptype='satellite', zoom=8)
PlotOnStaticMap(map,aqs.05.07.08$SITE_LATITUDE,aqs.05.07.08$SITE_LONGITUDE, cex=1.5,pch=19,col="black")


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

