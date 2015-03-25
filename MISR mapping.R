##############################################
# MISR 08/09 4km data visualization
# December 2014, February 2015
# Meredith Franklin
##############################################
library(RgoogleMaps) 
library(RColorBrewer) 
setwd("/Users/mf/Documents/MISR/Reports")

# Read processed data
AQS.08.09.ss<-read.csv("/Users/mf/Documents/AQS/PM25/AQS.08.09.ss.csv")
MISR.08.09<-read.csv("/Users/mf/Documents/MISR/Data/misr.08.09.csv")
MISR.08.09.monthly<-read.csv("/Users/mf/Documents/MISR/Data/misr.08.09.monthly.csv")
MISR.AQS<-read.csv("/Users/mf/Documents/MISR/Data/MISR.AQS.csv")
met.08.09<-read.csv("/Users/mf/Documents/MISR/Data/met.08.09")

proj.albers<-"+proj=aea +lat_1=34.0 +lat_2=40.5 +lon_0=-120.0 +x_0=0 +y_0=-4000000 +ellps=GRS80 +datum=NAD83 +units=km"
# Project the map
CA <- data.frame(map("state","california", plot=FALSE)[c("x","y")])
#project the map coordinates
newcoordsCA<-project(CA, proj=proj.albers)


# pick a particular date for mapping
misr.04.21.08<-misr.08.09[misr.08.09$year==2008 & misr.08.09$month==4 & misr.08.09$day==21,]
aqs.04.21.08<-AQS.08.09.ss[AQS.08.09.ss$year==2008 & AQS.08.09.ss$month==4 & AQS.08.09.ss$day==21,]
met.04.21.08<-met.08.09[met.08.09$year==2008 & met.08.09$month==4 & met.08.09$day==21,]
#Create a function to generate a continuous color palette
rbPal <- colorRampPalette(tim.colors(32))
AOD.breaks <-quantile(misr.04.21.08$AOD, seq(0,1,1/10))
col.ramp <-rbPal(length(AOD.breaks))  
misr.04.21.08$Col <- rbPal(10)[as.numeric(cut(misr.04.21.08$AOD, breaks = quantile(misr.04.21.08$AOD, seq(0,1,1/10))))]
pdf('MISRmap1.pdf')
map <- GetMap(center=c(34.42,  -118.1), maptype='satellite', zoom=7)
PlotOnStaticMap(map,misr.04.21.08$lat, misr.04.21.08$lon, cex=.7,pch=22,bg=misr.04.21.08$Col)
PlotOnStaticMap(map,aqs.04.21.08$SITE_LATITUDE,aqs.04.21.08$SITE_LONGITUDE, cex=1,pch=22,col="lightblue",lwd=2,add=TRUE)
#PlotOnStaticMap(map,st.so.ca$LAT, st.so.ca$LON, cex=1.5,pch="*", col='lightblue',add=TRUE)
legend('bottomleft',legend=c(round(AOD.breaks,digits=2),"AQS"),fill=c(col.ramp,"lightblue"),box.col='white',bty='o',bg="white",cex=0.5,title="MISR AOD 04/21/08")
dev.off()

rbPal <- colorRampPalette(tim.colors(32))
AOD.breaks <-quantile(misr.04.21.08$AODsmall, seq(0,1,1/10))
col.ramp <-rbPal(length(AOD.breaks))  
misr.04.21.08$Col <- rbPal(10)[as.numeric(cut(misr.04.21.08$AODsmall, breaks = quantile(misr.04.21.08$AODsmall, seq(0,1,1/10))))]
pdf('MISRmap2.pdf')
map <- GetMap(center=c(34.42,  -118.1), maptype='satellite', zoom=7)
PlotOnStaticMap(map,misr.04.21.08$lat, misr.04.21.08$lon, cex=.7,pch=22,bg=misr.04.21.08$Col)
PlotOnStaticMap(map,aqs.04.21.08$SITE_LATITUDE,aqs.04.21.08$SITE_LONGITUDE, cex=1,pch=22,col="lightblue",lwd=2,add=TRUE)
#PlotOnStaticMap(map,st.so.ca$LAT, st.so.ca$LON, cex=1.5,pch="*", col='lightblue',add=TRUE)
legend('bottomleft',legend=c(round(AOD.breaks,digits=2),"AQS"),fill=c(col.ramp,"lightblue"),box.col='white',bty='o',bg='white',cex=0.5,title="MISR AOD Small 04/21/08")
dev.off()


misr.aqs.04.21.08<-MISR.AQS[MISR.AQS$year==2008 & MISR.AQS$month==4 & MISR.AQS$day==21,]
met.04.21.08<-met.08.09[met.08.09$year==2008 & met.08.09$month==4 &met.08.09$day==21, ]
# Map matched data
pdf("MISRmap3.pdf")
map <- GetMap(center=c(34.42,  -118.1), maptype='satellite', zoom=7)
PlotOnStaticMap(map,misr.aqs.04.21.08$lat, misr.aqs.04.21.08$lon, cex=1,pch=22,col='lightblue')
PlotOnStaticMap(map,met.04.21.08$lat,met.04.21.08$lon, cex=1,pch="*",col="purple",add=TRUE)
legend(locator(1),legend=c("MISR-AQS","NOAA met site"),pch=c(22,8),col=c("lightblue","purple"),box.col='white',bty='o',bg='white',cex=0.5,title="Matched MISR AQS 04/21/08")
dev.off()

PlotOnStaticMap(map,ICVwarm.fine$lat,ICVwarm.fine$lon, cex=0.9,pch=21,bg="green",add=TRUE)
PlotOnStaticMap(map,MISR.Aeronet$lat, MISR.Aeronet$lon, pch="+", col='green',add=TRUE)
PlotOnStaticMap(map,MISR.Aeronet.08.09.merged.close$lat,MISR.Aeronet.08.09.merged.close$lon,pch="+",col="black",add=TRUE)
legend('bottomleft',legend=(round(AOD.breaks,digits=2)),fill=col.ramp,bty='o',box.col='white',cex=0.9,title="AOD Sep 09")


rbPal <- colorRampPalette(tim.colors(32))
AOD.breaks <-quantile(misr.04.21.08$AODsmall, seq(0,1,1/10))
col.ramp <-rbPal(length(AOD.breaks))  
misr.04.21.08$Col <- rbPal(10)[as.numeric(cut(misr.04.21.08$AODsmall, breaks = quantile(misr.04.21.08$AODsmall, seq(0,1,1/10))))]
pdf("MISRmap4.pdf")
map <- GetMap(center=c(34.5,  -118.5), maptype='satellite', zoom=8)
PlotOnStaticMap(map,misr.04.21.08$lat, misr.04.21.08$lon, cex=1.2,pch=22,bg=misr.04.21.08$Col)
PlotOnStaticMap(map,ICV$lat,ICV$lon, cex=1,pch=22,col="lightblue",add=TRUE)
legend('bottomleft',legend=c(round(AOD.breaks,digits=2),"ICV"),fill=c(col.ramp,"lightblue"),box.col='white',bty='o',bg='white',cex=0.5,title="MISR AOD Small 04/21/08")
dev.off()

PlotOnStaticMap(map,ICVwarm.fine$lat,ICVwarm.fine$lon, cex=0.9,pch=21,bg="green",add=TRUE)
PlotOnStaticMap(map,MISR.Aeronet$lat, MISR.Aeronet$lon, pch="+", col='green',add=TRUE)
PlotOnStaticMap(map,MISR.Aeronet.08.09.merged.close$lat,MISR.Aeronet.08.09.merged.close$lon,pch="+",col="black",add=TRUE)
legend('bottomleft',legend=(round(AOD.breaks,digits=2)),fill=col.ramp,bty='o',box.col='white',cex=0.9,title="AOD Sep 09")


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

