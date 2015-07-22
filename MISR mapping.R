##############################################
# MISR 08/09 4km data visualization
# December 2014, February 2015, June-July 2015
# Meredith Franklin
##############################################
library(RgoogleMaps) 
library(RColorBrewer) 
setwd("/Users/mf/Documents/MISR/Reports")

# Read processed data
# Individual files
aqspm25.08.09.ss2<-read.csv("/Users/mf/Documents/MISR/Data/aqsPM25_08_09_ss_frm.csv")
aqspm10.08.09.ss2<-read.csv("/Users/mf/Documents/MISR/Data/aqspm10_08_09_ss.csv")
stn.08.09.ss<-read.csv("/Users/mf/Documents/AQS/STN/stn_08_09_ss.csv")
met.08.09<-read.csv("/Users/mf/Documents/MISR/Data/met_08_09.csv")
misr.08.09<-read.csv("/Users/mf/Documents/MISR/Data/misr_08_09.csv")
misr.08.09.monthly<-read.csv("/Users/mf/Documents/MISR/Data/misr_08_09_monthly.csv")
# Matched MISR-AQS files
misr.aqspm25<-read.csv("/Users/mf/Documents/MISR/Data/misr_aqspm25.csv")
misr.aqspm10<-read.csv("/Users/mf/Documents/MISR/Data/misr_aqspm10.csv")

# Plot of all locations and MISR grid, subset data
# Remove MISR AOD over water
misr.08.09.ss<-misr.08.09[misr.08.09$land.water.mask==3,]
misr.grid<-unique(misr.08.09.ss[,1:2])
# Subset AQS data to MISR domain, take unique locations
aqspm25.points<-unique(aqspm25.08.09.ss2[,9:10])
aqspm25.points<-aqspm25.points[aqspm25.points$SITE_LATITUDE>33.65,]
stn.points<-unique(stn.08.09.ss[,c(20:21)])
aqspm10.points<-unique(aqspm10.08.09.ss2[,9:10])
aqspm10.points<-aqspm10.points[aqspm10.points$SITE_LATITUDE>33.65,]
met.08.09.ss<-met.08.09[met.08.09$lat>33.65,]
met.points<-unique(met.08.09.ss[,4:5])

# Matched MISR-AQS points
aqspm25.matchpoints<-unique(misr.aqspm25.ss2[,29:30])
aqspm10.matchpoints<-unique(misr.aqspm10.ss2[,29:30])


# Plot locations map
png('MISRmap_locations2.png')
  map <- GetMap(center=c(34.5,  -118.3), maptype='hybrid', zoom=7)
  PlotOnStaticMap(map,misr.grid$lat, misr.grid$lon, cex=.4,pch=15,col="grey")
  PlotOnStaticMap(map,aqspm25.points$SITE_LATITUDE,aqspm25.points$SITE_LONGITUDE, cex=1,pch=19,col="cyan",add=TRUE)
  PlotOnStaticMap(map,aqspm10.points$SITE_LATITUDE,aqspm10.points$SITE_LONGITUDE, cex=0.7,pch=19,col="deeppink",add=TRUE)
  PlotOnStaticMap(map,met.points$lat, met.points$lon, cex=.7,pch=15,col="green",add=TRUE)
  PlotOnStaticMap(map,stn.points$Latitude, stn.points$Longitude, cex=.8,pch=19,col="yellow",add=TRUE)
  #PlotOnStaticMap(map,aqspm25.matchpoints$SITE_LATITUDE,aqspm25.matchpoints$SITE_LONGITUDE, cex=0.5,pch=19,col="green",add=TRUE)
  #PlotOnStaticMap(map,aqspm10.matchpoints$SITE_LATITUDE,aqspm10.matchpoints$SITE_LONGITUDE, cex=1,pch='+',col="green",add=TRUE)
  legend(locator(1),legend=c("MISR","AQS PM2.5","AQS PM10","AQS STN","Meteorology"),pch=c(15,19,19,19,19),col=c("grey","cyan","deeppink","yellow","green"),
       box.col='white',bty='o',bg='white',cex=0.7,title="Legend")
dev.off()

# Plot st model predictions PM2.5
# Pick a particular date for mapping
misr.04.21.08<-misr.08.09[misr.08.09$year==2008 & misr.08.09$month==4 & misr.08.09$day==21,]
aqs.04.21.08<-AQS.08.09.ss[AQS.08.09.ss$year==2008 & AQS.08.09.ss$month==4 & AQS.08.09.ss$day==21,]
met.04.21.08<-met.08.09[met.08.09$year==2008 & met.08.09$month==4 & met.08.09$day==21,]
#Create a function to generate a continuous color palette
rbPal <- colorRampPalette(tim.colors(32))
AOD.breaks <-quantile(misr.04.21.08$AOD, seq(0,1,1/10))
col.ramp <-rbPal(length(AOD.breaks))  
misr.04.21.08$Col <- rbPal(10)[as.numeric(cut(misr.04.21.08$AOD, breaks = quantile(misr.04.21.08$AOD, seq(0,1,1/10))))]

pdf('MISRmap_predictionsPM25.pdf')
map <- GetMap(center=c(34.42,  -118.1), maptype='satellite', zoom=7)
PlotOnStaticMap(map,misr.04.21.08$lat, misr.04.21.08$lon, cex=.7,pch=22,bg=misr.04.21.08$Col)
PlotOnStaticMap(map,aqs.04.21.08$SITE_LATITUDE,aqs.04.21.08$SITE_LONGITUDE, cex=1,pch=22,col="lightblue",lwd=2,add=TRUE)
PlotOnStaticMap(map,STN.site.CA$Latitude, STN.site.CA$Longitude, cex=1.5,pch="*", col='purple',add=TRUE)
legend('bottomleft',legend=c(round(AOD.breaks,digits=2),"AQS"),fill=c(col.ramp,"lightblue"),box.col='white',bty='o',bg="white",cex=0.5,title="MISR AOD 04/21/08")
dev.off()

rbPal <- colorRampPalette(tim.colors(32))
AOD.breaks <-quantile(misr.04.21.08$AODsmall, seq(0,1,1/10))
col.ramp <-rbPal(length(AOD.breaks))  
misr.04.21.08$Col <- rbPal(10)[as.numeric(cut(misr.04.21.08$AODsmall, breaks = quantile(misr.04.21.08$AODsmall, seq(0,1,1/10))))]

pdf('MISRmap_predictionsPM10.pdf')
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
PlotOnStaticMap(map,aqs.04.21.08$SITE_LATITUDE, aqs.04.21.08$SITE_LONGITUDE, cex=1,pch=22,col='lightblue')
PlotOnStaticMap(map,met.04.21.08$lat,met.04.21.08$lon, cex=1,pch="*",col="purple",add=TRUE)
legend(locator(1),legend=c("MISR-AQS","NOAA met site"),pch=c(22,8),col=c("lightblue","purple"),box.col='white',bty='o',bg='white',cex=0.5,title="Matched MISR AQS 04/21/08")
dev.off()

##### PREDICTED PM25 ######
jet.colors <-
  colorRampPalette(c("#00007F", "blue", "#007FFF", "cyan",
                     "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"))
rbPal <- colorRampPalette(jet.colors(32))
PM.breaks <-quantile(misr.04.21.08$predPM25, seq(0,1,1/10))
col.ramp <-rbPal(length(PM.breaks))  
misr.04.21.08$Col <- rbPal(10)[as.numeric(cut(misr.04.21.08$predPM25, breaks = quantile(misr.04.21.08$predPM25, seq(0,1,1/10))))]

pdf('MISRmap4.pdf')
map <- GetMap(center=c(34.42,  -118.1), maptype='satellite', zoom=7)
PlotOnStaticMap(map,misr.04.21.08$lat, misr.04.21.08$lon, cex=.7,pch=22,bg=misr.04.21.08$Col)
#PlotOnStaticMap(map,AQS.08.09.ss$SITE_LATITUDE,AQS.08.09.ss$SITE_LONGITUDE, cex=1,pch=22,col="lightblue",lwd=2,add=TRUE)
legend('bottomleft',legend=c(round(PM.breaks,digits=2)),fill=c(col.ramp),box.col='white',bty='o',bg="white",cex=0.5,title="Predicted PM2.5 04/21/08")
dev.off()






