################################################
# MISR4km data visualization
# Requires AQS PM2.5, PM10, STN, Met datasets
################################################

library(ggmap)

setwd("/Users/mf/Documents/MISR/Reports")


misr<-read.csv("/Users/mf/Documents/MISR/Data/misr_2000_2011.csv")
misr<-misr[misr$land.water.mask==3,]
misr.grid<-unique(misr[,1:2])
misr_06_11<-misr[misr$year>=2006,]
misr.date<-unique(misr_06_11[,13])
# Matched sites
misr.aqspm25<-read.csv("/Users/mf/Documents/MISR/Data/Match 2000-2011/misr_pm25_2000_2011.csv")
misr.aqspm10<-read.csv("/Users/mf/Documents/MISR/Data/Match 2000-2011/misr_pm10_2000_2011.csv")
misr.aqspm1025<-read.csv("/Users/mf/Documents/MISR/Data/Match 2000-2011/misr_pm10_pm25_2000_2011.csv")
misr.stn<-read.csv("/Users/mf/Documents/MISR/Data/Match 2000-2011/misr_stn_2002_2011.csv")

# All sites
aqspm25<-read.csv("/Users/mf/Documents/AQS/PM25/pm25_2000_2011.csv")
aqspm10<-read.csv("/Users/mf/Documents/AQS/PM10/pm10_2000_2011.csv")
aqspm10_pm25<-read.csv("/Users/mf/Documents/AQS/PM10/pm10_pm25_2000_2011.csv")
stn<-read.csv("/Users/mf/Documents/AQS/STN/stn_2001_2011.csv")
met<-read.csv("/Users/mf/Documents/NCDC/met_2000_2011.csv")

aqspm25_06_11<-aqspm25[aqspm25$year>=2006,]
aqspm10_06_11<-aqspm10[aqspm10$year>=2006,]
aqspm10_pm25_06_11<-aqspm10_pm25[aqspm10_pm25$year>=2006,]

met_06_11<-met[met$year>=2006,]

# Subset 2006-2011 correct region
aqspm25_06_11<-read.csv("/Users/mf/Documents/MISR/Data/PM25_sites_06_11ss.csv")
aqspm10_06_11<-read.csv("/Users/mf/Documents/MISR/Data/PM10_sites_06_11ss.csv")
met_06_11<-read.csv("/Users/mf/Documents/MISR/Data/met_sites_06_11ss.csv")
stn<-read.csv("/Users/mf/Documents/AQS/STN/stn_2001_2011.csv")
stn_06_11<-stn[stn$year>=2006,]

aqspm25.points<-unique(misr.aqspm25[,38:39])
aqspm25.points2<-unique(aqspm25_06_11[,2:3])
#write.csv(aqspm25.points2,'PM25_sites_06_11.csv',row.names=FALSE)
aqspm10.points<-unique(misr.aqspm10[,38:39])
aqspm10.points2<-unique(aqspm10_06_11[,2:3])
#write.csv(aqspm10.points2,'PM10_sites_06_11.csv',row.names=FALSE)
aqspm10.pm25.points<-unique(misr.aqspm1025[,22:23])
aqspm10.pm25.points2<-unique(aqspm10_pm25_06_11[,1:2])
#write.csv(aqspm10.pm25.points2,'PM2510_sites_06_11.csv',row.names=FALSE)

stn.points<-unique(misr.stn[,22:23])
stn.points2<-unique(stn_06_11[,1:2])
write.csv(stn.points2,'STN_sites_06_11.csv',row.names=FALSE)

met.points<-unique(met_06_11[,2:3])
met.points<-met.points[met.points$lon< -116.16,]
met.points<-met.points[met.points$lat> 33.683,]
write.csv(met.points,'met_sites_06_11.csv',row.names=FALSE)
map <- qmap('Los Angeles', zoom = 7, maptype = 'satellite')
postscript("Pointsmap_new.eps", width = 480, height = 480)
map + geom_point(data = misr.grid, aes(x = lon, y = lat), shape=22, size=1, col="white",alpha=0.6)+
  geom_point(data = aqspm10.points2, aes(x = Longitude, y = Latitude),col="magenta", shape=15,size=4)+
  geom_point(data = aqspm25.points2, aes(x = Longitude, y = Latitude),col="cyan", size=2)+
  geom_point(data = stn.points2, aes(x = Longitude, y = Latitude),col="yellow", shape=18, size=4)+
  geom_point(data = met.points, aes(x = lon, y = lat),col="green", shape=43,size=5)

dev.off()


  geom_point(data = aqspm10.pm25.points, aes(x = Longitude, y = Latitude),shape=23,col="purple", size=3)+
  geom_point(data = aqspm10.pm25.points2, aes(x = Longitude, y = Latitude),shape=23,col="green", size=1)
    
  geom_point(data = stn.points, aes(x = Longitude, y = Latitude),col="yellow", size=2)+
  #geom_point(data = stn.points2, aes(x = Longitude, y = Latitude),shape=43, col="green", size=4)








## OLD
# Read processed data
# Individual files
misr.08.09<-read.csv("/Users/mf/Documents/MISR/Data/misr_08_09.csv")
# Remove MISR AOD over water
misr.08.09<-misr.08.09[misr.08.09$land.water.mask==3,]
# AQS
aqspm25<-read.csv("/Users/mf/Documents/MISR/Data/aqspm25_08_09_CAnew.csv")
aqspm10<-read.csv("/Users/mf/Documents/MISR/Data/aqspm10_08_09_CAnew.csv")
aqspm10_pm25<-read.csv("/Users/mf/Documents/MISR/Data/aqspm10_25_2008_2009_CAnew.csv")
stn.08.09<-read.csv("/Users/mf/Documents/AQS/STN/stn_08_09_ss.csv")
met.08.09<-read.csv("/Users/mf/Documents/MISR/Data/met_08_09.csv")

# Points for mapping
misr.grid<-unique(misr.08.09[,1:2])
write.csv(misr.grid,"/Users/mf/Documents/MISR/Data/misr_locations.csv")
aqspm25<-aqspm25[aqspm25$SITE_LONGITUDE2>=-120 & aqspm25$SITE_LONGITUDE2<= -117,]
aqspm25<-aqspm25[aqspm25$SITE_LATITUDE2>=33.65 & aqspm25$SITE_LATITUDE2<=35.5,]
aqspm25<-aqspm25[aqspm25$SITE_LONGITUDE2 != -119.4869,] #Remove Catalina
aqspm25.points<-unique(aqspm25[,20:21])
write.csv(aqspm25.points,"/Users/mf/Documents/MISR/Data/PM25_locations.csv")
aqspm10<-aqspm10[aqspm10$SITE_LONGITUDE2>=-120 & aqspm10$SITE_LONGITUDE2<= -117,]
aqspm10<-aqspm10[aqspm10$SITE_LATITUDE2>=33.65 & aqspm10$SITE_LATITUDE2<=35.5,]
aqspm10<-aqspm10[aqspm10$SITE_LONGITUDE2 != -119.4869,] #Remove Catalina
aqspm10.points<-unique(aqspm10[,23:24])
write.csv(aqspm10.points,"/Users/mf/Documents/MISR/Data/PM10_locations.csv")
aqspm10_pm25<-aqspm10_pm25[aqspm10_pm25$SITE_LONGITUDE2.x>=-120 & aqspm10_pm25$SITE_LONGITUDE2.x<= -117,]
aqspm10_pm25<-aqspm10_pm25[aqspm10_pm25$SITE_LATITUDE2.x>=33.65 & aqspm10_pm25$SITE_LATITUDE2.x<=35.5,]
aqspm10_pm25<-aqspm10_pm25[aqspm10_pm25$SITE_LONGITUDE2.x != -119.4869,] #Remove Catalina
aqspm10_25.points<-unique(aqspm10_pm25[,20:21])


stn.points<-unique(stn.08.09[,20:21])
write.csv(stn.points,"/Users/mf/Documents/MISR/Data/stn_locations.csv")
met.08.09.ss<-met.08.09[met.08.09$lat>33.65,]
met.points<-unique(met.08.09.ss[,4:5])
write.csv(met.points,"/Users/mf/Documents/MISR/Data/met_locations.csv")

#misr.08.09.monthly<-read.csv("/Users/mf/Documents/MISR/Data/misr_08_09_monthly.csv")
# Matched MISR-AQS files
misr.aqspm25.old<-read.csv("/Users/mf/Documents/MISR/Data/misr_aqspm25.csv")
misr.aqspm10.old<-read.csv("/Users/mf/Documents/MISR/Data/misr_aqspm10.csv")
# Matched MISR-AQS files on STN days
misr.aqspm25.match.stn<-read.csv("/Users/mf/Documents/MISR/Data/misr_aqspm25_stn_match.csv")
misr.aqspm10.match.stn<-read.csv("/Users/mf/Documents/MISR/Data/misr_aqspm10_stn_match.csv")

misr.08.14.09<-read.csv("/Users/mf/Documents/MISR/Data/predicted_pm25_misr_08_14_09.csv")
# Plot of all locations and MISR grid, subset data


# Matched MISR-AQS points
aqspm25.matchpoints<-unique(misr.aqspm25[,40:41])
aqspm10.matchpoints<-unique(misr.aqspm10[,43:44])
aqspm25.match.stn.matchpoints<-unique(misr.aqspm25.match.stn[,30:31])
aqspm2510.matchpoints<-unique(misr.aqspm2510[,40:41])
aqspm10.matchpoint1<-unique(misr.aqspm10[misr.aqspm10$AQS_SITE_ID=="06-065-0004",28:29])
aqspm25.matchpoint1<-unique(misr.aqspm25[misr.aqspm25$AQS_SITE_ID=="06-037-4002",29:30])


met.points<-unique(met[,4:5])
aqspm25.points<-unique(aqspm25.all[,17:18])
aqspm10.points<-unique(aqspm10.all[,17:18])
stn.points<-unique(stn.all[,1:2])
map <- qmap('Los Angeles', zoom =7, maptype = 'satellite')
map + geom_point(data = misr.grid, aes(x = lon, y = lat), shape=22, size=3, col="darkgrey",alpha=0.6)+ 
  geom_point(data = aqspm25.points, aes(x = Longitude, y = Latitude), shape=17,col="magenta", size=3)+ 
  geom_point(data = aqspm10.points, aes(x = Longitude, y = Latitude),col="cyan", size=2)+
  geom_point(data = stn.points, aes(x = Longitude, y = Latitude),shape =18,col="yellow", size=2)+ 
  geom_point(data = met.points, aes(x = lon, y = lat), shape=43,col="green", size=5)



# Plot st model predictions PM2.5
# Pick a particular date for mapping
misr.04.21.08<-misr.08.09[misr.08.09$year==2008 & misr.08.09$month==4 & misr.08.09$day==21,]
misr.06.27.09<-misr.08.09[misr.08.09$year==2009 & misr.08.09$month==6 & misr.08.09$day==27,]

aqs.04.21.08<-AQS.08.09.ss[AQS.08.09.ss$year==2008 & AQS.08.09.ss$month==4 & AQS.08.09.ss$day==21,]
met.04.21.08<-met.08.09[met.08.09$year==2008 & met.08.09$month==4 & met.08.09$day==21,]
#Create a function to generate a continuous color palette
rbPal <- colorRampPalette(tim.colors(32))
AOD.breaks <-quantile(misr.02.19.09.ss$AOD, seq(0,1,1/10))
col.ramp <-rbPal(length(AOD.breaks))  
misr.02.19.09.ss$AODCol <- rbPal(10)[as.numeric(cut(misr.02.19.09.ss$AOD, breaks = quantile(misr.02.19.09.ss$AOD, seq(0,1,1/10))))]

PM.breaks <-quantile(misr.04.21.08$predPM25, seq(0,1,1/10))
col.ramp <-rbPal(length(PM.breaks))  
misr.04.21.08$PredPMCol <- rbPal(10)[as.numeric(cut(misr.04.21.08$predPM25, breaks = quantile(misr.04.21.08$predPM25, seq(0,1,1/10))))]


misr.04.21.08.ss<-misr.04.21.08[misr.04.21.08$land.water.mask==3,]
misr.06.27.09.ss<-misr.06.27.09[misr.06.27.09$land.water.mask==3,]
misr.02.19.09.ss<-misr.02.19.09[misr.02.19.09$land.water.mask==3,]
misr.04.21.08.ss<-misr.04.21.08.ss[misr.04.21.08.ss$predPM10>0,]
misr.08.14.09.ss<-misr.08.14.09[misr.08.14.09$AOD<1,]
misr.06.27.09.ss<-misr.06.27.09.ss[misr.06.27.09.ss$AOD<1,]
#PM.breaks <-quantile(misr.04.21.08.ss$predPM10, seq(0,1,1/10))
#col.ramp <-rbPal(length(PM.breaks))  
#misr.04.21.08.ss$PredCol <- rbPal(10)[as.numeric(cut(misr.04.21.08.ss$predPM25, breaks = quantile(misr.04.21.08.ss$predPM25, seq(0,1,1/10))))]

map <- qmap('Los Angeles', zoom = 7, maptype = 'hybrid')
#plot the crime points on top
pdf('PM25pred04.21.08.pdf')
map + geom_point(data = misr.04.21.08.ss, aes(x = lon, y = lat, color=predPM25), shape=c, size=2, alpha=0.5)+ scale_color_gradientn(colours=matlab.like(20))
dev.off()
pdf('AOD04.21.08.pdf')
map + geom_point(data = misr.04.21.08.ss, aes(x = lon, y = lat, color=AOD), shape=c, size=2, alpha=0.5)+ scale_color_gradientn(colours=matlab.like(32))
dev.off()
pdf('AOD04.21.08.pdf')
map + geom_point(data = misr.06.27.09.ss, aes(x = lon, y = lat, color=AOD), shape=c, size=2, alpha=0.4)+ scale_color_gradientn(colours=matlab.like(10))
dev.off()

map <- qmap('Los Angeles', zoom = 7, maptype = 'hybrid')
#plot the crime points on top
pdf('PM25pred3.pdf')
map + geom_point(data = misr.02.19.09.ss, aes(x = lon, y = lat, color=AOD), shape=c, size=2, alpha=0.4)+ scale_color_gradientn(colours=matlab.like(10))
dev.off()



map <- qmap('Los Angeles', zoom = 7, maptype = 'hybrid')
#plot the crime points on top
pdf('PM10pred1.pdf')
map + geom_point(data = misr.04.21.08.ss, aes(x = lon, y = lat, color=predPM25), shape=c, size=2, alpha=0.4)+ scale_color_gradientn(colours=matlab.like(10))
dev.off()


pdf('AODmap04_21_08.pdf')
map <- GetMap(center=c(34.42,  -118.1), maptype='hybrid', zoom=7)
PlotOnStaticMap(map,misr.02.19.09.ss$lat, misr.02.19.09.ss$lon, cex=.7,pch=22,bg=misr.02.19.09.ss$AODCol)
#PlotOnStaticMap(map,AQS.08.09.ss$SITE_LATITUDE,AQS.08.09.ss$SITE_LONGITUDE, cex=1,pch=22,col="lightblue",lwd=2,add=TRUE)
legend(locator(1),legend=c(round(AOD.breaks,digits=2)),fill=c(col.ramp),box.col='white',bty='o',bg="white",cex=0.5,title="AOD 04/21/08")
dev.off()


pdf('MISRmap_predictionsPM25.pdf')
map <- GetMap(center=c(34.5,  -118.3), maptype='satellite', zoom=7)
PlotOnStaticMap(map,misr.04.21.08$lat, misr.04.21.08$lon, cex=.7,pch=22,bg=misr.04.21.08$PredPMCol)
#PlotOnStaticMap(map,aqs.04.21.08$SITE_LATITUDE,aqs.04.21.08$SITE_LONGITUDE, cex=1,pch=22,col="lightblue",lwd=2,add=TRUE)
#PlotOnStaticMap(map,STN.site.CA$Latitude, STN.site.CA$Longitude, cex=1.5,pch="*", col='purple',add=TRUE)
legend('bottomleft',legend=c(round(AOD.breaks,digits=2),"AOD"),fill=c(col.ramp,"lightblue"),box.col='white',bty='o',bg="white",cex=0.5,title="MISR AOD 04/21/08")
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

jet.colors <-
  colorRampPalette(c("#00007F", "blue", "#007FFF", "cyan",
                     "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"))
rbPal <- colorRampPalette(jet.colors(32))
PM.breaks <-quantile(misr.06.27.09$predPM25, seq(0,1,1/10))
col.ramp <-rbPal(length(PM.breaks))  
misr.06.27.09$Col <- rbPal(10)[as.numeric(cut(misr.06.27.09$predPM25, breaks = quantile(misr.06.27.09$predPM25, seq(0,1,1/10))))]

pdf('MISRmap5.pdf')
map <- GetMap(center=c(34.42,  -118.1), maptype='satellite', zoom=7)
PlotOnStaticMap(map,misr.06.27.09$lat, misr.06.27.09$lon, cex=.7,pch=22,bg=misr.06.27.09$Col)
#PlotOnStaticMap(map,AQS.08.09.ss$SITE_LATITUDE,AQS.08.09.ss$SITE_LONGITUDE, cex=1,pch=22,col="lightblue",lwd=2,add=TRUE)
legend('bottomleft',legend=c(round(PM.breaks,digits=2)),fill=c(col.ramp),box.col='white',bty='o',bg="white",cex=0.5,title="Predicted PM2.5 04/21/08")
dev.off()





