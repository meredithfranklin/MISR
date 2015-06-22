library(ncdf) # for reading netcdf file formats
library(date) # for converting julian dates
library(chron) # for converting julian dates
library(lubridate) # for date interval matching
library(plyr) # for easy merging/subsetting
library(dplyr) #for easy merging/subsetting
library(sas7bdat) # for reading SAS file formats
library(fields) # for spatial functions
library(proj4) # for map projections
library(R.utils) # decompressing NCDC data


#### Processed MISR 08-09 data see MISR Data Extract.R for details ####
misr.08.09<-read.csv("/Users/mf/Documents/MISR/Data/misr_08_09.csv")
#### Processed NOAA met data (daily from hourly to match MISR overpass) ####
met.08.09<-read.csv("/Users/mf/Documents/MISR/Data/met_08_09.csv")

# ICV data (monthly with odd start dates)
icv<-read.sas7bdat("/Volumes/Projects/CHSICV/Temp/TempRima/ICV2 Spatial Modeling/icv2spatial_01jun15.sas7bdat")
icv<-icv[,c(1:2,4:13,110:118)]
icv.long<-read.sas7bdat("/Volumes/Projects/CHSICV/Temp/TempRima/ICV2 Spatial Modeling/icv2temporal_09jun15.sas7bdat")
icv.long<-icv.long[,c(1,6:12)]
icv.new<-merge(icv,icv.long,by=c("Space_ID","startdate"))
icv.new<-icv.new[,c(1:13,18:21,26:27)]
icv.new$datestart<-dates(icv.new$startdate,origin=c(month=1,day=1,year=1960))
icv.new$datestart2<-mdy(icv.new$datestart)
icv.new$dateend<-dates(icv.new$enddate,origin=c(month=1,day=1,year=1960))
icv.new$dateend2<-mdy(icv.new$dateend)

#### Geographic projection for California applied to all lat/lon ####
proj.albers<-"+proj=aea +lat_1=34.0 +lat_2=40.5 +lon_0=-120.0 +x_0=0 +y_0=-4000000 +ellps=GRS80 +datum=NAD83 +units=km"
newcoords.icv<-project(as.matrix(cbind(icv.new$avg_lon, icv.new$avg_lat)), proj=proj.albers)
icv.new$x<-newcoords.icv[,1]
icv.new$y<-newcoords.icv[,2]

write.csv(icv.new,"/Users/mf/Documents/MISR/Data/icv_new.csv")

# find unique startdates and create interval to match MISR
ICV.startdays<-ICV.new %>% distinct(datestart2)
ICV.startdays$sampling.interval <- as.interval(ICV.startdays$datestart2, ICV.startdays$dateend2)

# MISR grid x,y to make 1km grid
misr.08.09<-misr.08.09[misr.08.09$land.water.mask==3,]
MISR.grid<-unique(misr.08.09[,19:20])

MISR.ICV.match.all<-vector('list',length(ICV.new$startdate))

for (i in 1:length(ICV.startdays$startdate)){
  # take MISR averages between each ICV start and end date (sampling.interval)
  misr.icv.date.match<-misr.08.09[misr.08.09$date2 %within% ICV.startdays$sampling.interval[i],]
  misr.monthly <- ddply(misr.icv.date.match, .(x,y), summarise, AOD.month=mean(AOD),
                        AODsmall.month=mean(AODsmall), AODmed.month=mean(AODmed), 
                        AODlarge.month=mean(AODlarge), AODnonsph.month=mean(AODnonspher))
  knots=dim(misr.icv.monthly)[1]/3
  misr.monthly.gam<-gam(AOD.month~s(x,y,k=knots),data=misr.monthly,na.action='na.exclude')
  misr.monthly.pred.smooth<-predict.gam(misr.monthly.gam,newdata = MISR.grid)
  
  # select icv observations for ith startdate  
  icv.monthly<-ICV.new[ICV.new$datestart2 %in% ICV.startdays$datestart2[i],]
  # remove missing locations
  icv.monthly<-icv.monthly[!is.na(icv.monthly$x),]
  # calculate distance between misr pixels and ICV sites
  dist<-rdist(cbind(misr.monthly$x,misr.monthly$y),cbind(icv.monthly$x,icv.monthly$y))
  
  MISR.ICV.match.list<-vector('list',length(dist[1,]))
  
  for (j in 1:length(dist[1,])){ 
    if (min(dist[,j])<=3){
      MISR.ICV.match.list[[j]]<-data.frame(misr.monthly[which.min(dist[,j]),],icv.monthly[j,]) # identifies misr pixel close to ICV site
    } 
  }
  MISR.ICV.match.all[[i]] <- do.call("rbind", MISR.ICV.match.list) 
}

MISR.ICV2 <- do.call("rbind", MISR.ICV.match.all)

write.csv(MISR.ICV,"/Users/mf/Documents/MISR/Data/MISR.ICV.csv",row.names=FALSE)  

#check
MISR.ICV.ss <- na.omit(subset(MISR.ICV,select=c(AOD.month,PM25)))
plot(PM25~AOD.month,data=MISR.ICV.ss)



#### MISR-ICV models ####
MISR.ICV.ss <- na.omit(subset(MISR.ICV,select=c(AOD.month,PM25,EC_PM25,OC_PM25)))

p<-qplot(AOD.month,PM25/1000, data=MISR.ICV.ss,xlab="MISR AOD",ylab="ICV PM2.5 (ug/m3)")
p+stat_smooth(method="gam",formula=y~s(x,k=4)) +stat_smooth(method='lm',formula=y~x,col='red')

MISR.ICV2.ss <- na.omit(subset(MISR.ICV2,select=c(AOD.month,PM25,EC_PM25,OC_PM25)))
plot(PM25/1000~AOD.month,data=MISR.ICV2.ss)
p<-qplot(AOD.month,PM25/1000, data=MISR.ICV2.ss,xlab="MISR AOD",ylab="ICV PM2.5 (ug/m3)")
p+stat_smooth(method="gam",formula=y~s(x,k=4)) +stat_smooth(method='lm',formula=y~x,col='red')

p<-qplot(AOD.month,PM25/1000, data=MISR.ICV2.ss,xlab="MISR AOD",ylab="ICV PM2.5 (ug/m3)")
p+stat_smooth(method="gam",formula=y~s(x,k=4)) +stat_smooth(method='lm',formula=y~x,col='red')

plot(EC_PM25/1000~AOD.month,data=MISR.ICV2.ss)
plot(OC_PM25/1000~AOD.month,data=MISR.ICV2.ss)

MISR.ICV.ss <- na.omit(subset(MISR.ICV,select=c(AODsmall.month,UltraFine,EC_uf,OC_uf)))
plot(UltraFine/1000~AODsmall.month,data=MISR.ICV.ss)
MISR.ICV.ss <- na.omit(subset(MISR.ICV,select=c(AODsmall.month,UltraFine)))
plot(UltraFine/1000~AODsmall.month,data=MISR.ICV.ss)

MISR.ICV$towncode2<-as.factor(MISR.ICV$towncode)

misr.icv.st.gam<-gamm4(PM25/1000~(AOD.month)+s(x,y,by=towncode2)+s(startdate),data=MISR.ICV,na.action="na.exclude")

