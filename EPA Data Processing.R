##### EPA AQS Daily PM25 and PM10 data, STN PM25 Species for California
##### Data downloaded from http://www3.epa.gov/airdata/ad_data_daily.html
##### Site information downloaded from http://aqsdr1.epa.gov/aqsweb/aqstmp/airdata/download_files.html#Meta

library(date) # for converting julian dates
library(chron) # for converting julian dates
library(lubridate) # for date interval matching
library(plyr) # for easy merging/subsetting
library(dplyr) #for easy merging/subsetting
library(proj4) # for map projections
library(ggmap) # for visual checks


# AQS DATA 

# PM2.5
aqspm25.files <- list.files("/Users/mf/Documents/AQS/PM25",pattern="*_PM25_*",full.names=TRUE)

# Extract data then subset
aqspm25.list<-vector('list',length(aqspm25.files))
for(i in 1:length(aqspm25.files)) { 
  dat.pm25<-read.csv(aqspm25.files[i],stringsAsFactors=FALSE)
  # separate m/d/y from Date
  date.pm25<-strsplit(dat.pm25$Date,"/")
  dat.pm25$month<-as.numeric(sapply(date.pm25, "[[", 1) )
  dat.pm25$day<-as.numeric(sapply(date.pm25,"[[",2))
  dat.pm25$year<-as.numeric(sapply(date.pm25,"[[",3))
  names(dat.pm25)[names(dat.pm25) == 'Daily.Mean.PM2.5.Concentration'] <- 'PM25'
  names(dat.pm25)[names(dat.pm25) == 'SITE_LATITUDE'] <- 'Latitude'
  names(dat.pm25)[names(dat.pm25) == 'SITE_LONGITUDE'] <- 'Longitude'

  aqspm25.list[[i]]<-dat.pm25
}  
aqspm25.all <- do.call("rbind", aqspm25.list)
aqspm25.all<-subset(aqspm25.all,Longitude >= -120.5 & Longitude <= -115 & Latitude>= 33.65 & Latitude < 35.5)

# PM10
aqspm10.files <- list.files("/Users/mf/Documents/AQS/PM10/",pattern="*_PM10_*",full.names=TRUE)

aqspm10.list<-vector('list',length(aqspm10.files))
for(i in 1:length(aqspm10.files)) {
  # PM10
  dat.pm10<-read.csv(aqspm10.files[i],stringsAsFactors=FALSE)
  # separate m/d/y from Date
  date.pm10<-strsplit(dat.pm10$Date,"/")
  dat.pm10$month<-as.numeric(sapply(date.pm10, "[[", 1) )
  dat.pm10$day<-as.numeric(sapply(date.pm10,"[[",2))
  dat.pm10$year<-as.numeric(sapply(date.pm10,"[[",3))
  names(dat.pm10)[names(dat.pm10) == 'Daily.Mean.PM10.Concentration'] <- 'PM10'
  names(dat.pm10)[names(dat.pm10) == 'SITE_LATITUDE'] <- 'Latitude'
  names(dat.pm10)[names(dat.pm10) == 'SITE_LONGITUDE'] <- 'Longitude'

  aqspm10.list[[i]]<-dat.pm10
}

aqspm10.all <- do.call("rbind", aqspm10.list) 
aqspm10.all<-subset(aqspm10.all,Longitude >= -120.5 & Longitude < -115.5 & Latitude>= 33.65 & Latitude < 35.5)


#aqspm10.all<-aqspm10.all[aqspm10.all$Longitude != -119.4869,] #Remove Catalina

# Coarse Mode PM10-PM25
aqspm10.all2<-aqspm10.all %>% select(-matches("Date"),-matches("STATE"),-matches("COUNTY"),-matches("UNITS"),
                      -matches("DAILY_AQI_VALUE"),-matches("DAILY_OBS_COUNT"),-matches("PERCENT_COMPLETE"),
                        -matches("AQS_PARAMETER_CODE"),-matches("AQS_PARAMETER_DESC"),-matches("CBSA_CODE"),
                            -matches("CBSA_NAME"),-matches("STATE_CODE"),-matches("COUNTY_CODE"),-matches("POC"))

aqspm10.pm25.all<-merge(aqspm25.all, aqspm10.all2, by=c('Latitude','Longitude','AQS_SITE_ID','month','day','year'))
aqspm10.pm25.all$pm10_pm25<-aqspm10.pm25.all$PM10-aqspm10.pm25.all$PM25


#Check
#diff<-aqspm10.pm25.all$AQS_SITE_ID.x-aqspm10.pm25.all$AQS_SITE_ID.y
#summary(aqspm10.pm25.all)

# STN 

stn.files <- list.files("/Users/mf/Documents/AQS/STN/New/",pattern="*.csv",full.names=TRUE)

# Extract data
stn.list<-vector('list',length(stn.files))
for(i in 1:length(stn.files)) { 
  dat.stn<-read.csv(stn.files[i],stringsAsFactors=FALSE)
  dat.stn$State.Code<-as.numeric(dat.stn$State.Code)
  #dat$CountyCode<-as.numeric(dat$CountyCode)
  #dat$SiteID<-as.numeric(dat$SiteID)
  dat.stn$POC<-as.numeric(dat.stn$POC)
  dat.stn$Concentration<-as.numeric(dat.stn$Arithmetic.Mean)
  dat.stn<-dat.stn[dat.stn$State.Code==6  & dat.stn$POC==5 & dat.stn$Parameter.Code>88000,] #Subset to CA and STN sites
  
  #PM25stn<-dat.stn[dat.stn$Parameter.Code==88101,c(1:7,9,12,30:33)]
  #PM25stn<-rename(PM25stn, PM25=Concentration)
  #PM25stn<-PM25stn[with(PM25stn,order(CountyCode, SiteID, month, day, year)),]
  #PM25stn<-PM25stn[!duplicated(PM25stn),]
  
  EC<-dat.stn[dat.stn$Parameter.Code==88307,c(6:7,12,30)] #88321 for 2011
  EC<-rename(EC, EC=Concentration)
  EC<-EC[!duplicated(EC),]
  
  EC2<-dat.stn[dat.stn$Parameter.Code==88321,c(6:7,12,30)]
  EC2<-rename(EC2, EC2=Concentration)
  EC2<-EC2[!duplicated(EC2),]
  
  OC<-dat.stn[dat.stn$Parameter.Code==88305,c(6:7,12,30)] 
  OC<-rename(OC, OC=Concentration)
  OC<-OC[!duplicated(OC),]
  
  OC2<-dat.stn[dat.stn$Parameter.Code==88325,c(6:7,12,30)] 
  OC2<-rename(OC2, OC2=Concentration)
  OC2<-OC2[!duplicated(OC2),]
  
  NO3<-dat.stn[dat.stn$Parameter.Code==88306,c(6:7,12,30)]
  NO3<-rename(NO3, NO3=Concentration)
  NO3<-NO3[!duplicated(NO3),]
  
  SO4<-dat.stn[dat.stn$Parameter.Code==88403,c(6:7,12,30)]
  SO4<-rename(SO4, SO4=Concentration)
  SO4<-SO4[!duplicated(SO4),]
  
  #join1<-join(EC, OC, by=c("Latitude","Longitude","Date.Local"))

  dat.stn.join<- Reduce(function(...) merge(..., all=TRUE), list(EC, EC2, OC, OC2, NO3, SO4))
 # dat.stn.join<-join[complete.cases(join),]
  
  # separate m/d/y from Date.Local
  date.stn<-strsplit(dat.stn.join$Date.Local,"-")
  dat.stn.join$year<-as.numeric(sapply(date.stn, "[[", 1) )
  dat.stn.join$month<-as.numeric(sapply(date.stn,"[[",2))
  dat.stn.join$day<-as.numeric(sapply(date.stn,"[[",3))

  stn.list[[i]]<-dat.stn.join
}

stn.all <- do.call("rbind", stn.list) 
stn.all<-subset(stn.all,Longitude >= -120.5 & Longitude < -115.5 & Latitude>= 33.65 & Latitude < 35.5)


# Visual Check
aqspm25.points<-unique(aqspm25.all[,17:18])
aqspm10.points<-unique(aqspm10.all[,17:18])
aqspm10.pm25.points<-unique(aqspm10.pm25.all)
stn.points<-unique(stn.all[,1:2])
 map <- qmap('Los Angeles', zoom = 7, maptype = 'satellite')
 map + geom_point(data = misr.grid, aes(x = lon, y = lat), shape=22, size=3, col="darkgrey",alpha=0.6)+ 
      geom_point(data = aqspm25.points, aes(x = Longitude, y = Latitude),col="magenta", size=4)+ 
        geom_point(data = aqspm10.points, aes(x = Longitude, y = Latitude),col="cyan", size=3)+
          geom_point(data = aqspm10.pm25.points, aes(x = Longitude, y = Latitude),shape=23,col="purple", size=3)+
            geom_point(data = stn.points, aes(x = Longitude, y = Latitude),col="yellow", size=2)
 
 #### Geographic projection for California applied to all lat/lon ####
 proj.albers<-"+proj=aea +lat_1=34.0 +lat_2=40.5 +lon_0=-120.0 +x_0=0 +y_0=-4000000 +ellps=GRS80 +datum=NAD83 +units=km"
 newcoords<-project(as.matrix(cbind(aqspm25.all$Longitude, aqspm25.all$Latitude)), proj=proj.albers)
 aqspm25.all$x<-newcoords[,1]
 aqspm25.all$y<-newcoords[,2]
 newcoords<-project(as.matrix(cbind(aqspm10.all$Longitude, aqspm10.all$Latitude)), proj=proj.albers)
 aqspm10.all$x<-newcoords[,1]
 aqspm10.all$y<-newcoords[,2]
 newcoords<-project(as.matrix(cbind(aqspm10.pm25.all$Longitude, aqspm10.pm25.all$Latitude)), proj=proj.albers)
 aqspm10.pm25.all$x<-newcoords[,1]
 aqspm10.pm25.all$y<-newcoords[,2]
 newcoords<-project(as.matrix(cbind(stn.all$Longitude, stn.all$Latitude)), proj=proj.albers)
 stn.all$x<-newcoords[,1]
 stn.all$y<-newcoords[,2]
 
 write.csv(aqspm25.all,paste("/Users/mf/Documents/AQS/PM25/pm25_", min(aqspm25.all$year),"_",max(aqspm25.all$year),".csv", sep=""),row.names=FALSE)
 write.csv(aqspm10.all,paste("/Users/mf/Documents/AQS/PM10/pm10_", min(aqspm10.all$year),"_",max(aqspm10.all$year),".csv", sep=""),row.names=FALSE)
 write.csv(aqspm10.pm25.all,paste("/Users/mf/Documents/AQS/PM10/pm10_pm25_", min(aqspm10.pm25.all$year),"_",max(aqspm10.pm25.all$year),".csv", sep=""),row.names=FALSE)
 write.csv(stn.all,paste("/Users/mf/Documents/AQS/STN/stn_", min(stn.all$year),"_",max(stn.all$year),".csv", sep=""),row.names=FALSE)
 ### OLD CODE ###
# Dealing with missing site_latitutde and site_longitude. Take out problem sites, merge in lat and lon from other data source

 
 

 
 pm10.pm25<-pm10.pm25[pm10.pm25$SITE_LONGITUDE2.x>=-120 & pm10.pm25$SITE_LONGITUDE2.x<= -117,]
 pm10.pm25<-pm10.pm25[pm10.pm25$SITE_LATITUDE2.x>=33.7 & pm10.pm25$SITE_LATITUDE2.x<=35,]
 pm10.pm25<-pm10.pm25[pm10.pm25$SITE_LONGITUDE2.x != -119.4869,] #Remove Catalina
 
 
 
 
 
 
 
 # Append fixed and ok data

data.pm25.new<-aqspm25.all[aqspm25.all$year %in% c(2008,2009) & aqspm25.all$AQS_PARAMETER_CODE==88101 & aqspm25.all$POC!=3, ]

pm25.problem.data<-data.pm25.new[data.pm25.new$SITE_LATITUDE=='.', ]
pm25.problem.sites<-unique(pm25.problem.data$AQS_SITE_ID)

data.pm25.old<-read.csv("/Users/mf/Documents/MISR/Data/aqsPM25_08_09_ss_frm.csv")
data.pm25.old$AQS_SITE_ID2<-as.numeric(gsub("-", "", data.pm25.old$AQS_SITE_ID))

pm25.fix.sites<-data.pm25.old[data.pm25.old$AQS_SITE_ID2 %in% pm25.problem.sites,c(9,10,17)]
pm25.fix.sites<-rename(pm25.fix.sites, c("AQS_SITE_ID2"="AQS_SITE_ID"))
pm25.fix.sites2<-pm25.fix.sites[!duplicated(pm25.fix.sites),]
pm25.fix.sites.all<-merge(pm25.problem.data,pm25.fix.sites2,by=c("AQS_SITE_ID"))
pm25.fix.sites.all<-rename(pm25.fix.sites.all, c("SITE_LATITUDE.y"="SITE_LATITUDE", "SITE_LONGITUDE.y"="SITE_LONGITUDE"))
# drop some columns
pm25.fix.sites.all<-subset(pm25.fix.sites.all, select=-c(SITE_LATITUDE.x,SITE_LONGITUDE.x))
# get ok data 
pm25.ok.data<-data.pm25.new[data.pm25.new$SITE_LATITUDE!='.', ]
# append
data.pm25.new.fixed<-rbind(pm25.fix.sites.all,pm25.ok.data)
data.pm25.new.fixed$SITE_LATITUDE2<-as.numeric(as.character(data.pm25.new.fixed$SITE_LATITUDE))
data.pm25.new.fixed$SITE_LONGITUDE2<-as.numeric(as.character(data.pm25.new.fixed$SITE_LONGITUDE))

newcoords.fix<-project(as.matrix(cbind(data.pm25.new.fixed$SITE_LONGITUDE2, data.pm25.new.fixed$SITE_LATITUDE2)), proj=proj.albers)
data.pm25.new.fixed$x<-newcoords.fix[,1]
data.pm25.new.fixed$y<-newcoords.fix[,2]

#write.csv(data.pm25.new.fixed,"/Users/mf/Documents/MISR/Data/aqspm25_08_09_CAnew.csv",row.names=FALSE)
data.pm25.new.fixed<-read.csv(data.pm25.new.fixed,"/Users/mf/Documents/MISR/Data/aqspm25_08_09_CAnew.csv",row.names=FALSE)

# Spatially subset as needed
data.pm25.new.fixed<-data.pm25.new.fixed[data.pm25.new.fixed$SITE_LONGITUDE2>=-120 & data.pm25.new.fixed$SITE_LONGITUDE2<= -117,]
data.pm25.new.fixed<-data.pm25.new.fixed[data.pm25.new.fixed$SITE_LATITUDE2>=33.7 & data.pm25.new.fixed$SITE_LATITUDE2<=35,]
data.pm25.new.fixed<-data.pm25.new.fixed[data.pm25.new.fixed$SITE_LONGITUDE2 != -119.4869,] #Remove Catalina



# PM10
data.pm10.new<-aqspm10.all[aqspm10.all$year %in% c(2008,2009) & aqspm10.all$AQS_PARAMETER_CODE==81102, ]

pm10.problem.data<-data.pm10.new[data.pm10.new$SITE_LATITUDE=='.', ]
pm10.problem.sites<-unique(pm10.problem.data$AQS_SITE_ID)

data.pm10.old<-read.csv("/Users/mf/Documents/MISR/Data/aqspm10_08_09_ss.csv")
data.pm10.old$AQS_SITE_ID2<-as.numeric(gsub("-", "", data.pm10.old$AQS_SITE_ID))

pm10.fix.sites<-data.pm10.old[data.pm10.old$AQS_SITE_ID2 %in% pm10.problem.sites,c(9,10,17)]
pm10.fix.sites<-rename(pm10.fix.sites, c("AQS_SITE_ID2"="AQS_SITE_ID"))
pm10.fix.sites2<-pm10.fix.sites[!duplicated(pm10.fix.sites),]
pm10.fix.sites.all<-merge(pm10.problem.data,pm10.fix.sites2,by=c("AQS_SITE_ID"))
pm10.fix.sites.all<-rename(pm10.fix.sites.all, c("SITE_LATITUDE.y"="SITE_LATITUDE", "SITE_LONGITUDE.y"="SITE_LONGITUDE"))
# drop some columns
pm10.fix.sites.all<-subset(pm10.fix.sites.all, select=-c(SITE_LATITUDE.x,SITE_LONGITUDE.x))
# get ok data 
pm10.ok.data<-data.pm10.new[data.pm10.new$SITE_LATITUDE!='.', ]
# append
data.pm10.new.fixed<-rbind(pm10.fix.sites.all,pm10.ok.data)
data.pm10.new.fixed$SITE_LATITUDE2<-as.numeric(as.character(data.pm10.new.fixed$SITE_LATITUDE))
data.pm10.new.fixed$SITE_LONGITUDE2<-as.numeric(as.character(data.pm10.new.fixed$SITE_LONGITUDE))

newcoords.fix<-project(as.matrix(cbind(data.pm10.new.fixed$SITE_LONGITUDE2, data.pm10.new.fixed$SITE_LATITUDE2)), proj=proj.albers)
data.pm10.new.fixed$x<-newcoords.fix[,1]
data.pm10.new.fixed$y<-newcoords.fix[,2]

data.pm10.new.fixed<-data.pm10.new.fixed[data.pm10.new.fixed$SITE_LONGITUDE2>=-120 & data.pm10.new.fixed$SITE_LONGITUDE2<= -117,]
data.pm10.new.fixed<-data.pm10.new.fixed[data.pm10.new.fixed$SITE_LATITUDE2>=33.7 & data.pm10.new.fixed$SITE_LATITUDE2<=35,]
data.pm10.new.fixed<-data.pm10.new.fixed[data.pm10.new.fixed$SITE_LONGITUDE2 != -119.4869,] #Remove Catalina


write.csv(data.pm10.new.fixed,"/Users/mf/Documents/MISR/Data/aqspm10_08_09_CAnew.csv",row.names=FALSE)

# Check
# aqspm10.points<-unique(data.pm10.new.fixed[,23:24])
# map <- qmap('Los Angeles', zoom =9, maptype = 'terrain')
# map + geom_point(data = aqspm10.points, aes(x = SITE_LONGITUDE2, y = SITE_LATITUDE2),col="red",size=3, shape=21)+
# geom_point(data = aqspm25.points, aes(x = SITE_LONGITUDE2, y = SITE_LATITUDE2),col="blue")

dim(aqspm25.points)
dim(aqspm10.points)

# Merge PM25 and PM10 for PM10-PM25
pm10.pm25<-merge(data.pm25.new.fixed, data.pm10.new.fixed, by=c('AQS_SITE_ID','POC','month','day','year'))
pm10.pm25$pm10_pm25<-pm10.pm25$Daily.Mean.PM10.Concentration-pm10.pm25$Daily.Mean.PM2.5.Concentration

pm10.pm25<-pm10.pm25[pm10.pm25$SITE_LONGITUDE2.x>=-120 & pm10.pm25$SITE_LONGITUDE2.x<= -117,]
pm10.pm25<-pm10.pm25[pm10.pm25$SITE_LATITUDE2.x>=33.7 & pm10.pm25$SITE_LATITUDE2.x<=35,]
pm10.pm25<-pm10.pm25[pm10.pm25$SITE_LONGITUDE2.x != -119.4869,] #Remove Catalina

# Check
# sum(!is.na(pm10.pm25$pm10_pm25))
# cor(pm10.pm25$Daily.Mean.PM2.5.Concentration,pm10.pm25$Daily.Mean.PM10.Concentration,use="complete")
# cor(pm10.pm25$Daily.Mean.PM2.5.Concentration,pm10.pm25$Daily.Mean.PM10.Concentration,use="complete")

# Write .csv
write.csv(pm10.pm25,paste("/Users/mf/Documents/MISR/Data/aqspm10_25_",min(pm10.pm25$year),"_",max(pm10.pm25$year),"_CAnew.csv",sep=""),row.names=FALSE) 


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

