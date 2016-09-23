##### Daily NCDC data #####
# Station Information
#file <- "ftp://ftp.ncdc.noaa.gov/pub/data/noaa/isd-history.csv"
#download.file(file, "isd-history.csv")
library(R.utils)

# get latest station information
download.file("ftp://ftp.ncdc.noaa.gov/pub/data/noaa/isd-history.csv", "/Users/mf/Documents/NCDC/stationlist.csv", method='wget') 

stations <- read.csv("/Users/mf/Documents/NCDC/stationlist.csv") 
st.ca<-stations[stations$CTRY=="US" & stations$STATE=="CA",]
st.ca$BEGIN <- as.numeric(substr(st.ca$BEGIN, 1, 4))
st.ca$END <- as.numeric(substr(st.ca$END, 1, 4))

st.so.ca<-st.ca[st.ca$LAT>=33.65 & st.ca$LAT<=35.5,]
st.so.ca<-st.so.ca[st.so.ca$LON>= -120.5 & st.so.ca$LON<= -115.9,]
# Remove Catalina and buoys
st.so.ca<-st.so.ca[-(grep(c("BUOY|CATALINA|ISLAND"),st.so.ca$STATION.NAME)),]
st.so.ca<-st.so.ca[complete.cases(st.so.ca),]
# write.csv(st.so.ca,"/Users/mf/Documents/NCDC/SoCalNCDCsites.csv",row.names = FALSE)
# st.so.ca<-read.csv("/Users/mf/Documents/NCDC/SoCalNCDCsites.csv")
met.list.all<-vector('list')
for (y in 2000:2011){
  y.la.list<-st.so.ca[st.so.ca$BEGIN<=y & st.so.ca$END>=y,]

  # Download from NOAA ftp 
  for (s in 1:dim(y.la.list)[1]){
    filename<-paste(sprintf("%06d",y.la.list[s,1]),"-",sprintf("%05d",y.la.list[s,2]),"-",y,".gz",sep="")
    download.file(paste("ftp://ftp.ncdc.noaa.gov/pub/data/noaa/", y,"/",filename,sep=""), paste("/Users/mf/Documents/NCDC/SoCal Met/",filename,sep=""), method='wget') 
  }
  # unzip
  files.gz <- list.files("./SoCal Met",full.names=TRUE,pattern=".gz")
    
    for(i in 1:length(files.gz)){
       gunzip(files.gz[[i]],overwrite=TRUE)
    }
  
  # Extract data from downloaded files
  # Need to define column widths, see ftp://ftp.ncdc.noaa.gov/pub/data/noaa/ish-format-document.pdf
  column.widths <- c(4, 6, 5, 4, 2, 2, 2, 2, 1, 6, 7, 5, 5, 5, 4, 3, 1, 1, 4, 1, 5, 1, 1, 1, 6, 1, 1, 1, 5, 1, 5, 1, 5, 1)

  met.files <- list.files("/Users/mf/Documents/NCDC/SoCal Met",pattern=paste("*-",y,sep=""),full.names=TRUE,include.dirs = FALSE, recursive=FALSE)
  met.list<-vector('list',length(met.files))

  for (i in 1:length(met.files)) {
    if (file.info(met.files[i])$size>0){
      met.data <- read.fwf(met.files[i], column.widths)
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
      met.data$rh=100*((112-0.1*met.data$temp+met.data$dew.point)/(112+0.9*met.data$temp))^8
      
      #drop some variables
      met.data<-subset(met.data, select=-c(ID,srcflag,typecode,callid,qcname))
    # take MISR overpass average 10am-12pm
      met.data.misr.hrs<-met.data[met.data$hour %in% c(10,11,12),]
      met.data.avg<- ddply(met.data.misr.hrs, .(year, month, day,lat,lon), summarise, temp=mean(temp,na.rm=TRUE),
                              dew.point=mean(dew.point,rm=TRUE), ceiling.ht=mean(ceiling.ht,na.rm=TRUE), wind.dir=mean(wind.dir,na.rm=TRUE),
                              wind.sp=mean(wind.sp,na.rm=TRUE), atm.press=mean(atm.press,na.rm=TRUE))

      met.list[[i]]<-met.data.avg
    }
    met.list.all[[y]]<- do.call("rbind", met.list) 
  }
}
met<- do.call("rbind", met.list.all) 

# add projected coordinates (California)
proj.albers<-"+proj=aea +lat_1=34.0 +lat_2=40.5 +lon_0=-120.0 +x_0=0 +y_0=-4000000 +ellps=GRS80 +datum=NAD83 +units=km"

newcoords.met<-project(as.matrix(cbind(met$lon, met$lat)), proj=proj.albers)
met$x<-newcoords.met[,1]
met$y<-newcoords.met[,2]

# read/write
write.csv(met,paste("met_",min(met$year),"_",max(met$year),".csv", sep=""),row.names=FALSE)
#met.08.09<-read.csv("/Users/mf/Documents/MISR/Data/met_08_09.csv")
# Check

met.points<-unique(met[,4:5])
map <- qmap('Los Angeles', zoom =8, maptype = 'satellite')
map + geom_point(data = met.points, aes(x = lon, y = lat),col="magenta", size=4)

