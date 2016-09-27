#### Match MISR, AQS, MET, by date and distance 
#### Run MISR Data Processing.R, EPA Data Processing.R, NCDC Data Processing.R to generate necessary datafiles

library(dplyr)
library(plyr)

# Read in processed data

# MISR 2000-2011
misr<-read.csv("/Users/mf/Documents/MISR/Data/misr_2000_2011.csv")
aqspm25<-read.csv("/Users/mf/Documents/AQS/PM25/pm25_2000_2011.csv")
aqspm10<-read.csv("/Users/mf/Documents/AQS/PM10/pm10_2000_2011.csv")
aqspm10_pm25<-read.csv("/Users/mf/Documents/AQS/PM10/pm10_pm25_2000_2011.csv")
stn<-read.csv("/Users/mf/Documents/AQS/STN/stn_2001_2011.csv")
met<-read.csv("/Users/mf/Documents/NCDC/met_2000_2011.csv")

# AQS 2008-2009 Separate files for PM2.5, PM10, PM10-PM2.5 (co-located)
#aqspm25<-read.csv("/Users/mf/Documents/MISR/Data/aqspm25_08_09_CAnew.csv")
#aqspm10<-read.csv("/Users/mf/Documents/MISR/Data/aqspm10_08_09_CAnew.csv")
#aqspm10_pm25<-read.csv("/Users/mf/Documents/MISR/Data/aqspm10_25_2008_2009_CAnew.csv")
#stn<-read.csv("/Users/mf/Documents/AQS/STN/stn_08_09_ss.csv")

# MET
#met<-read.csv("/Users/mf/Documents/MISR/Data/met_08_09.csv")


# Take unique dates from MISR file
misr.days<-misr %>% distinct(date2,.keep_all=TRUE) 
misr.aqspm25.match.all<-vector('list',length(misr.days$date))
misr.aqspm10.match.all<-vector('list',length(misr.days$date))
misr.aqspm2510.match.all<-vector('list',length(misr.days$date))
misr.stn.match.all<-vector('list',length(misr.days$date))

met.aqspm25.match.all<-vector('list',length(misr.days$date))
met.aqspm10.match.all<-vector('list',length(misr.days$date))   
met.aqspm2510.match.all<-vector('list',length(misr.days$date))   
met.stn.match.all<-vector('list',length(misr.days$date)) 

for (i in 1:length(misr.days$date)){ 
  # Select data on MISR days
  aqspm25.daily<-aqspm25[aqspm25$day %in% misr.days[i,]$day &
                           aqspm25$month %in% misr.days[i,]$month &
                           aqspm25$year %in% misr.days[i,]$year,] 
  
  aqspm10.daily<-aqspm10[aqspm10$day %in% misr.days[i,]$day & 
                           aqspm10$month %in% misr.days[i,]$month &
                           aqspm10$year %in% misr.days[i,]$year,] 
  
  aqspm2510.daily<-aqspm10_pm25[aqspm10_pm25$day %in% misr.days[i,]$day & 
                           aqspm10_pm25$month %in% misr.days[i,]$month &
                           aqspm10_pm25$year %in% misr.days[i,]$year,]
  
  stn.daily<-stn[stn$day %in% misr.days[i,]$day &
                           stn$month %in% misr.days[i,]$month &
                           stn$year %in% misr.days[i,]$year,] 
  
  misr.daily<-misr[misr$day %in% misr.days[i,]$day &
                           misr$month %in% misr.days[i,]$month &
                           misr$year %in% misr.days[i,]$year,] 
  
  met.daily<-met[met$day %in% misr.days[i,]$day &
                           met$month %in% misr.days[i,]$month & 
                           met$year %in% misr.days[i,]$year,]
  
  # distance matrices for each dataset
  if(dim(aqspm25.daily)[1]>0){
    dist.misr.pm25<-rdist(cbind(misr.daily$x,misr.daily$y),cbind(aqspm25.daily$x,aqspm25.daily$y))
    dist.pm25.met<-rdist(cbind(met.daily$x,met.daily$y),cbind(aqspm25.daily$x,aqspm25.daily$y))
    } else{
    dist.misr.pm25<-NULL
    dist.pm25.met<-NULL
    }

  if(dim(aqspm10.daily)[1]>0){
    dist.misr.pm10<-rdist(cbind(misr.daily$x,misr.daily$y),cbind(aqspm10.daily$x,aqspm10.daily$y))
    dist.pm10.met<-rdist(cbind(met.daily$x,met.daily$y),cbind(aqspm10.daily$x,aqspm10.daily$y))
    } else{
    dist.misr.pm10<-NULL
    dist.pm10.met<-NULL
    }

  if(dim(aqspm2510.daily)[1]>0){
    dist.misr.pm2510<-rdist(cbind(misr.daily$x,misr.daily$y),cbind(aqspm2510.daily$x,aqspm2510.daily$y))
    dist.pm2510.met<-rdist(cbind(met.daily$x,met.daily$y),cbind(aqspm2510.daily$x,aqspm2510.daily$y))
    } else{
    dist.misr.pm2510<-NULL
    dist.pm2510.met<-NULL
    }

  if(dim(stn.daily)[1]>0){
    dist.misr.stn<-rdist(cbind(misr.daily$x,misr.daily$y),cbind(stn.daily$x,stn.daily$y))
    dist.stn.met<-rdist(cbind(met.daily$x,met.daily$y),cbind(stn.daily$x,stn.daily$y))
    } else{
    dist.misr.stn<-NULL
    dist.stn.met<-NULL
    }
  
  # take pixel that is smallest distance from AQS site (within 5km)
  # identify row of distance matrix (misr pixel id), with smallest column is aqs site
  
  misr.aqspm25.match.list<-vector('list',length(dist.misr.pm25[1,]))
  misr.aqspm10.match.list<-vector('list',length(dist.misr.pm10[1,]))
  misr.aqspm2510.match.list<-vector('list',length(dist.misr.pm2510[1,]))
  misr.stn.match.list<-vector('list',length(dist.misr.stn[1,]))
  
  # Identify MISR pixel closest to AQS site
  # Match MISR and AQS PM25
  if(dim(aqspm25.daily)[1]>0){
    for (j in 1:length(dist.misr.pm25[1,])){ 
       if (min(dist.misr.pm25[,j])<=5){
         misr.aqspm25.match.list[[j]]<-data.frame(misr.daily[which.min(dist.misr.pm25[,j]),],aqspm25.daily[j,]) 
       } 
     }
  } 
  # Match MISR and AQS PM10 
  if(dim(aqspm10.daily)[1]>0){
    for (j in 1:length(dist.misr.pm10[1,])){ 
     if (min(dist.misr.pm10[,j])<=5){
      misr.aqspm10.match.list[[j]]<-data.frame(misr.daily[which.min(dist.misr.pm10[,j]),],aqspm10.daily[j,]) # identifies misr pixel close to AQS PM10 site
    } 
   }
  }  
  
  # Match MISR and AQS PM10-PM25 
  if(dim(aqspm2510.daily)[1]>0){
    for (j in 1:length(dist.misr.pm2510[1,])){ 
      if (min(dist.misr.pm2510[,j])<=5){
        misr.aqspm2510.match.list[[j]]<-data.frame(misr.daily[which.min(dist.misr.pm2510[,j]),],aqspm2510.daily[j,])
      } 
    } 
  }
  
  # Match MISR and STN  
  if (length(dist.misr.stn[1,])>0){
    for (j in 1:length(dist.misr.stn[1,])){ 
       if (min(dist.misr.stn[,j])<=5){
         misr.stn.match.list[[j]]<-data.frame(misr.daily[which.min(dist.misr.stn[,j]),],stn.daily[j,]) # identifies misr pixel close to STN site
       }
     } 
  }
  
  misr.aqspm25.match.all[[i]] <- do.call("rbind", misr.aqspm25.match.list) 
  misr.aqspm10.match.all[[i]] <- do.call("rbind", misr.aqspm10.match.list) 
  misr.aqspm2510.match.all[[i]] <- do.call("rbind", misr.aqspm2510.match.list) 
  misr.stn.match.all[[i]] <- do.call("rbind", misr.stn.match.list) 
  
  # now match AQS site with closest NOAA met site
  met.pm25.match.list<-vector('list',length(dist.pm25.met[1,]))
  met.pm10.match.list<-vector('list',length(dist.pm10.met[1,]))
  met.pm2510.match.list<-vector('list',length(dist.pm2510.met[1,]))
  met.stn.match.list<-vector('list',length(dist.stn.met[1,]))
  
  # match AQS PM25 with met
  if(dim(aqspm25.daily)[1]>0){
     for (j in 1:length(dist.pm25.met[1,])){ 
       if (min(dist.pm25.met[,j])<=25){
         met.pm25.match.list[[j]]<-data.frame(met.daily[which.min(dist.pm25.met[,j]),],aqspm25.daily[j,]) 
       }
     }
  }
  # match AQS PM10 with met
  if(dim(aqspm10.daily)[1]>0){
  for (j in 1:length(dist.pm10.met[1,])){ 
    if (min(dist.pm10.met[,j])<=25){
      met.pm10.match.list[[j]]<-data.frame(met.daily[which.min(dist.pm10.met[,j]),],aqspm10.daily[j,]) 
      }
    }
  }
  # match AQS PM10-PM25 with met
  if(dim(aqspm2510.daily)[1]>0){
    for (j in 1:length(dist.pm2510.met[1,])){ 
      if (min(dist.pm2510.met[,j])<=25){
        met.pm2510.match.list[[j]]<-data.frame(met.daily[which.min(dist.pm2510.met[,j]),],aqspm2510.daily[j,]) 
      }
    }
  }
  
  # match STN with met
   if (length(dist.stn.met[1,])>0){
     for (j in 1:length(dist.stn.met[1,])){ 
       if (min(dist.stn.met[,j])<=25){
       met.stn.match.list[[j]]<-data.frame(met.daily[which.min(dist.stn.met[,j]),],stn.daily[j,]) 
     }
    }
   }
  
  met.aqspm25.match.all[[i]] <- do.call("rbind", met.pm25.match.list)
  met.aqspm10.match.all[[i]] <- do.call("rbind", met.pm10.match.list)
  met.aqspm2510.match.all[[i]] <- do.call("rbind", met.pm2510.match.list)
  met.stn.match.all[[i]] <- do.call("rbind", met.stn.match.list)
}

misr.aqspm25 <- do.call("rbind", misr.aqspm25.match.all)
#write.csv(misr.aqspm25,"/Users/mf/Documents/MISR/Data/misr_aqspm25_new.csv",row.names=FALSE)
#Check
#cor(misr.aqspm25$AOD,misr.aqspm25$PM25,use="complete")

misr.aqspm10 <- do.call("rbind", misr.aqspm10.match.all)
#write.csv(misr.aqspm10,"/Users/mf/Documents/MISR/Data/misr_aqspm10_new.csv",row.names=FALSE)
#Check
#cor(misr.aqspm10$AODlarge,misr.aqspm10$PM10,use="complete")

misr.aqspm2510 <- do.call("rbind", misr.aqspm2510.match.all)
#misr.aqspm2510b <-misr.aqspm2510[complete.cases(misr.aqspm2510$pm10_pm25) & misr.aqspm2510$pm10_pm25>0,]
#write.csv(misr.aqspm2510b,"/Users/mf/Documents/MISR/Data/misr_aqspm2510_new.csv",row.names=FALSE)
#Check
#sum(!is.na(misr.aqspm2510$pm10_pm25))
#hist(misr.aqspm2510b$pm10_pm25)
#cor(misr.aqspm2510b$AODlarge,misr.aqspm2510b$pm10_pm25,use="complete")

misr.stn <- do.call("rbind", misr.stn.match.all)
#write.csv(misr.stn,"/Users/mf/Documents/MISR/Data/misr_stn.csv",row.names=FALSE)

aqspm25.met <- do.call("rbind", met.aqspm25.match.all)
#write.csv(aqspm25.met, "/Users/mf/Documents/MISR/Data/aqspm25_met_new.csv",row.names=FALSE)

aqspm10.met <- do.call("rbind", met.aqspm10.match.all)
#write.csv(aqspm10.met, "/Users/mf/Documents/MISR/Data/aqspm10_met_new.csv",row.names=FALSE)

aqspm2510.met <- do.call("rbind", met.aqspm2510.match.all)
#aqspm2510.metb <-aqspm2510.met[complete.cases(aqspm2510.met$pm10_pm25) & aqspm2510.met$pm10_pm25>0,]
#write.csv(aqspm2510.metb, "/Users/mf/Documents/MISR/Data/aqspm2510_met_new.csv",row.names=FALSE)

aqsstn.met <- do.call("rbind", met.stn.match.all)
#write.csv(aqsstn.met, "/Users/mf/Documents/MISR/Data/aqsstn_met.csv",row.names=FALSE)

# merge with met and write dataset
misr.aqspm25.met<-join(misr.aqspm25, aqspm25.met, by=c('AQS_SITE_ID','month','day','year'))
write.csv(misr.aqspm10.met,paste("/Users/mf/Documents/MISR/Data/Match 2000-2011/misr_pm10_met_", min(misr.aqspm10.met$year),"_",max(misr.aqspm10.met$year),".csv", sep=""),row.names=FALSE)

misr.aqspm10.met<-join(misr.aqspm10, aqspm10.met, by=c('AQS_SITE_ID','month','day','year'))
write.csv(misr.aqspm10.met,paste("/Users/mf/Documents/MISR/Data/Match 2000-2011/misr_pm10_met_", min(misr.aqspm10.met$year),"_",max(misr.aqspm10.met$year),".csv", sep=""),row.names=FALSE)

misr.aqspm2510.met<-join(misr.aqspm2510, aqspm2510.met, by=c('AQS_SITE_ID','month','day','year'))
write.csv(misr.aqspm2510.met,paste("/Users/mf/Documents/MISR/Data/Match 2000-2011/misr_pm10_pm25_met_", min(misr.aqspm2510.met$year),"_",max(misr.aqspm2510.met$year),".csv", sep=""),row.names=FALSE)

#sum(!is.na(misr.aqspm2510.met$pm10_pm25))

misr.aqsstn.met<-join(misr.stn, aqsstn.met, by=c('lat','lon','month','day','year'))
write.csv(misr.aqsstn.met,paste("/Users/mf/Documents/MISR/Data/Match 2000-2011/misr_stn_met_", min(misr.aqsstn.met$year),"_",max(misr.aqsstn.met$year),".csv", sep=""),row.names=FALSE)

