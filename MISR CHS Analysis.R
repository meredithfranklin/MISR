library(foreign)
library(dplyr)
library(ggplot2)
library(chron) # for converting julian dates
library(lubridate) # for date interval matching
library(data.table)

# MISR predictions PM2.5 and PM10
misr_06_11_pred_pm25<-read.csv("/Users/mf/Documents/MISR/Data/predicted_pm25_2006_2011.csv")
misr_06_11_pred_pm25<-misr_06_11_pred_pm25[,-1]
misr_06_11_pred_pm25$x<-round(misr_06_11_pred_pm25$x.1,6)
misr_06_11_pred_pm25$y<-round(misr_06_11_pred_pm25$y.1,6)

misr_06_11_pred_pm10<-read.csv("/Users/mf/Documents/MISR/Data/predicted_pm10_2006_2011.csv")
# merge predicted datasets and keep only merging variables and predicted PM
misr_06_11_pred_pm10<-misr_06_11_pred_pm10[ ,c(14:16,23:26)]
misr_06_11_pred_pm10$x<-round(misr_06_11_pred_pm10$x.1,6)
misr_06_11_pred_pm10$y<-round(misr_06_11_pred_pm10$y.1,6)

setDT(misr_06_11_pred_pm25)[misr_06_11_pred_pm10, on = c('x', 'y', 'year','month','day')]
setDT(misr_06_11_pred_pm25)
setDT(misr_06_11_pred_pm10)
misr_06_11_allpred<-merge(misr_06_11_pred_pm25, misr_06_11_pred_pm10, by = c('x', 'y', 'year','month','day'))
# Coarse PM
misr_06_11_allpred$predictedPM10_25<-misr_06_11_allpred$predicted.pm10-misr_06_11_allpred$predicted.pm25

# Annual concentrations by grid cell
misr_pm25_annual <- group_by(misr_06_11_pred_pm25, lat, lon, year)
misr_annual_pred_pm25 <- summarize(misr_pm25_annual, count = n(), PM25ann = mean(predicted.pm25, na.rm = T), 
                                   AODsm_med_ann = mean(AODsm_med, na.rm = T))
misr_annual_pred_pm25_2006<-misr_annual_pred_pm25[misr_annual_pred_pm25$year==2006,]
misr_annual_pred_pm25_2007<-misr_annual_pred_pm25[misr_annual_pred_pm25$year==2007,]
misr_annual_pred_pm25_2008<-misr_annual_pred_pm25[misr_annual_pred_pm25$year==2008,]
misr_annual_pred_pm25_2009<-misr_annual_pred_pm25[misr_annual_pred_pm25$year==2009,]
misr_annual_pred_pm25_2010<-misr_annual_pred_pm25[misr_annual_pred_pm25$year==2010,]
misr_annual_pred_pm25_2011<-misr_annual_pred_pm25[misr_annual_pred_pm25$year==2011,]


# export to ArcGIS for examination
write.csv(misr_annual_pred_pm25_2011,"/Users/mf/Documents/MISR/Data/MISR PM predictions/misr_annual_pred_pm25_2011.csv",row.names=FALSE)

#######################
# health data
# match CHS locations with MISR data 

chs<-read.csv("/Volumes/Projects_EH/CHSICV/Temp/TempMeredith/CohortE/cohortE_all.csv")

# Summary Statistics
# Full cohort
summary(chs)

# By follow-up year

s<-chs %>% 
  group_by(yr) %>% 
  summarise(age=mean(AGEPFT,na.rm=TRUE), agesd=sd(AGEPFT,na.rm=TRUE),
            boys=mean(MALE,na.rm=TRUE), ht=mean(HEIGHT,na.rm=TRUE), htsd=sd(HEIGHT,na.rm=TRUE),
            wt=mean(WEIGHT,na.rm=TRUE),  wtsd=sd(WEIGHT,na.rm=TRUE),
            fev=mean(fev1,na.rm=TRUE), fevsd=sd(fev1,na.rm=TRUE),
            fvc=mean(FVC,na.rm=TRUE), fvcsd=sd(FVC,na.rm=TRUE), n=n())
# community specific
s<-chs %>% 
  group_by(townabbr, yr) %>% 
  summarise(age=mean(AGEPFT,na.rm=TRUE), agesd=sd(AGEPFT,na.rm=TRUE),
            boys=mean(MALE,na.rm=TRUE), ht=mean(HEIGHT,na.rm=TRUE), htsd=sd(HEIGHT,na.rm=TRUE),
            wt=mean(WEIGHT,na.rm=TRUE),  wtsd=sd(WEIGHT,na.rm=TRUE),
            fev=mean(fev1,na.rm=TRUE), fevsd=sd(fev1,na.rm=TRUE),
            fvc=mean(FVC,na.rm=TRUE), fvcsd=sd(FVC,na.rm=TRUE), n=n())


pdf("/Users/mf/Documents/MISR/Papers and Reports/MISR CHS Paper/FEV_growth_plot.pdf")
p <- ggplot(na.omit(chs[,c("sex","fev1","yr","id")]), aes(x = as.factor(yr), y = fev1, group = id))
p + geom_point() + stat_smooth(aes(group = 1), method = "lm",se=TRUE) + stat_summary(aes(group = 1), geom = "point", fun.y = mean, shape = 15, size = 4,col="blue")+
  facet_grid(. ~ sex) +
  scale_x_discrete(name="Age", labels=c("6" = "11", "8" = "13", "10" = "15"))+
  scale_y_discrete(name="FEV1")+
  theme(axis.text=element_text(size=14), axis.title=element_text(size=14))
dev.off()


pdf("/Users/mf/Documents/MISR/Papers and Reports/MISR CHS Paper/FVC_growth_plot.pdf")
p <- ggplot(na.omit(chs[,c("sex","FVC","yr","id")]), aes(x = as.factor(yr), y = FVC, group = id))
p + geom_point() + stat_smooth(aes(group = 1), method = "lm",se=TRUE) + stat_summary(aes(group = 1), geom = "point", fun.y = mean, shape = 15, size = 4, col="blue")+
  facet_grid(. ~ sex) +
  scale_x_discrete(name="Age", labels=c("6" = "11", "8" = "13", "10" = "15"))+
  theme(axis.text=element_text(size=14), axis.title=element_text(size=14))
dev.off()


icv.points<-unique(icv_new2[,32:33])

# find unique startdates and create interval to match MISR
CHS_startdays<- chs %>% distinct(datestart2,.keep_all=TRUE)
CHS_startdays$samp_int <- interval(CHS_startdays$pftstart, CHS_startdays$pftend)

# MISR grid x,y to make 1km grid
misr_06_11<-misr_06_11_pred_pm25[misr_06_11_pred_pm25$land.water.mask==3,]

MISR_grid<-unique(misr_06_11[,1:2])

MISR_CHS_match_all<-vector('list',length(chs$pftstart))

for (i in 1:length(ICV_startdays$startdate)){
  # take MISR averages between each ICV start and end date (sampling.interval)
  date_match<-misr_06_11[ymd(misr_06_11$date2) %within% CHS_startdays$samp_int[i],]
  misr_pftavg <- ddply(date_match, .(x,y), summarise, lat.misr=mean(lat),lon.misr=mean(lon),
                        AODm=mean(AOD), AODsmallm=mean(AODsmall), AODmedm=mean(AODmed), 
                        AODlargem=mean(AODlarge), AODnonsphm=mean(AODnonspher),
                        predPM25=mean(predicted.pm25),predPM10=mean(predicted.pm10),predPM10_25=mean(predictedPM10_25))
  #knots=dim(misr_monthly)[1]/3
  #misr_monthly_gam<-gam(AODm~s(x,y,k=knots),data=misr_monthly,na.action='na.exclude')
  #misr_monthly_pred_smooth<-predict.gam(misr_monthly_gam,newdata = MISR_grid)
  
  # select icv observations for ith startdate  
  icv_monthly<-icv_new2[icv_new2$datestart2 %in% ICV_startdays$datestart2[i],]
  # remove missing locations
  icv_monthly<-icv_monthly[!is.na(icv_monthly$x),]
  # calculate distance between misr pixels and ICV sites
  dist<-rdist(cbind(misr_monthly$x,misr_monthly$y),cbind(icv_monthly$x,icv_monthly$y))
  
  MISR_ICV_match_list<-vector('list',length(dist[1,]))
  
  for (j in 1:length(dist[1,])){ 
    if (min(dist[,j])<=3){
      MISR_ICV_match_list[[j]]<-data.frame(misr_monthly[which.min(dist[,j]),],icv_monthly[j,]) # identifies misr pixel close to ICV site
    } 
  }
  MISR_ICV_match_all[[i]] <- do.call("rbind", MISR_ICV_match_list) 
}

MISR_ICV <- do.call("rbind", MISR_ICV_match_all)


as.numeric.factor <- function(x) {as.numeric(levels(x))[x]}


hosp.adm<-read.csv("/Volumes/projects_cen/NIS/Paper/Meredith Franklin/hosp_ca_08_09.csv")
keep.vars<-c("AGE","AMONTH","YEAR","HOSPZIP","FEMALE","RACE","ischemic","MS","Bronchitis","ari","Pneumonia","Influenza","copd","Asthma","HOSPADDR","HOSPCITY","HOSPNAME")
hosp.adm<-hosp.adm[keep.vars]
write.csv(hosp.adm,"/Volumes/projects_cen/NIS/Paper/Meredith Franklin/hosp_ca_ss_08_09.csv")

# zip code data for LA to subset hosp adm
zip<-read.dbf("/Users/mf/Documents/MISR/Data/Health/LA_zip.dbf")
zip$HOSPZIP<-as.numeric.factor(zip$ZCTA5CE10)
zip$lat<-as.numeric.factor(zip$INTPTLAT10)
zip$lon<-as.numeric.factor(zip$INTPTLON10)

# subset hosp admit 
hosp.adm<-read.csv("/Volumes/projects_cen/NIS/Paper/Meredith Franklin/hosp_ca_ss_08_09.csv")
hosp.adm$age2<-as.numeric.factor(hosp.adm$AGE)


# merge zip data with hosp admit
zip.hosp.adm<-join(zip, hosp.adm, by='HOSPZIP')
child.hosp.adm<-zip.hosp.adm[zip.hosp.adm$age2<18,]
summary(child.hosp.adm)
count(child.hosp.adm, c("ari"))
table(child.hosp.adm$ari)
count(child.hosp.adm, c("Bronchitis"))
count(child.hosp.adm, c("Pneumonia"))
count(child.hosp.adm, c("Influenza"))

# take counts by zip code
log.mod<-glm(ari~as.factor(RACE)+FEMALE+ age2,data=child.hosp.adm)
# match zip lat/lon with MISR pixel


