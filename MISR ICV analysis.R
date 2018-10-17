library(sas7bdat) # for reading SAS file formats

library(lubridate) # for date interval matching
library(chron)

library(plyr) # for easy merging/subsetting
library(dplyr) #for easy merging/subsetting

library(fields) # for spatial functions
library(proj4) # for map projections

library(mgcv)

library(data.table)

#### Processed MISR 08-09 data see MISR Data Extract.R for details ####
misr_08_09<-read.csv("/Users/mf/Documents/MISR/Data/misr_08_09.csv")
#misr_08_09$AODsm_med<-misr_08_09$AODsmall+misr_08_09$AODmed

# MISR data with predicted PM2.5
misr_06_11_pred_pm25<-read.csv("/Users/mf/Documents/MISR/Data/predicted_pm25_2006_2011.csv")
# subset to ICV years predicted PM2.5
misr_08_09_pred_pm25<-misr_06_11_pred_pm25[misr_06_11_pred_pm25$year %in% c(2008,2009),-1]
misr_08_09_pred_pm25$x<-round(misr_08_09_pred_pm25$x.1,6)
misr_08_09_pred_pm25$y<-round(misr_08_09_pred_pm25$y.1,6)

# MISR data with predicted PM10
misr_06_11_pred_pm10<-read.csv("/Users/mf/Documents/MISR/Data/predicted_pm10_2006_2011.csv")
# subset to ICV years keep only merging variables and predicted PM10
misr_08_09_pred_pm10<-misr_06_11_pred_pm10[misr_06_11_pred_pm10$year %in% c(2008,2009),c(14:16,23:26)]
misr_08_09_pred_pm10$x<-round(misr_08_09_pred_pm10$x.1,6)
misr_08_09_pred_pm10$y<-round(misr_08_09_pred_pm10$y.1,6)
#data.table setDT
setDT(misr_08_09_pred_pm25)[misr_08_09_pred_pm10, on = c('x', 'y', 'year','month','day')]
setDT(misr_08_09_pred_pm25)
setDT(misr_08_09_pred_pm10)
misr_08_09_allpred<-merge(misr_08_09_pred_pm25, misr_08_09_pred_pm10, by = c('x', 'y', 'year','month','day'))

misr_08_09_allpred$predictedPM10_25<-misr_08_09_allpred$predicted.pm10-misr_08_09_allpred$predicted.pm25

# averages by season for mapping
pm25.pred.warm.avg<-ddply(misr_08_09_pred_pm25[misr_08_09_pred_pm25$month %in% c(5:10),], .(lat,lon, year), summarise, AOD=mean(AOD,na.rm=TRUE),AODsm_med=mean(AODsm_med,na.rm=TRUE),PM25=mean(predicted.pm25,na.rm=TRUE))
#write.csv(pm25.pred.warm.avg[pm25.pred.warm.avg$year==2008,],"/Users/mf/Documents/MISR/Data/predpm25_warm_08.csv")

#### Processed NOAA met data (daily from hourly to match MISR overpass) ####
#met_08_09<-read.csv("/Users/mf/Documents/MISR/Data/met_08_09.csv")

# ICV data (monthly with odd start dates)
icv<-read.sas7bdat("/Volumes/Projects_EH/CHSICV/Temp/TempRima/ICV2 Spatial Modeling/icv2spatial_01jun15.sas7bdat")
# Keep only relevant columns
icv<-icv[,c(1:6,110,115)]
# ICV data (monthly with enddates)
icv_long<-read.sas7bdat("/Volumes/Projects_EH/CHSICV/Temp/TempRima/ICV2 Spatial Modeling/icv2temporal_31jul15.sas7bdat")
icv_long<-icv_long[,c(1:16,113:117,462)]
icv_new<-merge(icv,icv_long,by=c("Space_ID","startdate"),all=TRUE)

icv_new$datestart<-dates(icv_new$startdate,origin=c(month=1,day=1,year=1960))
icv_new$datestart2<-mdy(icv_new$datestart)
icv_new$dateend<-dates(icv_new$enddate,origin=c(month=1,day=1,year=1960))
icv_new$dateend2<-mdy(icv_new$dateend)



unique.icv<-icv_new %>% distinct(lat , town.x, season)
table(unique.icv$season,unique.icv$town.x)

#### Geographic projection for California applied to all lat/lon ####
proj.albers<-"+proj=aea +lat_1=34.0 +lat_2=40.5 +lon_0=-120.0 +x_0=0 +y_0=-4000000 +ellps=GRS80 +datum=NAD83 +units=km"
newcoords_icv<-project(as.matrix(cbind(icv_new$lon, icv_new$lat)), proj=proj.albers)
icv_new$x<-newcoords_icv[,1]
icv_new$y<-newcoords_icv[,2]

icv_new2<-icv_new[!is.na(icv_new$PM25),]
unique.icv2<-icv_new2 %>% distinct(lat , town.x, season)
table(unique.icv2$season,unique.icv2$town.x)
View(icv_new2)
#write.csv(icv_new2,"/Users/mf/Documents/MISR/Data/icv_new2.csv",row.names=FALSE)
icv_new2<-read.csv("/Users/mf/Documents/MISR/Data/icv_new2.csv")


icv_new_w<-icv_new2[icv_new2$season=="warm", ]
icv_new_c<-icv_new2[icv_new2$season=="cool", ]

icv_points_w<-unique(icv_new_w[,13:14])#n=218
icv_points_c<-unique(icv_new_c[,13:14])#n=203

newcoords_icv<-project(as.matrix(cbind(icv.points$lon, icv.points$lat)), proj=proj.albers)
icv_dist<-dist(newcoords_icv)
icv_dist2<-icv_dist[icv_dist<=0.5]
hist(icv_dist2)
# find unique startdates and create interval to match MISR
ICV_startdays<-icv_new2 %>% distinct(datestart2,.keep_all=TRUE)
ICV_startdays$samp_int <- interval(ICV_startdays$datestart2, ICV_startdays$dateend2)

# MISR grid x,y to make 1km grid
misr_08_09<-misr_08_09_allpred[misr_08_09_allpred$land.water.mask==3,]
#misr_08_09<-misr_08_09_pred_pm10[misr_08_09_pred_pm10$land.water.mask==3,]
MISR_grid<-unique(misr_08_09[,1:2])

MISR_ICV_match_all<-vector('list',length(icv_new2$startdate))

for (i in 1:length(ICV_startdays$startdate)){
  # take MISR averages between each ICV start and end date (sampling.interval)
  date_match<-misr_08_09[ymd(misr_08_09$date2) %within% ICV_startdays$samp_int[i],]
  misr_monthly <- ddply(date_match, .(x,y), summarise, lat.misr=mean(lat),lon.misr=mean(lon),
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

# number of MISR grid cells per community
unique.misr<-MISR_ICV %>% distinct(x, town.x)

# number of ICV sites per community
unique.icv<-MISR_ICV %>% distinct(lat, town.x, season)
table(unique.icv$season,unique.icv$town.x)
# number of ICV sites per MISR grid cell
unique.misr.icv<-MISR_ICV %>% distinct(lat, x, y, town.x)
table(unique.misr.icv$x,unique.misr.icv$town.x)


write.csv(MISR_ICV,"/Users/mf/Documents/MISR/Data/MISR_ICV_new.csv",row.names=FALSE)
MISR_ICV<-read.csv("/Users/mf/Documents/MISR/Data/MISR_ICV_new.csv")

# summary statistics

s<-MISR_ICV %>%
  group_by(townname, season) %>%
  summarise(mean=mean(AODm,na.rm=TRUE),median=median(AODm,na.rm=TRUE),iqr=IQR(AODm,na.rm=TRUE),min=min(AODm,na.rm=TRUE),max=max(AODm,na.rm=TRUE),sd=sd(AODm,na.rm=TRUE),n=n())
s2<-data.frame(s)
sum(s2[s2$season=="warm",]$n)
sum(s2[s2$season=="cool",]$n)

#MISR_ICVss<-MISR_ICV[,c(1:35,141:143,703:708)]

# remove grid cells with only one ICV
MISR_ICV2<-MISR_ICV %>% group_by(AODm) %>% mutate(count = n())
#MISR_ICV2<-MISR_ICV2[MISR_ICV2$count>1,]

s2<-MISR_ICV2 %>%
  group_by(townname, season) %>%
summarise(mean=mean(count,na.rm=TRUE),meanpredPM=mean((predPM25),na.rm=TRUE),sdpredPM=sd(predPM25),
          meanpredPMC=mean(predPM10_25,na.rm=TRUE),sdpredPMC=sd(predPM10_25),
          meanPM=mean((PM25/1000)),sdPM=sd(PM25/1000),medPM=median(PM25/1000),iqrPM=IQR(PM25/1000),
          meanPMC=mean(Coarse/1000),sdPMC=sd(Coarse/1000))
s2<-data.frame(s2)
summary(lm(s2$meanpredPM~ s2$meanPM))

unique.misr2<-MISR_ICV2 %>% distinct(x, town.x,season)
table(unique.misr2$town.x,unique.misr2$season)
unique.icv2<-MISR_ICV2 %>% distinct(PM25, town.x,season)
table(unique.icv2$town.x,unique.icv2$season)

# ICC
library(irr)
library(psych)
library(lme4)
# treat misr grid as a grouping variable (random effect)
MISR_ICV2$MISRpredPM25<-as.factor(MISR_ICV2$predPM25)

# Cool
MISR_ICV_c<-MISR_ICV2[MISR_ICV2$season=="cool",]
MISR_ICV_c$AODsm_medm<-MISR_ICV_c$AODsmallm+MISR_ICV_c$AODmedm
MISR_ICV_ssc <- na.omit(subset(MISR_ICV_c,select=c(AODm,count)))
MISR_ICV_ssc2 <- na.omit(subset(MISR_ICV_c,select=c(AODsm_medm,PM25,count)))
MISR_ICV_ssc3 <- na.omit(subset(MISR_ICV_c,select=c(predPM25,PM25,count)))
MISR_ICV_ssc4 <- na.omit(subset(MISR_ICV_c,select=c(predPM10_25,Coarse,count)))
MISR_ICV_ssc5 <- na.omit(subset(MISR_ICV_c,select=c(AODlargem,Coarse,count)))


lme.pm.c<-lmer((PM25/1000)~ (1|MISRpredPM25),data=MISR_ICV_c)
summary(lme.pm.c)
CV.c=100*1.338/13.4106
ICC=16.99/(16.99+1.791)

lme.c.c<-lmer((Coarse/1000)~ (1|predPM10_25),data=MISR_ICV_c)
summary(lme.c.c)
CV.c.c=100*1.179/11.2282
ICC=13.71/(13.71+1.39)

#anova.pm25.cool<-aov(lm(PM25/1000~as.factor(predPM25),data=MISR_ICV_c))
#icc.pm25.cool<-icc(cbind(PM=MISR_ICV_c$PM25/1000,group=as.factor(MISR_ICV_c$predPM25)),model="oneway",type="agreement",unit="average")
#icc.pm25.cool<-ICC(cbind(PM=(MISR_ICV_c$PM25/1000),group=as.factor(MISR_ICV_c$predPM25)))

# Warm
MISR_ICV_w<-MISR_ICV2[MISR_ICV2$season=="warm",]
MISR_ICV_w$AODsm_medm<-MISR_ICV_w$AODsmallm+MISR_ICV_w$AODmedm
MISR_ICV_ssw <- na.omit(subset(MISR_ICV_w,select=c(AODm,PM25,count)))
MISR_ICV_ssw2 <- na.omit(subset(MISR_ICV_w,select=c(AODsm_medm,PM25,count)))
MISR_ICV_ssw3 <- na.omit(subset(MISR_ICV_w,select=c(predPM25,PM25,count)))
MISR_ICV_ssw4 <- na.omit(subset(MISR_ICV_w,select=c(predPM10_25,Coarse,count)))
MISR_ICV_ssw5 <- na.omit(subset(MISR_ICV_w,select=c(AODlargem,Coarse,count)))

lme.pm.w<-lmer((PM25/1000)~ (1|predPM25),data=MISR_ICV_w)
summary(lme.pm.w)
CV.w=100*1.539/15.5272
ICC=0.6545/(0.6545+2.3674)

icc.pm25.warm<-icc(cbind(PM=MISR_ICV_w$PM25/1000,group=as.factor(MISR_ICV_w$predPM25)),model="oneway",type="agreement",unit="average")
icc.pm25.warm<-ICC(cbind(PM=MISR_ICV_w$PM25/1000,group=as.factor(MISR_ICV_w$predPM25)))


lme.c.w<-lmer((Coarse/1000)~ (1|predPM10_25),data=MISR_ICV_w)
summary(lme.c.w)
CV.c.w=100*2.649/13.942
ICC=5.608/(5.608+7.018)

anova.coarse.cool<-aov(lm(Coarse/1000~as.factor(predPM10_25),data=MISR_ICV_c))
icc.coarse.cool<-icc(cbind(PM=MISR_ICV_c$Coarse/1000,group=as.factor(MISR_ICV_c$predPM10_25)),model="twoway",type="agreement",unit="average")
icc.coarse.cool<-ICC(cbind(PM=MISR_ICV_c$Coarse/1000,group=as.factor(MISR_ICV_c$predPM10_25)))


anova.coarse.warm<-aov(lm(Coarse/1000~as.factor(predPM10_25),data=MISR_ICV_w))
icc.coarse.warm<-icc(cbind(PM=MISR_ICV_w$Coarse/1000,group=as.factor(MISR_ICV_w$predPM10_25)),model="twoway",type="agreement",unit="average")
icc.coarse.warm<-ICC(cbind(PM=MISR_ICV_w$Coarse/1000,group=as.factor(MISR_ICV_w$predPM10_25)))

# average variance in grid cells
s<-MISR_ICV_c %>%
  group_by(predPM25) %>%
  summarise(counts=mean(count,na.rm=TRUE),varpm25=var(PM25/1000,na.rm=TRUE),meanPM25=mean(PM25/1000,na.rm=TRUE),
            varcoarse=var(Coarse/1000,na.rm=TRUE),meancoarse=mean(Coarse/1000,na.rm=TRUE),rangecoarse=max(Coarse/1000)-min(Coarse/1000))
# variability plots
mean(s$varpm25,na.rm=TRUE)
mean(s$varcoarse,na.rm=TRUE)
mean(s$rangecoarse,na.rm=TRUE)

pdf("/Users/mf/Documents/MISR/Papers and Reports/MISR Subgrid/predPM_ICV_cool_new2.pdf")
p.cool<-ggplot(MISR_ICV_ssc3, aes(x = predPM25, y=PM25/1000)) + geom_point(size=3,colour="blue") +
           scale_colour_gradientn(colours=rev(rainbow(4)),name="Count ICV")
p.cool+labs(x = expression("MISR 4.4km Grid PM"[2.5]*", ug/m"^3), y = expression("ICV PM"[2.5]*", ug/m"^3))+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10),limits=c(0,25)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10),limits=c(0,25))+
  #geom_abline(intercept = 0, slope = 1)
  theme(axis.text=element_text(size=14), axis.title=element_text(size=14))+
  annotate("text", x=5, y=24, label = "ICC=0.90")+
  annotate("text", x=5, y=23, label = "CV=10%")
dev.off()

pdf("/Users/mf/Documents/MISR/Papers and Reports/MISR Subgrid/predPM10_25_ICV_cool_new2.pdf")
p.cool<-ggplot(MISR_ICV_ssc4, aes(x = predPM10_25, y=Coarse/1000)) + 
  geom_point(size=3, colour="blue") +
  scale_colour_gradientn(colours=rev(rainbow(4)),name="Count ICV")
p.cool+labs(x = expression('MISR 4.4km Grid PM'[2.5-10]*', ug/m'^3), y = expression('ICV PM'[2.5-10]*', ug/m'^3))+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10),limits=c(0,25)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10),limits=c(0,25)) +
  theme(axis.text=element_text(size=14), axis.title=element_text(size=14)) +
  annotate("text", x=5, y=24, label = "ICC = 0.91")+
  annotate("text", x=5, y=23, label = "CV = 11%")
dev.off()



pdf("/Users/mf/Documents/MISR/Papers and Reports/MISR Subgrid/predPM_ICV_warm_new2.pdf")
p.warm<-ggplot(MISR_ICV_ssw3, aes(x = predPM25, y=PM25/1000))+ geom_point(size=3,colour="red") +
  scale_colour_gradientn(colours=rev(rainbow(4)),name="Count ICV")
p.warm+labs(x = expression('MISR 4.4km Grid PM'[2.5]*', ug/m'^3), y = expression('ICV PM'[2.5]*', ug/m'^3))+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10),limits=c(0,25))+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10),limits=c(0,25))+
  theme(axis.text=element_text(size=14), axis.title=element_text(size=14))+
  annotate("text", x=5, y=24, label = "ICC=0.22")+
  annotate("text", x=5, y=23, label = "CV=9.9%")
dev.off()

pdf("/Users/mf/Documents/MISR/Papers and Reports/MISR Subgrid/predPM10_PM25_ICV_warm_new2.pdf")
p.warm<-ggplot(MISR_ICV_ssw4, aes(x = predPM10_25, y=Coarse/1000))+ geom_point(size=3,colour="red") +
  scale_colour_gradientn(colours=rev(rainbow(4)),name="Count ICV")
p.warm+labs(x = expression('MISR 4.4km Grid PM'[2.5-10]*', ug/m'^3), y = expression('ICV PM'[2.5-10]*', ug/m'^3))+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10),limits=c(0,45)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10),limits=c(0,35)) +
  theme(axis.text=element_text(size=14), axis.title=element_text(size=14)) +
  annotate("text", x=5, y=30, label = "ICC = 0.44")+
  annotate("text", x=5, y=28, label = "CV = 19%")
dev.off()


# counts and variance by MISR grid cell
pmvariance<-ddply(MISR_ICV_c, .(AODm), summarise, AOD=sd(AODm,na.rm=TRUE),AODsm_med=sd(AODsm_medm,na.rm=TRUE),PM25=sd(predPM25,na.rm=TRUE))
pmcounts<-ddply(MISR_ICV_c, .(lon.misr,lat.misr), summarise, AOD=length(AODm))

pm25freq_means_sd_cool<-ddply(MISR_ICV_c, .(x,y), summarise, town=town.x[1], avgPMc=mean(PM25/1000,na.rm=TRUE), varPMc=(sd(PM25/1000,na.rm=TRUE))^2,freqPMc=length(PM25))
pm25freq_means_sd_warm<-ddply(MISR_ICV_w, .(x,y), summarise, town=town.x[1], avgPMw=mean(PM25/1000,na.rm=TRUE), varPMw=(sd(PM25/1000,na.rm=TRUE))^2,freqPMw=length(PM25))

pm25ECfreq_means_sd_cool<-ddply(MISR_ICV_c, .(x,y), summarise, town=town.x[1], avgPMECc=mean(EC_PM25/1000,na.rm=TRUE), varPMECc=(sd(EC_PM25/1000,na.rm=TRUE))^2,freqPMECc=length(EC_PM25))
pm25ECfreq_means_sd_warm<-ddply(MISR_ICV_w, .(x,y), summarise, town=town.x[1], avgPMECw=mean(EC_PM25/1000,na.rm=TRUE), varPMECw=(sd(EC_PM25/1000,na.rm=TRUE))^2,freqPMECw=length(EC_PM25))


pmcoarsefreq_means_sd_cool<-ddply(MISR_ICV_c, .(x,y), summarise, town=town.x[1], avgCoarsec=mean(Coarse/1000,na.rm=TRUE), varCoarsec=(sd(Coarse/1000,na.rm=TRUE))^2,freqCoarsec=length(Coarse))
pmcoarsefreq_means_sd_warm<-ddply(MISR_ICV_w, .(x,y), summarise, town=town.x[1], avgCoarsew=mean(Coarse/1000,na.rm=TRUE), varCoarsew=(sd(Coarse/1000,na.rm=TRUE))^2,freqCoarsew=length(Coarse))

PMvar_all1<-merge(pm25freq_means_sd_cool, pm25freq_means_sd_warm, by = c('x', 'y'),all=TRUE)
PMvar_all2<-merge(pmcoarsefreq_means_sd_cool, pmcoarsefreq_means_sd_warm, by = c('x', 'y'),all=TRUE)
PMvar_all<-merge(PMvar_all1,PMvar_all2,by=c('x','y'),all=TRUE)

PMvar_all<-merge(PMvar_all, by=c('x','y'))

write.csv(PMvar_all,"/Users/mf/Documents/MISR/Papers and Reports/MISR Subgrid/subgridvarianceICV.csv",row.names = FALSE)

# Average ICV vs MISR PM2.5
pm25freq<-ddply(MISR_ICV, .(predPM25), summarise, season=season[1],town=town.x[1], avgPM=mean(PM25/1000,na.rm=TRUE),freqPM=length(PM25))
summary(lm((pm25freq$avgPM)~(pm25freq$predPM25)))
levels(pm25freq$season) <- rev(levels(pm25freq$season))

pdf("/Users/mf/Documents/MISR/Papers and Reports/MISR Subgrid/PredPM25_ICVPM25_regression.pdf")
p12<-qplot(log(predPM25),log(avgPM), data=pm25freq,color=season,xlab=expression(' MISR 4.4km Grid PM'[2.5]*', ug/m'^3),ylab=expression('Grid Average ICV PM'[2.5]*', ug/m'^3))
p12+stat_smooth(method='lm',formula=y~x,col='black')+
  theme(axis.text=element_text(size=14), axis.title=element_text(size=14))
dev.off()

MISR_ICV_PM10_25<-MISR_ICV[MISR_ICV$predPM10_25>0,]
pm10freq<-ddply(MISR_ICV_PM10_25, .(predPM10_25), summarise, season=season[1],town=town.x[1], avgPM=mean(Coarse/1000,na.rm=TRUE), varPM=(sd(Coarse/1000,na.rm=TRUE))^2)
summary(lm(pm10freq$avgPM~pm10freq$predPM10_25))
levels(pm10freq$season) <- rev(levels(pm10freq$season))


pdf("/Users/mf/Documents/MISR/Papers and Reports/MISR Subgrid/PredCoarse_ICVCoarse_regression.pdf")
p13<-qplot(predPM10_25,avgPM, data=pm10freq,color=season,xlab=expression('MISR 4.4km Grid PM'[10-2.5]*', ug/m'^3),ylab=expression('Grid Average ICV PM'[10-2.5]*', ug/m'^3))
p13+stat_smooth(method='lm',formula=y~x,col='black')+
  theme(axis.text=element_text(size=14), axis.title=element_text(size=14))
dev.off()

# plots by community
MISR_ICV_c<-MISR_ICV[MISR_ICV$season=="cool", ]
pm25freq<-ddply(MISR_ICV_c, .(town.x), summarise,avgMISRPM=mean(predPM10_25), avgPM=mean(Coarse/1000,na.rm=TRUE),freqPM=length(PM25))
summary(lm((pm25freq$avgPM)~(pm25freq$avgMISRPM)))

p14<-qplot(avgMISRPM,avgPM, data=pm25freq,xlab=expression('MISR 4.4km Grid PM'[2.5]*', ug/m'^3),ylab=expression('Grid Average ICV PM'[2.5]*', ug/m'^3))
p14+stat_smooth(method='lm',formula=y~x,col='black')+
  theme(axis.text=element_text(size=14), axis.title=element_text(size=14))



MISR_ICV_w<-MISR_ICV[MISR_ICV$season=="warm", ]
pm25freq<-ddply(MISR_ICV_w, .(town.x), summarise,avgMISRPM=mean(predPM25), avgPM=mean(PM25/1000,na.rm=TRUE),freqPM=length(PM25))
summary(lm((pm25freq$avgPM)~(pm25freq$avgMISRPM)))

p15<-qplot(avgMISRPM,avgPM, data=pm25freq,xlab=expression('MISR 4.4km Grid PM'[2.5]*', ug/m'^3),ylab=expression('Grid Average ICV PM'[2.5]*', ug/m'^3))
p15+stat_smooth(method='lm',formula=y~x,col='black')+
  theme(axis.text=element_text(size=14), axis.title=element_text(size=14))

levels(pm25freq$season) <- rev(levels(pm25freq$season))

# spatial analysis of residuals

library(geoR)

# Use all data MISR_ICV
# Remove Santa Barbara
MISR_ICV$MISRpredPM25<-as.factor(MISR_ICV$predPM25)
MISR_ICV$MISRpredCoarse<-as.factor(MISR_ICV$predPM10_25)
MISR_ICV_c<-MISR_ICV[MISR_ICV$season=="cool", ]
MISR_ICV_c<-MISR_ICV_c[MISR_ICV_c$town.x!="SB", ]

#MISR_ICV2_c<-MISR_ICV2[MISR_ICV2$season=="cool", ]

lme.pm.c<-lmer(PM25/1000 ~ (1|MISRpredPM25),data=MISR_ICV_c)
lme.pmC.c<-lmer(Coarse/1000 ~ (1|MISRpredCoarse),data=MISR_ICV_c)

MISR_ICV_c$icv.cool.resids<-residuals(lme.pm.c)
MISR_ICV_c$icv.cool.Cresids<-residuals(lme.pmC.c)
MISR_ICV_cool <- na.omit(subset(MISR_ICV_c,select=c(x.1,y.1,PM25,Coarse,icv.cool.resids,icv.cool.Cresids)))
MISR_ICV_cool$PM25ug<-MISR_ICV_cool$PM25/1000
MISR_ICV_cool$PMCug<-MISR_ICV_cool$Coarse/1000


# COOL PM25 empirical semivariograms
icv.pm25.geoc<-as.geodata(MISR_ICV_cool,coords.col=c(1,2),data.col=7)
plot(icv.pm25.geoc)

# larger scale
icv.pm25.variog.c<-variog(icv.pm25.geoc,max.dist=15, uvec=seq(0,15,l=15),estimator.type="modulus",bin.cloud = TRUE)
pdf("/Users/mf/Documents/MISR/Papers and Reports/MISR Subgrid/Vario_boxplot_cool.pdf")
plot(icv.pm25.variog.c,bin.cloud=TRUE, cex = 1.5, pch= 16, col = "black",xlab="Distance (Km)", ylab = ~ "Semivariance ( " *( mu^2 * g / m^6) * ")", font.main = 1, cex.lab = 2)
dev.off()
median(icv.pm25.variog.c$bin.cloud[[5]]) #1km= 0.95 2km=0.95 3km=1.0 4km=1.1 5km= 1.1 ug/m3

#multiple comparison test icv.pm25.variog.c$bin.cloud[[15]]
for(i in 1:length(icv.pm25.variog.c$bin.cloud)){ 
  icv.pm25.variog.c$bin.cloud[[i]] <- cbind(icv.pm25.variog.c$bin.cloud[[i]], 
                                            id=rep(i, length(icv.pm25.variog.c$bin.cloud[[i]])))}
icv.pm25.variog.c<-do.call("rbind", icv.pm25.variog.c$bin.cloud)
colnames(icv.pm25.variog.c)<-c("pm","id")
pairwise.t.test(as.vector(icv.pm25.variog.c[,1]), as.vector(icv.pm25.variog.c[,2]), pool.sd=TRUE,p.adj="bonferroni")

# subgrid scale PM2.5
icv.pm25.variog.c2<-variog(icv.pm25.geoc,max.dist=4.4,uvec=seq(0,4.4,l=18),estimator.type="modulus",bin.cloud = TRUE)
pdf("/Users/mf/Documents/MISR/Papers and Reports/MISR Subgrid/Vario_boxplot_subgrid_cool.pdf")
plot(icv.pm25.variog.c2,bin.cloud=TRUE,xlab="Distance, km", ylab="Semivariance")
dev.off()

#multiple comparison test icv.pm25.variog.c2$bin.cloud[[15]]
for(i in 1:length(icv.pm25.variog.c2$bin.cloud)){ 
  icv.pm25.variog.c2$bin.cloud[[i]] <- cbind(icv.pm25.variog.c2$bin.cloud[[i]], 
                                            id=rep(i, length(icv.pm25.variog.c2$bin.cloud[[i]])))}
icv.pm25.variog.c2<-do.call("rbind", icv.pm25.variog.c2$bin.cloud)
colnames(icv.pm25.variog.c2)<-c("pm","id")
pairwise.t.test(as.vector(icv.pm25.variog.c2[,1]), as.vector(icv.pm25.variog.c2[,2]), pool.sd=FALSE,p.adj="bonferroni")

# small distance median concentrations
median(icv.pm25.variog.c2$bin.cloud[[1]]) #1st = 0.69, 2nd=0.86, 3rd= 0.94 ug/m3


# fitting semivariograms by wls and ML
#variog.fit.pm25<-variofit(icv.pm25.variog,max.dist=icv.pm25.variog$max.dist,ini.cov.pars=c(30,10),nugget=0.1, fix.nugget=FALSE, cov.model="gaussian")
#plot(icv.pm25.variog, xlab="Distance, km")
#lines(variog.fit.pm25,col="red")

#lik.fit.pm25<-likfit(icv.pm25.geoc, ini.cov.pars=c(40,10),nugget=0.5, fix.nugget=FALSE, cov.model="gaussian")
#pdf("/Users/mf/Documents/MISR/Papers and Reports/MISR Subgrid/Variolikfit_PM25_Cool.pdf")
#plot(icv.pm25.variog, pch=19,cex=0.8,xlab="Distance, km",xlim=c(0,10),ylim=c(0,20))
#lines(lik.fit.pm25,col="red")
#dev.off()

## Cool coarse PM empirical semivariograms
icv.pmc.geoc<-as.geodata(MISR_ICV_cool,coords.col=c(1,2),data.col=8)
plot(icv.pmc.geoc)
icv.pmc.variog.c<-variog(icv.pmc.geoc,uvec=seq(0,15,l=15),estimator.type="modulus",bin.cloud = TRUE)
pdf("/Users/mf/Documents/MISR/Papers and Reports/MISR Subgrid/Vario_boxplotCoarse_cool.pdf")
plot(icv.pmc.variog.c,bin.cloud=TRUE,xlaxb="Distance, km", ylab="Semivariance")
dev.off()
median(icv.pmc.variog.c$bin.cloud[[4]]) #1km = 0.86, 2km=0.92, 3km = 0.99, 4km = 0.96 ug/m3

#multiple comparison test icv.pmc.variog.c$bin.cloud[[15]]
for(i in 1:length(icv.pmc.variog.c$bin.cloud)){ 
  icv.pmc.variog.c$bin.cloud[[i]] <- cbind(icv.pmc.variog.c$bin.cloud[[i]], 
                                            id=rep(i, length(icv.pmc.variog.c$bin.cloud[[i]])))}
icv.pmc.variog.c<-do.call("rbind", icv.pmc.variog.c$bin.cloud)
colnames(icv.pmc.variog.c)<-c("pm","id")
pairwise.t.test(as.vector(icv.pmc.variog.c[,1]), as.vector(icv.pmc.variog.c[,2]), pool.sd=TRUE,p.adj="bonferroni")


# small scale coarse cool
icv.pmc.variog.c2<-variog(icv.pmc.geoc,uvec=seq(0,4.4,l=18),estimator.type="modulus",bin.cloud = TRUE)
pdf("/Users/mf/Documents/MISR/Papers and Reports/MISR Subgrid/Vario_boxplotCoarse_subgrid_cool.pdf")
plot(icv.pmc.variog.c2,bin.cloud=TRUE,xlaxb="Distance, km", ylab="Semivariance")
dev.off()
median(icv.pmc.variog.c2$bin.cloud[[3]]) #1st = 0.73, 2nd=0.76, 3rd = 1.03 ug/m3

#multiple comparison test icv.pmc.variog.c$bin.cloud[[15]]
for(i in 1:length(icv.pmc.variog.c2$bin.cloud)){ 
  icv.pmc.variog.c2$bin.cloud[[i]] <- cbind(icv.pmc.variog.c2$bin.cloud[[i]], 
                                           id=rep(i, length(icv.pmc.variog.c2$bin.cloud[[i]])))}
icv.pmc.variog.c2<-do.call("rbind", icv.pmc.variog.c2$bin.cloud)
colnames(icv.pmc.variog.c2)<-c("pm","id")
pairwise.t.test(as.vector(icv.pmc.variog.c2[,1]), as.vector(icv.pmc.variog.c2[,2]), pool.sd=TRUE,p.adj="bonferroni")


# just variogram (no boxplot)
plot(icv.pmc.variog.c,pch=19,xlab="Distance, km")
variog.fit.pmC<-variofit(icv.pmc.variog.c,max.dist=icv.pmc.variog.c$max.dist,ini.cov.pars=c(30,10),nugget=0.1, fix.nugget=FALSE, cov.model="gaussian")
plot(icv.pmc.variog.c, xlab="Distance, km")
lines(variog.fit.pmC,col="red")


lik.fit.pmc<-likfit(icv.pmc.geoc, ini.cov.pars=c(40,10),nugget=0.5, fix.nugget=FALSE, cov.model="matern")
pdf("/Users/mf/Documents/MISR/Papers and Reports/MISR Subgrid/Variolikfit_PMc_Cool.pdf")
plot(icv.pmc.variog.c, pch=19,cex=0.8,xlab="Distance, km",xlim=c(0,20),ylim=c(0,30))
lines(lik.fit.pm25,col="red")
dev.off()



# WARM

MISR_ICV_w<-MISR_ICV[MISR_ICV$season=="warm", ]
MISR_ICV_w<-MISR_ICV_w[MISR_ICV_w$town.x!="SB", ]

lme.pm.w<-lmer(PM25/1000 ~ (1|MISRpredPM25),data=MISR_ICV_w)
lme.pmC.w<-lmer(Coarse/1000 ~ (1|MISRpredCoarse),data=MISR_ICV_w)

MISR_ICV_w$icv.warm.resids<-residuals(lme.pm.w)
MISR_ICV_w$icv.warm.Cresids<-residuals(lme.pmC.w)
MISR_ICV_warm <- na.omit(subset(MISR_ICV_w,select=c(x.1,y.1,PM25,Coarse,icv.warm.resids,icv.warm.Cresids)))
MISR_ICV_warm$PM25ug<-MISR_ICV_warm$PM25/1000
MISR_ICV_warm$PMCug<-MISR_ICV_warm$Coarse/1000

icv.pm25.geow<-as.geodata(MISR_ICV_warm,coords.col=c(1,2),data.col=7)
plot(icv.pm25.geow)

icv.pm25.variog.w<-variog(icv.pm25.geow,uvec=seq(0,15,l=15),estimator.type="modulus",bin.cloud = TRUE)
pdf("/Users/mf/Documents/MISR/Papers and Reports/MISR Subgrid/Vario_boxplot_warm.pdf")
plot(icv.pm25.variog.w,bin.cloud=TRUE,xlab="Distance, km",ylab="Semivariance")
dev.off()
median(icv.pm25.variog.w$bin.cloud[[3]]) #1st = 1.00, 2nd=1.0, 3rd = 0.98, 4th =1.1 ug/m3

#multiple comparison test icv.pm25.variog.w$bin.cloud[[15]]
for(i in 1:length(icv.pm25.variog.w$bin.cloud)){ 
  icv.pm25.variog.w$bin.cloud[[i]] <- cbind(icv.pm25.variog.w$bin.cloud[[i]], 
                                            id=rep(i, length(icv.pm25.variog.w$bin.cloud[[i]])))}
icv.pm25.variog.w<-do.call("rbind", icv.pm25.variog.w$bin.cloud)
colnames(icv.pm25.variog.w)<-c("pm","id")
pairwise.t.test(as.vector(icv.pm25.variog.w[,1]), as.vector(icv.pm25.variog.w[,2]), pool.sd=TRUE,p.adj="bonferroni")


# subgrid
icv.pm25.variog.w2<-variog(icv.pm25.geow,uvec=seq(0,4.4,l=18),estimator.type="modulus",bin.cloud = TRUE)
plot(icv.pm25.variog.w2,bin.cloud=TRUE,xlab="Distance, km",ylab="Semivariance")
median(icv.pm25.variog.w2$bin.cloud[[3]]) #1st = 0.825, 2nd=0.952, 3rd = 1.09 ug/m3

lik.fit.pm25.w<-likfit(icv.pm25.geow, ini.cov.pars=c(1,1),nugget=0.5, fix.nugget=FALSE, cov.model="spherical")
pdf("/Users/mf/Documents/MISR/Papers and Reports/MISR Subgrid/Variolikfit_PM25_Cool.pdf")
plot(icv.pm25.variog.w, pch=19,cex=0.8,xlab="Distance, km",xlim=c(0,25),ylim=c(0,5))
lines(lik.fit.pm25.w,col="red")
dev.off()
variog.fit.pm.w<-variofit(icv.pm25.variog.w,ini.cov.pars=c(1,4),nugget=0.5, fix.nugget=FALSE, cov.model="matern")
plot(icv.pm25.variog.w, pch=19,cex=0.8,xlab="Distance, km",xlim=c(0,25),ylim=c(0,5))
lines(variog.fit.pm.w,col="red")


icv.pmc.geow<-as.geodata(MISR_ICV_warm,coords.col=c(1,2),data.col=8)
plot(icv.pmc.geow)

icv.pmc.variog.w<-variog(icv.pmc.geow,uvec=seq(0,15,l=15),estimator.type="modulus",bin.cloud = TRUE)
pdf("/Users/mf/Documents/MISR/Papers and Reports/MISR Subgrid/Vario_boxplotCoarse_warm.pdf")
plot(icv.pmc.variog.w,bin.cloud=TRUE,xlab="Distance, km",ylab="Semivariance")
dev.off()

median(icv.pmc.variog.c$bin.cloud[[1]]) #1st = 1.09, 2nd=0.76, 5th = 1.25 ug/m3

#multiple comparison test icv.pmc.variog.c$bin.cloud[[15]]
for(i in 1:length(icv.pmc.variog.w$bin.cloud)){ 
  icv.pmc.variog.w$bin.cloud[[i]] <- cbind(icv.pmc.variog.w$bin.cloud[[i]], 
                                           id=rep(i, length(icv.pmc.variog.w$bin.cloud[[i]])))}
icv.pmc.variog.w<-do.call("rbind", icv.pmc.variog.w$bin.cloud)
colnames(icv.pmc.variog.w)<-c("pm","id")
pairwise.t.test(as.vector(icv.pmc.variog.w[,1]), as.vector(icv.pmc.variog.w[,2]), pool.sd=TRUE,p.adj="bonferroni")


# subgrid
icv.pmc.variog.w2<-variog(icv.pmc.geow,uvec=seq(0,4.4,l=18),estimator.type="modulus",bin.cloud = TRUE)
plot(icv.pmc.variog.w2,bin.cloud=TRUE,xlab="Distance, km",ylab="Semivariance")
median(icv.pmc.variog.w2$bin.cloud[[3]]) #1st = 0.825, 2nd=0.952, 3rd = 1.09 ug/m3


for(i in 1:length(icv.pmc.variog.w2$bin.cloud)){ 
  icv.pmc.variog.w2$bin.cloud[[i]] <- cbind(icv.pmc.variog.w2$bin.cloud[[i]], 
                                           id=rep(i, length(icv.pmc.variog.w2$bin.cloud[[i]])))}
icv.pmc.variog.w2<-do.call("rbind", icv.pmc.variog.w2$bin.cloud)
colnames(icv.pmc.variog.w2)<-c("pm","id")
pairwise.t.test(as.vector(icv.pmc.variog.w2[,1]), as.vector(icv.pmc.variog.w2[,2]), pool.sd=TRUE,p.adj="bonferroni")





plot(icv.pmc.variog.w,pch=19)
variog.fit.pmc.w<-variofit(icv.pmc.variog.w,ini.cov.pars=c(15,4),nugget=0.5, fix.nugget=FALSE, cov.model="gaussian")
plot(icv.pmc.variog.w, xlab="Distance, km")
lines(variog.fit.pmc.w,col="red")

icv.pmc.variog.w2<-variog(icv.pmc.geow,uvec=seq(0,4.4,l=18),estimator.type="modulus",bin.cloud = TRUE)
pdf("/Users/mf/Documents/MISR/Papers and Reports/MISR Subgrid/Vario_boxplotCoarse_warm.pdf")
plot(icv.pmc.variog.w2,bin.cloud=TRUE,xlab="Distance, km",ylab="Semivariance")
dev.off()

lik.fit.pmc.w<-likfit(icv.pmc.geow, ini.cov.pars=c(5,10),nugget=0.5, fix.nugget=FALSE, cov.model="matern")
pdf("/Users/mf/Documents/MISR/Papers and Reports/MISR Subgrid/Variolikfit_PMc_Warm.pdf")
plot(icv.pmc.variog.w, xlab="Distance, km", xlim=c(0,15), pch=19)
lines(lik.fit.pmc.w,col="red")
dev.off()


variog.fit.pm25.w<-variofit(icv.pm25.variog.w,ini.cov.pars=c(3,4),nugget=1, fix.nugget=FALSE, cov.model="gaussian")
plot(icv.pm25.variog.w, xlab="Distance, km", main="Semivariogram ICV PM2.5 Warm")
lines(variog.fit.pm25,col="red")


lik.fit.pm25.w<-likfit(icv.pm25.geow, ini.cov.pars=c(5,10),nugget=1, fix.nugget=FALSE, cov.model="matern")
pdf("/Users/mf/Documents/MISR/Papers and Reports/MISR Subgrid/Variolikfit_PM25_Warm.pdf")
plot(icv.pm25.variog.w, xlab="Distance, km", xlim=c(0,15), pch=19)
lines(lik.fit.pm25.w,col="red")
dev.off()


icv_new_cool<-icv_new[icv_new$season=="cool",]
icv_new_cool<-na.omit(subset(icv_new_cool,select=c(x,y,PM25)))
icv_new_cool$PM25ug<-icv_new_cool$PM25/1000

icv.pm25c.geo<-as.geodata(icv_new_cool,coords.col=c(1,2),data.col=4)
plot(icv.pm25c.geo)
icv.pm25c.variog<-variog(icv.pm25c.geo,uvec=seq(0,15,l=15),estimator.type="modulus")

variog.fit.pm25c<-variofit(icv.pm25c.variog, max.dist=20,ini.cov.pars=c(25,5),nugget=1, fix.nugget=FALSE, cov.model="gaussian")
plot(icv.pm25c.variog, xlab="Distance, km", main="Semivariogram ICV PM2.5 Warm")
lines(variog.fit.pm25c,col="red")

likfit.pm25c<-likfit(icv.pm25c.geo, ini.cov.pars=c(10,5),nugget=1, fix.nugget=FALSE, cov.model="gaussian")
plot(icv.pm25c.variog, pch=19,cex=0.5,max.dist=10, xlim=c(0,15),xlab="Distance, km")
lines(likfit.pm25c,col="red")


icv_new_warm<-icv_new[icv_new$season=="warm",]
icv_new_warm<-na.omit(subset(icv_new_warm,select=c(x,y,PM25)))
icv_new_warm$PM25ug<-icv_new_warm$PM25/1000
icv_new_warm<-icv_new_warm %>% distinct(x , y, .keep_all=TRUE)

icv.pm25w.geo<-as.geodata(icv_new_warm,coords.col=c(1,2),data.col=4)
plot(icv.pm25w.geo)
icv.pm25w.variog<-variog(icv.pm25w.geo,uvec=seq(0,5,l=7),estimator.type="modulus")
plot(icv.pm25w.variog)


likfit.pm25w<-likfit(icv.pm25w.geo, ini.cov.pars=c(10,15),nugget=1, fix.nugget=FALSE, cov.model="gaussian")
pdf("/Users/mf/Documents/MISR/Papers and Reports/MISR Subgrid/Semivar_PM25_Warm.pdf")
plot(icv.pm25w.variog, pch=19,cex=0.5, max.dist=30,xlab="Distance, km", main="Semivariogram ICV PM2.5 Warm")
lines(likfit.pm25w,col="red")
dev.off()

pdf("/Users/mf/Documents/MISR/Presentations/ICV_PM25_semivar.pdf")
par(mfrow=c(1,2))
plot(icv.pm25.variog, xlab="Distance, km", main="ICV PM2.5 Warm")
lines(variog.fit.pm25,col="red")
plot(icv.pm25c.variog, xlab="Distance, km", main="ICV PM2.5 Cool")
lines(variog.fit.pm25c,col="red")
dev.off()


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


###### AQS data in region 2008-2009 #####
aqspm25<-read.csv("/Users/mf/Documents/AQS/PM25/pm25_2000_2011.csv")
aqspm10<-read.csv("/Users/mf/Documents/AQS/PM10/pm10_2000_2011.csv")
aqspm25_08_09<-aqspm25[aqspm25$year %in% c(2008,2009), ]

# match ICV with closest AQS and in date range

for (i in 1:length(ICV_startdays$startdate)){
  # take MISR averages between each ICV start and end date (sampling.interval)
  date_match<-aqspm25_08_09[ymd(misr_08_09$date2) %within% ICV_startdays$samp_int[i],]
  misr_monthly <- ddply(date_match, .(x,y), summarise, lat.misr=mean(lat),lon.misr=mean(lon),
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
