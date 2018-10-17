# Predictions from best models
# See Franklin et al 2017 RSE

library(mgcv)

misr.aqspm25.met.06.11<-read.csv("/Users/mf/Documents/MISR/Data/Match 2000-2011/misr_pm25_met_2006_2011.csv")
# Final model R2=0.67
gam.st.pm25<-gam(PM25~s(AODsm_med)+te(x.1,y.1,julian2,k=c(25,8),d=c(2,1),bs=c('tp','cr'))+te(julian2, year,month, k=c(12,4,7),d=c(1,1,1),bs=c("cr","cr","cc"))+as.factor(dow), data=misr.aqspm25.met.06.11)

# all misr data 
misr<-read.csv("/Users/mf/Documents/MISR/Data/misr_2000_2011.csv")
misr<-misr[misr$land.water.mask==3,]
misr.grid<-unique(misr[,1:2])
misr_06_11<-misr[misr$year>=2006,]
misr.date<-unique(misr_06_11[,13])

misr.days<-misr_06_11 %>% distinct(date2,.keep_all=TRUE) 
pm25.predicted<-vector('list',length(misr.days$date))
for (i in 1:length(misr.days$date)){
  misr.daily<-misr_06_11[misr_06_11$day %in% misr.days[i,]$day &
                           misr_06_11$month %in% misr.days[i,]$month &
                           misr_06_11$year %in% misr.days[i,]$year ,] 
  misr.daily$julian2<-misr.daily$julian/10000
  misr.daily$x.1<-misr.daily$x
  misr.daily$y.1<-misr.daily$y
  misr.daily$dow<-(weekdays(as.Date(misr.daily$date,"%m/%d/%y")))
  misr.daily$AODsm_med<-misr.daily$AODsmall+misr.daily$AODmed
  predicted.pm25<-predict.gam(gam.st.pm25, newdata=misr.daily)
  misr.daily$predicted.pm25<-predicted.pm25
  pm25.predicted[[i]]<-misr.daily
}
pm25.predicted.all <- do.call("rbind", pm25.predicted)
pm25.predicted.all<-pm25.predicted.all[pm25.predicted.all$AOD<1,]
pm25.predicted.all<-pm25.predicted.all[pm25.predicted.all$predicted.pm25>0,]

write.csv(pm25.predicted.all, "/Users/mf/Documents/MISR/Data/predicted_pm25_2006_2011.csv")


