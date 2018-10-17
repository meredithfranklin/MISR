##############################################
# 2000-2011 4.4km MISR-PM data analysis
# Speciation Trends PM2.5
# Meredith Franklin
##############################################

library(mgcv)
library(ggplot2)
library(colorRamps)
library(ggmaps)
library(gridExtra)
library(dplyr)
setwd("/Users/mf/Documents/MISR/Reports")

# Data
misr.stn<-read.csv("/Users/mf/Documents/MISR/Data/Match 2000-2011/misr_stn_2002_2011.csv")
misr.stn.met<-read.csv("/Users/mf/Documents/MISR/Data/Match 2000-2011/misr_stn_met_2002_2011.csv")
misr.stn.met$rh=100*((112-0.1*misr.stn.met$temp+misr.stn.met$dew.point)/(112+0.9*misr.stn.met$temp))^8

# create new julian date
misr.stn$julian2<-misr.stn$julian/10000
misr.stn.met$julian2<-misr.stn.met$julian/10000

# add day of week for models
misr.stn$dow<-(weekdays(as.Date(misr.stn$date,"%m/%d/%y")))
misr.stn.met$dow<-(weekdays(as.Date(misr.stn.met$date,"%m/%d/%y")))

# create aod small+med
misr.stn$AODsm_med<-misr.stn$AODsmall+misr.stn$AODmed
misr.stn.met$AODsm_med<-misr.stn.met$AODsmall+misr.stn.met$AODmed

# remove AOD greater than 1
misr.stn.ss<-misr.stn[misr.stn$AOD<1,]
misr.stn.met.ss<-misr.stn.met[misr.stn.met$AOD<1,]

# Summary Statistics STN
misr.stn.met.06.11<-misr.stn.met.ss[misr.stn.met.ss$year>=2006,]
misr.stn.points<-unique(misr.stn.met.06.11[,22:23])

# PM2.5 from STN sites
misr.aqspm25.met.POC5<-misr.stn.met.ss[misr.stn.met.ss$POC==5,]
misr.aqspm25.met.06.11.POC5<-misr.aqspm25.met.POC5[misr.aqspm25.met.POC5$year>=2006,]
misr.stnpm25.points<-unique(misr.aqspm25.met.06.11.POC5[,38:39])

# Title (writes new file)
cat("STN Summary Stats", file = "SummaryStatsSTN_2006_2011.txt")
# add new lines
cat("\n", file = "SummaryStatsSTN_2006_2011.txt", append = TRUE)

# export summary statistics output
cat("Dim PM2.5", file = "SummaryStatsSTN_2006_2011.txt", append = TRUE)
capture.output(length(misr.stn.met.06.11$SO4), file = "SummaryStatsSTN_2006_2011.txt", append = TRUE)
cat("Median AOD\n", file = "SummaryStatsSTN_2006_2011.txt", append = TRUE)
capture.output(median(misr.stn.met.06.11$AOD), file = "SummaryStatsSTN_2006_2011.txt", append = TRUE)
cat("IQR AOD\n", file = "SummaryStatsSTN_2006_2011.txt", append = TRUE)
capture.output(IQR(misr.stn.met.06.11$AOD), file = "SummaryStatsSTN_2006_2011.txt", append = TRUE)
cat("Median SO4\n", file = "SummaryStatsSTN_2006_2011.txt", append = TRUE)
capture.output(median(misr.stn.met.06.11$SO4), file = "SummaryStatsSTN_2006_2011.txt", append = TRUE)
cat("IQR SO4\n", file = "SummaryStatsSTN_2006_2011.txt", append = TRUE)
capture.output(IQR(misr.stn.met.06.11$SO4), file = "SummaryStatsSTN_2006_2011.txt", append = TRUE)
cat("Cor AOD SO4\n", file = "SummaryStatsSTN_2006_2011.txt", append = TRUE)
capture.output(cor(misr.stn.met.06.11$AOD,misr.stn.met.06.11$PM10), file = "SummaryStatsSTN_2006_2011.txt", append = TRUE)
cat("Cor AODlarge SO4\n", file = "SummaryStatsSTN_2006_2011.txt", append = TRUE)
capture.output(cor(misr.stn.met.06.11$AODlarge,misr.stn.met.06.11$PM10), file = "SummaryStatsSTN_2006_2011.txt", append = TRUE)
cat("Cor AODsmall SO4\n", file = "SummaryStatsSTN_2006_2011.txt", append = TRUE)
capture.output(cor(misr.stn.met.06.11$AODsmall,misr.stn.met.06.11$PM10), file = "SummaryStatsSTN_2006_2011.txt", append = TRUE)
cat("Cor AOD STN PM25 met\n", file = "SummaryStatsSTN_2006_2011.txt", append = TRUE)
capture.output(cor(misr.aqspm25.met.06.11.POC5$AOD,misr.aqspm25.met.06.11.POC5$PM25), file = "SummaryStatsSTN_2006_2011.txt", append = TRUE)
cat("Cor AODlarge STN PM25 met\n", file = "SummaryStatsSTN_2006_2011.txt", append = TRUE)
capture.output(cor(misr.aqspm25.met.06.11.POC5$AODlarge,misr.aqspm25.met.06.11.POC5$PM25), file = "SummaryStatsSTN_2006_2011.txt", append = TRUE)
cat("Cor AODsmall STN PM25 met\n", file = "SummaryStatsSTN_2006_2011.txt", append = TRUE)
capture.output(cor(misr.aqspm25.met.06.11.POC5$AODsmall,misr.aqspm25.met.06.11.POC5$PM25), file = "SummaryStatsSTN_2006_2011.txt", append = TRUE)



# Univariate Linear models with species
cat("linear mod AOD STNEC\n", file = "SummaryStatsSTN_2006_2011.txt", append = TRUE)
capture.output(summary(lm(EC~AOD, data=misr.stn.met.06.11)), file = "SummaryStatsSTN_2006_2011.txt", append = TRUE)
cat("linear mod AOD STNOC\n", file = "SummaryStatsSTN_2006_2011.txt", append = TRUE)
capture.output(summary(lm(OC~AOD,data=misr.stn.met.06.11)), file = "SummaryStatsSTN_2006_2011.txt", append = TRUE)
cat("linear mod AOD STNSO4\n", file = "SummaryStatsSTN_2006_2011.txt", append = TRUE)
capture.output(summary(lm(SO4~AOD,data=misr.stn.met.06.11)), file = "SummaryStatsSTN_2006_2011.txt", append = TRUE)
cat("linear mod AOD STNNO3\n", file = "SummaryStatsSTN_2006_2011.txt", append = TRUE)
capture.output(summary(lm(NO3~AOD, data=misr.stn.met.06.11)), file = "SummaryStatsSTN_2006_2011.txt", append = TRUE)
cat("linear mod AOD STNPM25\n", file = "SummaryStatsSTN_2006_2011.txt", append = TRUE)
capture.output(summary(lm(PM25~AOD, data=misr.aqspm25.met.06.11.POC5)), file = "SummaryStatsSTN_2006_2011.txt", append = TRUE)

cat("linear mod AOD small STNEC\n", file = "SummaryStatsSTN_2006_2011.txt", append = TRUE)
capture.output(summary(lm(EC~AODsmall, data=misr.stn.met.06.11)), file = "SummaryStatsSTN_2006_2011.txt", append = TRUE)
cat("linear mod AOD small STNOC\n", file = "SummaryStatsSTN_2006_2011.txt", append = TRUE)
capture.output(summary(lm(OC~AODsmall,data=misr.stn.met.06.11)), file = "SummaryStatsSTN_2006_2011.txt", append = TRUE)
cat("linear mod AOD small STNSO4\n", file = "SummaryStatsSTN_2006_2011.txt", append = TRUE)
capture.output(summary(lm(SO4~AODsmall,data=misr.stn.met.06.11)), file = "SummaryStatsSTN_2006_2011.txt", append = TRUE)
cat("linear mod AOD small STNNO3\n", file = "SummaryStatsSTN_2006_2011.txt", append = TRUE)
capture.output(summary(lm(NO3~AODsmall, data=misr.stn.met.06.11)), file = "SummaryStatsSTN_2006_2011.txt", append = TRUE)
cat("linear mod AOD small STNPM25\n", file = "SummaryStatsSTN_2006_2011.txt", append = TRUE)
capture.output(summary(lm(PM25~AODsmall, data=misr.aqspm25.met.06.11.POC5)), file = "SummaryStatsSTN_2006_2011.txt", append = TRUE)


cat("linear mod AOD large STNEC\n", file = "SummaryStatsSTN_2006_2011.txt", append = TRUE)
capture.output(summary(lm(EC~AODlarge, data=misr.stn.met.06.11)), file = "SummaryStatsSTN_2006_2011.txt", append = TRUE)
cat("linear mod AOD large STNOC\n", file = "SummaryStatsSTN_2006_2011.txt", append = TRUE)
capture.output(summary(lm(OC~AODlarge,data=misr.stn.met.06.11)), file = "SummaryStatsSTN_2006_2011.txt", append = TRUE)
cat("linear mod AOD large STNSO4\n", file = "SummaryStatsSTN_2006_2011.txt", append = TRUE)
capture.output(summary(lm(SO4~AODlarge,data=misr.stn.met.06.11)), file = "SummaryStatsSTN_2006_2011.txt", append = TRUE)
cat("linear mod AOD large STNNO3\n", file = "SummaryStatsSTN_2006_2011.txt", append = TRUE)
capture.output(summary(lm(NO3~AODlarge, data=misr.stn.met.06.11)), file = "SummaryStatsSTN_2006_2011.txt", append = TRUE)
cat("linear mod AOD large STNPM25\n", file = "SummaryStatsSTN_2006_2011.txt", append = TRUE)
capture.output(summary(lm(PM25~AODlarge, data=misr.aqspm25.met.06.11.POC5)), file = "SummaryStatsSTN_2006_2011.txt", append = TRUE)


cat("linear mod AOD sm_med STNEC\n", file = "SummaryStatsSTN_2006_2011.txt", append = TRUE)
capture.output(summary(lm(EC~AODsm_med, data=misr.stn.met.06.11)), file = "SummaryStatsSTN_2006_2011.txt", append = TRUE)
cat("linear mod AOD sm_med STNOC\n", file = "SummaryStatsSTN_2006_2011.txt", append = TRUE)
capture.output(summary(lm(OC~AODsm_med,data=misr.stn.met.06.11)), file = "SummaryStatsSTN_2006_2011.txt", append = TRUE)
cat("linear mod AOD sm_med STNSO4\n", file = "SummaryStatsSTN_2006_2011.txt", append = TRUE)
capture.output(summary(lm(SO4~AODsm_med,data=misr.stn.met.06.11)), file = "SummaryStatsSTN_2006_2011.txt", append = TRUE)
cat("linear mod AOD sm_med STNNO3\n", file = "SummaryStatsSTN_2006_2011.txt", append = TRUE)
capture.output(summary(lm(NO3~AODsm_med, data=misr.stn.met.06.11)), file = "SummaryStatsSTN_2006_2011.txt", append = TRUE)
cat("linear mod AOD sm_med STNPM25\n", file = "SummaryStatsSTN_2006_2011.txt", append = TRUE)
capture.output(summary(lm(PM25~AODsm_med, data=misr.aqspm25.met.06.11.POC5)), file = "SummaryStatsSTN_2006_2011.txt", append = TRUE)


# Univariate GAM models
cat("GAM mod AOD STNPM25\n", file = "SummaryStatsSTN_2006_2011.txt", append = TRUE)
capture.output(summary(gam(PM25~s(AOD),data=misr.aqspm25.met.06.11.POC5)), file = "SummaryStatsSTN_2006_2011.txt", append = TRUE)
cat("GAM mod AOD STNEC\n", file = "SummaryStatsSTN_2006_2011.txt", append = TRUE)
capture.output(summary(gam(EC~s(AOD),data=misr.stn.met.06.11)), file = "SummaryStatsSTN_2006_2011.txt", append = TRUE)
cat("GAM mod AOD STNOC\n", file = "SummaryStatsSTN_2006_2011.txt", append = TRUE)
capture.output(summary(gam(OC~s(AOD),data=misr.stn.met.06.11)), file = "SummaryStatsSTN_2006_2011.txt", append = TRUE)
cat("GAM mod AOD STN5O4\n", file = "SummaryStatsSTN_2006_2011.txt", append = TRUE)
capture.output(summary(gam(SO4~s(AOD),data=misr.stn.met.06.11)), file = "SummaryStatsSTN_2006_2011.txt", append = TRUE)
cat("GAM mod AOD STNO3\n", file = "SummaryStatsSTN_2006_2011.txt", append = TRUE)
capture.output(summary(gam(NO3~s(AOD),data=misr.stn.met.06.11)), file = "SummaryStatsSTN_2006_2011.txt", append = TRUE)

cat("GAM mod AODlarge STNPM25\n", file = "SummaryStatsSTN_2006_2011.txt", append = TRUE)
capture.output(summary(gam(PM25~s(AODlarge),data=misr.aqspm25.met.06.11.POC5)), file = "SummaryStatsSTN_2006_2011.txt", append = TRUE)
cat("GAM mod AODlarge STNEC\n", file = "SummaryStatsSTN_2006_2011.txt", append = TRUE)
capture.output(summary(gam(EC~s(AODlarge),data=misr.stn.met.06.11)), file = "SummaryStatsSTN_2006_2011.txt", append = TRUE)
cat("GAM mod AODlarge STNOC\n", file = "SummaryStatsSTN_2006_2011.txt", append = TRUE)
capture.output(summary(gam(OC~s(AODlarge),data=misr.stn.met.06.11)), file = "SummaryStatsSTN_2006_2011.txt", append = TRUE)
cat("GAM mod AODlarge STN5O4\n", file = "SummaryStatsSTN_2006_2011.txt", append = TRUE)
capture.output(summary(gam(SO4~s(AODlarge),data=misr.stn.met.06.11)), file = "SummaryStatsSTN_2006_2011.txt", append = TRUE)
cat("GAM mod AODlarge STNNO3\n", file = "SummaryStatsSTN_2006_2011.txt", append = TRUE)
capture.output(summary(gam(NO3~s(AODlarge),data=misr.stn.met.06.11)), file = "SummaryStatsSTN_2006_2011.txt", append = TRUE)

cat("GAM mod AODsmall STNPM25\n", file = "SummaryStatsSTN_2006_2011.txt", append = TRUE)
capture.output(summary(gam(PM25~s(AODsmall),data=misr.aqspm25.met.06.11.POC5)), file = "SummaryStatsSTN_2006_2011.txt", append = TRUE)
cat("GAM mod AODsmall STNEC\n", file = "SummaryStatsSTN_2006_2011.txt", append = TRUE)
capture.output(summary(gam(EC~s(AODsmall),data=misr.stn.met.06.11)), file = "SummaryStatsSTN_2006_2011.txt", append = TRUE)
cat("GAM mod AODsmall STNOC\n", file = "SummaryStatsSTN_2006_2011.txt", append = TRUE)
capture.output(summary(gam(OC~s(AODsmall),data=misr.stn.met.06.11)), file = "SummaryStatsSTN_2006_2011.txt", append = TRUE)
cat("GAM mod AODsmall STN5O4\n", file = "SummaryStatsSTN_2006_2011.txt", append = TRUE)
capture.output(summary(gam(SO4~s(AODsmall),data=misr.stn.met.06.11)), file = "SummaryStatsSTN_2006_2011.txt", append = TRUE)
cat("GAM mod AODsmall STNNO3\n", file = "SummaryStatsSTN_2006_2011.txt", append = TRUE)
capture.output(summary(gam(NO3~s(AODsmall),data=misr.stn.met.06.11)), file = "SummaryStatsSTN_2006_2011.txt", append = TRUE)

cat("GAM mod AODsmall+med STNPM25\n", file = "SummaryStatsSTN_2006_2011.txt", append = TRUE)
capture.output(summary(gam(PM25~s(AODsm_med),data=misr.aqspm25.met.06.11.POC5)), file = "SummaryStatsSTN_2006_2011.txt", append = TRUE)
cat("GAM mod AODsmall+med STNEC\n", file = "SummaryStatsSTN_2006_2011.txt", append = TRUE)
capture.output(summary(gam(EC~s(AODsm_med),data=misr.stn.met.06.11)), file = "SummaryStatsSTN_2006_2011.txt", append = TRUE)
cat("GAM mod AODsmall+med STNOC\n", file = "SummaryStatsSTN_2006_2011.txt", append = TRUE)
capture.output(summary(gam(OC~s(AODsm_med),data=misr.stn.met.06.11)), file = "SummaryStatsSTN_2006_2011.txt", append = TRUE)
cat("GAM mod AODsmall+med STN5O4\n", file = "SummaryStatsSTN_2006_2011.txt", append = TRUE)
capture.output(summary(gam(SO4~s(AODsm_med),data=misr.stn.met.06.11)), file = "SummaryStatsSTN_2006_2011.txt", append = TRUE)
cat("GAM mod AODsmall+med STNNO3\n", file = "SummaryStatsSTN_2006_2011.txt", append = TRUE)
capture.output(summary(gam(NO3~s(AODsm_med),data=misr.stn.met.06.11)), file = "SummaryStatsSTN_2006_2011.txt", append = TRUE)

# S-T GAM models
cat("GAM mod AOD STNPM25\n", file = "SummaryStatsSTN_2006_2011.txt", append = TRUE)
capture.output(summary(gam(PM25~s(AOD,k=3)+te(julian2,year, month,  k=c(7,3,4),d=c(1,1,1),bs=c("cr","cr","cc")) ,data=misr.aqspm25.met.06.11.POC5)), file = "SummaryStatsSTN_2006_2011.txt", append = TRUE)
cat("GAM mod AOD STNEC\n", file = "SummaryStatsSTN_2006_2011.txt", append = TRUE)
capture.output(summary(gam(EC~s(AOD,k=3)+s(julian2)+s(month,  bs="cc"),data=misr.stn.met.06.11)), file = "SummaryStatsSTN_2006_2011.txt", append = TRUE)
cat("GAM mod AOD STNOC\n", file = "SummaryStatsSTN_2006_2011.txt", append = TRUE)
capture.output(summary(gam(OC~s(AOD,k=3)+s(julian2)+s(month,  bs="cc"),data=misr.stn.met.06.11)), file = "SummaryStatsSTN_2006_2011.txt", append = TRUE)
cat("GAM mod AOD STN5O4\n", file = "SummaryStatsSTN_2006_2011.txt", append = TRUE)
capture.output(summary(gam(SO4~s(AOD,k=3)+te(julian2,year, month,  k=c(7,3,4),d=c(1,1,1),bs=c("cr","cr","cc")),data=misr.stn.met.06.11)), file = "SummaryStatsSTN_2006_2011.txt", append = TRUE)
cat("GAM mod AOD STNO3\n", file = "SummaryStatsSTN_2006_2011.txt", append = TRUE)
capture.output(summary(gam(NO3~s(AOD,k=3)+te(julian2,year, month,  k=c(7,3,4),d=c(1,1,1),bs=c("cr","cr","cc")),data=misr.stn.met.06.11)), file = "SummaryStatsSTN_2006_2011.txt", append = TRUE)


# Cross Validation
misr.stn.points<-unique(misr.stn.met.06.11[,22:23])
misr.stnpm.points<-unique(misr.aqspm25.met.06.11.POC5[,38:39])
gam.pred.SO4.list<-vector('list',dim(misr.stn.points)[1])
gam.rsq.SO4.list<-vector('list')

gam.pred.NO3.list<-vector('list',dim(misr.stn.points)[1])
gam.rsq.NO3.list<-vector('list')

gam.pred.OC.list<-vector('list',dim(misr.stn.points)[1])
gam.rsq.OC.list<-vector('list')

gam.pred.stnpm.list<-vector('list',dim(misr.stnpm.points)[1])
gam.rsq.stnpm.list<-vector('list')

#:dim(misr.aqspm10.points)[1]


for (i in 1:dim(misr.stn.points)[1]){
  location.sample1<-misr.stn.points[i,]
  
  # Training data
  train0<-misr.stn.met.06.11[!(misr.stn.met.06.11$Latitude %in% location.sample1$Latitude
                               & misr.stn.met.06.11$Longitude %in% location.sample1$Longitude), ]
  # Test data: leave site out
  test0<-misr.stn.met.06.11[(misr.stn.met.06.11$Latitude %in% location.sample1$Latitude
                             & misr.stn.met.06.11$Longitude %in% location.sample1$Longitude), ]
  
  gam.st.SO4<-gam(SO4~s(AOD,k=3)+te(julian2,year, month,  k=c(4,3,4),d=c(1,1,1),bs=c("cr","cr","cc")), na.action=na.exclude,data=train0)
  
  gam.st.NO3<-gam(NO3~s(AOD,k=3)+te(julian2,year, month,  k=c(4,3,4),d=c(1,1,1),bs=c("cr","cr","cc")), na.action=na.exclude,data=train0)
  
  gam.st.OC<-gam(OC~s(AOD,k=3)+s(julian2,k=4)+s(month, k=4, bs="cc"), na.action=na.exclude,data=train0)
  
  gam.st.pred.SO4<-predict.gam(gam.st.SO4,newdata=test0)
  gam.st.pred.SO4.merge<-cbind(gam.st.pred.SO4,test0)
  rsq.SO4<-(summary(lm(gam.st.pred.SO4.merge$SO4~gam.st.pred.SO4.merge$gam.st.pred.SO4))$r.squared)
  rsq.SO4<-data.frame(rsq.SO4,location.sample1)
  
  gam.st.pred.NO3<-predict.gam(gam.st.NO3,newdata=test0)
  gam.st.pred.NO3.merge<-cbind(gam.st.pred.NO3,test0)
  rsq.NO3<-(summary(lm(gam.st.pred.NO3.merge$NO3~gam.st.pred.NO3.merge$gam.st.pred.NO3))$r.squared)
  rsq.NO3<-data.frame(rsq.NO3,location.sample1)
  
  gam.st.pred.OC<-predict.gam(gam.st.OC,newdata=test0)
  gam.st.pred.OC.merge<-cbind(gam.st.pred.OC,test0)
  rsq.OC<-(summary(lm(gam.st.pred.OC.merge$OC~gam.st.pred.OC.merge$gam.st.pred.OC))$r.squared)
  rsq.OC<-data.frame(rsq.OC,location.sample1)
  
  gam.pred.SO4.list[[i]]<-gam.st.pred.SO4.merge
  gam.rsq.SO4.list[[i]]<-rsq.SO4
  
  gam.pred.NO3.list[[i]]<-gam.st.pred.NO3.merge
  gam.rsq.NO3.list[[i]]<-rsq.NO3
  
  gam.pred.OC.list[[i]]<-gam.st.pred.OC.merge
  gam.rsq.OC.list[[i]]<-rsq.OC
  
}

gam.pred.SO4 <- do.call("rbind", gam.pred.SO4.list)
gam.SO4.rsq<-do.call("rbind", gam.rsq.SO4.list)
write.csv(gam.pred.SO4,"gam.SO4.met.pred.csv",row.names=FALSE)
write.csv(gam.SO4.rsq,"gam.SO4.met.cv.rsq.csv",row.names=FALSE)

gam.pred.NO3 <- do.call("rbind", gam.pred.NO3.list)
gam.NO3.rsq<-do.call("rbind", gam.rsq.NO3.list)
write.csv(gam.pred.NO3,"gam.NO3.met.pred.csv",row.names=FALSE)
write.csv(gam.NO3.rsq,"gam.NO3.met.cv.rsq.csv",row.names=FALSE)

gam.pred.OC <- do.call("rbind", gam.pred.OC.list)
gam.OC.rsq<-do.call("rbind", gam.rsq.OC.list)
write.csv(gam.pred.OC,"gam.OC.met.pred.csv",row.names=FALSE)
write.csv(gam.OC.rsq,"gam.OC.met.cv.rsq.csv",row.names=FALSE)




## STN PLOTS ##

p1<-qplot(AOD,PM25, data=misr.stn25.met.06.11,xlab="MISR AOD",ylab=expression('STN PM'[2.5]*', ug/m'^3))
plot1<-p1 +stat_smooth(method='lm',formula=y~x,col='black')
plot1
#plot1<-p1+stat_smooth(method="gam",formula=y~s(x,k=4)) +stat_smooth(method='lm',formula=y~x,col='red')

p2<-qplot(AOD,OC, data=misr.stn.met.06.11,xlab="MISR AOD",ylab=expression('STN OC, ug/m'^3))
plot2<-p2 +stat_smooth(method='lm',formula=y~x,col='black')
plot2
#plot2<-p2+stat_smooth(method="gam",formula=y~s(x,k=4)) +stat_smooth(method='lm',formula=y~x,col='red')

p3<-qplot(AOD,SO4, data=misr.stn.met.06.11,xlab="MISR AOD",ylab=expression('STN SO'[4]*', ug/m'^3))
plot3<-p3+stat_smooth(method='lm',formula=y~x,col='black')
plot3
#plot3<-p3+stat_smooth(method="gam",formula=y~s(x,k=4)) +stat_smooth(method='lm',formula=y~x,col='red')

p4<-qplot(AOD,NO3, data=misr.stn.met.06.11,xlab="MISR AOD",ylab=expression('STN NO'[3]*', ug/m'^3))
plot4<-p4+stat_smooth(method='lm',formula=y~x,col='black')
plot4
#plot4<-p4+stat_smooth(method="gam",formula=y~s(x,k=4)) +stat_smooth(method='lm',formula=y~x,col='red')

pdf('/Users/mf/Documents/MISR/Papers and Reports/RSE Revision/Revision 3/Figure3.pdf')
grid.arrange(plot1,plot2,plot3,plot4,nrow=2,ncol=2)
dev.off()
