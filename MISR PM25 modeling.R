##############################################
# 2000-2011 4.4km MISR-PM data analysis
# PM2.5
# Meredith Franklin
##############################################
library(mgcv)
library(ggplot2)
library(colorRamps)
library(ggmaps)
library(gridExtra)
library(dplyr)

setwd("/Users/mf/Documents/MISR/Reports")

# 2000-2011 Data (revision)
misr.aqspm25<-read.csv("/Users/mf/Documents/MISR/Data/Match 2000-2011/misr_pm25_2000_2011.csv")
misr.aqspm25.met<-read.csv("/Users/mf/Documents/MISR/Data/Match 2000-2011/misr_pm25_met_2000_2011.csv")
misr.aqspm25.met$rh=100*((112-0.1*misr.aqspm25.met$temp+misr.aqspm25.met$dew.point)/(112+0.9*misr.aqspm25.met$temp))^8

# Create new Julian date for time indexing, divide by 10000
misr.aqspm25$julian2<-misr.aqspm25$julian/10000
misr.aqspm25.met$julian2<-misr.aqspm25.met$julian/10000

# Create day of week variable for modeling
misr.aqspm25$dow<-(weekdays(as.Date(misr.aqspm25$date,"%m/%d/%y")))
misr.aqspm25.met$dow<-(weekdays(as.Date(misr.aqspm25.met$date,"%m/%d/%y")))

# Create AOD small+medium
misr.aqspm25$AODsm_med<-misr.aqspm25$AODsmall+misr.aqspm25$AODmed
misr.aqspm25.met$AODsm_med<-misr.aqspm25.met$AODsmall+misr.aqspm25.met$AODmed

# Remove AOD greater than 1 
misr.aqspm25.ss<-misr.aqspm25[misr.aqspm25$AOD<1,]
misr.aqspm25.met.ss<-misr.aqspm25.met[misr.aqspm25.met$AOD<1,]

#### PM2.5 Summary Statistics #####
misr.aqspm25.met.06.11<-misr.aqspm25.met.ss[misr.aqspm25.met.ss$year>=2006,]

# Remove STN and BAMS sites
misr.aqspm25.met.06.11<-misr.aqspm25.met.06.11[misr.aqspm25.met.06.11$POC %in% c(1,2,4,6,11,12),]
misr.aqspm25.points<-unique(misr.aqspm25.met.06.11[,38:39])

# Title (writes new file)
cat("PM25 Summary Stats", file = "SummaryStatsPM25_2006_2011.txt")
# add new lines
cat("\n", file = "SummaryStatsPM25_2006_2011.txt", append = TRUE)
# export summary statistics output
cat("Dim PM2.5", file = "SummaryStatsPM25_2006_2011.txt", append = TRUE)
capture.output(length(misr.aqspm25.met.06.11$PM25), file = "SummaryStatsPM25_2006_2011.txt", append = TRUE)
cat("Median AOD\n", file = "SummaryStatsPM25_2006_2011.txt", append = TRUE)
capture.output(median(misr.aqspm25.met.06.11$AOD), file = "SummaryStatsPM25_2006_2011.txt", append = TRUE)
cat("IQR AOD\n", file = "SummaryStatsPM25_2006_2011.txt", append = TRUE)
capture.output(IQR(misr.aqspm25.met.06.11$AOD), file = "SummaryStatsPM25_2006_2011.txt", append = TRUE)
cat("Median PM25\n", file = "SummaryStatsPM25_2006_2011.txt", append = TRUE)
capture.output(median(misr.aqspm25.met.06.11$PM25), file = "SummaryStatsPM25_2006_2011.txt", append = TRUE)
cat("IQR PM25\n", file = "SummaryStatsPM25_2006_2011.txt", append = TRUE)
capture.output(IQR(misr.aqspm25.met.06.11$PM25), file = "SummaryStatsPM25_2006_2011.txt", append = TRUE)
cat("Mean PM25\n", file = "SummaryStatsPM25_2006_2011.txt", append = TRUE)
capture.output(mean(misr.aqspm25.met.06.11$PM25), file = "SummaryStatsPM25_2006_2011.txt", append = TRUE)
cat("St Dev PM25\n", file = "SummaryStatsPM25_2006_2011.txt", append = TRUE)
capture.output(sd(misr.aqspm25.met.06.11$PM25), file = "SummaryStatsPM25_2006_2011.txt", append = TRUE)
cat("Cor AOD PM25\n", file = "SummaryStatsPM25_2006_2011.txt", append = TRUE)
capture.output(cor(misr.aqspm25.met.06.11$AOD,misr.aqspm25.met.06.11$PM25), file = "SummaryStatsPM25_2006_2011.txt", append = TRUE)
cat("Cor AODlarge PM25\n", file = "SummaryStatsPM25_2006_2011.txt", append = TRUE)
capture.output(cor(misr.aqspm25.met.06.11$AODlarge,misr.aqspm25.met.06.11$PM25), file = "SummaryStatsPM25_2006_2011.txt", append = TRUE)
cat("Cor AODsmall PM25\n", file = "SummaryStatsPM25_2006_2011.txt", append = TRUE)
capture.output(cor(misr.aqspm25.met.06.11$AODsmall,misr.aqspm25.met.06.11$PM25), file = "SummaryStatsPM25_2006_2011.txt", append = TRUE)
cat("Cor AOD PM25 met\n", file = "SummaryStatsPM25_2006_2011.txt", append = TRUE)
capture.output(cor(misr.aqspm25.met.06.11$AOD,misr.aqspm25.met.06.11$PM25), file = "SummaryStatsPM25_2006_2011.txt", append = TRUE)
cat("Cor AODlarge PM25 met\n", file = "SummaryStatsPM25_2006_2011.txt", append = TRUE)
capture.output(cor(misr.aqspm25.met.06.11$AODlarge,misr.aqspm25.met.06.11$PM25), file = "SummaryStatsPM25_2006_2011.txt", append = TRUE)
cat("Cor AODsmall PM25 met\n", file = "SummaryStatsPM25_2006_2011.txt", append = TRUE)
capture.output(cor(misr.aqspm25.met.06.11$AODsmall,misr.aqspm25.met.06.11$PM25), file = "SummaryStatsPM25_2006_2011.txt", append = TRUE)

# Linear Models PM25
cat("LM AOD PM25 met\n", file = "SummaryStatsPM25_2006_2011.txt", append = TRUE)
capture.output(summary(lm(PM25~AOD, data=misr.aqspm25.met.06.11)), file = "SummaryStatsPM25_2006_2011.txt", append = TRUE)
cat("LM AODsmall PM25 met\n", file = "SummaryStatsPM25_2006_2011.txt", append = TRUE)
capture.output(summary(lm(PM25~AODsmall, data=misr.aqspm25.met.06.11)), file = "SummaryStatsPM25_2006_2011.txt", append = TRUE)
cat("LM AODsmall_med PM25 met\n", file = "SummaryStatsPM25_2006_2011.txt", append = TRUE)
capture.output(summary(lm(PM25~AODsm_med, data=misr.aqspm25.met.06.11)), file = "SummaryStatsPM25_2006_2011.txt", append = TRUE)
cat("LM AODlarge PM25 met\n", file = "SummaryStatsPM25_2006_2011.txt", append = TRUE)
capture.output(summary(lm(PM25~AODlarge, data=misr.aqspm25.met.06.11)), file = "SummaryStatsPM25_2006_2011.txt", append = TRUE)

# GAM models PM25
cat("LM AOD PM25 \n", file = "SummaryStatsPM25_2006_2011.txt", append = TRUE)
capture.output(summary(gam(PM25~s(AOD), data=misr.aqspm25.met.06.11)), file = "SummaryStatsPM25_2006_2011.txt", append = TRUE)
cat("LM AODsmall PM25 \n", file = "SummaryStatsPM25_2006_2011.txt", append = TRUE)
capture.output(summary(gam(PM25~s(AODsmall), data=misr.aqspm25.met.06.11)), file = "SummaryStatsPM25_2006_2011.txt", append = TRUE)
cat("LM AODsmall_med PM25 \n", file = "SummaryStatsPM25_2006_2011.txt", append = TRUE)
capture.output(summary(gam(PM25~s(AODsm_med), data=misr.aqspm25.met.06.11)), file = "SummaryStatsPM25_2006_2011.txt", append = TRUE)
cat("LM AODlarge PM25 \n", file = "SummaryStatsPM25_2006_2011.txt", append = TRUE)
capture.output(summary(gam(PM25~s(AODlarge), data=misr.aqspm25.met.06.11)), file = "SummaryStatsPM25_2006_2011.txt", append = TRUE)

# S-T GAM models PM25
misr.aqspm25.met.06.11.POC<-misr.aqspm25.met.06.11[misr.aqspm25.met.06.11$POC %in% c(1,5),]
cat("LM AOD PM25 met\n", file = "SummaryStatsPM25_2006_2011.txt", append = TRUE)
capture.output(summary(gam(PM25~s(AOD)+te(x.1,y.1,julian2,k=c(25,8),d=c(2,1),bs=c('tp','cr'))+te(julian2,year, month,  k=c(12,4,7),d=c(1,1,1),bs=c("cr","cr","cc"))+as.factor(dow), data=misr.aqspm25.met.06.11)), file = "SummaryStatsPM25_2006_2011.txt", append = TRUE)
cat("LM AODsmall PM25 met\n", file = "SummaryStatsPM25_2006_2011.txt", append = TRUE)
capture.output(summary(gam(PM25~s(AODsmall)+te(x.1,y.1,julian2,k=c(25,8),d=c(2,1),bs=c('tp','cr'))+te(julian2,year, month,  k=c(12,4,7),d=c(1,1,1),bs=c("cr","cr","cc"))+as.factor(dow), data=misr.aqspm25.met.06.11)), file = "SummaryStatsPM25_2006_2011.txt", append = TRUE)
cat("LM AODsmall_med PM25 met\n", file = "SummaryStatsPM25_2006_2011.txt", append = TRUE)
capture.output(summary(gam(PM25~s(AODsm_med)+te(x.1,y.1,julian2,k=c(25,8),d=c(2,1),bs=c('tp','cr'))+te(julian2,year, month,  k=c(12,4,7),d=c(1,1,1),bs=c("cr","cr","cc"))+as.factor(dow), data=misr.aqspm25.met.06.11)), file = "SummaryStatsPM25_2006_2011.txt", append = TRUE)
cat("LM AODlarge PM25 met\n", file = "SummaryStatsPM25_2006_2011.txt", append = TRUE)
capture.output(summary(gam(PM25~s(AODlarge)+te(x.1,y.1,julian2,k=c(25,8),d=c(2,1),bs=c('tp','cr'))+te(julian2,year, month,  k=c(12,4,7),d=c(1,1,1),bs=c("cr","cr","cc"))+as.factor(dow), data=misr.aqspm25.met.06.11)), file = "SummaryStatsPM25_2006_2011.txt", append = TRUE)

# S-T GAM models with met PM25
cat("LM AOD PM25 met\n", file = "SummaryStatsPM25_2006_2011.txt", append = TRUE)
capture.output(summary(gam(PM25~s(AOD)+te(x.1,y.1,julian2,k=c(25,8),d=c(2,1),bs=c('tp','cr'))+te(julian2, year,month, k=c(12,4,7),d=c(1,1,1),bs=c("cr","cr","cc"))+as.factor(dow)+rh+wind.sp, data=misr.aqspm25.met.06.11)), file = "SummaryStatsPM25_2006_2011.txt", append = TRUE)
cat("LM AODsmall PM25 met\n", file = "SummaryStatsPM25_2006_2011.txt", append = TRUE)
capture.output(summary(gam(PM25~s(AODsmall)+te(x.1,y.1,julian2,k=c(25,8),d=c(2,1),bs=c('tp','cr'))+te(julian2, year,month, k=c(12,4,7),d=c(1,1,1),bs=c("cr","cr","cc"))+as.factor(dow)+rh+wind.sp, data=misr.aqspm25.met.06.11)), file = "SummaryStatsPM25_2006_2011.txt", append = TRUE)
cat("LM AODsmall_med PM25 met\n", file = "SummaryStatsPM25_2006_2011.txt", append = TRUE)
capture.output(summary(gam(PM25~s(AODsm_med)+te(x.1,y.1,julian2,k=c(25,8),d=c(2,1),bs=c('tp','cr'))+te(julian2, year,month, k=c(12,4,7),d=c(1,1,1),bs=c("cr","cr","cc"))+as.factor(dow)+rh+wind.sp, data=misr.aqspm25.met.06.11)), file = "SummaryStatsPM25_2006_2011.txt", append = TRUE)
cat("LM AODlarge PM25 met\n", file = "SummaryStatsPM25_2006_2011.txt", append = TRUE)
capture.output(summary(gam(PM25~s(AODlarge)+te(x.1,y.1,julian2,k=c(25,8),d=c(2,1),bs=c('tp','cr'))+te(julian2, year,month, k=c(12,4,7),d=c(1,1,1),bs=c("cr","cr","cc"))+as.factor(dow)+rh+wind.sp, data=misr.aqspm25.met.06.11)), file = "SummaryStatsPM25_2006_2011.txt", append = TRUE)

# S-T GAM models with met PM25
cat("LM AOD PM25 met\n", file = "SummaryStatsPM25_2006_2011.txt", append = TRUE)
capture.output(summary(gam(PM25~s(AOD)+s(x.1,y.1,k=25)+te(julian2, year,month, k=c(12,4,7),d=c(1,1,1),bs=c("cr","cr","cc"))+as.factor(dow)+rh+wind.sp, data=misr.aqspm25.met.06.11)), file = "SummaryStatsPM25_2006_2011.txt", append = TRUE)
cat("LM AODsmall PM25 met\n", file = "SummaryStatsPM25_2006_2011.txt", append = TRUE)
capture.output(summary(gam(PM25~s(AODsmall)+s(x.1,y.1,k=25)+te(julian2, year,month, k=c(12,4,7),d=c(1,1,1),bs=c("cr","cr","cc"))+as.factor(dow)+rh+wind.sp, data=misr.aqspm25.met.06.11)), file = "SummaryStatsPM25_2006_2011.txt", append = TRUE)
cat("LM AODsmall_med PM25 met\n", file = "SummaryStatsPM25_2006_2011.txt", append = TRUE)
capture.output(summary(gam(PM25~s(AODsm_med)+s(x.1,y.1,k=25)+te(julian2, year,month, k=c(12,4,7),d=c(1,1,1),bs=c("cr","cr","cc"))+as.factor(dow)+rh+wind.sp, data=misr.aqspm25.met.06.11)), file = "SummaryStatsPM25_2006_2011.txt", append = TRUE)
cat("LM AODlarge PM25 met\n", file = "SummaryStatsPM25_2006_2011.txt", append = TRUE)
capture.output(summary(gam(PM25~s(AODlarge)+s(x.1,y.1,k=25)+te(julian2, year,month, k=c(12,4,7),d=c(1,1,1),bs=c("cr","cr","cc"))+as.factor(dow)+rh+wind.sp, data=misr.aqspm25.met.06.11)), file = "SummaryStatsPM25_2006_2011.txt", append = TRUE)
# END PM25
# END PM25

# Match STN days
misr.aqspm25.match.stn<-misr.aqspm25.met.06.11[misr.aqspm25.met.06.11$day %in% misr.stn.met.06.11$day &
                                              misr.aqspm25.met.06.11$month %in% misr.stn.met.06.11$month &
                                              misr.aqspm25.met.06.11$year %in% misr.stn.met.06.11$year,] 
misr.aqspm25.match.stn.points<-unique(misr.aqspm25.match.stn[,38:39])

median(misr.aqspm25.match.stn$AOD)
IQR(misr.aqspm25.match.stn$AOD)
median(misr.aqspm25.match.stn$PM25)
IQR(misr.aqspm25.match.stn$PM25)
summary(lm(PM25~AOD, data=misr.aqspm25.match.stn))
summary(lm(PM25~AODsmall, data=misr.aqspm25.match.stn))
summary(lm(PM25~AODsm_med, data=misr.aqspm25.match.stn))
summary(lm(PM25~AODlarge, data=misr.aqspm25.match.stn))

## Cross Validation
#### PM2.5 Cross-Validation ####
misr.aqspm25.points<-unique(misr.aqspm25.met.06.11[,38:39])

gam.pred.pm25.list<-vector('list',dim(misr.aqspm25.points)[1])
gam.rsq.pm25.list<-vector('list')
gam.resid.pm25.list<-vector('list',dim(misr.aqspm25.points)[1])

gam.pred.pm25.list2<-vector('list',dim(misr.aqspm25.points)[1])
gam.rsq.pm25.list2<-vector('list')
gam.resid.pm25.list2<-vector('list',dim(misr.aqspm25.points)[1])

gam.pred.pm25.list3<-vector('list',dim(misr.aqspm25.points)[1])
gam.rsq.pm25.list3<-vector('list')
gam.resid.pm25.list3<-vector('list',dim(misr.aqspm25.points)[1])

gam.pred.pm25.list4<-vector('list',dim(misr.aqspm25.points)[1])
gam.rsq.pm25.list4<-vector('list')
gam.resid.pm25.list4<-vector('list',dim(misr.aqspm25.points)[1])

for (i in 26:dim(misr.aqspm25.points)[1]){
  location.sample1<-misr.aqspm25.points[i,]
  
  # Training data
  train0<-misr.aqspm25.met.06.11[!(misr.aqspm25.met.06.11$Latitude %in% location.sample1$Latitude
                            & misr.aqspm25.met.06.11$Longitude %in% location.sample1$Longitude), ]
  # Test data: leave site out
  test0<-misr.aqspm25.met.06.11[(misr.aqspm25.met.06.11$Latitude %in% location.sample1$Latitude
                          & misr.aqspm25.met.06.11$Longitude %in% location.sample1$Longitude), ]
  n<-dim(test0)[1]
  if (n>9){
 # gam.st.pm25<-gam(PM25~s(AOD)+te(x.1,y.1,julian2,k=c(25,8),d=c(2,1),bs=c('tp','cr'))+te(julian2,year,month, k=c(12,4,7),d=c(1,1,1),bs=c("cr","cr","cc"))+as.factor(dow), na.action=na.exclude,data=train0)
  
 # gam.st2.pm25<-gam(PM25~s(AODsm_med)+te(x.1,y.1,julian2,k=c(25,8),d=c(2,1),bs=c('tp','cr'))+te(julian2,year, month,  k=c(12,4,7),d=c(1,1,1),bs=c("cr","cr","cc"))+as.factor(dow), na.action=na.exclude,data=train0)
  
 # gam.st3.pm25<-gam(PM25~s(AOD)+te(x.1,y.1,julian2,k=c(25,8),d=c(2,1),bs=c('tp','cr'))+te(julian2,year, month,  k=c(12,4,7),d=c(1,1,1),bs=c("cr","cr","cc"))+as.factor(dow)+rh+wind.sp, na.action=na.exclude,data=train0)
  
  gam.st4.pm25<-gam(PM25~s(AODsm_med)+te(x.1,y.1,julian2,k=c(25,8),d=c(2,1),bs=c('tp','cr'))+te(julian2,year, month,  k=c(12,4,7),d=c(1,1,1),bs=c("cr","cr","cc"))+as.factor(dow)+rh+wind.sp, na.action=na.exclude,data=train0)
  #gam.st4.pm25<-gam(PM25~s(AODsm_med)+s(x.1,y.1,k=25)+te(julian2,year,k=c(12,4),d=c(1,1),bs=c("cr","cr"))+s(month,k=6,bs="cc")+as.factor(dow)+rh+wind.sp, na.action=na.exclude,data=train0)
  

  #gam.st.pred.pm25<-predict.gam(gam.st.pm25,newdata=test0)
  #gam.st.pred.pm25.merge<-cbind(gam.st.pred.pm25,test0,rep(n,length(gam.st.pred.pm25)))
  #rsq<-(summary(lm(gam.st.pred.pm25.merge$PM25~gam.st.pred.pm25.merge$gam.st.pred.pm25))$r.squared)
  #rsq<-data.frame(rsq,location.sample1,n)
  #gam.st.resid.pm25<-data.frame(resid=gam.st.pm25$residuals, y=gam.st.pm25$y, fitted=gam.st.pm25$fitted.values)
  
  
  #gam.st2.pred.pm25<-predict.gam(gam.st2.pm25,newdata=test0)
  #gam.st2.pred.pm25.merge<-cbind(gam.st2.pred.pm25,test0,rep(n,length(gam.st2.pred.pm25)))
  #rsq2<-(summary(lm(gam.st2.pred.pm25.merge$PM25~gam.st2.pred.pm25.merge$gam.st2.pred.pm25))$r.squared)
  #rsq2<-data.frame(rsq2,location.sample1,n)
  #gam.st2.resid.pm25<-data.frame(resid=gam.st2.pm25$residuals, y=gam.st2.pm25$y,fitted=gam.st2.pm25$fitted.values)
  
  
  #gam.st3.pred.pm25<-predict.gam(gam.st3.pm25,newdata=test0)
  #gam.st3.pred.pm25.merge<-cbind(gam.st3.pred.pm25,test0,rep(n,length(gam.st3.pred.pm25)))
  #rsq3<-(summary(lm(gam.st3.pred.pm25.merge$PM25~gam.st3.pred.pm25.merge$gam.st3.pred.pm25))$r.squared)
  #rsq3<-data.frame(rsq3,location.sample1,n)
  #gam.st3.resid.pm25<-data.frame(resid=gam.st3.pm25$residuals, y=gam.st3.pm25$y,fitted=gam.st3.pm25$fitted.values)
  
  
  gam.st4.pred.pm25<-predict.gam(gam.st4.pm25,newdata=test0)
  gam.st4.pred.pm25.merge<-cbind(gam.st4.pred.pm25,test0,rep(n,length(gam.st4.pred.pm25)))
  rsq4<-(summary(lm(gam.st4.pred.pm25.merge$PM25~gam.st4.pred.pm25.merge$gam.st4.pred.pm25))$r.squared)
  rsq4<-data.frame(rsq4,location.sample1,n)
  #gam.st4.resid.pm25<-data.frame(resid=gam.st4.pm25$residuals, y=gam.st4.pm25$y,fitted=gam.st4.pm25$fitted.values)
  
  
 # gam.pred.pm25.list[[i]]<-gam.st.pred.pm25.merge
#  gam.resid.pm25.list[[i]]<-gam.st.resid.pm25
#  gam.rsq.pm25.list[[i]]<-rsq
  
  #gam.pred.pm25.list2[[i]]<-gam.st2.pred.pm25.merge
  #gam.resid.pm25.list2[[i]]<-gam.st2.resid.pm25
  #gam.rsq.pm25.list2[[i]]<-rsq2
  
  #gam.pred.pm25.list3[[i]]<-gam.st3.pred.pm25.merge
  #gam.resid.pm25.list3[[i]]<-gam.st3.resid.pm25
  #gam.rsq.pm25.list3[[i]]<-rsq3
  
  gam.pred.pm25.list4[[i]]<-gam.st4.pred.pm25.merge
  #gam.resid.pm25.list4[[i]]<-gam.st4.resid.pm25
  gam.rsq.pm25.list4[[i]]<-rsq4
  }
}

gam.pred.pm25 <- do.call("rbind", gam.pred.pm25.list) # AOD no met
gam.rsq<-do.call("rbind", gam.rsq.pm25.list)
write.csv(gam.pred.pm25,"gam.pm25.pred.mod1.csv",row.names=FALSE)
write.csv(gam.rsq,"gam.pm25.cv.rsq.mod1.csv",row.names=FALSE)

gam.pred2.pm25 <- do.call("rbind", gam.pred.pm25.list2) # AOD sm+med no met
gam.rsq2<-do.call("rbind", gam.rsq.pm25.list2)
write.csv(gam.pred2.pm25,"gam.pm25.pred.mod2.csv",row.names=FALSE)
write.csv(gam.rsq2,"gam.pm25.cv.rsq.mod2.csv",row.names=FALSE)

rsq.geo2<-as.geodata(rsq.points2, coords.col=5:4, data.col=1)
points(rsq.geo2)
points(rsq.geo2,cex.max=1,col= rev(heat.colors(10)), x.leg= -77,y.leg=36, pt.divide="quintiles", main="PM2.5 concentrations (ug/m3)", xlab="Longitude",ylab="Latitude")

gam.pred3.pm25 <- do.call("rbind", gam.pred.pm25.list3) # AOD with met
gam.rsq3<-do.call("rbind", gam.rsq.pm25.list3)
write.csv(gam.pred3.pm25,"gam.pm25.pred.mod3.csv",row.names=FALSE)
write.csv(gam.rsq3,"gam.pm25.cv.rsq.mod3.csv",row.names=FALSE)


gam.pred4.pm25b <- do.call("rbind", gam.pred.pm25.list4) # AOD sm+med with met
gam.rsq4b<-do.call("rbind", gam.rsq.pm25.list4)
write.csv(gam.pred4.pm25,"gam.pm25.pred.mod4.csv",row.names=FALSE)
write.csv(gam.rsq4,"gam.pm25.cv.rsq.mod4.csv",row.names=FALSE)

gam.pred4.pm25 <- do.call("rbind", gam.pred.pm25.list4) # AOD sm+med with met
gam.rsq4<-do.call("rbind", gam.rsq.pm25.list4)
write.csv(gam.pred4.pm25,"gam.pm25.pred.mod4.csv",row.names=FALSE)
write.csv(gam.rsq4,"gam.pm25.cv.rsq.mod4.csv",row.names=FALSE)



# Plots

# PM25 AOD sm+med scatterplot
pdf('MISR.AOD.PM25_new.pdf')
p<-qplot(AOD,PM25, data=misr.aqspm25.met.06.11,xlab="MISR AOD",ylab=expression("AQS PM"[2.5]*", ug/m"^3))
p+stat_smooth(method="gam",formula=y~s(x,k=4)) +stat_smooth(method='lm',formula=y~x,col='red')
dev.off()


# Cross validation

gam.pred4.pm25b<-gam.pred4.pm25[gam.pred4.pm25$gam.st4.pred.pm25<100,]
gam.pred4.pm25b<-gam.pred4.pm25b[gam.pred4.pm25b$gam.st4.pred.pm25>0,]
summary(lm(gam.pred4.pm25b$PM25~gam.pred4.pm25b$gam.st4.pred.pm25))
#PM2.5
pdf("CV_PM25_06_11.pdf")
p12<-qplot(gam.st4.pred.pm25,PM25, data=gam.pred4.pm25b,xlab=expression('Predicted PM'[2.5]*', ug/m'^3),ylab=expression('Observed PM'[2.5]*', ug/m'^3))
plot12<-p12+geom_abline(intercept = 0, slope = 1, color="red",size=1.5)+stat_smooth(method='lm',formula=y~x,col='blue')
plot12
dev.off()

# Map of CV R2
map <- get_map(c(lon=-118.3, lat=34.1), zoom = 7 , maptype = 'hybrid')
ggmap(map,extent="panel",legend="bottomleft") + geom_point(data = gam.rsq4, aes(x = Longitude, y = Latitude, color=rsq4),  size=1.1, alpha=0.5)+ 
  scale_color_gradientn(colours=matlab.like(20), limits=c(10,95),breaks=seq(10,100,by=10))
rsq.pts<-as.geodata(gam.rsq4,coords.col = 3:2, data.col = 1)
plot(rsq.pts)
points(rsq.pts)
map('state',"California",add=TRUE)


# MISR data to convert AOD to PM25 and map
library(colorRamps)
library(ggmaps)
misr<-read.csv("/Users/mf/Documents/MISR/Data/misr_2000_2011.csv")
misr<-misr[misr$land.water.mask==3,]
misr.grid<-unique(misr[,1:2])
misr_06_11<-misr[misr$year>=2006,]
misr.date<-unique(misr_06_11[,13])
# PM2.5 model
gam.st.pm25<-gam(PM25~s(AODsm_med)+te(x.1,y.1,julian2,k=c(25,8),d=c(2,1),bs=c('tp','cr'))+te(julian2, year,month, k=c(12,4,7),d=c(1,1,1),bs=c("cr","cr","cc"))+as.factor(dow), data=misr.aqspm25.met.06.11)

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


pm25.pred.summer<-pm25.predicted.all[pm25.predicted.all$month %in% c(5,6,7,8,9,10),]
pm25.pred.summer.avg<-ddply(pm25.pred.summer, .(lat,lon), summarise, AODsm_med=mean(AODsm_med,na.rm=TRUE),PM25=mean(predicted.pm25,na.rm=TRUE))
write.csv(pm25.pred.summer.avg, "/Users/mf/Documents/MISR/Data/predicted_pm25_warm_2006_2011.csv")


pm25.pred.winter<-pm25.predicted.all[pm25.predicted.all$month %in% c(11,12,1,2,3,4),]
pm25.pred.winter.avg<-ddply(pm25.pred.winter, .(lat,lon), summarise, AODsm_med=mean(AODsm_med,na.rm=TRUE),PM25=mean(predicted.pm25,na.rm=TRUE))
write.csv(pm25.pred.winter.avg, "/Users/mf/Documents/MISR/Data/predicted_pm25_cool_2006_2011.csv")


map <- get_map(c(lon=-118.3, lat=34.1), zoom = 7 , maptype = 'hybrid')

pdf("winter_pm25_06_11.pdf")
ggmap(map,extent="panel",legend="bottomleft") + geom_point(data = pm25.pred.winter.avg, aes(x = lon, y = lat, color=PM25),  size=1.1, alpha=0.5)+ 
  scale_color_gradientn(colours=matlab.like(20), limits=c(5,35),breaks=c(5,10,15,20,25,30,35,40))
dev.off()

ggmap(map,extent="panel",legend="bottomleft") + geom_point(data = pm25.pred.winter.avg, aes(x = lon, y = lat, color=AOD),  size=1.1, alpha=0.5)+ 
  scale_color_gradientn(colours=matlab.like(20), limits=c(0,0.5),breaks=seq(0,.5,by=0.1))

pdf("summer_pm25_06_11.pdf")
ggmap(map,extent="panel",legend="bottomleft") + geom_point(data = pm25.pred.summer.avg, aes(x = lon, y = lat, color=PM25),  size=1.1, alpha=0.5)+ 
  scale_color_gradientn(colours=matlab.like(20), limits=c(5,35),breaks=c(5,10,15,20,25,30,35,40))
dev.off()

ggmap(map,extent="panel",legend="bottomleft") + geom_point(data = pm25.pred.summer.avg, aes(x = lon, y = lat, color=AOD),  size=1.1, alpha=0.5)+ 
  scale_color_gradientn(colours=matlab.like(20), limits=c(0,0.5),breaks=seq(0,.5,by=0.1))
qp1 <- qplot(pm25.pred.summer.avg$AOD,geom="histogram",xlab="AOD",ylab="count",bins=55, col="red", fill="red", alpha = .2)+
  theme(plot.background = element_rect(fill = "transparent",colour = NA),text=element_text(colour="grey"),
        axis.text=element_text(colour="white"),legend.position = "none")
print(qp1, vp=viewport(.25, .3, .33, .37))


# Observed PM25 summer verages
pm25.obs.summer<-misr.aqspm25.met.06.11[misr.aqspm25.met.06.11$month %in% c(5,6,7,8,9,10),]
pm25.obs.summer.avg<-ddply(pm25.obs.summer, .(Latitude,Longitude), summarise,PM25=mean(PM25,na.rm=TRUE),site=unique(CBSA_NAME))
# Observed PM25 winter averages
pm25.obs.winter<-misr.aqspm25.met.06.11[misr.aqspm25.met.06.11$month %in% c(11,12,1,2,3,4),]
pm25.obs.winter.avg<-ddply(pm25.obs.winter, .(Latitude,Longitude), summarise,PM25=mean(PM25,na.rm=TRUE),site=unique(CBSA_NAME))


