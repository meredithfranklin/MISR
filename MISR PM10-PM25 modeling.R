##############################################
# 2000-2011 4.4km MISR-PM data analysis
# pm10_pm25
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
misr.aqspm2510<-read.csv("/Users/mf/Documents/MISR/Data/Match 2000-2011/misr_pm10_pm25_2000_2011.csv")
misr.aqspm2510.met<-read.csv("/Users/mf/Documents/MISR/Data/Match 2000-2011/misr_pm10_pm25_met_2000_2011.csv")
misr.aqspm2510.met$rh=100*((112-0.1*misr.aqspm2510.met$temp+misr.aqspm2510.met$dew.point)/(112+0.9*misr.aqspm2510.met$temp))^8

misr.aqspm2510$julian2<-misr.aqspm2510$julian/10000
misr.aqspm2510.met$julian2<-misr.aqspm2510.met$julian/1000

misr.aqspm2510$dow<-(weekdays(as.Date(misr.aqspm2510$date2,"%Y-%m-%d")))
misr.aqspm2510.met$dow<-(weekdays(as.Date(misr.aqspm2510.met$date2,"%Y-%m-%d")))

misr.aqspm2510$AODsm_med<-misr.aqspm2510$AODsmall+misr.aqspm2510$AODmed
misr.aqspm2510.met$AODsm_med<-misr.aqspm2510.met$AODsmall+misr.aqspm2510.met$AODmed

misr.aqspm2510.ss<-misr.aqspm2510[misr.aqspm2510$AOD<1,]
misr.aqspm2510.met.ss<-misr.aqspm2510.met[misr.aqspm2510.met$AOD<1,]

# Summary Statistics pm10_pm25
misr.aqspm2510.met.06.11<-misr.aqspm2510.met.ss[misr.aqspm2510.met.ss$year>=2006,]
misr.aqspm2510.met.06.11<-misr.aqspm2510.met.06.11[misr.aqspm2510.met.06.11$pm10_pm25>=0,]
hist(misr.aqspm2510.met.06.11$pm10_pm25)
misr.aqspm2510.points<-unique(misr.aqspm2510.met.06.11[,22:23])

cat("pm10_pm25 Summary Stats", file = "SummaryStatspm10_pm25_2006_2011.txt")
# add new lines
cat("\n", file = "SummaryStatspm10_pm25_2006_2011.txt", append = TRUE)
# export summary statistics output
cat("Dim PM2.5", file = "SummaryStatspm10_pm25_2006_2011.txt", append = TRUE)
capture.output(length(misr.aqspm2510.met.06.11$pm10_pm25), file = "SummaryStatspm10_pm25_2006_2011.txt", append = TRUE)
cat("Median AOD\n", file = "SummaryStatspm10_pm25_2006_2011.txt", append = TRUE)
capture.output(median(misr.aqspm2510.met.06.11$AOD), file = "SummaryStatspm10_pm25_2006_2011.txt", append = TRUE)
cat("IQR AOD\n", file = "SummaryStatspm10_pm25_2006_2011.txt", append = TRUE)
capture.output(IQR(misr.aqspm2510.met.06.11$AOD), file = "SummaryStatspm10_pm25_2006_2011.txt", append = TRUE)
cat("Median pm10_pm25\n", file = "SummaryStatspm10_pm25_2006_2011.txt", append = TRUE)
capture.output(median(misr.aqspm2510.met.06.11$pm10_pm25), file = "SummaryStatspm10_pm25_2006_2011.txt", append = TRUE)
cat("IQR pm10_pm25\n", file = "SummaryStatspm10_pm25_2006_2011.txt", append = TRUE)
capture.output(IQR(misr.aqspm2510.met.06.11$pm10_pm25), file = "SummaryStatspm10_pm25_2006_2011.txt", append = TRUE)
cat("Mean pm10_pm25\n", file = "SummaryStatspm10_pm25_2006_2011.txt", append = TRUE)
capture.output(mean(misr.aqspm2510.met.06.11$pm10_pm25), file = "SummaryStatspm10_pm25_2006_2011.txt", append = TRUE)
cat("St Dev pm10_pm25\n", file = "SummaryStatspm10_pm25_2006_2011.txt", append = TRUE)
capture.output(sd(misr.aqspm2510.met.06.11$pm10_pm25), file = "SummaryStatspm10_pm25_2006_2011.txt", append = TRUE)
cat("Cor AOD pm10_pm25\n", file = "SummaryStatspm10_pm25_2006_2011.txt", append = TRUE)
capture.output(cor(misr.aqspm2510.met.06.11$AOD,misr.aqspm2510.met.06.11$pm10_pm25), file = "SummaryStatspm10_pm25_2006_2011.txt", append = TRUE)
cat("Cor AODlarge pm10_pm25\n", file = "SummaryStatspm10_pm25_2006_2011.txt", append = TRUE)
capture.output(cor(misr.aqspm2510.met.06.11$AODlarge,misr.aqspm2510.met.06.11$pm10_pm25), file = "SummaryStatspm10_pm25_2006_2011.txt", append = TRUE)
cat("Cor AODsmall pm10_pm25\n", file = "SummaryStatspm10_pm25_2006_2011.txt", append = TRUE)
capture.output(cor(misr.aqspm2510.met.06.11$AODsmall,misr.aqspm2510.met.06.11$pm10_pm25), file = "SummaryStatspm10_pm25_2006_2011.txt", append = TRUE)
cat("Cor AOD pm10_pm25 met\n", file = "SummaryStatspm10_pm25_2006_2011.txt", append = TRUE)
capture.output(cor(misr.aqspm2510.met.06.11$AOD,misr.aqspm2510.met.06.11$pm10_pm25), file = "SummaryStatspm10_pm25_2006_2011.txt", append = TRUE)
cat("Cor AODlarge pm10_pm25 met\n", file = "SummaryStatspm10_pm25_2006_2011.txt", append = TRUE)
capture.output(cor(misr.aqspm2510.met.06.11$AODlarge,misr.aqspm2510.met.06.11$pm10_pm25), file = "SummaryStatspm10_pm25_2006_2011.txt", append = TRUE)
cat("Cor AODsmall pm10_pm25 met\n", file = "SummaryStatspm10_pm25_2006_2011.txt", append = TRUE)
capture.output(cor(misr.aqspm2510.met.06.11$AODsmall,misr.aqspm2510.met.06.11$pm10_pm25), file = "SummaryStatspm10_pm25_2006_2011.txt", append = TRUE)


# Linear Models pm10_pm25
cat("LM AOD pm10_pm25 met\n", file = "SummaryStatspm10_pm25_2006_2011.txt", append = TRUE)
capture.output(summary(lm(pm10_pm25~AOD, data=misr.aqspm2510.met.06.11)), file = "SummaryStatspm10_pm25_2006_2011.txt", append = TRUE)
cat("LM AODsmall pm10_pm25 met\n", file = "SummaryStatspm10_pm25_2006_2011.txt", append = TRUE)
capture.output(summary(lm(pm10_pm25~AODsmall, data=misr.aqspm2510.met.06.11)), file = "SummaryStatspm10_pm25_2006_2011.txt", append = TRUE)
cat("LM AODsmall_med pm10_pm25 met\n", file = "SummaryStatspm10_pm25_2006_2011.txt", append = TRUE)
capture.output(summary(lm(pm10_pm25~AODsm_med, data=misr.aqspm2510.met.06.11)), file = "SummaryStatspm10_pm25_2006_2011.txt", append = TRUE)
cat("LM AODlarge pm10_pm25 met\n", file = "SummaryStatspm10_pm25_2006_2011.txt", append = TRUE)
capture.output(summary(lm(pm10_pm25~AODlarge, data=misr.aqspm2510.met.06.11)), file = "SummaryStatspm10_pm25_2006_2011.txt", append = TRUE)

# GAM models pm10_pm25
cat("LM AOD pm10_pm25 \n", file = "SummaryStatspm10_pm25_2006_2011.txt", append = TRUE)
capture.output(summary(gam(pm10_pm25~s(AOD), data=misr.aqspm2510.met.06.11)), file = "SummaryStatspm10_pm25_2006_2011.txt", append = TRUE)
cat("LM AODsmall pm10_pm25 \n", file = "SummaryStatspm10_pm25_2006_2011.txt", append = TRUE)
capture.output(summary(gam(pm10_pm25~s(AODsmall), data=misr.aqspm2510.met.06.11)), file = "SummaryStatspm10_pm25_2006_2011.txt", append = TRUE)
cat("LM AODsmall_med pm10_pm25 \n", file = "SummaryStatspm10_pm25_2006_2011.txt", append = TRUE)
capture.output(summary(gam(pm10_pm25~s(AODsm_med), data=misr.aqspm2510.met.06.11)), file = "SummaryStatspm10_pm25_2006_2011.txt", append = TRUE)
cat("LM AODlarge pm10_pm25 \n", file = "SummaryStatspm10_pm25_2006_2011.txt", append = TRUE)
capture.output(summary(gam(pm10_pm25~s(AODlarge), data=misr.aqspm2510.met.06.11)), file = "SummaryStatspm10_pm25_2006_2011.txt", append = TRUE)

# S-T GAM models pm10_pm25
misr.aqspm2510.met.06.11.POC<-misr.aqspm2510.met.06.11[misr.aqspm2510.met.06.11$POC %in% c(1,5),]
cat("LM AOD pm10_pm25 met\n", file = "SummaryStatspm10_pm25_2006_2011.txt", append = TRUE)
capture.output(summary(gam(pm10_pm25~s(AOD,k=3)+te(x.1,y.1,julian2,k=c(10,8),d=c(2,1),bs=c('tp','cr'))+te(julian2,year, month,  k=c(7,3,4),d=c(1,1,1),bs=c("cr","cr","cc"))+as.factor(dow), data=misr.aqspm2510.met.06.11)), file = "SummaryStatspm10_pm25_2006_2011.txt", append = TRUE)
cat("LM AODlarge pm10_pm25 met\n", file = "SummaryStatspm10_pm25_2006_2011.txt", append = TRUE)
capture.output(summary(gam(pm10_pm25~s(AODlarge)+te(x.1,y.1,julian2,k=c(10,8),d=c(2,1),bs=c('tp','cr'))+te(julian2,year, month,  k=c(7,3,4),d=c(1,1,1),bs=c("cr","cr","cc"))+as.factor(dow), data=misr.aqspm2510.met.06.11)), file = "SummaryStatspm10_pm25_2006_2011.txt", append = TRUE)

# S-T GAM models with met pm10_pm25
cat("LM AOD pm10_pm25 met\n", file = "SummaryStatspm10_pm25_2006_2011.txt", append = TRUE)
capture.output(summary(gam(pm10_pm25~s(AOD)+te(x.1,y.1,julian2,k=c(25,8),d=c(2,1),bs=c('tp','cr'))+te(julian2,year)+s(month, bs="cc",k=7)+as.factor(dow)+rh+wind.sp, data=misr.aqspm2510.met.06.11)), file = "SummaryStatspm10_pm25_2006_2011.txt", append = TRUE)
cat("LM AODsmall pm10_pm25 met\n", file = "SummaryStatspm10_pm25_2006_2011.txt", append = TRUE)
capture.output(summary(gam(pm10_pm25~s(AODsmall)+te(x.1,y.1,julian2,k=c(25,8),d=c(2,1),bs=c('tp','cr'))+te(julian2,year)+s(month, bs="cc",k=7)+as.factor(dow)+rh+wind.sp, data=misr.aqspm2510.met.06.11)), file = "SummaryStatspm10_pm25_2006_2011.txt", append = TRUE)
cat("LM AODsmall_med pm10_pm25 met\n", file = "SummaryStatspm10_pm25_2006_2011.txt", append = TRUE)
capture.output(summary(gam(pm10_pm25~s(AODsm_med)+te(x.1,y.1,julian2,k=c(25,8),d=c(2,1),bs=c('tp','cr'))+te(julian2,year)+s(month, bs="cc",k=7)+as.factor(dow)+rh+wind.sp, data=misr.aqspm2510.met.06.11)), file = "SummaryStatspm10_pm25_2006_2011.txt", append = TRUE)
cat("LM AODlarge pm10_pm25 met\n", file = "SummaryStatspm10_pm25_2006_2011.txt", append = TRUE)
capture.output(summary(gam(pm10_pm25~s(AODlarge)+te(x.1,y.1,julian2,k=c(25,8),d=c(2,1),bs=c('tp','cr'))+te(julian2,year)+s(month, bs="cc",k=7)+as.factor(dow)+rh+wind.sp, data=misr.aqspm2510.met.06.11)), file = "SummaryStatspm10_pm25_2006_2011.txt", append = TRUE)
# END MODELS pm10_pm25

# Cross Validation
misr.aqspm2510.points<-unique(misr.aqspm2510.met.06.11[,22:23])

gam.pred.pm2510.list<-vector('list',dim(misr.aqspm2510.points)[1])
gam.rsq.pm2510.list<-vector('list')

gam.pred.pm2510.list2<-vector('list',dim(misr.aqspm2510.points)[1])
gam.rsq.pm2510.list2<-vector('list')

gam.pred.pm2510.list3<-vector('list',dim(misr.aqspm2510.points)[1])
gam.rsq.pm2510.list3<-vector('list')

gam.pred.pm2510.list4<-vector('list',dim(misr.aqspm2510.points)[1])
gam.rsq.pm2510.list4<-vector('list')

for (i in 1:dim(misr.aqspm2510.points)[1]){
  location.sample1<-misr.aqspm2510.points[i,]
  
  # Training data
  train0<-misr.aqspm2510.met.06.11[!(misr.aqspm2510.met.06.11$Latitude %in% location.sample1$Latitude
                                     & misr.aqspm2510.met.06.11$Longitude %in% location.sample1$Longitude), ]
  # Test data: leave site out
  test0<-misr.aqspm2510.met.06.11[(misr.aqspm2510.met.06.11$Latitude %in% location.sample1$Latitude
                                   & misr.aqspm2510.met.06.11$Longitude %in% location.sample1$Longitude), ]
  n<-dim(test0)[1]
  if (n>9){
    #gam.st.pm2510<-gam(pm10_pm25~s(AOD)+te(x.1,y.1,julian2,k=c(15,6),d=c(2,1),bs=c('tp','cr'))+te(julian2,year,month, k=c(12,4,7),d=c(1,1,1),bs=c("cr","cr","cc"))+as.factor(dow), na.action=na.exclude,data=train0)
    gam.st.pm2510<-gam(pm10_pm25~s(AOD)+s(x.1,y.1,k=15)+te(julian2,year,month, k=c(12,4,7),d=c(1,1,1),bs=c("cr","cr","cc"))+as.factor(dow), na.action=na.exclude,data=train0)
    
    # gam.st2.pm2510<-gam(pm10_pm25~s(AODlarge)+te(x.1,y.1,julian2,k=c(15,6),d=c(2,1),bs=c('tp','cr'))+te(julian2,year, month,  k=c(12,4,7),d=c(1,1,1),bs=c("cr","cr","cc"))+as.factor(dow), na.action=na.exclude,data=train0)
    gam.st2.pm2510<-gam(pm10_pm25~s(AODlarge)+s(x.1,y.1,k=15)+te(julian2,year,month, k=c(12,4,7),d=c(1,1,1),bs=c("cr","cr","cc"))+as.factor(dow), na.action=na.exclude,data=train0)
    
   #  gam.st3.pm2510<-gam(pm10_pm25~s(AOD)+te(x.1,y.1,julian2,k=c(15,6),d=c(2,1),bs=c('tp','cr'))+te(julian2,year, month,  k=c(12,4,7),d=c(1,1,1),bs=c("cr","cr","cc"))+as.factor(dow)+rh+wind.sp, na.action=na.exclude,data=train0)
    gam.st3.pm2510<-gam(pm10_pm25~s(AOD)+s(x.1,y.1,k=15)+te(julian2,year,month, k=c(12,4,7),d=c(1,1,1),bs=c("cr","cr","cc"))+as.factor(dow)+rh+wind.sp, na.action=na.exclude,data=train0)
    
   # gam.st4.pm2510<-gam(pm10_pm25~s(AODlarge)+te(x.1,y.1,julian2,k=c(15,6),d=c(2,1),bs=c('tp','cr'))+te(julian2,year,month,  k=c(12,4,7),d=c(1,1,1),bs=c("cr","cr","cc"))+as.factor(dow)+rh+wind.sp, na.action=na.exclude,data=train0)
    gam.st4.pm2510<-gam(pm10_pm25~s(AODlarge)+s(x.1,y.1,k=15)+te(julian2,year,month, k=c(12,4,7),d=c(1,1,1),bs=c("cr","cr","cc"))+as.factor(dow)+rh+wind.sp, na.action=na.exclude,data=train0)
    

    
    gam.st.pred.pm2510<-predict.gam(gam.st.pm2510,newdata=test0)
    gam.st.pred.pm2510.merge<-cbind(gam.st.pred.pm2510,test0,rep(n,length(gam.st.pred.pm2510)))
    rsq<-(summary(lm(gam.st.pred.pm2510.merge$pm10_pm25~gam.st.pred.pm2510.merge$gam.st.pred.pm2510))$r.squared)
    rsq<-data.frame(rsq,location.sample1,n)
 
    gam.st2.pred.pm2510<-predict.gam(gam.st2.pm2510,newdata=test0)
    gam.st2.pred.pm2510.merge<-cbind(gam.st2.pred.pm2510,test0,rep(n,length(gam.st2.pred.pm2510)))
    rsq2<-(summary(lm(gam.st2.pred.pm2510.merge$pm10_pm25~gam.st2.pred.pm2510.merge$gam.st2.pred.pm2510))$r.squared)
    rsq2<-data.frame(rsq2,location.sample1,n)
    
    gam.st3.pred.pm2510<-predict.gam(gam.st3.pm2510,newdata=test0)
    gam.st3.pred.pm2510.merge<-cbind(gam.st3.pred.pm2510,test0,rep(n,length(gam.st3.pred.pm2510)))
    rsq3<-(summary(lm(gam.st3.pred.pm2510.merge$pm10_pm25~gam.st3.pred.pm2510.merge$gam.st3.pred.pm2510))$r.squared)
    rsq3<-data.frame(rsq3,location.sample1,n)

    gam.st4.pred.pm2510<-predict.gam(gam.st4.pm2510,newdata=test0)
    gam.st4.pred.pm2510.merge<-cbind(gam.st4.pred.pm2510,test0,rep(n,length(gam.st4.pred.pm2510)))
    rsq4<-(summary(lm(gam.st4.pred.pm2510.merge$pm10_pm25~gam.st4.pred.pm2510.merge$gam.st4.pred.pm2510))$r.squared)
    rsq4<-data.frame(rsq4,location.sample1,n)
   
    
    gam.pred.pm2510.list[[i]]<-gam.st.pred.pm2510.merge
    gam.rsq.pm2510.list[[i]]<-rsq
    
    gam.pred.pm2510.list2[[i]]<-gam.st2.pred.pm2510.merge
    gam.rsq.pm2510.list2[[i]]<-rsq2
    
    gam.pred.pm2510.list3[[i]]<-gam.st3.pred.pm2510.merge
    gam.rsq.pm2510.list3[[i]]<-rsq3
    
    gam.pred.pm2510.list4[[i]]<-gam.st4.pred.pm2510.merge
    gam.rsq.pm2510.list4[[i]]<-rsq4
  }
}
gam.pred.pm2510 <- do.call("rbind", gam.pred.pm2510.list) # AOD 
gam.rsq<-do.call("rbind", gam.rsq.pm2510.list)
write.csv(gam.pred.pm2510,"gam.pm2510.pred.mod1.csv",row.names=FALSE)
write.csv(gam.rsq,"gam.pm2510.cv.rsq.mod1.csv",row.names=FALSE)

gam.pred2.pm2510 <- do.call("rbind", gam.pred.pm2510.list2) # AOD large
gam.rsq2<-do.call("rbind", gam.rsq.pm2510.list2)
write.csv(gam.pred2.pm2510,"gam.pm2510.pred.mod2.csv",row.names=FALSE)
write.csv(gam.rsq2,"gam.pm2510.cv.rsq.mod2.csv",row.names=FALSE)

gam.pred3.pm2510 <- do.call("rbind", gam.pred.pm2510.list3) # AOD with met
gam.rsq3<-do.call("rbind", gam.rsq.pm2510.list3)
write.csv(gam.pred3.pm2510,"gam.pm2510.pred.mod3.csv",row.names=FALSE)
write.csv(gam.rsq3,"gam.pm2510.cv.rsq.mod3.csv",row.names=FALSE)


gam.pred4.pm2510 <- do.call("rbind", gam.pred.pm2510.list4) # AOD large with met
gam.rsq4<-do.call("rbind", gam.rsq.pm2510.list4)
write.csv(gam.pred4.pm2510,"gam.pm2510.pred.mod4.csv",row.names=FALSE)
write.csv(gam.rsq4,"gam.pm2510.cv.rsq.mod4.csv",row.names=FALSE)
