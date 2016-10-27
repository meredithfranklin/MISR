##############################################
# 2000-2011 4.4km MISR-PM data analysis
# PM10
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
misr.aqspm10<-read.csv("/Users/mf/Documents/MISR/Data/Match 2000-2011/misr_pm10_2000_2011.csv")
misr.aqspm10.met<-read.csv("/Users/mf/Documents/MISR/Data/Match 2000-2011/misr_pm10_met_2000_2011.csv")
misr.aqspm10.met$rh=100*((112-0.1*misr.aqspm10.met$temp+misr.aqspm10.met$dew.point)/(112+0.9*misr.aqspm10.met$temp))^8

misr.aqspm10$julian2<-misr.aqspm10$julian/10000
misr.aqspm10.met$julian2<-misr.aqspm10.met$julian/10000
misr.aqspm10$dow<-(weekdays(as.Date(misr.aqspm10$date2,"%Y-%m-%d")))
misr.aqspm10.met$dow<-(weekdays(as.Date(misr.aqspm10.met$date2,"%Y-%m-%d")))
misr.aqspm10$AODsm_med<-misr.aqspm10$AODsmall+misr.aqspm10$AODmed
misr.aqspm10.met$AODsm_med<-misr.aqspm10.met$AODsmall+misr.aqspm10.met$AODmed
misr.aqspm10.ss<-misr.aqspm10[misr.aqspm10$AOD<1,]
misr.aqspm10.met.ss<-misr.aqspm10.met[misr.aqspm10.met$AOD<1,]

# Summary Statistics PM10
misr.aqspm10.met.06.11<-misr.aqspm10.met.ss[misr.aqspm10.met.ss$year>=2006,]
misr.aqspm10.points<-unique(misr.aqspm10.met.06.11[,38:39])

cat("PM10 Summary Stats", file = "SummaryStatsPM10_2006_2011.txt")
# add new lines
cat("\n", file = "SummaryStatsPM10_2006_2011.txt", append = TRUE)
# export summary statistics output
cat("Dim PM2.5", file = "SummaryStatsPM10_2006_2011.txt", append = TRUE)
capture.output(length(misr.aqspm10.met.06.11$PM10), file = "SummaryStatsPM10_2006_2011.txt", append = TRUE)
cat("Median AOD\n", file = "SummaryStatsPM10_2006_2011.txt", append = TRUE)
capture.output(median(misr.aqspm10.met.06.11$AOD), file = "SummaryStatsPM10_2006_2011.txt", append = TRUE)
cat("IQR AOD\n", file = "SummaryStatsPM10_2006_2011.txt", append = TRUE)
capture.output(IQR(misr.aqspm10.met.06.11$AOD), file = "SummaryStatsPM10_2006_2011.txt", append = TRUE)
cat("Median PM10\n", file = "SummaryStatsPM10_2006_2011.txt", append = TRUE)
capture.output(median(misr.aqspm10.met.06.11$PM10), file = "SummaryStatsPM10_2006_2011.txt", append = TRUE)
cat("IQR PM10\n", file = "SummaryStatsPM10_2006_2011.txt", append = TRUE)
capture.output(IQR(misr.aqspm10.met.06.11$PM10), file = "SummaryStatsPM10_2006_2011.txt", append = TRUE)
cat("Mean PM10\n", file = "SummaryStatsPM10_2006_2011.txt", append = TRUE)
capture.output(mean(misr.aqspm10.met.06.11$PM10), file = "SummaryStatsPM10_2006_2011.txt", append = TRUE)
cat("St Dev PM10\n", file = "SummaryStatsPM10_2006_2011.txt", append = TRUE)
capture.output(sd(misr.aqspm10.met.06.11$PM10), file = "SummaryStatsPM10_2006_2011.txt", append = TRUE)
cat("Cor AOD PM10\n", file = "SummaryStatsPM10_2006_2011.txt", append = TRUE)
capture.output(cor(misr.aqspm10.met.06.11$AOD,misr.aqspm10.met.06.11$PM10), file = "SummaryStatsPM10_2006_2011.txt", append = TRUE)
cat("Cor AODlarge PM10\n", file = "SummaryStatsPM10_2006_2011.txt", append = TRUE)
capture.output(cor(misr.aqspm10.met.06.11$AODlarge,misr.aqspm10.met.06.11$PM10), file = "SummaryStatsPM10_2006_2011.txt", append = TRUE)
cat("Cor AODsmall PM10\n", file = "SummaryStatsPM10_2006_2011.txt", append = TRUE)
capture.output(cor(misr.aqspm10.met.06.11$AODsmall,misr.aqspm10.met.06.11$PM10), file = "SummaryStatsPM10_2006_2011.txt", append = TRUE)
cat("Cor AOD PM10 met\n", file = "SummaryStatsPM10_2006_2011.txt", append = TRUE)
capture.output(cor(misr.aqspm10.met.06.11$AOD,misr.aqspm10.met.06.11$PM10), file = "SummaryStatsPM10_2006_2011.txt", append = TRUE)
cat("Cor AODlarge PM10 met\n", file = "SummaryStatsPM10_2006_2011.txt", append = TRUE)
capture.output(cor(misr.aqspm10.met.06.11$AODlarge,misr.aqspm10.met.06.11$PM10), file = "SummaryStatsPM10_2006_2011.txt", append = TRUE)
cat("Cor AODsmall PM10 met\n", file = "SummaryStatsPM10_2006_2011.txt", append = TRUE)
capture.output(cor(misr.aqspm10.met.06.11$AODsmall,misr.aqspm10.met.06.11$PM10), file = "SummaryStatsPM10_2006_2011.txt", append = TRUE)


# Linear Models PM10
cat("LM AOD PM10 met\n", file = "SummaryStatsPM10_2006_2011.txt", append = TRUE)
capture.output(summary(lm(PM10~AOD, data=misr.aqspm10.met.06.11)), file = "SummaryStatsPM10_2006_2011.txt", append = TRUE)
cat("LM AODsmall PM10 met\n", file = "SummaryStatsPM10_2006_2011.txt", append = TRUE)
capture.output(summary(lm(PM10~AODsmall, data=misr.aqspm10.met.06.11)), file = "SummaryStatsPM10_2006_2011.txt", append = TRUE)
cat("LM AODsmall_med PM10 met\n", file = "SummaryStatsPM10_2006_2011.txt", append = TRUE)
capture.output(summary(lm(PM10~AODsm_med, data=misr.aqspm10.met.06.11)), file = "SummaryStatsPM10_2006_2011.txt", append = TRUE)
cat("LM AODlarge PM10 met\n", file = "SummaryStatsPM10_2006_2011.txt", append = TRUE)
capture.output(summary(lm(PM10~AODlarge, data=misr.aqspm10.met.06.11)), file = "SummaryStatsPM10_2006_2011.txt", append = TRUE)

# GAM models PM10
cat("LM AOD PM10 \n", file = "SummaryStatsPM10_2006_2011.txt", append = TRUE)
capture.output(summary(gam(PM10~s(AOD), data=misr.aqspm10.met.06.11)), file = "SummaryStatsPM10_2006_2011.txt", append = TRUE)
cat("LM AODsmall PM10 \n", file = "SummaryStatsPM10_2006_2011.txt", append = TRUE)
capture.output(summary(gam(PM10~s(AODsmall), data=misr.aqspm10.met.06.11)), file = "SummaryStatsPM10_2006_2011.txt", append = TRUE)
cat("LM AODsmall_med PM10 \n", file = "SummaryStatsPM10_2006_2011.txt", append = TRUE)
capture.output(summary(gam(PM10~s(AODsm_med), data=misr.aqspm10.met.06.11)), file = "SummaryStatsPM10_2006_2011.txt", append = TRUE)
cat("LM AODlarge PM10 \n", file = "SummaryStatsPM10_2006_2011.txt", append = TRUE)
capture.output(summary(gam(PM10~s(AODlarge), data=misr.aqspm10.met.06.11)), file = "SummaryStatsPM10_2006_2011.txt", append = TRUE)

# S-T GAM models PM10
misr.aqspm10.met.06.11.POC<-misr.aqspm10.met.06.11[misr.aqspm10.met.06.11$POC %in% c(1,5),]
cat("LM AOD PM10 met\n", file = "SummaryStatsPM10_2006_2011.txt", append = TRUE)
capture.output(summary(gam(PM10~s(AOD)+te(x.1,y.1,julian2,k=c(25,8),d=c(2,1),bs=c('tp','cr'))+te(julian2,year, month,  k=c(12,4,7),d=c(1,1,1),bs=c("cr","cr","cc"))+as.factor(dow), data=misr.aqspm10.met.06.11)), file = "SummaryStatsPM10_2006_2011.txt", append = TRUE)
cat("LM AODsmall PM10 met\n", file = "SummaryStatsPM10_2006_2011.txt", append = TRUE)
capture.output(summary(gam(PM10~s(AODsmall)+te(x.1,y.1,julian2,k=c(25,8),d=c(2,1),bs=c('tp','cr'))+te(julian2,year, month,  k=c(12,4,7),d=c(1,1,1),bs=c("cr","cr","cc"))+as.factor(dow), data=misr.aqspm10.met.06.11)), file = "SummaryStatsPM10_2006_2011.txt", append = TRUE)
cat("LM AODsmall_med PM10 met\n", file = "SummaryStatsPM10_2006_2011.txt", append = TRUE)
capture.output(summary(gam(PM10~s(AODsm_med)+te(x.1,y.1,julian2,k=c(25,8),d=c(2,1),bs=c('tp','cr'))+te(julian2,year, month,  k=c(12,4,7),d=c(1,1,1),bs=c("cr","cr","cc"))+as.factor(dow), data=misr.aqspm10.met.06.11)), file = "SummaryStatsPM10_2006_2011.txt", append = TRUE)
cat("LM AODlarge PM10 met\n", file = "SummaryStatsPM10_2006_2011.txt", append = TRUE)
capture.output(summary(gam(PM10~s(AODlarge)+te(x.1,y.1,julian2,k=c(25,8),d=c(2,1),bs=c('tp','cr'))+te(julian2,year, month,  k=c(12,4,7),d=c(1,1,1),bs=c("cr","cr","cc"))+as.factor(dow), data=misr.aqspm10.met.06.11)), file = "SummaryStatsPM10_2006_2011.txt", append = TRUE)

# S-T GAM models with met PM10
cat("LM AOD PM10 met\n", file = "SummaryStatsPM10_2006_2011.txt", append = TRUE)
capture.output(summary(gam(PM10~s(AOD)+te(x.1,y.1,julian2,k=c(25,8),d=c(2,1),bs=c('tp','cr'))+te(julian2,year, month,  k=c(12,4,7),d=c(1,1,1),bs=c("cr","cr","cc"))+as.factor(dow)+dew.point+wind.sp, data=misr.aqspm10.met.06.11)), file = "SummaryStatsPM10_2006_2011.txt", append = TRUE)
cat("LM AODsmall PM10 met\n", file = "SummaryStatsPM10_2006_2011.txt", append = TRUE)
#capture.output(summary(gam(PM10~s(AODsmall)+te(x.1,y.1,julian2,k=c(25,8),d=c(2,1),bs=c('tp','cr'))+te(julian2,year, month,  k=c(12,4,7),d=c(1,1,1),bs=c("cr","cr","cc"))+as.factor(dow)+dew.point+wind.sp, data=misr.aqspm10.met.06.11)), file = "SummaryStatsPM10_2006_2011.txt", append = TRUE)
#cat("LM AODsmall_med PM10 met\n", file = "SummaryStatsPM10_2006_2011.txt", append = TRUE)
capture.output(summary(gam(PM10~s(AODsm_med)+te(x.1,y.1,julian2,k=c(25,8),d=c(2,1),bs=c('tp','cr'))+te(julian2,year, month,  k=c(12,4,7),d=c(1,1,1),bs=c("cr","cr","cc"))+as.factor(dow)+dew.point+wind.sp, data=misr.aqspm10.met.06.11)), file = "SummaryStatsPM10_2006_2011.txt", append = TRUE)
cat("LM AODlarge PM10 met\n", file = "SummaryStatsPM10_2006_2011.txt", append = TRUE)
capture.output(summary(gam(PM10~s(AODlarge)+te(x.1,y.1,julian2,k=c(25,8),d=c(2,1),bs=c('tp','cr'))+te(julian2,year, month,  k=c(12,4,7),d=c(1,1,1),bs=c("cr","cr","cc"))+as.factor(dow)+dew.point+wind.sp, data=misr.aqspm10.met.06.11)), file = "SummaryStatsPM10_2006_2011.txt", append = TRUE)
# END MODELS PM10

# Match STN days
misr.aqspm10.match.stn<-misr.aqspm10.met.06.11[misr.aqspm10.met.06.11$day %in% misr.stn.met.06.11$day &
                                                 misr.aqspm10.met.06.11$month %in% misr.stn.met.06.11$month &
                                                 misr.aqspm10.met.06.11$year %in% misr.stn.met.06.11$year,] 
misr.aqspm10.match.stn.points<-unique(misr.aqspm10.match.stn[,38:39])

median(misr.aqspm10.match.stn$AOD)
IQR(misr.aqspm10.match.stn$AOD)
median(misr.aqspm10.match.stn$PM10)
IQR(misr.aqspm10.match.stn$PM10)
summary(lm(PM10~AOD, data=misr.aqspm10.match.stn))
summary(lm(PM10~AODsmall, data=misr.aqspm10.match.stn))
summary(lm(PM10~AODsm_med, data=misr.aqspm10.match.stn))
summary(lm(PM10~AODlarge, data=misr.aqspm10.match.stn))

# Cross Validation PARALLEL
misr.aqspm10.points<-unique(misr.aqspm10.met.06.11[,38:39])

gam.pred.pm10.list<-vector('list',dim(misr.aqspm10.points)[1])
gam.rsq.pm10.list<-vector('list')
gam.resid.pm10.list<-vector('list',dim(misr.aqspm10.points)[1])

gam.pred.pm10.list2<-vector('list',dim(misr.aqspm10.points)[1])
gam.rsq.pm10.list2<-vector('list')
gam.resid.pm10.list2<-vector('list',dim(misr.aqspm10.points)[1])


gam.pred.pm10.list3<-vector('list',dim(misr.aqspm10.points)[1])
gam.rsq.pm10.list3<-vector('list')
gam.resid.pm10.list3<-vector('list',dim(misr.aqspm10.points)[1])

gam.pred.pm10.list4<-vector('list',dim(misr.aqspm10.points)[1])
gam.rsq.pm10.list4<-vector('list')
gam.resid.pm10.list4<-vector('list',dim(misr.aqspm10.points)[1])
#:dim(misr.aqspm10.points)[1]


for (i in 1:dim(misr.aqspm10.points)[1]){
  location.sample1<-misr.aqspm10.points[i,]
  
  # Training data
  train0<-misr.aqspm10.met.06.11[!(misr.aqspm10.met.06.11$Latitude %in% location.sample1$Latitude
                                   & misr.aqspm10.met.06.11$Longitude %in% location.sample1$Longitude), ]
  # Test data: leave site out
  test0<-misr.aqspm10.met.06.11[(misr.aqspm10.met.06.11$Latitude %in% location.sample1$Latitude
                                 & misr.aqspm10.met.06.11$Longitude %in% location.sample1$Longitude), ]
  n<-dim(test0)[1]
  if (n>9){
  #gam.st.pm10<-gam(PM10~s(AOD)+te(x.1,y.1,julian2,k=c(25,8),d=c(2,1),bs=c('tp','cr'))+te(julian2,year,month, k=c(12,4,7),d=c(1,1,1),bs=c("cr","cr","cc"))+as.factor(dow), na.action=na.exclude,data=train0)
  
  #gam.st2.pm10<-gam(PM10~s(AODlarge)+te(x.1,y.1,julian2,k=c(25,8),d=c(2,1),bs=c('tp','cr'))+te(julian2,year, month,  k=c(12,4,7),d=c(1,1,1),bs=c("cr","cr","cc"))+as.factor(dow), na.action=na.exclude,data=train0)
  
  gam.st3.pm10<-gam(PM10~s(AOD)+te(x.1,y.1,julian2,k=c(22,8),d=c(2,1),bs=c('tp','cr'))+te(julian2,year, month,  k=c(12,4,7),d=c(1,1,1),bs=c("cr","cr","cc"))+as.factor(dow)+dew.point+wind.sp, na.action=na.exclude,data=train0)
  
  #gam.st4.pm10<-gam(PM10~s(AODlarge)+te(x.1,y.1,julian2,k=c(22,8),d=c(2,1),bs=c('tp','cr'))+te(julian2,year, month,  k=c(12,4,7),d=c(1,1,1),bs=c("cr","cr","cc"))+as.factor(dow)+dew.point+wind.sp, na.action=na.exclude,data=train0)
  
  #gam.st.pred.pm10<-predict.gam(gam.st.pm10,newdata=test0)
  #gam.st.pred.pm10.merge<-cbind(gam.st.pred.pm10,test0,rep(n,length(gam.st.pred.pm10)))
  #gam.st.pred.pm10.merge<-gam.st.pred.pm10.merge[gam.st.pred.pm10.merge$gam.st.pred.pm10<200,]
  #gam.st.pred.pm10.merge<-gam.st.pred.pm10.merge[gam.st.pred.pm10.merge$gam.st.pred.pm10>=0,]
  #rsq<-(summary(lm(gam.st.pred.pm10.merge$PM10~gam.st.pred.pm10.merge$gam.st.pred.pm10))$r.squared)
  #rsq<-data.frame(rsq,location.sample1,n)
  #gam.st.resid.pm10<-data.frame(resid=gam.st.pm10$residuals, y=gam.st.pm10$y, fitted=gam.st.pm10$fitted.values)
  
  
  #gam.st2.pred.pm10<-predict.gam(gam.st2.pm10,newdata=test0)
  #gam.st2.pred.pm10.merge<-cbind(gam.st2.pred.pm10,test0)
  #gam.st2.pred.pm10.merge<-gam.st2.pred.pm10.merge[gam.st2.pred.pm10.merge$gam.st2.pred.pm10<200,]
  #gam.st2.pred.pm10.merge<-gam.st2.pred.pm10.merge[gam.st2.pred.pm10.merge$gam.st2.pred.pm10>=0,]
  #rsq2<-(summary(lm(gam.st2.pred.pm10.merge$PM10~gam.st2.pred.pm10.merge$gam.st2.pred.pm10))$r.squared)
  #rsq2<-data.frame(rsq2,location.sample1,n)
  #gam.st2.resid.pm10<-data.frame(resid=gam.st2.pm10$residuals, y=gam.st2.pm10$y,fitted=gam.st2.pm10$fitted.values)
  
  
  gam.st3.pred.pm10<-predict.gam(gam.st3.pm10,newdata=test0)
  gam.st3.pred.pm10.merge<-cbind(gam.st3.pred.pm10,test0)
  gam.st3.pred.pm10.merge<-gam.st3.pred.pm10.merge[gam.st3.pred.pm10.merge$gam.st3.pred.pm10<200,]
  gam.st3.pred.pm10.merge<-gam.st3.pred.pm10.merge[gam.st3.pred.pm10.merge$gam.st3.pred.pm10>=0,]
  rsq3<-(summary(lm(gam.st3.pred.pm10.merge$PM10~gam.st3.pred.pm10.merge$gam.st3.pred.pm10))$r.squared)
  rsq3<-data.frame(rsq3,location.sample1,n)
  gam.st3.resid.pm10<-data.frame(resid=gam.st3.pm10$residuals, y=gam.st3.pm10$y,fitted=gam.st3.pm10$fitted.values)
  
  
  #gam.st4.pred.pm10<-predict.gam(gam.st4.pm10,newdata=test0)
  #gam.st4.pred.pm10.merge<-cbind(gam.st4.pred.pm10,test0,rep(n,length(gam.st4.pred.pm10)))
  #gam.st4.pred.pm10.merge<-gam.st4.pred.pm10.merge[gam.st4.pred.pm10.merge$gam.st4.pred.pm10<200,]
  #gam.st4.pred.pm10.merge<-gam.st4.pred.pm10.merge[gam.st4.pred.pm10.merge$gam.st4.pred.pm10>=0,]
  #gam.st4.pred.pm10.merge<-gam.st4.pred.pm10.merge[gam.st4.pred.pm10.merge$n>9,]
  #rsq4<-(summary(lm(gam.st4.pred.pm10.merge$PM10~gam.st4.pred.pm10.merge$gam.st4.pred.pm10))$r.squared)
  #rsq4<-data.frame(rsq4,location.sample1,n)
  
  
  #gam.pred.pm10.list[[i]]<-gam.st.pred.pm10.merge
  #gam.rsq.pm10.list[[i]]<-rsq
  
  #gam.pred.pm10.list2[[i]]<-gam.st2.pred.pm10.merge
  #gam.rsq.pm10.list2[[i]]<-rsq2
  
  #gam.pred.pm10.list3[[i]]<-gam.st3.pred.pm10.merge
  #gam.rsq.pm10.list3[[i]]<-rsq3
  
  gam.pred.pm10.list4[[i]]<-gam.st4.pred.pm10.merge
  gam.rsq.pm10.list4[[i]]<-rsq4
  }
}

gam.pred.pm10 <- do.call("rbind", gam.pred.pm10.list) # AOD no met
gam.pm10.rsq<-do.call("rbind", gam.rsq.pm10.list)
write.csv(gam.pred.pm10,"gam.pm10.pred.mod1.csv",row.names=FALSE)
write.csv(gam.pm10.rsq,"gam.pm10.cv.rsq.mod1.csv",row.names=FALSE)

gam.pred2.pm10 <- do.call("rbind", gam.pred.pm10.list2) # AOD large no met
gam.pm10.rsq2<-do.call("rbind", gam.rsq.pm10.list2)
write.csv(gam.pred2.pm10,"gam.pm10.pred.mod2.csv",row.names=FALSE)
write.csv(gam.pm10.rsq2,"gam.pm10.cv.rsq.mod2.csv",row.names=FALSE)

gam.pred3.pm10 <- do.call("rbind", gam.pred.pm10.list3) # AOD with met
gam.pm10.rsq3<-do.call("rbind", gam.rsq.pm10.list3)
write.csv(gam.pred3.pm10,"gam.pm10.pred.mod3.csv",row.names=FALSE)
write.csv(gam.pm10.rsq3,"gam.pm10.cv.rsq.mod3.csv",row.names=FALSE)

gam.pred4.pm10 <- do.call("rbind", gam.pred.pm10.list4) # AOD large with met
gam.pm10.rsq4<-do.call("rbind", gam.rsq.pm10.list4)
gam.pm10.rsq4<-gam.pm10.rsq4[gam.pm10.rsq4$n>9,]
write.csv(gam.pred4.pm10,"gam.pm10.pred.mod4.csv",row.names=FALSE)
write.csv(gam.pm10.rsq4,"gam.pm10.cv.rsq.mod4.csv",row.names=FALSE)

plot(gam.pred4.pm10$PM10,gam.pred4.pm10$gam.st4.pred.pm10,xlim=c(0,100),ylim=c(0,100))
summary(lm(gam.pred.pm10$PM10~gam.pred.pm10$gam.st.pred.pm10))

#  Plots

# PM10 - AOD large scatterplot
pdf('MISR.AOD.PM10_new.pdf')
p<-qplot(AODlarge,PM10, data=misr.aqspm10.met.06.11,xlab="MISR AOD Large",ylab=expression("AQS PM"[10]*", ug/m"^3))
p+stat_smooth(method="gam",formula=y~s(x,k=3)) +stat_smooth(method='lm',formula=y~x,col='red')
dev.off()

# cross validation plot
# PM10
gam.pred4.pm10b<-gam.pred4.pm10[gam.pred4.pm10$gam.st4.pred.pm10>0,]
gam.pred4.pm10b<-gam.pred4.pm10b[gam.pred4.pm10$gam.st4.pred.pm10<200,]
gam.pred4.pm10b<-gam.pred4.pm10b[gam.pred4.pm10$PM10<200,]
pdf("CV_PM10_06_11.pdf")
p11<-qplot(gam.st4.pred.pm10,PM10,data=gam.pred4.pm10,xlab=expression('Predicted PM'[10]*', ug/m'^3),ylab=expression('Observed PM'[10]*', ug/m'^3))
plot11<-p11+stat_smooth(method='lm',formula=y~x-1,col='red')+stat_smooth(method='lm',formula=y~x,col='blue')
plot11
dev.off()

pdf('MISR.PM25.PM10.CV_new.pdf')
grid.arrange(plot6,plot11,nrow=2,ncol=1)
dev.off()

# Map of CV R2
map <- get_map(c(lon=-118.3, lat=34.1), zoom = 7 , maptype = 'hybrid')
ggmap(map,extent="panel",legend="bottomleft") + geom_point(data = gam.pm10.rsq4, aes(x = Longitude, y = Latitude, color=rsq4),  size=1.1, alpha=0.5)+ 
  scale_color_gradientn(colours=matlab.like(20), limits=c(10,95),breaks=seq(10,100,by=10))
rsq.pts<-as.geodata(gam.pm10.rsq4,coords.col = 3:2, data.col = 1)
points(rsq.pts)
map('state',"California",add=TRUE)

# Prediction Map 
# PM10 model
gam.st.pm10<-gam(PM10~s(AODlarge)+te(x.1,y.1,julian2,k=c(25,8),d=c(2,1),bs=c('tp','cr'))+te(julian2, year,month, k=c(12,4,7),d=c(1,1,1),bs=c("cr","cr","cc"))+as.factor(dow), data=misr.aqspm10.met.06.11)

# Convert all AOD to PM10
misr.days<-misr_06_11 %>% distinct(date2,.keep_all=TRUE) 
pm10.predicted<-vector('list',length(misr.days$date))
for (i in 1:length(misr.days$date)){
  misr.daily<-misr_06_11[misr_06_11$day %in% misr.days[i,]$day &
                           misr_06_11$month %in% misr.days[i,]$month &
                           misr_06_11$year %in% misr.days[i,]$year ,] 
  misr.daily$julian2<-misr.daily$julian/10000
  misr.daily$x.1<-misr.daily$x
  misr.daily$y.1<-misr.daily$y
  misr.daily$dow<-(weekdays(as.Date(misr.daily$date,"%m/%d/%y")))
  predicted.pm10<-predict.gam(gam.st.pm10, newdata=misr.daily)
  misr.daily$predicted.pm10<-predicted.pm10
  pm10.predicted[[i]]<-misr.daily
}
pm10.predicted.all <- do.call("rbind", pm10.predicted)
pm10.predicted.all<-pm10.predicted.all[pm10.predicted.all$AOD<1,]
pm10.predicted.all<-pm10.predicted.all[pm10.predicted.all$predicted.pm10>0,]
write.csv(pm10.predicted.all, "/Users/mf/Documents/MISR/Data/predicted_pm10_2006_2011.csv")

# Take seasonal averages
pm10.pred.summer<-pm10.predicted.all[pm10.predicted.all$month %in% c(5,6,7,8,9,10),]
pm10.pred.summer.avg<-ddply(pm10.pred.summer, .(lat,lon), summarise, AODlarge=mean(AODlarge,na.rm=TRUE),PM10=mean(predicted.pm10,na.rm=TRUE))
write.csv(pm10.pred.summer.avg, "/Users/mf/Documents/MISR/Data/predicted_pm10_warm_2006_2011.csv")
pm10.pred.winter<-pm10.predicted.all[pm10.predicted.all$month %in% c(11,12,1,2,3,4),]
pm10.pred.winter.avg<-ddply(pm10.pred.winter, .(lat,lon), summarise, AODlarge=mean(AODlarge,na.rm=TRUE),PM10=mean(predicted.pm10,na.rm=TRUE))
write.csv(pm10.pred.winter.avg, "/Users/mf/Documents/MISR/Data/predicted_pm10_cool_2006_2011.csv")


map <- get_map(c(lon=-118.3, lat=34.1), zoom = 7 , maptype = 'hybrid')
pdf("winter_pm10_06_11.pdf")
ggmap(map,extent="panel",legend="bottomleft") + geom_point(data = pm10.pred.winter.avg, aes(x = lon, y = lat, color=PM10),  size=1.1, alpha=0.5)+ 
  scale_color_gradientn(colours=matlab.like(20), limits=c(10,95),breaks=seq(10,100,by=10))
dev.off()
pdf("summer_pm10_06_11.pdf")
ggmap(map,extent="panel",legend="bottomleft") + geom_point(data = pm10.pred.summer.avg, aes(x = lon, y = lat, color=PM10),  size=1.1, alpha=0.5)+ 
  scale_color_gradientn(colours=matlab.like(20), limits=c(10,95),breaks=seq(10,100,by=10))
dev.off()

# Observed PM10 summer averages
pm10.obs.summer<-misr.aqspm10.met.06.11[misr.aqspm10.met.06.11$month %in% c(5,6,7,8,9,10),]
pm10.obs.summer.avg<-ddply(pm10.obs.summer, .(Latitude,Longitude), summarise,PM10=mean(PM10,na.rm=TRUE),site=unique(CBSA_NAME))
# Observed PM10 winter averages
pm10.obs.winter<-misr.aqspm10.met.06.11[misr.aqspm10.met.06.11$month %in% c(11,12,1,2,3,4),]
pm10.obs.winter.avg<-ddply(pm10.obs.winter, .(Latitude,Longitude), summarise,PM10=mean(PM10,na.rm=TRUE),site=unique(CBSA_NAME))


