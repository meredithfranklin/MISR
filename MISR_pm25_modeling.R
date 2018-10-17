##############################################
# 2000-2011 4.4km MISR-PM data analysis
# PM2.5
# Meredith Franklin
##############################################
library(mgcv)
library(ggplot2)
library(ggmap)
library(gridExtra)
library(dplyr)

# 2000-2011 Data (revision) two datasets: with and without meteorology
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


# Subset to 2006+
# Remove STN and BAMS sites
misr.aqspm25.met.06.11<-misr.aqspm25.met.ss[misr.aqspm25.met.ss$year>=2006,]
misr.aqspm25.met.06.11<-misr.aqspm25.met.06.11[misr.aqspm25.met.06.11$POC %in% c(1,2,4,6,11,12),]
misr.aqspm25.points<-unique(misr.aqspm25.met.06.11[,38:39])

# read/write analytic dataset
# write.csv(misr.aqspm25.met.06.11,"/Users/mf/Documents/MISR/Data/Match 2000-2011/misr_pm25_met_2006_2011.csv",row.names = FALSE)
misr.aqspm25.met.06.11 <- read.csv("/Users/mf/Documents/MISR/Data/Match 2000-2011/misr_pm25_met_2006_2011.csv")


#### MODELS PRESENTED IN RSE 2017 ####
# Linear, smooth univariate models of AOD sizes and PM2.5
# Smooth spatio-temporal models of AOD sizes and PM2.5
# Smooth spatio-temporal models of AOD sizes and PM2.5 with linear meteorology (RH, windsp)

# save output in txt file
setwd("/Users/mf/Documents/MISR/Papers and Reports/Results")
outputfile1<-"SummaryStatsPM25_2006_2011.txt" # for summary stats
outputfile2<-"LinearModPM25_2006_2011.txt" # for linear model output
outputfile3<-"GAMModPM25_2006_2011.txt" # for univariate gam model output
outputfile4<-"GAMstModPM25_2006_2011.txt" # for st gam model output
outputfile5<-"GAMstmetModPM25_2006_2011.txt" # for st gam model with met output

# Title (writes new file)
cat("PM25 Summary Stats", file = outputfile1)
# add new lines
cat("\n", file = outputfile1, append=TRUE)
# export summary statistics output
cat("Dim PM2.5", file = outputfile1, append = TRUE)
capture.output(length(misr.aqspm25.met.06.11$PM25), file = outputfile1, append = TRUE)
cat("Median AOD\n", file = outputfile1, append = TRUE)
capture.output(median(misr.aqspm25.met.06.11$AOD), file = outputfile1, append = TRUE)
cat("IQR AOD\n", file = outputfile1, append = TRUE)
capture.output(IQR(misr.aqspm25.met.06.11$AOD), file = outputfile1, append = TRUE)
cat("Median PM25\n", file = outputfile1, append = TRUE)
capture.output(median(misr.aqspm25.met.06.11$PM25), file = outputfile1, append = TRUE)
cat("IQR PM25\n", file = outputfile1, append = TRUE)
capture.output(IQR(misr.aqspm25.met.06.11$PM25), file = outputfile1, append = TRUE)
cat("Mean PM25\n", file = outputfile1, append = TRUE)
capture.output(mean(misr.aqspm25.met.06.11$PM25), file = outputfile1, append = TRUE)
cat("St Dev PM25\n", file = outputfile1, append = TRUE)
capture.output(sd(misr.aqspm25.met.06.11$PM25), file = outputfile1, append = TRUE)
cat("Cor AOD PM25\n", file = outputfile1, append = TRUE)
capture.output(cor(misr.aqspm25.met.06.11$AOD,misr.aqspm25.met.06.11$PM25), file = outputfile1, append = TRUE)
cat("Cor AODlarge PM25\n", file = outputfile1, append = TRUE)
capture.output(cor(misr.aqspm25.met.06.11$AODlarge,misr.aqspm25.met.06.11$PM25), file = outputfile1, append = TRUE)
cat("Cor AODsmall PM25\n", file = outputfile1, append = TRUE)
capture.output(cor(misr.aqspm25.met.06.11$AODsmall,misr.aqspm25.met.06.11$PM25), file = outputfile1, append = TRUE)
cat("Cor AOD PM25 met\n", file = outputfile1, append = TRUE)
capture.output(cor(misr.aqspm25.met.06.11$AOD,misr.aqspm25.met.06.11$PM25), file = outputfile1, append = TRUE)
cat("Cor AODlarge PM25 met\n", file = outputfile1, append = TRUE)
capture.output(cor(misr.aqspm25.met.06.11$AODlarge,misr.aqspm25.met.06.11$PM25), file = outputfile1, append = TRUE)
cat("Cor AODsmall PM25 met\n", file = outputfile1, append = TRUE)
capture.output(cor(misr.aqspm25.met.06.11$AODsmall,misr.aqspm25.met.06.11$PM25), file = outputfile1, append = TRUE)


# Linear Models PM25
cat("PM25 Linear Models", file = outputfile2)
# add new lines
cat("\n", file = outputfile2, append=TRUE)
cat("LM AOD PM25 met\n", file = outputfile2, append = TRUE)
capture.output(summary(lm(PM25~AOD, data=misr.aqspm25.met.06.11)), file = outputfile2, append = TRUE)
cat("LM AODsmall PM25 met\n", file = outputfile2, append = TRUE)
capture.output(summary(lm(PM25~AODsmall, data=misr.aqspm25.met.06.11)), file = outputfile2, append = TRUE)
cat("LM AODsmall_med PM25 met\n", file = outputfile2, append = TRUE)
capture.output(summary(lm(PM25~AODsm_med, data=misr.aqspm25.met.06.11)), file = outputfile2, append = TRUE)
cat("LM AODlarge PM25 met\n", file = outputfile2, append = TRUE)
capture.output(summary(lm(PM25~AODlarge, data=misr.aqspm25.met.06.11)), file = outputfile2, append = TRUE)


# Univariate GAM models PM25
cat("PM25 Univariate GAM", file = outputfile3)
# add new lines
cat("\n", file = outputfile3, append=TRUE)

cat("LM AOD PM25 \n", file = outputfile3, append = TRUE)
capture.output(summary(gam(PM25~s(AOD), data=misr.aqspm25.met.06.11)), file = outputfile3, append = TRUE)
cat("LM AODsmall PM25 \n", file = outputfile3, append = TRUE)
capture.output(summary(gam(PM25~s(AODsmall), data=misr.aqspm25.met.06.11)), file = outputfile3, append = TRUE)
cat("LM AODsmall_med PM25 \n", file = outputfile3, append = TRUE)
capture.output(summary(gam(PM25~s(AODsm_med), data=misr.aqspm25.met.06.11)), file = outputfile3, append = TRUE)
cat("LM AODlarge PM25 \n", file = outputfile3, append = TRUE)
capture.output(summary(gam(PM25~s(AODlarge), data=misr.aqspm25.met.06.11)), file = outputfile3, append = TRUE)

# S-T GAM models PM25
cat("PM25 S-T GAM", file = outputfile4)
# add new lines
cat("\n", file = outputfile4, append=TRUE)

cat("LM AOD PM25 met\n", file = outputfile4, append = TRUE)
capture.output(summary(gam(PM25~s(AOD)+te(x.1,y.1,julian2,k=c(25,8),d=c(2,1),bs=c('tp','cr'))+te(julian2,year, month,  k=c(12,4,7),d=c(1,1,1),bs=c("cr","cr","cc"))+as.factor(dow), data=misr.aqspm25.met.06.11)), file = outputfile4, append = TRUE)
cat("LM AODsmall PM25 met\n", file = outputfile4, append = TRUE)
capture.output(summary(gam(PM25~s(AODsmall)+te(x.1,y.1,julian2,k=c(25,8),d=c(2,1),bs=c('tp','cr'))+te(julian2,year, month,  k=c(12,4,7),d=c(1,1,1),bs=c("cr","cr","cc"))+as.factor(dow), data=misr.aqspm25.met.06.11)), file = outputfile4, append = TRUE)
cat("LM AODsmall_med PM25 met\n", file = outputfile4, append = TRUE)
capture.output(summary(gam(PM25~s(AODsm_med)+te(x.1,y.1,julian2,k=c(25,8),d=c(2,1),bs=c('tp','cr'))+te(julian2,year, month,  k=c(12,4,7),d=c(1,1,1),bs=c("cr","cr","cc"))+as.factor(dow), data=misr.aqspm25.met.06.11)), file = outputfile4, append = TRUE)
cat("LM AODlarge PM25 met\n", file = outputfile4, append = TRUE)
capture.output(summary(gam(PM25~s(AODlarge)+te(x.1,y.1,julian2,k=c(25,8),d=c(2,1),bs=c('tp','cr'))+te(julian2,year, month,  k=c(12,4,7),d=c(1,1,1),bs=c("cr","cr","cc"))+as.factor(dow), data=misr.aqspm25.met.06.11)), file = outputfile4, append = TRUE)

# S-T GAM models with met PM25
cat("PM25 S-T GAM", file = outputfile5)
# add new lines
cat("\n", file = outputfile5, append=TRUE)

cat("LM AOD PM25 met\n", file = outputfile5, append = TRUE)
capture.output(summary(gam(PM25~s(AOD)+te(x.1,y.1,julian2,k=c(25,8),d=c(2,1),bs=c('tp','cr'))+te(julian2, year,month, k=c(12,4,7),d=c(1,1,1),bs=c("cr","cr","cc"))+as.factor(dow)+rh+wind.sp, data=misr.aqspm25.met.06.11)), file = outputfile5, append = TRUE)
cat("LM AODsmall PM25 met\n", file = outputfile5, append = TRUE)
capture.output(summary(gam(PM25~s(AODsmall)+te(x.1,y.1,julian2,k=c(25,8),d=c(2,1),bs=c('tp','cr'))+te(julian2, year,month, k=c(12,4,7),d=c(1,1,1),bs=c("cr","cr","cc"))+as.factor(dow)+rh+wind.sp, data=misr.aqspm25.met.06.11)), file = outputfile5, append = TRUE)
cat("LM AODsmall_med PM25 met\n", file = outputfile5, append = TRUE)
capture.output(summary(gam(PM25~s(AODsm_med)+te(x.1,y.1,julian2,k=c(25,8),d=c(2,1),bs=c('tp','cr'))+te(julian2, year,month, k=c(12,4,7),d=c(1,1,1),bs=c("cr","cr","cc"))+as.factor(dow)+rh+wind.sp, data=misr.aqspm25.met.06.11)), file = outputfile5, append = TRUE)
cat("LM AODlarge PM25 met\n", file = outputfile5, append = TRUE)
capture.output(summary(gam(PM25~s(AODlarge)+te(x.1,y.1,julian2,k=c(25,8),d=c(2,1),bs=c('tp','cr'))+te(julian2, year,month, k=c(12,4,7),d=c(1,1,1),bs=c("cr","cr","cc"))+as.factor(dow)+rh+wind.sp, data=misr.aqspm25.met.06.11)), file = outputfile5, append = TRUE)


##### Leave one site out Cross Validation ######
# VALIDATING BEST MODEL:
# gam(PM25~s(AODsm_med)+te(x.1,y.1,julian2,k=c(25,8),d=c(2,1),bs=c('tp','cr'))+te(julian2, year,month, k=c(12,4,7),d=c(1,1,1),bs=c("cr","cr","cc"))+as.factor(dow)+rh+wind.sp
# unique site locations
misr.aqspm25.points<-unique(misr.aqspm25.met.06.11[,38:39])

# lists for saving output
gam.pred.pm25.list<-vector('list',dim(misr.aqspm25.points)[1])
gam.rsq.pm25.list<-vector('list')
gam.resid.pm25.list<-vector('list',dim(misr.aqspm25.points)[1])

#break at i=25
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
        gam.st.pm25 <- gam(PM25~s(AODsm_med)+te(x.1,y.1,julian2,k=c(25,8),d=c(2,1),bs=c('tp','cr'))+te(julian2, year,month, k=c(12,4,7),d=c(1,1,1),bs=c("cr","cr","cc"))+as.factor(dow)+rh+wind.sp, na.action=na.exclude,data=train0)
     
    
        gam.st.pred.pm25<-predict.gam(gam.st.pm25,newdata=test0)
        gam.st.pred.pm25.merge<-cbind(gam.st.pred.pm25,test0,rep(n,length(gam.st.pred.pm25)))
        rsq<-(summary(lm(gam.st.pred.pm25.merge$PM25~gam.st.pred.pm25.merge$gam.st.pred.pm25))$r.squared)
        rsq<-data.frame(rsq,location.sample1,n)
        gam.st.resid.pm25<-data.frame(resid=gam.st.pm25$residuals, y=gam.st.pm25$y, fitted=gam.st.pm25$fitted.values)
    
  
        gam.pred.pm25.list[[i]]<-gam.st.pred.pm25.merge
        gam.resid.pm25.list[[i]]<-gam.st.resid.pm25
        gam.rsq.pm25.list[[i]]<-rsq
    
  }
}

gam.pred.pm25 <- do.call("rbind", gam.pred.pm25.list) 
gam.rsq<-do.call("rbind", gam.rsq.pm25.list)
write.csv(gam.pred.pm25,"gam.pm25.pred.bestmod.csv",row.names=FALSE)
write.csv(gam.rsq,"gam.pm25.cv.rsq.bestmod.csv",row.names=FALSE)
