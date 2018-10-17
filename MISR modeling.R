##############################################
# 2000-2011 4.4km MISR-PM data analysis
# Meredith Franklin
##############################################
library(mgcv)
library(ggplot2)
library(colorRamps)
library(ggmaps)
library(gridExtra)
library(dplyr)

setwd("/Users/mf/Documents/MISR/Reports")
# 2008-2009 Data (original analysis)
# Matched MISR-AQS datasets
misr.aqspm25<-read.csv("/Users/mf/Documents/MISR/Data/misr_aqspm25_new.csv")
misr.aqspm10<-read.csv("/Users/mf/Documents/MISR/Data/misr_aqspm10_new.csv")
misr.aqspm2510<-read.csv("/Users/mf/Documents/MISR/Data/misr_aqspm2510_new.csv")
misr.stn<-read.csv("/Users/mf/Documents/MISR/Data/misr_stn.csv")
misr.08.09<-read.csv("/Users/mf/Documents/MISR/Data/misr_08_09.csv")
misr.08.09<-misr.08.09[misr.08.09$land.water.mask==3,]
# Matched MISR-AQS-MET datasets
misr.aqspm25.met<-read.csv("/Users/mf/Documents/MISR/Data/misr_aqspm25_met_new.csv")

misr.aqspm10.met<-read.csv("/Users/mf/Documents/MISR/Data/misr_aqspm10_met_new.csv")

misr.aqspm2510.met<-read.csv("/Users/mf/Documents/MISR/Data/misr_aqspm2510_met_new.csv")

misr.stn.met<-read.csv("/Users/mf/Documents/MISR/Data/misr_aqsstn_met.csv")
misr.aqspm25.met$rh=100*((112-0.1*misr.aqspm25.met$temp+misr.aqspm25.met$dew.point)/(112+0.9*misr.aqspm25.met$temp))^8
misr.aqspm10.met$rh=100*((112-0.1*misr.aqspm10.met$temp+misr.aqspm10.met$dew.point)/(112+0.9*misr.aqspm10.met$temp))^8

# 2000-2011 Data (revision)
misr.aqspm25<-read.csv("/Users/mf/Documents/MISR/Data/Match 2000-2011/misr_pm25_2000_2011.csv")
misr.aqspm10<-read.csv("/Users/mf/Documents/MISR/Data/Match 2000-2011/misr_pm10_2000_2011.csv")
misr.aqspm2510<-read.csv("/Users/mf/Documents/MISR/Data/Match 2000-2011/misr_pm10_pm25_2000_2011.csv")
misr.stn<-read.csv("/Users/mf/Documents/MISR/Data/Match 2000-2011/misr_stn_2002_2011.csv")


misr.aqspm25.met<-read.csv("/Users/mf/Documents/MISR/Data/Match 2000-2011/misr_pm25_met_2000_2011.csv")
misr.aqspm10.met<-read.csv("/Users/mf/Documents/MISR/Data/Match 2000-2011/misr_pm10_met_2000_2011.csv")
misr.aqspm2510.met<-read.csv("/Users/mf/Documents/MISR/Data/Match 2000-2011/misr_pm10_pm25_met_2000_2011.csv")
misr.stn.met<-read.csv("/Users/mf/Documents/MISR/Data/Match 2000-2011/misr_stn_met_2002_2011.csv")
misr.aqspm25.met$rh=100*((112-0.1*misr.aqspm25.met$temp+misr.aqspm25.met$dew.point)/(112+0.9*misr.aqspm25.met$temp))^8
misr.aqspm10.met$rh=100*((112-0.1*misr.aqspm10.met$temp+misr.aqspm10.met$dew.point)/(112+0.9*misr.aqspm10.met$temp))^8
misr.stn.met$rh=100*((112-0.1*misr.stn.met$temp+misr.stn.met$dew.point)/(112+0.9*misr.stn.met$temp))^8


# Create new Julian date for time indexing, divide by 10000
misr.aqspm25$julian2<-misr.aqspm25$julian/10000
misr.aqspm10$julian2<-misr.aqspm10$julian/10000
misr.aqspm2510$julian2<-misr.aqspm2510$julian/10000
misr.stn$julian2<-misr.stn$julian/10000

misr.aqspm25.met$julian2<-misr.aqspm25.met$julian/10000
misr.aqspm10.met$julian2<-misr.aqspm10.met$julian/10000
misr.stn.met$julian2<-misr.stn.met$julian/10000
misr.aqspm2510.met$julian2<-misr.aqspm2510.met$julian/1000

# Create day of week variable for modeling
misr.aqspm25$dow<-(weekdays(as.Date(misr.aqspm25$date,"%m/%d/%y")))
misr.aqspm10$dow<-(weekdays(as.Date(misr.aqspm10$date2,"%Y-%m-%d")))
misr.aqspm2510$dow<-(weekdays(as.Date(misr.aqspm2510$date2,"%Y-%m-%d")))
misr.stn$dow<-(weekdays(as.Date(misr.stn$date,"%m/%d/%y")))

misr.aqspm25.met$dow<-(weekdays(as.Date(misr.aqspm25.met$date,"%m/%d/%y")))
misr.aqspm10.met$dow<-(weekdays(as.Date(misr.aqspm10.met$date2,"%Y-%m-%d")))
misr.aqspm2510.met$dow<-(weekdays(as.Date(misr.aqspm2510.met$date2,"%Y-%m-%d")))
misr.stn.met$dow<-(weekdays(as.Date(misr.stn.met$date,"%m/%d/%y")))

# Create AOD small+medium

misr.aqspm25$AODsm_med<-misr.aqspm25$AODsmall+misr.aqspm25$AODmed
misr.aqspm10$AODsm_med<-misr.aqspm10$AODsmall+misr.aqspm10$AODmed
misr.aqspm2510$AODsm_med<-misr.aqspm2510$AODsmall+misr.aqspm2510$AODmed
misr.stn$AODsm_med<-misr.stn$AODsmall+misr.stn$AODmed

misr.aqspm25.met$AODsm_med<-misr.aqspm25.met$AODsmall+misr.aqspm25.met$AODmed
misr.aqspm10.met$AODsm_med<-misr.aqspm10.met$AODsmall+misr.aqspm10.met$AODmed
misr.aqspm2510.met$AODsm_med<-misr.aqspm2510.met$AODsmall+misr.aqspm2510.met$AODmed
misr.stn.met$AODsm_med<-misr.stn.met$AODsmall+misr.stn.met$AODmed

# Remove AOD greater than 1 
misr.aqspm25.ss<-misr.aqspm25[misr.aqspm25$AOD<1,]
misr.aqspm10.ss<-misr.aqspm10[misr.aqspm10$AOD<1,]
misr.aqspm2510.ss<-misr.aqspm2510[misr.aqspm2510$AOD<1,]
misr.stn.ss<-misr.stn[misr.stn$AOD<1,]

misr.aqspm25.met.ss<-misr.aqspm25.met[misr.aqspm25.met$AOD<1,]
misr.aqspm10.met.ss<-misr.aqspm10.met[misr.aqspm10.met$AOD<1,]
misr.aqspm2510.met.ss<-misr.aqspm2510.met[misr.aqspm2510.met$AOD<1,]
misr.stn.met.ss<-misr.stn.met[misr.stn.met$AOD<1,]

write.csv(misr.aqspm25.met.ss,"/Users/mf/Documents/MISR/Data/misr_aqspm25_met_all.csv")
write.csv(misr.aqspm10.met.ss,"/Users/mf/Documents/MISR/Data/misr_aqspm10_met_all.csv")
write.csv(misr.aqspm2510.met.ss,"/Users/mf/Documents/MISR/Data/misr_aqspm2510_met_all.csv")

# Identify STN days and subset PM25 and PM10 matched AOD for these days
misr.aqspm25.match.stn<-misr.aqspm25.met.ss[misr.aqspm25.met.ss$day %in% misr.stn$day &
                                  misr.aqspm25.met.ss$month %in% misr.stn$month &
                                  misr.aqspm25.met.ss$year %in% misr.stn$year,] 

misr.aqspm10.match.stn<-misr.aqspm10.ss[misr.aqspm10.ss$day %in% misr.stn$day &
                                              misr.aqspm10.ss$month %in% misr.stn$month &
                                              misr.aqspm10.ss$year %in% misr.stn$year,]

misr.aqspm10.match.met.stn<-misr.aqspm10.met.ss[misr.aqspm10.met.ss$day %in% misr.stn$day &
                                              misr.aqspm10.met.ss$month %in% misr.stn$month &
                                              misr.aqspm10.met.ss$year %in% misr.stn$year,]

misr.aqspm2510.match.stn<-misr.aqspm2510.met.ss[misr.aqspm2510.met.ss$day %in% misr.stn$day &
                                              misr.aqspm2510.met.ss$month %in% misr.stn$month &
                                              misr.aqspm2510.met.ss$year %in% misr.stn$year,]

write.csv(misr.aqspm25.match.stn,"/Users/mf/Documents/MISR/Data/misr_aqspm25_stn_match.csv")
write.csv(misr.aqspm10.match.stn,"/Users/mf/Documents/MISR/Data/misr_aqspm10_stn_match.csv")
write.csv(misr.aqspm2510.match.stn,"/Users/mf/Documents/MISR/Data/misr_aqspm2510_stn_match.csv")

#### MISR AOD and AQS PM2.5 ####
# Summary statistics
cor.test(misr.aqspm25.ss$AOD,misr.aqspm25.ss$PM25)
cor.test(misr.aqspm25.ss$AODsmall,misr.aqspm25.ss$PM25)
cor.test(misr.aqspm25.ss$AODlarge,misr.aqspm25.ss$PM25)
cor(data.frame(misr.aqspm25.met.ss$rh,misr.aqspm25.met.ss$temp,misr.aqspm25.met.ss$wind.dir,misr.aqspm25.met.ss$wind.sp,
               misr.aqspm25.met.ss$atm.press, misr.aqspm25.met.ss$dew.point),use="complete")
#### MISR AOD and AQS PM10 ####
# Summary statistics
cor.test(misr.aqspm10.ss$AOD,misr.aqspm10.ss$PM10)
cor.test(misr.aqspm10.ss$AODsmall,misr.aqspm10.ss$PM10)
cor.test(misr.aqspm10.ss$AODlarge,misr.aqspm10.ss$PM10)

#### MISR AOD, AOD large and AQS PM10-PM2.5 ####
cor(misr.aqspm2510.ss$AODlarge,misr.aqspm2510.ss$pm10_pm25,use="complete")
cor(misr.aqspm2510.met.ss$AODsm_med,misr.aqspm2510.met.ss$pm10_pm25,use="complete")

# Distributions of PM and AOD
hist(misr.aqspm25.ss$PM25)
hist(misr.aqspm10.ss$PM10)
hist(misr.aqspm2510.ss$pm10_pm25)


#### PM2.5 Summary Statistics #####
misr.aqspm25.met.06.11<-misr.aqspm25.met.ss[misr.aqspm25.met.ss$year>=2006,]
misr.aqspm25.met.06.11<-misr.aqspm25.met.06.11[misr.aqspm25.met.06.11$POC %in% c(1,2,4,6,11,12),]
misr.aqspm25.points<-unique(misr.aqspm25.met.06.11.POC[,38:39])

misr.aqspm25.match.stn.06.11<-misr.aqspm25.match.stn[misr.aqspm25.match.stn$year>=2006,]

table(misr.aqspm25.met.06.11$date)
table(misr.aqspm25.met.06.11$POC)
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
misr.aqspm25.met.06.11POC5<-misr.aqspm25.met.06.11[misr.aqspm25.met.06.11$POC==5,]
cor.test(misr.aqspm25.met.06.11POC5$AOD,misr.aqspm25.met.06.11POC5$PM25)
# Linear Models PM25
cat("LM AOD PM25 met\n", file = "SummaryStatsPM25_2006_2011.txt", append = TRUE)
capture.output(summary(lm(PM25~AOD, data=misr.aqspm25.met.06.11)), file = "SummaryStatsPM25_2006_2011.txt", append = TRUE)
cat("LM AODsmall PM25 met\n", file = "SummaryStatsPM25_2006_2011.txt", append = TRUE)
capture.output(summary(lm(PM25~AODsmall, data=misr.aqspm25.met.06.11)), file = "SummaryStatsPM25_2006_2011.txt", append = TRUE)
cat("LM AODsmall_med PM25 met\n", file = "SummaryStatsPM25_2006_2011.txt", append = TRUE)
capture.output(summary(lm(PM25~AODsm_med, data=misr.aqspm25.met.06.11)), file = "SummaryStatsPM25_2006_2011.txt", append = TRUE)
cat("LM AODlarge PM25 met\n", file = "SummaryStatsPM25_2006_2011.txt", append = TRUE)
capture.output(summary(lm(AODlarge~PM25, data=misr.aqspm25.met.06.11)), file = "SummaryStatsPM25_2006_2011.txt", append = TRUE)

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
capture.output(summary(gam(PM25~s(AOD)+te(x.1,y.1,julian2,k=c(25,8),d=c(2,1),bs=c('tp','cr'))+te(julian2,year)+s(month, bs="cc",k=7)+as.factor(dow), data=misr.aqspm25.met.06.11)), file = "SummaryStatsPM25_2006_2011.txt", append = TRUE)
cat("LM AODsmall PM25 met\n", file = "SummaryStatsPM25_2006_2011.txt", append = TRUE)
capture.output(summary(gam(PM25~s(AODsmall)+te(x.1,y.1,julian2,k=c(25,8),d=c(2,1),bs=c('tp','cr'))+te(julian2,year)+s(month, bs="cc",k=7)+as.factor(dow), data=misr.aqspm25.met.06.11)), file = "SummaryStatsPM25_2006_2011.txt", append = TRUE)
cat("LM AODsmall_med PM25 met\n", file = "SummaryStatsPM25_2006_2011.txt", append = TRUE)
capture.output(summary(gam(PM25~s(AODsm_med)+te(x.1,y.1,julian2,k=c(25,8),d=c(2,1),bs=c('tp','cr'))+te(julian2,year)+s(month, bs="cc",k=7)+as.factor(dow), data=misr.aqspm25.met.06.11)), file = "SummaryStatsPM25_2006_2011.txt", append = TRUE)
cat("LM AODlarge PM25 met\n", file = "SummaryStatsPM25_2006_2011.txt", append = TRUE)
capture.output(summary(gam(PM25~s(AODlarge)+te(x.1,y.1,julian2,k=c(25,8),d=c(2,1),bs=c('tp','cr'))+te(julian2,year)+s(month, bs="cc",k=7)+as.factor(dow), data=misr.aqspm25.met.06.11)), file = "SummaryStatsPM25_2006_2011.txt", append = TRUE)

# S-T GAM models with met PM25
cat("LM AOD PM25 met\n", file = "SummaryStatsPM25_2006_2011.txt", append = TRUE)
capture.output(summary(gam(PM25~s(AOD)+te(x.1,y.1,julian2,k=c(25,8),d=c(2,1),bs=c('tp','cr'))+te(julian2,year)+s(month, bs="cc",k=7)+as.factor(dow)+rh+wind.sp, data=misr.aqspm25.met.06.11)), file = "SummaryStatsPM25_2006_2011.txt", append = TRUE)
cat("LM AODsmall PM25 met\n", file = "SummaryStatsPM25_2006_2011.txt", append = TRUE)
capture.output(summary(gam(PM25~s(AODsmall)+te(x.1,y.1,julian2,k=c(25,8),d=c(2,1),bs=c('tp','cr'))+te(julian2,year)+s(month, bs="cc",k=7)+as.factor(dow)+rh+wind.sp, data=misr.aqspm25.met.06.11)), file = "SummaryStatsPM25_2006_2011.txt", append = TRUE)
cat("LM AODsmall_med PM25 met\n", file = "SummaryStatsPM25_2006_2011.txt", append = TRUE)
capture.output(summary(gam(PM25~s(AODsm_med)+s(x.1,y.1,julian2,k=c(25,8),d=c(2,1),bs=c('tp','cr'))+te(julian2,year)+s(month, bs="cc",k=7)+as.factor(dow)+rh+wind.sp, data=misr.aqspm25.met.06.11)), file = "SummaryStatsPM25_2006_2011.txt", append = TRUE)
cat("LM AODlarge PM25 met\n", file = "SummaryStatsPM25_2006_2011.txt", append = TRUE)
capture.output(summary(gam(PM25~s(AODlarge)+s(x.1,y.1,julian2,k=c(25,8),d=c(2,1),bs=c('tp','cr'))+te(julian2,year)+s(month, bs="cc",k=7)+as.factor(dow)+rh+wind.sp, data=misr.aqspm25.met.06.11)), file = "SummaryStatsPM25_2006_2011.txt", append = TRUE)
# END PM25


# Summary Statistics PM10
misr.aqspm10.met.06.11<-misr.aqspm10.met.ss[misr.aqspm10.met.ss$year>=2006,]
misr.aqspm10.points<-unique(misr.aqspm10.met.06.11[,38:39])

# S-T GAM models with met PM10

cat("LM AOD PM25 met\n", file = "SummaryStatsPM25_2006_2011.txt", append = TRUE)
capture.output(summary(gam(PM10~s(AOD)+te(x.1,y.1,julian2,k=c(25,8),d=c(2,1),bs=c('tp','cr'))+te(julian2,year)+s(month, bs="cc",k=6)+as.factor(dow)+rh+wind.sp, data=misr.aqspm10.met.06.11)), file = "SummaryStatsPM25_2006_2011.txt", append = TRUE)
cat("LM AODsmall PM25 met\n", file = "SummaryStatsPM25_2006_2011.txt", append = TRUE)
capture.output(summary(gam(PM10~s(AODsmall)+te(x.1,y.1,julian2,k=c(25,8),d=c(2,1),bs=c('tp','cr'))+te(julian2,year)+s(month, bs="cc",k=6)+as.factor(dow)+rh+wind.sp, data=misr.aqspm10.met.06.11)), file = "SummaryStatsPM25_2006_2011.txt", append = TRUE)
cat("LM AODsmall_med PM25 met\n", file = "SummaryStatsPM25_2006_2011.txt", append = TRUE)
capture.output(summary(gam(PM10~s(AODsm_med)+te(x.1,y.1,julian2,k=c(25,8),d=c(2,1),bs=c('tp','cr'))+te(julian2,year)+s(month, bs="cc")+as.factor(dow)+rh+wind.sp, data=misr.aqspm10.met.06.11)), file = "SummaryStatsPM25_2006_2011.txt", append = TRUE)
cat("LM AODlarge PM25 met\n", file = "SummaryStatsPM25_2006_2011.txt", append = TRUE)
capture.output(summary(gam(PM10~s(AODlarge)+te(x.1,y.1,julian2,k=c(25,8),d=c(2,1),bs=c('tp','cr'))+te(julian2,year)+s(month, bs="cc")+as.factor(dow)+rh+wind.sp, data=misr.aqspm10.met.06.11)), file = "SummaryStatsPM25_2006_2011.txt", append = TRUE)
summary(gam(PM10~s(AODlarge)+te(x.1,y.1,julian2,k=c(25,8),d=c(2,1),bs=c('tp','cr'))+te(julian2,year)+s(month, bs="cc")+as.factor(dow)+temp+wind.sp, data=misr.aqspm10.met.06.11))

# Summary Statistics PM10-PM25
misr.aqspm2510.met.06.11<-misr.aqspm2510.met.ss[misr.aqspm2510.met.ss$year>=2006,]
misr.aqspm2510.met.06.11<-misr.aqspm2510.met.06.11[misr.aqspm2510.met.06.11$pm10_pm25>=0,]
misr.aqspm2510.points<-unique(misr.aqspm2510.met.06.11[,22:23])
hist(misr.aqspm2510.met.06.11$pm10_pm25)


summary(lm(misr.aqspm25.ss$PM25~(misr.aqspm25.ss$AODsm_med)))
summary(gam(misr.aqspm25.ss$PM25~s(misr.aqspm25.ss$AOD,k=5)))
plot(gam(misr.aqspm25.ss$PM25~s(misr.aqspm25.ss$AOD,k=5)))
summary(gam(PM25~s(AOD,k=5)+s(Latitude,Longitude,k=40)+s(julian2)+s(month,bs="cc",k=6)+as.factor(dow),data=misr.aqspm25.ss))
summary(gam(PM25~s(AOD,k=5)+s(Latitude,Longitude,k=40)+s(julian2,year)+s(month,bs="cc",k=6)+as.factor(dow)+rh+wind.sp,data=misr.aqspm25.met.ss))

summary(lm(misr.aqspm10.ss$PM10~(misr.aqspm10.ss$AOD)))
summary(gam(misr.aqspm10.ss$PM10~s(misr.aqspm10.ss$AOD)))
plot(gam(misr.aqspm10.ss$PM10~s(misr.aqspm10.ss$AODlarge)))
summary(gam(PM10~s(AOD,k=5)+s(Latitude,Longitude,k=30)+s(julian2,year)+s(month,bs="cc",k=6)+as.factor(dow),data=misr.aqspm10.ss))
summary(gam(PM10~s(AOD,k=5)+s(Latitude,Longitude,k=20)+s(julian2,year)+s(month,bs="cc",k=6)+as.factor(dow)+atm.press+wind.sp,data=misr.aqspm10.met.ss))


summary(lm(misr.aqspm25.match.stn$PM25~misr.aqspm25.match.stn$AOD))
summary(lm(misr.aqspm10.match.stn$PM10~misr.aqspm10.match.stn$AOD))

summary(lm(misr.aqspm2510.ss$pm10_pm25~misr.aqspm2510.met.ss$AODsm_med))
summary(lm(misr.aqspm2510.met.ss$pm10_pm25~misr.aqspm2510.met.ss$AODsmall))

#PM10-PM25
summary(lm(misr.aqspm2510.met.ss$pm10_pm25~misr.aqspm2510.met.ss$AOD))
summary(lm(misr.aqspm2510.met.ss$pm10_pm25~misr.aqspm2510.met.ss$AODlarge))
summary(lm(misr.aqspm2510.met.ss$pm10_pm25~misr.aqspm2510.met.ss$AODmed))
summary(lm(misr.aqspm2510.met.ss$pm10_pm25~misr.aqspm2510.met.ss$AODsmall))
summary(gam(pm10_pm25~s(AOD),data=misr.aqspm2510.met.ss))
summary(gam(pm10_pm25~s(AODlarge),data=misr.aqspm2510.met.ss))
summary(gam(pm10_pm25~s(AODlarge,k=5)+s(julian2)+s(Latitude,Longitude,k=12),data=misr.aqspm2510.met.ss))
summary(gam(Daily.Mean.PM10.Concentration~s(AODlarge),data=misr.aqspm2510.met.ss))
summary(gam(Daily.Mean.PM10.Concentration~s(AODlarge),data=misr.aqspm2510.met.ss))

# AOD with AODsm_med
summary(gam(Daily.Mean.PM2.5.Concentration.1~s(AODsm_med),data=misr.aqspm2510.met.ss))
summary(gam(Daily.Mean.PM10.Concentration.1~s(AODsm_med),data=misr.aqspm2510.met.ss))
summary(gam(Daily.Mean.PM2.5.Concentration.1~s(AODsm_med),data=misr.aqspm2510.met.ss))

# Univariate GAM models
cat("GAM mod AOD PM25\n", file = "SummaryStatsPM25.txt", append = TRUE)
capture.output(summary(gam(Daily.Mean.PM2.5.Concentration~s(AOD),data=misr.aqspm25.ss)), file = "SummaryStatsPM25.txt", append = TRUE)
cat("GAM mod logAOD PM25\n", file = "SummaryStatsPM25.txt", append = TRUE)
capture.output(summary(gam(Daily.Mean.PM2.5.Concentration~s(log(AOD)),data=misr.aqspm25.ss)), file = "SummaryStatsPM25.txt", append = TRUE)

cat("GAM mod AODlarge PM25\n", file = "SummaryStatsPM25.txt", append = TRUE)
capture.output(summary(gam(Daily.Mean.PM2.5.Concentration~s(AODlarge), data=misr.aqspm25.ss),na.action='na.exclude'), file = "SummaryStatsPM25.txt", append = TRUE)
cat("GAM mod logAODlarge PM25\n", file = "SummaryStatsPM25.txt", append = TRUE)
capture.output(summary(gam(Daily.Mean.PM2.5.Concentration~s(log(AODlarge)), data=misr.aqspm25.ss),na.action='na.exclude'), file = "SummaryStatsPM25.txt", append = TRUE)

cat("GAM mod AODsmall PM25\n", file = "SummaryStatsPM25.txt", append = TRUE)
capture.output(summary(gam(Daily.Mean.PM2.5.Concentration~s(AODsmall), data=misr.aqspm25.ss)), file = "SummaryStatsPM25.txt", append = TRUE)
cat("GAM mod logAODsmall PM25\n", file = "SummaryStatsPM25.txt", append = TRUE)
capture.output(summary(gam(Daily.Mean.PM2.5.Concentration~s(log(AODsmall)), data=misr.aqspm25.ss)), file = "SummaryStatsPM25.txt", append = TRUE)

capture.output(summary(gam(Daily.Mean.PM2.5.Concentration~s(AODsm_med), data=misr.aqspm25.ss)), file = "SummaryStatsPM25.txt", append = TRUE)

cat("GAM mod AOD PM25 match STN days\n", file = "SummaryStatsPM25.txt", append = TRUE)
capture.output(summary(gam(Daily.Mean.PM2.5.Concentration~s(AOD), data=misr.aqspm25.match.stn)), file = "SummaryStatsPM25.txt", append = TRUE)
capture.output(summary(gam(Daily.Mean.PM2.5.Concentration~s(AODsmall), data=misr.aqspm25.match.stn)), file = "SummaryStatsPM25.txt", append = TRUE)
capture.output(summary(gam(Daily.Mean.PM2.5.Concentration~s(AODlarge), data=misr.aqspm25.match.stn)), file = "SummaryStatsPM25.txt", append = TRUE)
capture.output(summary(gam(Daily.Mean.PM2.5.Concentration~s(AODsm_med), data=misr.aqspm25.match.stn)), file = "SummaryStatsPM25.txt", append = TRUE)


#### MISR AOD and AQS PM10 ####
# Summary statistics
cor.test(misr.aqspm10.ss$AOD,misr.aqspm10.ss$Daily.Mean.PM10.Concentration)
cor.test(misr.aqspm10.ss$AODsmall,misr.aqspm10.ss$Daily.Mean.PM10.Concentration)
cor.test(misr.aqspm10.ss$AODlarge,misr.aqspm10.ss$Daily.Mean.PM10.Concentration)

# Title (writes new file)
cat("PM10 Summary Stats", file = "SummaryStatsPM10.txt")
# add new lines
cat("\n", file = "SummaryStatsPM10.txt", append = TRUE)
# export anova test output
cat("Median AOD\n", file = "SummaryStatsPM10.txt", append = TRUE)
capture.output(median(misr.aqspm10.ss$AOD), file = "SummaryStatsPM10.txt", append = TRUE)
cat("IQR AOD\n", file = "SummaryStatsPM10.txt", append = TRUE)
capture.output(IQR(misr.aqspm10.ss$AOD), file = "SummaryStatsPM10.txt", append = TRUE)
cat("Mean PM10\n", file = "SummaryStatsPM10.txt", append = TRUE)
capture.output(mean(misr.aqspm10.ss$Daily.Mean.PM10.Concentration), file = "SummaryStatsPM10.txt", append = TRUE)
cat("St Dev PM10\n", file = "SummaryStatsPM10.txt", append = TRUE)
capture.output(sd(misr.aqspm10.ss$Daily.Mean.PM10.Concentration), file = "SummaryStatsPM10.txt", append = TRUE)
cat("Cor AOD PM10\n", file = "SummaryStatsPM10.txt", append = TRUE)
capture.output(cor(misr.aqspm10.ss$AOD,misr.aqspm10.ss$Daily.Mean.PM10.Concentration), file = "SummaryStatsPM10.txt", append = TRUE)
cat("Cor AODlarge PM10\n", file = "SummaryStatsPM10.txt", append = TRUE)
capture.output(cor(misr.aqspm10.ss2$AODlarge,misr.aqspm10.ss2$Daily.Mean.PM10.Concentration), file = "SummaryStatsPM10.txt", append = TRUE)
cat("Cor AODsmall PM10\n", file = "SummaryStatsPM10.txt", append = TRUE)
capture.output(cor(misr.aqspm10.ss$AODsmall,misr.aqspm10.ss$Daily.Mean.PM10.Concentration), file = "SummaryStatsPM10.txt", append = TRUE)
cat("Cor AOD PM10 met\n", file = "SummaryStatsPM10.txt", append = TRUE)
capture.output(cor(misr.aqspm10.met.ss$AOD,misr.aqspm10.met.ss$Daily.Mean.PM10.Concentration), file = "SummaryStatsPM10.txt", append = TRUE)
cat("Cor AODlarge PM10 met\n", file = "SummaryStatsPM10.txt", append = TRUE)
capture.output(cor(misr.aqspm10.met.ss$AODlarge,misr.aqspm10.met.ss$Daily.Mean.PM10.Concentration), file = "SummaryStatsPM10.txt", append = TRUE)
cat("Cor AODsmall PM10 met\n", file = "SummaryStatsPM10.txt", append = TRUE)
capture.output(cor(misr.aqspm10.met.ss$AODsmall,misr.aqspm10.met.ss$Daily.Mean.PM10.Concentration), file = "SummaryStatsPM10.txt", append = TRUE)
cat("Cor AODsm_med PM10 met\n", file = "SummaryStatsPM10.txt", append = TRUE)
capture.output(summary(lm(misr.aqspm10.ss$Daily.Mean.PM10.Concentration~misr.aqspm10.ss$AODsm_med)), file = "SummaryStatsPM25.txt", append = TRUE)

# Univariate Linear models
cat("linear mod AOD PM10\n", file = "SummaryStatsPM10.txt", append = TRUE)
capture.output()summary(lm(misr.aqspm10.ss$Daily.Mean.PM10.Concentration~(misr.aqspm10.ss$AOD)), file = "SummaryStatsPM10.txt", append = TRUE)
cat("linear mod logAOD PM10\n", file = "SummaryStatsPM10.txt", append = TRUE)
capture.output(summary(lm(misr.aqspm10.ss$Daily.Mean.PM10.Concentration~log(misr.aqspm10.ss$AOD))), file = "SummaryStatsPM10.txt", append = TRUE)

cat("linear mod AODlarge PM10\n", file = "SummaryStatsPM10.txt", append = TRUE)
capture.output(summary(lm(misr.aqspm10.ss$Daily.Mean.PM10.Concentration~(misr.aqspm10.ss$AODlarge))), file = "SummaryStatsPM10.txt", append = TRUE)
cat("linear mod logAODlarge PM10\n", file = "SummaryStatsPM10.txt", append = TRUE)
capture.output(summary(lm(misr.aqspm10.ss$Daily.Mean.PM10.Concentration~log(misr.aqspm10.ss2$AODlarge))), file = "SummaryStatsPM10.txt", append = TRUE)

cat("linear mod AODsmall PM10\n", file = "SummaryStatsPM10.txt", append = TRUE)
capture.output(summary(lm(misr.aqspm10.ss$Daily.Mean.PM10.Concentration~(misr.aqspm10.ss$AODsmall))), file = "SummaryStatsPM10.txt", append = TRUE)
cat("linear mod logAODsmall PM10\n", file = "SummaryStatsPM10.txt", append = TRUE)
capture.output(summary(lm(misr.aqspm10.ss$Daily.Mean.PM10.Concentration~log(misr.aqspm10.ss$AODsmall))), file = "SummaryStatsPM10.txt", append = TRUE)

# Univariate GAM models
cat("GAM mod AOD PM10\n", file = "SummaryStatsPM10.txt", append = TRUE)
capture.output(summary(gam(Daily.Mean.PM10.Concentration~s(AOD), data=misr.aqspm10.ss)), file = "SummaryStatsPM10.txt", append = TRUE)

cat("GAM mod AODlarge PM10\n", file = "SummaryStatsPM10.txt", append = TRUE)
capture.output(summary(gam(Daily.Mean.PM10.Concentration~s(AODlarge), data=misr.aqspm10.ss),na.action='na.exclude'), file = "SummaryStatsPM10.txt", append = TRUE)

cat("GAM mod AODsmall PM10\n", file = "SummaryStatsPM10.txt", append = TRUE)
capture.output(summary(gam(Daily.Mean.PM10.Concentration~s(AODsmall), data=misr.aqspm10.ss)), file = "SummaryStatsPM10.txt", append = TRUE)

cat("GAM mod logAODsmall PM10\n", file = "SummaryStatsPM10.txt", append = TRUE)
capture.output(summary(gam(Daily.Mean.PM10.Concentration~s(log(AODsmall)), data=misr.aqspm10.ss)), file = "SummaryStatsPM10.txt", append = TRUE)
cat("GAM mod AOD PM10 STN days\n", file = "SummaryStatsPM10.txt", append = TRUE)

capture.output(summary(gam(Daily.Mean.PM10.Concentration~s(AODsm_med), data=misr.aqspm10.ss)), file = "SummaryStatsPM10.txt", append = TRUE)

capture.output(summary(gam(Daily.Mean.PM10.Concentration~s(AOD), data=misr.aqspm10.match.stn)), file = "SummaryStatsPM10.txt", append = TRUE)
capture.output(summary(gam(Daily.Mean.PM10.Concentration~s(AODlarge), data=misr.aqspm10.match.stn)), file = "SummaryStatsPM10.txt", append = TRUE)
capture.output(summary(gam(Daily.Mean.PM10.Concentration~s(AODsmall), data=misr.aqspm10.match.stn)), file = "SummaryStatsPM10.txt", append = TRUE)

capture.output(summary(gam(Daily.Mean.PM10.Concentration~s(AODsm_med), data=misr.aqspm10.match.stn)), file = "SummaryStatsPM10.txt", append = TRUE)

# Plots
pdf('MISR.AOD.PM25_new.pdf')
p<-qplot(AOD,PM25, data=misr.aqspm25.met.06.11,xlab="MISR AOD",ylab=expression("AQS PM"[2.5]*", ug/m"^3))
p+stat_smooth(method="gam",formula=y~s(x,k=3)) +stat_smooth(method='lm',formula=y~x,col='red')
dev.off()

pdf('MISR.AOD.PM25.stnmatch_new.pdf')
p<-qplot(AOD,Daily.Mean.PM2.5.Concentration, data=misr.aqspm25.match.stn,xlab="MISR AOD",ylab=expression("AQS PM"[2.5]*", ug/m"^3))
p+stat_smooth(method="gam",formula=y~s(x,k=4)) +stat_smooth(method='lm',formula=y~x,col='red')
dev.off()

pdf('MISR.AOD.PM10_new.pdf')
p<-qplot(AOD,Daily.Mean.PM10.Concentration, data=misr.aqspm10.ss,xlab="MISR AOD",ylab=expression("AQS PM"[10]*", ug/m"^3))
p+stat_smooth(method="gam",formula=y~s(x,k=4)) +stat_smooth(method='lm',formula=y~x,col='red')
dev.off()

pdf('MISR.AOD.PM10.stnmatch_new.pdf')
p<-qplot(AOD,Daily.Mean.PM10.Concentration, data=misr.aqspm10.match.stn,xlab="MISR AOD",ylab=expression("AQS PM"[10]*", ug/m"^3))
p+stat_smooth(method="gam",formula=y~s(x,k=4)) +stat_smooth(method='lm',formula=y~x,col='red')
dev.off()

# MISR AOD and STN
# Title (writes new file)
cat("PM Speciation Summary Stats", file = "SummaryStatsPMSpeciation.txt")
# add new lines
cat("\n", file = "SummaryStatsPMSpeciation.txt", append = TRUE)
# export anova test output
cat("Median AOD\n", file = "SummaryStatsPMSpeciation.txt", append = TRUE)
capture.output(median(misr.stn$AOD), file = "SummaryStatsPMSpeciation.txt", append = TRUE)
cat("IQR AOD\n", file = "SummaryStatsPMSpeciation.txt", append = TRUE)
capture.output(IQR(misr.stn$AOD), file = "SummaryStatsPMSpeciation.txt", append = TRUE)
cat("Mean STNPM25\n", file = "SummaryStatsPMSpeciation.txt", append = TRUE)
capture.output(mean(misr.stn$PM25), file = "SummaryStatsPMSpeciation.txt", append = TRUE)
cat("St Dev STNPM25\n", file = "SummaryStatsPMSpeciation.txt", append = TRUE)
capture.output(sd(misr.stn$PM25), file = "SummaryStatsPMSpeciation.txt", append = TRUE)
cat("Cor AOD STNPM25\n", file = "SummaryStatsPMSpeciation.txt", append = TRUE)
capture.output(cor(misr.stn$AOD,misr.stn$PM25), file = "SummaryStatsPMSpeciation.txt", append = TRUE)
cat("Cor AODlarge STNPM25\n", file = "SummaryStatsPMSpeciation.txt", append = TRUE)
capture.output(cor(misr.stn$AODlarge,misr.stn$PM25), file = "SummaryStatsPMSpeciation.txt", append = TRUE)
cat("Cor AODsmall STNPM25\n", file = "SummaryStatsPMSpeciation.txt", append = TRUE)
capture.output(cor(misr.stn$AODsmall,misr.stn$PM25), file = "SummaryStatsPMSpeciation.txt", append = TRUE)



# Plots
p1<-qplot(AOD,PM25, data=misr.stn,xlab="MISR AOD",ylab=expression('STN PM'[2.5]*', ug/m'^3))
plot1<-p1 +stat_smooth(method='lm',formula=y~x,col='red')
#plot1<-p1+stat_smooth(method="gam",formula=y~s(x,k=4)) +stat_smooth(method='lm',formula=y~x,col='red')

p2<-qplot(AOD,OC, data=misr.stn,xlab="MISR AOD",ylab=expression('STN OC, ug/m'^3))
plot2<-p2 +stat_smooth(method='lm',formula=y~x,col='red')
#plot2<-p2+stat_smooth(method="gam",formula=y~s(x,k=4)) +stat_smooth(method='lm',formula=y~x,col='red')

p3<-qplot(AOD,SO4, data=misr.stn,xlab="MISR AOD",ylab=expression('STN SO'[4]*', ug/m'^3))
plot3<-p3+stat_smooth(method='lm',formula=y~x,col='red')
#plot3<-p3+stat_smooth(method="gam",formula=y~s(x,k=4)) +stat_smooth(method='lm',formula=y~x,col='red')

p4<-qplot(AOD,NH4, data=misr.stn,xlab="MISR AOD",ylab=expression('STN NO'[3]*', ug/m'^3))
plot4<-p4+stat_smooth(method='lm',formula=y~x,col='red')
#plot4<-p4+stat_smooth(method="gam",formula=y~s(x,k=4)) +stat_smooth(method='lm',formula=y~x,col='red')

pdf('MISR.STN_new.pdf')
grid.arrange(plot1,plot2,plot3,plot4,nrow=2,ncol=2)
dev.off()

#### Spatio-temporal regression with and without meteorology#####
# PM2.5
# Title (writes new file)
cat("PM AOD Spatio-Temporal Models", file = "SpatioTemporalModels2_new.txt")
# add new lines
cat("\n", file = "SpatioTemporalModels2_new.txt", append = TRUE)

# ST PM25-AOD
cat("ST model PM25 AOD\n", file = "SpatioTemporalModels2_new.txt", append = TRUE)
cat("\n", file = "SpatioTemporalModels2_new.txt", append = TRUE)
capture.output(summary(gam(Daily.Mean.PM2.5.Concentration~s(AOD)+s(x.1,y.1,k=16)+s(julian2)+s(month,bs="cc")+as.factor(dow), na.action=na.exclude,data=misr.aqspm25.ss)), file = "SpatioTemporalModels2_new.txt", append = TRUE)

cat("ST model PM25 AODlarge\n", file = "SpatioTemporalModels2_new.txt", append = TRUE)
cat("\n", file = "SpatioTemporalModels2_new.txt", append = TRUE)
capture.output(summary(gam(Daily.Mean.PM2.5.Concentration~s(AODlarge)+s(x.1,y.1,k=16)+s(julian2)+s(month,bs="cc")+as.factor(dow), na.action=na.exclude,data=misr.aqspm25.ss)), file = "SpatioTemporalModels2_new.txt", append = TRUE)

cat("ST model PM25 AODsm_med\n", file = "SpatioTemporalModels2_new.txt", append = TRUE)
cat("\n", file = "SpatioTemporalModels2_new.txt", append = TRUE)
capture.output(summary(gam(Daily.Mean.PM2.5.Concentration~s(AODsm_med)+s(x.1,y.1,k=16)+s(julian2)+s(month,bs="cc")+as.factor(dow), na.action=na.exclude,data=misr.aqspm25.ss)), file = "SpatioTemporalModels2_new.txt", append = TRUE)

cat("ST model PM25 AOD match STN \n", file = "SpatioTemporalModels2_new.txt", append = TRUE)
capture.output(summary(gam(Daily.Mean.PM2.5.Concentration~s(AOD)+s(x.1,y.1,k=16)+s(julian2)+s(month,bs="cc")+as.factor(dow), na.action=na.exclude,data=misr.aqspm25.match.stn)), file = "SpatioTemporalModels2_new.txt", append = TRUE)
cat("ST model PM25 AODlarge match STN \n", file = "SpatioTemporalModels2_new.txt", append = TRUE)
capture.output(summary(gam(Daily.Mean.PM2.5.Concentration~s(AODlarge)+s(x.1,y.1,k=16)+s(julian2)+s(month,bs="cc")+as.factor(dow), na.action=na.exclude,data=misr.aqspm25.match.stn)), file = "SpatioTemporalModels2_new.txt", append = TRUE)
cat("ST model PM25 AODsm_med match STN \n", file = "SpatioTemporalModels2_new.txt", append = TRUE)
capture.output(summary(gam(Daily.Mean.PM2.5.Concentration~s(AODsm_med)+s(x.1,y.1,k=16)+s(julian2)+s(month,bs="cc")+as.factor(dow), na.action=na.exclude,data=misr.aqspm25.match.stn)), file = "SpatioTemporalModels2_new.txt", append = TRUE)


summary(gam(Daily.Mean.PM2.5.Concentration~s(AODsm_med)+s(x.1,y.1,k=16)+s(julian2)+s(month,bs="cc")+as.factor(dow), na.action=na.exclude,data=misr.aqspm25.ss))
summary(gam(Daily.Mean.PM2.5.Concentration~s(AODsm_med)+s(x.1,y.1,k=16)+s(julian2)+s(month,bs="cc")+as.factor(dow), na.action=na.exclude,data=misr.aqspm25.match.stn))
summary(gam(Daily.Mean.PM2.5.Concentration~s(AODlarge)+s(x.1,y.1,k=16)+s(julian2)+s(month,bs="cc")+as.factor(dow), na.action=na.exclude,data=misr.aqspm25.ss))
summary(gam(Daily.Mean.PM2.5.Concentration~s(AODlarge)+s(x.1,y.1,k=16)+s(julian2)+s(month,bs="cc")+as.factor(dow), na.action=na.exclude,data=misr.aqspm25.match.stn))


#ST PM10-AOD
cat("ST model PM10 AOD\n", file = "SpatioTemporalModels2_new.txt", append = TRUE)
capture.output(summary(gam(Daily.Mean.PM10.Concentration~s(AOD)+s(x.1,y.1,k=12)+s(julian2)+s(month,bs="cc")+as.factor(dow), na.action=na.exclude,data=misr.aqspm10.ss)), file = "SpatioTemporalModels2_new.txt", append = TRUE)
#cat("ST model PM10 AOD version 2\n", file = "SpatioTemporalModels2_new.txt", append = TRUE)
#capture.output(summary(gam(Daily.Mean.PM10.Concentration~s(AOD)+s(x.1,y.1,k=12)+s(julian2,k=4)+s(month,bs="cc")+as.factor(dow), na.action=na.exclude,data=misr.aqspm10.ss)), file = "SpatioTemporalModels2_new.txt", append = TRUE)
#cat("ST model PM10 AOD version 3\n", file = "SpatioTemporalModels2_new.txt", append = TRUE)
#capture.output(summary(gam(Daily.Mean.PM10.Concentration~s(AOD)+s(x.1,y.1,k=12)+s(julian2,k=3)+s(month,bs="cc")+as.factor(dow), na.action=na.exclude,data=misr.aqspm10.ss)), file = "SpatioTemporalModels2_new.txt", append = TRUE)
#cat("ST model PM10 AOD version 3\n", file = "SpatioTemporalModels2_new.txt", append = TRUE)
#capture.output(summary(gam(Daily.Mean.PM10.Concentration~s(AOD)+s(x.1,y.1,k=12)+s(julian2,k=3)+s(month,bs="cc")+as.factor(dow), na.action=na.exclude,data=misr.aqspm10.match.stn)), file = "SpatioTemporalModels2_new.txt", append = TRUE)

cat("ST model PM10 AODlarge\n", file = "SpatioTemporalModels2_new.txt", append = TRUE)
capture.output(summary(gam(Daily.Mean.PM10.Concentration~s(AODlarge)+s(x.1,y.1,k=12)+s(julian2)+s(month,bs="cc")+as.factor(dow), na.action=na.exclude,data=misr.aqspm10.ss)), file = "SpatioTemporalModels2_new.txt", append = TRUE)
#cat("ST model PM10 AODlarge version 2 \n", file = "SpatioTemporalModels2_new.txt", append = TRUE)
#capture.output(summary(gam(Daily.Mean.PM10.Concentration~s(AODlarge)+s(x.1,y.1,k=12)+s(julian2,k=4)+s(month,bs="cc",k=4)+as.factor(dow), na.action=na.exclude,data=misr.aqspm10.ss)), file = "SpatioTemporalModels2_new.txt", append = TRUE)
#cat("ST model PM10 AODlarge version 3 \n", file = "SpatioTemporalModels2_new.txt", append = TRUE)
#capture.output(summary(gam(Daily.Mean.PM10.Concentration~s(AODlarge)+s(x.1,y.1,k=12)+s(julian2,k=3)+s(month,bs="cc")+as.factor(dow), na.action=na.exclude,data=misr.aqspm10.ss)), file = "SpatioTemporalModels2_new.txt", append = TRUE)
#cat("ST model PM10 match STN AODlarge  \n", file = "SpatioTemporalModels2_new.txt", append = TRUE)
#capture.output(summary(gam(Daily.Mean.PM10.Concentration~s(AODlarge,k=4)+s(x.1,y.1,k=12)+s(julian2)+s(month,bs="cc")+as.factor(dow), na.action=na.exclude,data=misr.aqspm10.match.stn)), file = "SpatioTemporalModels2_new.txt", append = TRUE)
cat("ST model PM10 AODsm_med\n", file = "SpatioTemporalModels2_new.txt", append = TRUE)
capture.output(summary(gam(Daily.Mean.PM10.Concentration~s(AODsm_med)+s(x.1,y.1,k=12)+s(julian2)+s(month,bs="cc")+as.factor(dow), na.action=na.exclude,data=misr.aqspm10.ss)), file = "SpatioTemporalModels2_new.txt", append = TRUE)
cat("ST model PM10 AOD Match STN\n", file = "SpatioTemporalModels2_new.txt", append = TRUE)
capture.output(summary(gam(Daily.Mean.PM10.Concentration~s(AOD)+s(x.1,y.1,k=12)+s(julian2)+s(month,bs="cc")+as.factor(dow), na.action=na.exclude,data=misr.aqspm10.match.stn)), file = "SpatioTemporalModels2_new.txt", append = TRUE)
cat("ST model PM10 AODsm_med Match STN\n", file = "SpatioTemporalModels2_new.txt", append = TRUE)
capture.output(summary(gam(Daily.Mean.PM10.Concentration~s(AODsm_med)+s(x.1,y.1,k=12)+s(julian2)+s(month,bs="cc")+as.factor(dow), na.action=na.exclude,data=misr.aqspm10.match.stn)), file = "SpatioTemporalModels2_new.txt", append = TRUE)
cat("ST model PM10 AODlarge MAtch STN\n", file = "SpatioTemporalModels2_new.txt", append = TRUE)
capture.output(summary(gam(Daily.Mean.PM10.Concentration~s(AODlarge)+s(x.1,y.1,k=12)+s(julian2)+s(month,bs="cc")+as.factor(dow), na.action=na.exclude,data=misr.aqspm10.match.stn)), file = "SpatioTemporalModels2_new.txt", append = TRUE)

summary(gam(Daily.Mean.PM10.Concentration~s(AOD)+s(x.1,y.1,k=12)+s(julian2)+s(month,bs="cc")+as.factor(dow), na.action=na.exclude,data=misr.aqspm10.ss))
summary(gam(Daily.Mean.PM10.Concentration~s(AOD)+s(x.1,y.1,k=12)+s(julian2)+s(month,bs="cc")+as.factor(dow), na.action=na.exclude,data=misr.aqspm10.match.stn))
summary(gam(Daily.Mean.PM10.Concentration~s(AODsm_med)+s(x.1,y.1,k=12)+s(julian2)+s(month,bs="cc")+as.factor(dow), na.action=na.exclude,data=misr.aqspm10.ss))
summary(gam(Daily.Mean.PM10.Concentration~s(AODsm_med)+s(x.1,y.1,k=12)+s(julian2)+s(month,bs="cc")+as.factor(dow), na.action=na.exclude,data=misr.aqspm10.match.stn))
summary(gam(Daily.Mean.PM10.Concentration~s(AODlarge)+s(x.1,y.1,k=12)+s(julian2)+s(month,bs="cc")+as.factor(dow), na.action=na.exclude,data=misr.aqspm10.ss))
summary(gam(Daily.Mean.PM10.Concentration~s(AODlarge)+s(x.1,y.1,k=12)+s(julian2)+s(month,bs="cc")+as.factor(dow), na.action=na.exclude,data=misr.aqspm10.match.stn))

# ST PM25-AOD with met
cat("ST model PM25 AOD met\n", file = "SpatioTemporalModels2_new.txt", append = TRUE)
capture.output(summary(gam(Daily.Mean.PM2.5.Concentration~s(AOD)+s(x.1,y.1,k=16)+s(julian2)+s(month, bs="cc")+as.factor(dow)+rh+wind.sp+atm.press, na.action=na.exclude,data=misr.aqspm25.met.ss)), file = "SpatioTemporalModels2_new.txt", append = TRUE)
cat("ST model PM25 AODsm_med met\n", file = "SpatioTemporalModels2_new.txt", append = TRUE)
capture.output(summary(gam(Daily.Mean.PM2.5.Concentration~s(AODsm_med)+s(x.1,y.1,k=16)+s(julian2)+s(month, bs="cc")+as.factor(dow)+temp+wind.sp, na.action=na.exclude,data=misr.aqspm25.met.ss)), file = "SpatioTemporalModels2_new.txt", append = TRUE)
cat("ST model PM25 AODlarge met\n", file = "SpatioTemporalModels2_new.txt", append = TRUE)
capture.output(summary(gam(Daily.Mean.PM2.5.Concentration~s(AODlarge)+s(x.1,y.1,k=16)+s(julian2)+s(month, bs="cc")+as.factor(dow)+temp+wind.sp, na.action=na.exclude,data=misr.aqspm25.met.ss)), file = "SpatioTemporalModels2_new.txt", append = TRUE)
cat("ST model PM25 AOD met STN match days\n", file = "SpatioTemporalModels2_new.txt", append = TRUE)
capture.output(summary(gam(Daily.Mean.PM2.5.Concentration~s(AOD)+s(x.1,y.1,k=16)+s(julian2)+s(month, bs="cc")+as.factor(dow)+temp+wind.sp, na.action=na.exclude,data=misr.aqspm25.match.stn)), file = "SpatioTemporalModels2_new.txt", append = TRUE)
cat("ST model PM25 AODsm_med met STN match days\n", file = "SpatioTemporalModels2_new.txt", append = TRUE)
capture.output(summary(gam(Daily.Mean.PM2.5.Concentration~s(AODsm_med)+s(x.1,y.1,k=16)+s(julian2)+s(month, bs="cc")+as.factor(dow)+temp+wind.sp, na.action=na.exclude,data=misr.aqspm25.match.stn)), file = "SpatioTemporalModels2_new.txt", append = TRUE)
cat("ST model PM25 AODlarge met STN match days\n", file = "SpatioTemporalModels2_new.txt", append = TRUE)
capture.output(summary(gam(Daily.Mean.PM2.5.Concentration~s(AODlarge)+s(x.1,y.1,k=16)+s(julian2)+s(month, bs="cc")+as.factor(dow)+temp+wind.sp, na.action=na.exclude,data=misr.aqspm25.match.stn)), file = "SpatioTemporalModels2_new.txt", append = TRUE)


# ST PM10-AOD with met
cat("ST model PM10 AOD met\n", file = "SpatioTemporalModels2_new.txt", append = TRUE)
capture.output(summary(gam(Daily.Mean.PM10.Concentration~s(AOD)+s(x.1,y.1,k=10)+s(julian2,k=5)+s(month, bs="cc")+as.factor(dow)+wind.sp+atm.press, na.action=na.exclude,data=misr.aqspm10.met.ss)), file = "SpatioTemporalModels2_new.txt", append = TRUE)
cat("ST model PM10 AODsm_med met\n", file = "SpatioTemporalModels2_new.txt", append = TRUE)
capture.output(summary(gam(Daily.Mean.PM10.Concentration~s(AODsm_med)+s(x.1,y.1,k=10)+s(julian2,k=5)+s(month, bs="cc")+as.factor(dow)+wind.sp+atm.press, na.action=na.exclude,data=misr.aqspm10.met.ss)), file = "SpatioTemporalModels2_new.txt", append = TRUE)
cat("ST model PM10 AODlarge met\n", file = "SpatioTemporalModels2_new.txt", append = TRUE)
capture.output(summary(gam(Daily.Mean.PM10.Concentration~s(AODlarge)+s(x.1,y.1,k=10)+s(julian2,k=5)+s(month, bs="cc")+as.factor(dow)+wind.sp+atm.press, na.action=na.exclude,data=misr.aqspm10.met.ss)), file = "SpatioTemporalModels2_new.txt", append = TRUE)

#  ST PM10-AOD with met STN match
cat("ST model PM10 AOD met STN match\n", file = "SpatioTemporalModels2_new.txt", append = TRUE)
capture.output(summary(gam(Daily.Mean.PM10.Concentration~s(AOD)+s(x.1,y.1,k=10)+s(julian2,k=5)+s(month, bs="cc")+as.factor(dow)+wind.sp+atm.press, na.action=na.exclude,data=misr.aqspm10.match.stn)), file = "SpatioTemporalModels2_new.txt", append = TRUE)
cat("ST model PM10 AODsm_med met STN match\n", file = "SpatioTemporalModels2_new.txt", append = TRUE)
capture.output(summary(gam(Daily.Mean.PM10.Concentration~s(AODsm_med)+s(x.1,y.1,k=10)+s(julian2,k=5)+s(month, bs="cc")+as.factor(dow)+wind.sp+atm.press, na.action=na.exclude,data=misr.aqspm10.match.stn)), file = "SpatioTemporalModels2_new.txt", append = TRUE)
cat("ST model PM10 AODlarge met STN match\n", file = "SpatioTemporalModels2_new.txt", append = TRUE)
capture.output(summary(gam(Daily.Mean.PM10.Concentration~s(AODlarge)+s(x.1,y.1,k=10)+s(julian2,k=5)+s(month, bs="cc")+as.factor(dow)+wind.sp+atm.press, na.action=na.exclude,data=misr.aqspm10.match.stn)), file = "SpatioTemporalModels2_new.txt", append = TRUE)
# cat("ST model PM10 linear AODlarge met version 2\n", file = "SpatioTemporalModels.txt", append = TRUE)
# capture.output(summary(gam(Daily.Mean.PM10.Concentration~s(AODlarge)+s(x.1,y.1,k=11)+s(julian2,k=4)+wind.sp+atm.press, na.action=na.exclude,data=misr.aqspm10.met.ss)), file = "SpatioTemporalModels.txt", append = TRUE)
# cat("ST model PM10 linear AODlarge met version 3\n", file = "SpatioTemporalModels.txt", append = TRUE)
# capture.output(summary(gam(Daily.Mean.PM10.Concentration~s(AODlarge)+s(x.1,y.1,k=11)+s(julian2,k=5)+wind.sp+atm.press, na.action=na.exclude,data=misr.aqspm10.met.ss)), file = "SpatioTemporalModels.txt", append = TRUE)
# cat("ST model PM10 linear AODlarge met version 4\n", file = "SpatioTemporalModels.txt", append = TRUE)
# capture.output(summary(gam(Daily.Mean.PM10.Concentration~s(AODlarge)+s(x.1,y.1,k=11)+julian2+wind.sp+atm.press, na.action=na.exclude,data=misr.aqspm10.met.ss)), file = "SpatioTemporalModels.txt", append = TRUE)
summary(gam(Daily.Mean.PM10.Concentration~s(AOD)+s(x.1,y.1,k=10)+s(julian2)+s(month,bs="cc")+as.factor(dow)+wind.sp+atm.press, na.action=na.exclude,data=misr.aqspm10.met.ss))
summary(gam(Daily.Mean.PM10.Concentration~s(AODsm_med)+s(x.1,y.1,k=10)+s(julian2)+s(month,bs="cc")+as.factor(dow)+wind.sp+atm.press, na.action=na.exclude,data=misr.aqspm10.met.ss))
summary(gam(Daily.Mean.PM10.Concentration~s(AODlarge)+s(x.1,y.1,k=10)+s(julian2)+s(month,bs="cc")+as.factor(dow)+wind.sp+atm.press, na.action=na.exclude,data=misr.aqspm10.met.ss))

summary(gam(Daily.Mean.PM10.Concentration.1~s(AOD)+s(x.1,y.1,k=10)+s(julian2)+s(month,bs="cc")+as.factor(dow)+wind.sp+atm.press, na.action=na.exclude,data=misr.aqspm10.match.met.stn))
summary(gam(Daily.Mean.PM10.Concentration.1~s(AODsm_med)+s(x.1,y.1,k=10)+s(julian2)+s(month,bs="cc")+as.factor(dow)+wind.sp+atm.press, na.action=na.exclude,data=misr.aqspm10.match.met.stn))
summary(gam(Daily.Mean.PM10.Concentration.1~s(AODlarge)+s(x.1,y.1,k=10)+s(julian2)+s(month,bs="cc")+as.factor(dow)+wind.sp+atm.press, na.action=na.exclude,data=misr.aqspm10.match.met.stn))

#### PM2.5 Cross-Validation ####
#misr.aqspm25.match.stn
#misr.aqspm25.met.ss
#misr.aqspm25.ss[,45:46]
misr.aqspm25.points<-unique(misr.aqspm25.met.ss[,46:47])
#misr.aqspm25.points<-unique(misr.aqspm25.match.stn[,46:47])

gam.pred.pm25.list<-vector('list',dim(misr.aqspm25.points)[1])
gam.resid.pm25.list<-vector('list',dim(misr.aqspm25.points)[1])
gam.pred.pm25.met.list<-vector('list',dim(misr.aqspm25.points)[1])
gam.resid.pm25.met.list<-vector('list',dim(misr.aqspm25.points)[1])

gam.pred.pm25.list2<-vector('list',dim(misr.aqspm25.points)[1])
gam.resid.pm25.list2<-vector('list',dim(misr.aqspm25.points)[1])
gam.pred.pm25.met.list2<-vector('list',dim(misr.aqspm25.points)[1])
gam.resid.pm25.met.list2<-vector('list',dim(misr.aqspm25.points)[1])

gam.pred.pm25.list3<-vector('list',dim(misr.aqspm25.points)[1])
gam.resid.pm25.list3<-vector('list',dim(misr.aqspm25.points)[1])
gam.pred.pm25.met.list3<-vector('list',dim(misr.aqspm25.points)[1])
gam.resid.pm25.met.list3<-vector('list',dim(misr.aqspm25.points)[1])

# PM2.5 cross validation misr.aqspm25.match.stn misr.aqspm25.met.ss
for (i in 1:dim(misr.aqspm25.points)[1]){
  location.sample1<-misr.aqspm25.met.ss[i,]

  train0<-misr.aqspm25.ss[!(misr.aqspm25.ss$x.1 %in% location.sample1$x.1
                                & misr.aqspm25.ss$y.1 %in% location.sample1$y.1), ]
  test0<-misr.aqspm25.ss[(misr.aqspm25.ss$x.1 %in% location.sample1$x.1
                              & misr.aqspm25.ss$y.1 %in% location.sample1$y.1), ]
  
  train1<-misr.aqspm25.met.ss[!(misr.aqspm25.met.ss$x.1 %in% location.sample1$x.1
                           & misr.aqspm25.met.ss$y.1 %in% location.sample1$y.1), ]
  test1<-misr.aqspm25.met.ss[(misr.aqspm25.met.ss$x.1 %in% location.sample1$x.1
                         & misr.aqspm25.met.ss$y.1 %in% location.sample1$y.1), ]

  gam.st.pm25<-gam(Daily.Mean.PM2.5.Concentration~s(AOD)+s(x.1,y.1,k=16)+s(julian2)+s(month,bs='cc')+as.factor(dow), na.action=na.exclude,data=train0)
  gam.st.pm25.met<-gam(Daily.Mean.PM2.5.Concentration.1~s(AOD)+s(x.1,y.1,k=16)+s(julian2)+s(month,bs='cc')+as.factor(dow)+temp+wind.sp, na.action=na.exclude,data=train1)
  
  gam.st2.pm25<-gam(Daily.Mean.PM2.5.Concentration~s(AODlarge)+s(x.1,y.1,k=16)+s(julian2)+s(month,bs='cc')+as.factor(dow), na.action=na.exclude,data=train0)
  gam.st2.pm25.met<-gam(Daily.Mean.PM2.5.Concentration.1~s(AODlarge)+s(x.1,y.1,k=16)+s(julian2)+s(month,bs='cc')+as.factor(dow)+temp+wind.sp, na.action=na.exclude,data=train1)
  
  gam.st3.pm25<-gam(Daily.Mean.PM2.5.Concentration~s(AODsm_med)+s(x.1,y.1,k=16)+s(julian2)+s(month,bs='cc')+as.factor(dow), na.action=na.exclude,data=train0)
  gam.st3.pm25.met<-gam(Daily.Mean.PM2.5.Concentration.1~s(AODsm_med)+s(x.1,y.1,k=16)+s(julian2)+s(month,bs='cc')+as.factor(dow)+temp+wind.sp, na.action=na.exclude,data=train1)
  
  gam.st.pred.pm25<-predict.gam(gam.st.pm25,newdata=test0)
  gam.st.pred.pm25.merge<-cbind(gam.st.pred.pm25,test0)
  gam.st.resid.pm25<-data.frame(resid=gam.st.pm25$residuals, y=gam.st.pm25$y, fitted=gam.st.pm25$fitted.values)

  gam.st.pred.pm25.met<-predict.gam(gam.st.pm25.met,newdata=test1)
  gam.st.pred.pm25.met.merge<-cbind(gam.st.pred.pm25.met,test1)
  gam.st.resid.pm25.met<-data.frame(resid=gam.st.pm25.met$residuals, y=gam.st.pm25.met$y,fitted=gam.st.pm25.met$fitted.values)
  
  gam.st2.pred.pm25<-predict.gam(gam.st2.pm25,newdata=test0)
  gam.st2.pred.pm25.merge<-cbind(gam.st2.pred.pm25,test0)
  gam.st2.resid.pm25<-data.frame(resid=gam.st2.pm25$residuals, y=gam.st2.pm25$y,fitted=gam.st2.pm25$fitted.values)
  
  gam.st2.pred.pm25.met<-predict.gam(gam.st2.pm25.met,newdata=test1)
  gam.st2.pred.pm25.met.merge<-cbind(gam.st2.pred.pm25.met,test1)
  gam.st2.resid.pm25.met<-data.frame(resid=gam.st2.pm25.met$residuals, y=gam.st2.pm25.met$y,fitted=gam.st2.pm25.met$fitted.values)
  
  gam.st3.pred.pm25<-predict.gam(gam.st3.pm25,newdata=test0)
  gam.st3.pred.pm25.merge<-cbind(gam.st3.pred.pm25,test0)
  gam.st3.resid.pm25<-data.frame(resid=gam.st3.pm25$residuals, y=gam.st3.pm25$y,fitted=gam.st3.pm25$fitted.values)
  
  gam.st3.pred.pm25.met<-predict.gam(gam.st3.pm25.met,newdata=test1)
  gam.st3.pred.pm25.met.merge<-cbind(gam.st3.pred.pm25.met,test1)
  gam.st3.resid.pm25.met<-data.frame(resid=gam.st3.pm25.met$residuals, y=gam.st3.pm25.met$y,fitted=gam.st3.pm25.met$fitted.values)
  
  
  gam.pred.pm25.list[[i]]<-gam.st.pred.pm25.merge
  gam.pred.pm25.met.list[[i]]<-gam.st.pred.pm25.met.merge
  gam.resid.pm25.list[[i]]<-gam.st.resid.pm25
  gam.resid.pm25.met.list[[i]]<-gam.st.resid.pm25.met
  
  gam.pred.pm25.list2[[i]]<-gam.st2.pred.pm25.merge
  gam.pred.pm25.met.list2[[i]]<-gam.st2.pred.pm25.met.merge
  gam.resid.pm25.list2[[i]]<-gam.st2.resid.pm25
  gam.resid.pm25.met.list2[[i]]<-gam.st2.resid.pm25.met
  
  gam.pred.pm25.list3[[i]]<-gam.st3.pred.pm25.merge
  gam.pred.pm25.met.list3[[i]]<-gam.st3.pred.pm25.met.merge
  gam.resid.pm25.list3[[i]]<-gam.st3.resid.pm25
  gam.resid.pm25.met.list3[[i]]<-gam.st3.resid.pm25.met

}

gam.pred.pm25 <- do.call("rbind", gam.pred.pm25.list) # AOD no met
gam.pred.pm25.met <- do.call("rbind", gam.pred.pm25.met.list) # AOD with met
gam.pred2.pm25 <- do.call("rbind", gam.pred.pm25.list2) # AOD large no met
gam.pred2.pm25.met <- do.call("rbind", gam.pred.pm25.met.list2) # AOD large with met
gam.pred3.pm25 <- do.call("rbind", gam.pred.pm25.list3) # AOD large no met
gam.pred3.pm25.met <- do.call("rbind", gam.pred.pm25.met.list3) # AOD large with met


obs.pred.AOD<-lm(Daily.Mean.PM2.5.Concentration~gam.st.pred.pm25, data=gam.pred.pm25)
obs.pred.AODmet<-lm(Daily.Mean.PM2.5.Concentration~gam.st.pred.pm25.met, data=gam.pred.pm25.met)

obs.pred.AODlarge<-lm(Daily.Mean.PM2.5.Concentration~gam.st2.pred.pm25, data=gam.pred2.pm25)
obs.pred.AODlargemet<-lm(Daily.Mean.PM2.5.Concentration~gam.st2.pred.pm25.met, data=gam.pred2.pm25.met)

obs.pred.AODsm_med<-lm(Daily.Mean.PM2.5.Concentration~gam.st3.pred.pm25, data=gam.pred3.pm25)
obs.pred.AODsm_medmet<-lm(Daily.Mean.PM2.5.Concentration~gam.st3.pred.pm25.met, data=gam.pred3.pm25.met)


cor.test(gam.pred.pm25$gam.st.pred.pm25,gam.pred.pm25$Daily.Mean.PM2.5.Concentration)

p5<-qplot(gam.st.pred.pm25,Daily.Mean.PM2.5.Concentration, data=gam.pred.pm25,xlab=expression('Predicted PM'[2.5]*', ug/m'^3),ylab=expression('Observed PM'[2.5]*', ug/m'^3))
plot5<-p5+stat_smooth(method='lm',formula=y~x-1,col='red')+stat_smooth(method='lm',formula=y~x,col='blue')
plot5

pdf("PM25_cv_plot.pdf")
p6<-qplot(gam.st.pred.pm25.met,Daily.Mean.PM2.5.Concentration, data=gam.pred.pm25.met,xlab=expression('Predicted PM'[2.5]*', ug/m'^3),ylab=expression('Observed PM'[2.5]*', ug/m'^3))
plot6<-p6+stat_smooth(method='lm',formula=y~x-1,col='red')+stat_smooth(method='lm',formula=y~x,col='blue')
plot6
dev.off()

p7<-qplot(gam.st2.pred.pm25,Daily.Mean.PM2.5.Concentration, data=gam.pred2.pm25,xlab="Predicted",ylab="Observed",xlim=c(0,30))
plot7<-p7+stat_smooth(method='lm',formula=y~x-1,col='red')+stat_smooth(method='lm',formula=y~x,col='blue')
plot7

p8<-qplot(gam.pred.pm10.met,Daily.Mean.PM10.Concentration, data=gam.pred.pm10.met,xlab=expression('Predicted PM'[10]*', ug/m'^3),ylab=expression('Observed PM'[10]*', ug/m'^3))
plot8<-p8+stat_smooth(method='lm',formula=y~x-1,col='red')+stat_smooth(method='lm',formula=y~x,col='blue')
plot8

p8<-qplot(gam.st2.pred.pm25.met,Daily.Mean.PM2.5.Concentration, data=gam.pred2.pm25.met,xlab="Predicted PM2.5",ylab=expression('hi'[5]*'there'[6]^8*'you'['down here']*'and'^'up'*'there'))

tiff('MISR.PM25.CV.tiff')
grid.arrange(plot6,plot8,nrow=1,ncol=2)
dev.off()

# Plot prediction over time with observed gam.pred.pm25 and gam.pred.pm25.met
# Site with max latitude (remote, low concentration) pred.site1, site in LA with dense monitoring pred.site2
pred.site1<-gam.pred.pm25.met[gam.pred.pm25.met$AQS_SITE_ID==60374002,] # use 06-037-4002
pred.site2<-gam.pred.pm25.met[gam.pred.pm25.met$AQS_SITE_ID==60719004,]

pdf('MISR.PM25.CV.LA_new.pdf')
ggplot(data = pred.site1, aes(as.Date(date2,"%Y-%m-%d"))) + 
  geom_line(aes(y= gam.st.pred.pm25.met, color="red")) + 
  geom_line(aes(y=Daily.Mean.PM2.5.Concentration)) +
  geom_point(aes(y=Daily.Mean.PM2.5.Concentration)) +
  geom_point(aes(y= gam.st.pred.pm25.met, color="red"))+
  scale_y_continuous(breaks = round(seq(3, 30, by = 3),1)) +
  scale_x_date(date_breaks = "4 months")+
  theme(legend.position="none",
        axis.line = element_line(colour = "black"),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())+
  labs(x = "Date",y=expression('PM'[2.5]*', ug/m'^3))
dev.off()

pdf('MISR.PM25.CV.LA.pdf')
ggplot(data = pred.site2, aes(as.Date(Date,"%m/%d/%y"))) + 
  geom_line(aes(y= gam.st.pred.pm25, color="red")) + 
  geom_line(aes(y=Daily.Mean.PM2.5.Concentration)) +
  geom_point(aes(y=Daily.Mean.PM2.5.Concentration)) +
  geom_point(aes(y= gam.st.pred.pm25, color="red"))+
  theme(legend.position="none",
        axis.line = element_line(colour = "black"),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())+
  labs(x = "Date",y=expression('PM'[2.5]*', ug/m'^3))
dev.off()


# Cross validation PM10 misr.aqspm10.ss, misr.aqspm10.met.ss  misr.aqspm10.match.stn, misr.aqspm10.match.met.stn
misr.aqspm10.points<-unique(misr.aqspm10.met.ss[,46:47])
gam.pred.pm10.list<-vector('list',length(misr.aqspm10.points))
gam.resid.pm10.list<-vector('list',length(misr.aqspm10.points))
gam.pred.pm10.met.list<-vector('list',length(misr.aqspm10.points))
gam.resid.pm10.met.list<-vector('list',length(misr.aqspm10.points))

gam.pred.pm10.list2<-vector('list',length(misr.aqspm10.points))
gam.resid.pm10.list2<-vector('list',length(misr.aqspm10.points))
gam.pred.pm10.met.list2<-vector('list',length(misr.aqspm10.points))
gam.resid.pm10.met.list2<-vector('list',length(misr.aqspm10.points))

gam.pred.pm10.list3<-vector('list',length(misr.aqspm10.points))
gam.resid.pm10.list3<-vector('list',length(misr.aqspm10.points))
gam.pred.pm10.met.list3<-vector('list',length(misr.aqspm10.points))
gam.resid.pm10.met.list3<-vector('list',length(misr.aqspm10.points))

for (i in 1:dim(misr.aqspm10.points)[1]){
  location.sample2<-misr.aqspm10.points[i,]

  train2<-misr.aqspm10.ss[!(misr.aqspm10.ss$x.1 %in% location.sample2$x.1
                                & misr.aqspm10.ss$y.1 %in% location.sample2$y.1), ]
  test2<-misr.aqspm10.ss[(misr.aqspm10.ss$x.1 %in% location.sample2$x.1
                              & misr.aqspm10.ss$y.1 %in% location.sample2$y.1), ]
  
  train3<-misr.aqspm10.met.ss[!(misr.aqspm10.met.ss$x.1 %in% location.sample2$x.1
                              & misr.aqspm10.met.ss$y.1 %in% location.sample2$y.1), ]

  test3<-misr.aqspm10.met.ss[(misr.aqspm10.met.ss$x.1 %in% location.sample2$x.1
                            & misr.aqspm10.met.ss$y.1 %in% location.sample2$y.1), ]


  gam.st.pm10<-gam(Daily.Mean.PM10.Concentration~s(AOD)+s(x.1,y.1,k=11)+s(julian2)+s(month,bs='cc')+as.factor(dow), na.action=na.exclude,data=train2)
  gam.st.pm10.met<-gam(Daily.Mean.PM10.Concentration.1~s(AOD)+s(x.1,y.1,k=9)+s(julian2)+s(month,bs='cc')+as.factor(dow)+atm.press+wind.sp, na.action=na.exclude,data=train3)

  gam.st2.pm10<-gam(Daily.Mean.PM10.Concentration~s(AODlarge)+s(x.1,y.1,k=11)+s(julian2)+s(month,bs='cc')+as.factor(dow), na.action=na.exclude,data=train2)
  gam.st2.pm10.met<-gam(Daily.Mean.PM10.Concentration.1~s(AODlarge)+s(x.1,y.1,k=9)+s(julian2)+s(month,bs='cc')+as.factor(dow)+atm.press+wind.sp, na.action=na.exclude,data=train3)

  gam.st3.pm10<-gam(Daily.Mean.PM10.Concentration~s(AODsm_med)+s(x.1,y.1,k=11)+s(julian2)+s(month,bs='cc')+as.factor(dow), na.action=na.exclude,data=train2)
  gam.st3.pm10.met<-gam(Daily.Mean.PM10.Concentration.1~s(AODsm_med)+s(x.1,y.1,k=9)+s(julian2)+s(month,bs='cc')+as.factor(dow)+atm.press+wind.sp, na.action=na.exclude,data=train3)
  
  
  gam.st.pred.pm10<-predict.gam(gam.st.pm10,newdata=test2)
  gam.st.pred.pm10.merge<-cbind(gam.st.pred.pm10,test2)
  gam.st.resid.pm10<-data.frame(resid=gam.st.pm10$residuals, y=gam.st.pm10$y,fitted=gam.st.pm10$fitted.values)

  gam.st.pred.pm10.met<-predict.gam(gam.st.pm10.met,newdata=test3)
  gam.st.pred.pm10.met.merge<-cbind(gam.st.pred.pm10.met,test3)
  gam.st.resid.pm10.met<-data.frame(resid=gam.st.pm10.met$residuals, y=gam.st.pm10.met$y,fitted=gam.st.pm10.met$fitted.values)

  gam.st2.pred.pm10<-predict.gam(gam.st2.pm10,newdata=test2)
  gam.st2.pred.pm10.merge<-cbind(gam.st2.pred.pm10,test2)
  gam.st2.resid.pm10<-data.frame(resid=gam.st2.pm10$residuals, y=gam.st2.pm10$y,fitted=gam.st2.pm10$fitted.values)

  gam.st2.pred.pm10.met<-predict.gam(gam.st2.pm10.met,newdata=test3)
  gam.st2.pred.pm10.met.merge<-cbind(gam.st2.pred.pm10.met,test3)
  gam.st2.resid.pm10.met<-data.frame(resid=gam.st2.pm10.met$residuals, y=gam.st2.pm10.met$y,fitted=gam.st2.pm10.met$fitted.values)

  gam.st3.pred.pm10<-predict.gam(gam.st3.pm10,newdata=test2)
  gam.st3.pred.pm10.merge<-cbind(gam.st3.pred.pm10,test2)
  gam.st3.resid.pm10<-data.frame(resid=gam.st3.pm10$residuals, y=gam.st2.pm10$y,fitted=gam.st2.pm10$fitted.values)
  
  gam.st3.pred.pm10.met<-predict.gam(gam.st3.pm10.met,newdata=test3)
  gam.st3.pred.pm10.met.merge<-cbind(gam.st3.pred.pm10.met,test3)
  gam.st3.resid.pm10.met<-data.frame(resid=gam.st3.pm10.met$residuals, y=gam.st3.pm10.met$y,fitted=gam.st3.pm10.met$fitted.values)
  
  gam.pred.pm10.list[[i]]<-gam.st.pred.pm10.merge
  gam.pred.pm10.met.list[[i]]<-gam.st.pred.pm10.met.merge
  gam.resid.pm10.list[[i]]<-gam.st.resid.pm10
  gam.resid.pm10.met.list[[i]]<-gam.st.resid.pm10.met

  gam.pred.pm10.list2[[i]]<-gam.st2.pred.pm10.merge
  gam.pred.pm10.met.list2[[i]]<-gam.st2.pred.pm10.met.merge
  gam.resid.pm10.list2[[i]]<-gam.st2.resid.pm10
  gam.resid.pm10.met.list2[[i]]<-gam.st2.resid.pm10.met
  
  gam.pred.pm10.list3[[i]]<-gam.st3.pred.pm10.merge
  gam.pred.pm10.met.list3[[i]]<-gam.st3.pred.pm10.met.merge
  gam.resid.pm10.list3[[i]]<-gam.st3.resid.pm10
  gam.resid.pm10.met.list3[[i]]<-gam.st3.resid.pm10.met

}
gam.pred.pm10 <- do.call("rbind", gam.pred.pm10.list) 
gam.pred.pm10.met <- do.call("rbind", gam.pred.pm10.met.list) 
gam.pred2.pm10 <- do.call("rbind", gam.pred.pm10.list2) 
gam.pred2.pm10.met <- do.call("rbind", gam.pred.pm10.met.list2) 
gam.pred3.pm10 <- do.call("rbind", gam.pred.pm10.list3) 
gam.pred3.pm10.met <- do.call("rbind", gam.pred.pm10.met.list3) 

obs.pred.AOD<-lm(Daily.Mean.PM10.Concentration~gam.st.pred.pm10, data=gam.pred.pm10)
obs.pred.AODmet<-lm(Daily.Mean.PM10.Concentration~gam.st.pred.pm10.met, data=gam.pred.pm10.met)

obs.pred.AODlarge<-lm(Daily.Mean.PM10.Concentration~gam.st2.pred.pm10, data=gam.pred2.pm10)
obs.pred.AODlargemet<-lm(Daily.Mean.PM10.Concentration~gam.st2.pred.pm10.met, data=gam.pred2.pm10.met)

obs.pred.AODsm_med<-lm(Daily.Mean.PM10.Concentration~gam.st3.pred.pm10, data=gam.pred3.pm10)
obs.pred.AODsm_medmet<-lm(Daily.Mean.PM10.Concentration~gam.st3.pred.pm10.met, data=gam.pred3.pm10.met)


p9<-qplot(gam.st.pred.pm10,Daily.Mean.PM10.Concentration, data=gam.pred.pm10,xlab="Predicted",ylab="Observed")
plot9<-p9+stat_smooth(method='lm',formula=y~x-1,col='red')+stat_smooth(method='lm',formula=y~x,col='blue')
plot9

p10<-qplot(gam.st.pred.pm10.met,Daily.Mean.PM10.Concentration, data=gam.pred.pm10.met,xlab="Predicted",ylab="Observed",ylim=c(0,80),xlim=c(0,65))
plot10<-p10+stat_smooth(method='lm',formula=y~x-1,col='red')+stat_smooth(method='lm',formula=y~x,col='blue')
plot10

p11<-qplot(gam.st2.pred.pm10,Daily.Mean.PM10.Concentration, data=gam.pred2.pm10,xlab=expression('Predicted PM'[10]*', ug/m'^3),ylab=expression('Observed PM'[10]*', ug/m'^3))
plot11<-p11+stat_smooth(method='lm',formula=y~x-1,col='red')+stat_smooth(method='lm',formula=y~x,col='blue')
plot11

p12<-qplot(gam.st2.pred.pm10.met,Daily.Mean.PM10.Concentration, data=gam.pred2.pm10.met,xlab="Predicted",ylab="Observed")
plot12<-p12+stat_smooth(method='lm',formula=y~x-1,col='red')+stat_smooth(method='lm',formula=y~x,col='blue')
plot12

pdf('MISR.PM25.PM10.CV_new.pdf')
grid.arrange(plot6,plot11,nrow=2,ncol=1)
dev.off()



pred.site3<-gam.pred2.pm10[gam.pred2.pm10$AQS_SITE_ID==060379033,] # "06-071-9004" San Bernadino
pred.site4<-gam.pred2.pm10[gam.pred2.pm10$AQS_SITE_ID==60374002,]

pdf('MISR.PM10.AODlarge.CV.OC.pdf')
ggplot(data = pred.site3, aes(as.Date(date2,"%Y-%m-%d"))) + 
  geom_line(aes(y= gam.st.pred.pm10, color="red")) + 
  geom_line(aes(y=Daily.Mean.PM10.Concentration)) +
  geom_point(aes(y=Daily.Mean.PM10.Concentration)) +
  geom_point(aes(y= gam.st.pred.pm10, color="red"))+
  theme(legend.position="none",
        axis.line = element_line(colour = "black"),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())+
  labs(x = "Date",y=expression('PM'[10]*', ug/m'^3))
dev.off()

pdf('MISR.PM25.PM10.CV.3.pdf')
ggplot(data = pred.site3, aes(as.Date(date2,"%Y-%m-%d"))) + 
  geom_line(aes(y= gam.st2.pred.pm10, color="red")) + 
  geom_line(aes(y=Daily.Mean.PM10.Concentration)) +
  geom_point(aes(y=Daily.Mean.PM10.Concentration)) +
  geom_point(aes(y= gam.st2.pred.pm10, color="red"))+
  geom_line(data=pred.site1,aes(y=gam.st.pred.pm25.met,color="blue")) +
  geom_point(data=pred.site1,aes(y=gam.st.pred.pm25.met,color="blue")) +
  geom_line(data=pred.site1,aes(y=Daily.Mean.PM2.5.Concentration)) +
  geom_point(data=pred.site1,aes(y=Daily.Mean.PM2.5.Concentration)) +
  theme(legend.position="none",
        axis.line = element_line(colour = "black"),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())+
  labs(x = "Date",y=expression('PM, ug/m'^3))
dev.off()


##### Use s-t model to predict PM2.5 and PM10 from full AOD ###

count(misr.08.09,"date")

# PM2.5 model
gam.st.pm25<-gam(Daily.Mean.PM2.5.Concentration~s(AOD)+s(x.1,y.1,k=16)+s(julian2)+s(month,bs="cc")+as.factor(dow), na.action=na.exclude,data=misr.aqspm25.ss)
# make predictions for all days in spring 2009 and then take average

# misr data for spring 2008
misr.spring<-misr.08.09[misr.08.09$month %in% c(3,4,5),]
misr.summer<-misr.08.09[misr.08.09$year==2008 & misr.08.09$month %in% c(6,7,8),]

misr.days<-misr.08.09 %>% distinct(date2) 
pm25.predicted<-vector('list',length(misr.days$date))
for (i in 1:length(misr.days$date)){
  misr.daily<-misr.08.09[misr.08.09$day %in% misr.days[i,]$day &
                           misr.08.09$month %in% misr.days[i,]$month &
                           misr.08.09$year %in% misr.days[i,]$year ,] 
  misr.daily$julian2<-misr.daily$julian/10000
  misr.daily$x.1<-misr.daily$x
  misr.daily$y.1<-misr.daily$y
  misr.daily$dow<-(weekdays(as.Date(misr.daily$date,"%m/%d/%y")))
  predicted.pm25<-predict.gam(gam.st.pm25, newdata=misr.daily)
  misr.daily$predicted.pm25<-predicted.pm25
  pm25.predicted[[i]]<-misr.daily
}

pm25.predicted.all <- do.call("rbind", pm25.predicted)
pm25.predicted.all<-pm25.predicted.all[pm25.predicted.all$AOD<1,]
pm25.predicted.all<-pm25.predicted.all[pm25.predicted.all$predicted.pm25>0,]
write.csv(pm25.predicted.all, "/Users/mf/Documents/MISR/Data/predicted_pm25data.csv")
pm25.predicted.all<-read.csv("/Users/mf/Documents/MISR/Data/predicted_pm25data.csv")

pm25.pred.summer08<-pm25.predicted.all[pm25.predicted.all$year==2008 & pm25.predicted.all$month %in% c(6,7,8),]
pm25.pred.spring08<-pm25.predicted.all[pm25.predicted.all$year==2008 & pm25.predicted.all$month %in% c(3,4,5),]
pm25.pred.fall08<-pm25.predicted.all[pm25.predicted.all$year==2008 & pm25.predicted.all$month %in% c(9,10,11),]
pm25.pred.winter08<-pm25.predicted.all[pm25.predicted.all$year==2008 & pm25.predicted.all$month %in% c(12),]
pm25.pred.winter09<-pm25.predicted.all[pm25.predicted.all$year==2009 & pm25.predicted.all$month %in% c(1,2),]
pm25.pred.winter<-rbind(pm25.pred.winter08,pm25.pred.winter09)

pm25.pred.warm<-pm25.predicted.all[pm25.predicted.all$year==2008 & pm25.predicted.all$month %in% c(6,7,8,9),]
pm25.pred.cool1<-pm25.predicted.all[pm25.predicted.all$year==2008 & pm25.predicted.all$month %in% c(1,2,3),]
pm25.pred.cool2<-pm25.predicted.all[pm25.predicted.all$year==2009 & pm25.predicted.all$month %in% c(1,2,3),]
pm25.pred.cool<-rbind(pm25.pred.cool1,pm25.pred.cool2)

pm25.obs.summer08<-misr.aqspm25[misr.aqspm25$year==2008 & misr.aqspm25$month %in% c(6,7,8),]
pm25.obs.winter09<-misr.aqspm25[misr.aqspm25$year==2009 & misr.aqspm25$month %in% c(1,2,3),]  
#pm25.obs.winter09<-misr.aqspm25[misr.aqspm25$year==2009 & misr.aqspm25$month %in% c(1,2,3),]  
pm25.obs.winter<-rbind(pm25.obs.winter08,pm25.obs.winter09)
#take average by misr grid cell
misr.2008<-ddply(pm25.pred.08, .(lat,lon), summarise, AOD=mean(AOD,na.rm=TRUE),PM25=mean(predicted.pm25,na.rm=TRUE))

misr.winter.avg<- ddply(pm25.pred.winter, .(lat,lon), summarise, AOD=mean(AOD,na.rm=TRUE),PM25=mean(predicted.pm25,na.rm=TRUE))
misr.summer08.avg<- ddply(pm25.pred.summer08, .(lat,lon), summarise, AOD=mean(AOD,na.rm=TRUE),PM25=mean(predicted.pm25,na.rm=TRUE))
misr.spring08.avg<- ddply(pm25.pred.spring08, .(lat,lon), summarise, AOD=mean(AOD,na.rm=TRUE),PM25=mean(predicted.pm25,na.rm=TRUE))
misr.fall08.avg<- ddply(pm25.pred.fall08, .(lat,lon), summarise, AOD=mean(AOD,na.rm=TRUE),PM25=mean(predicted.pm25,na.rm=TRUE))

misr.warm08.avg<- ddply(pm25.pred.warm, .(lat,lon), summarise, AOD=mean(AOD,na.rm=TRUE),PM25=mean(predicted.pm25,na.rm=TRUE))
misr.cool.avg<- ddply(pm25.pred.cool2, .(lat,lon), summarise, AOD=mean(AOD,na.rm=TRUE),PM25=mean(predicted.pm25,na.rm=TRUE))

obs.summer08.avg<- ddply(pm25.obs.summer08, .(SITE_LATITUDE2,SITE_LONGITUDE2), summarise, PM25obs=mean(Daily.Mean.PM2.5.Concentration,na.rm=TRUE))
obs.winter.avg<- ddply(pm25.obs.winter, .(SITE_LATITUDE2,SITE_LONGITUDE2), summarise, PM25obs=mean(Daily.Mean.PM2.5.Concentration,na.rm=TRUE))


write.csv(misr.winter.avg,"predPM_winter.csv")
write.csv(misr.summer08.avg,"predPM_summer08.csv")
write.csv(misr.spring08.avg,"predPM_spring08.csv")
write.csv(misr.fall08.avg,"predPM_fall08.csv")
write.csv(misr.warm08.avg,"predPM_warm08.csv")
write.csv(misr.cool.avg,"predPM_cool.csv")


map <- get_map(c(lon=-118.3, lat=34.1), zoom = 7 , maptype = 'hybrid')


ggmap(map,extent="panel",legend="bottomleft") + geom_point(data = misr.warm08.avg, aes(x = lon, y = lat, color=PM25),  size=1.1, alpha=0.5)+ 
  scale_color_gradientn(colours=matlab.like(20), limits=c(5,20),breaks=c(5,10,15,20,25))

ggmap(map,extent="panel",legend="bottomleft") + geom_point(data = misr.winter.avg, aes(x = lon, y = lat, color=PM25),  size=1.1, alpha=0.5)+ 
  scale_color_gradientn(colours=matlab.like(20), limits=c(5,25),breaks=c(5,10,15,20,25)) 

pdf("summer08_aod.pdf")
ggmap(map,extent="panel",legend="bottomleft") + geom_point(data = misr.summer08.avg, 
                                                           aes(x = lon, y = lat,color=AOD), size=1.1,alpha=0.5)+
  scale_color_gradientn(colours=matlab.like(20), limits=c(0,0.6),breaks=c(0,0.1,0.2,0.3,0.4,0.5))

qp1 <- qplot(pm25.pred.summer08$AOD,geom="histogram",xlab="AOD",ylab="count",bins=55, col="red", fill="red", alpha = .2)+
  theme(plot.background = element_rect(fill = "transparent",colour = NA),text=element_text(colour="grey"),
        axis.text=element_text(colour="white"),legend.position = "none")
print(qp1, vp=viewport(.25, .3, .33, .37))
dev.off()

pdf("cool09_aod.pdf")
ggmap(map,extent="panel",legend="bottomleft") + geom_point(data = misr.cool.avg, 
                                                           aes(x = lon, y = lat,color=AOD), size=1.1,alpha=0.5)+
  scale_color_gradientn(colours=matlab.like(20), limits=c(0,0.5),breaks=c(0,0.1,0.2,0.3,0.4))

qp2 <- qplot(pm25.pred.cool2$AOD,geom="histogram",xlab="AOD",ylab="count",bins=55, col="red", fill="red", alpha = .2)+
  theme(plot.background = element_rect(fill = "transparent",colour = NA),text=element_text(colour="grey"),
        axis.text=element_text(colour="white"),legend.position = "none")
print(qp2, vp=viewport(.25, .3, .33, .37))
dev.off()


pdf("summer08_pm25.pdf")
ggmap(map,extent="panel",legend="bottomleft") + geom_point(data = misr.summer08.avg, 
                                                           aes(x = lon, y = lat,color=PM25), size=1.1,alpha=0.5)+
  scale_color_gradientn(colours=matlab.like(20), limits=c(0,25),breaks=c(0,5,10,15,20))

qp3 <- qplot(pm25.pred.summer08$predicted.pm25,geom="histogram",xlab="PM2.5",ylab="count",bins=55, col="red", fill="red", alpha = .2)+
  theme(plot.background = element_rect(fill = "transparent",colour = NA),text=element_text(colour="grey"),
        axis.text=element_text(colour="white"),legend.position = "none")
print(qp3, vp=viewport(.25, .3, .33, .37))
dev.off()

pdf("cool09_pm25.pdf")
ggmap(map,extent="panel",legend="bottomleft") + geom_point(data = misr.cool.avg, 
                                                           aes(x = lon, y = lat,color=PM25), size=1.1,alpha=0.5)+
  scale_color_gradientn(colours=matlab.like(20), limits=c(0,30),breaks=c(0,5,10,15,20,25))

qp4 <- qplot(pm25.pred.cool2$predicted.pm25,geom="histogram",xlab="PM25",ylab="count",bins=55, col="red", fill="red", alpha = .2)+
  theme(plot.background = element_rect(fill = "transparent",colour = NA),text=element_text(colour="grey"),
        axis.text=element_text(colour="white"),legend.position = "none")
print(qp4, vp=viewport(.25, .3, .33, .37))
dev.off()



misr.04.21.08<-misr.08.09[misr.08.09$year==2008 & misr.08.09$month==4 & misr.08.09$day==21,]
misr.04.21.08$julian2<-misr.04.21.08$julian/10000
misr.04.21.08$x.1<-misr.04.21.08$x
misr.04.21.08$y.1<-misr.04.21.08$y
misr.04.21.08$dow<-(weekdays(as.Date(misr.04.21.08$date,"%m/%d/%y")))

predicted.pm25.04.21.08<-predict.gam(gam.st.pm25, newdata=misr.04.21.08)
# merge with data
misr.04.21.08$predPM25<-predicted.pm25.04.21.08
misr.04.21.08<-misr.04.21.08[misr.04.21.08$AOD<1,]
misr.04.21.08<-misr.04.21.08[misr.04.21.08$predPM25>0,]
write.csv(misr.04.21.08,"predicted_pm25_misr_04_21_08.csv")

misr.06.27.09<-misr.08.09[misr.08.09$year==2009 & misr.08.09$month==6 & misr.08.09$day==27,]
misr.06.27.09$julian2<-misr.06.27.09$julian/10000
misr.06.27.09$x.1<-misr.06.27.09$x
misr.06.27.09$y.1<-misr.06.27.09$y
misr.06.27.09$dow<-(weekdays(as.Date(misr.06.27.09$date,"%m/%d/%y")))

predicted.pm25.06.27.09<-predict.gam(gam.st.pm25, newdata=misr.06.27.09)

# merge with data
misr.06.27.09$predPM25<-predicted.pm25.06.27.09
write.csv(misr.06.27.09,"predicted_pm25_misr_06_27_09.csv")


misr.08.14.09<-misr.08.09[misr.08.09$year==2009 & misr.08.09$month==8 & misr.08.09$day==14,]
misr.08.14.09$julian2<-misr.08.14.09$julian/10000
misr.08.14.09$x.1<-misr.08.14.09$x
misr.08.14.09$y.1<-misr.08.14.09$y
misr.08.14.09$dow<-(weekdays(as.Date(misr.08.14.09$date,"%m/%d/%y")))

predicted.pm25.08.14.09<-predict.gam(gam.st.pm25, newdata=misr.08.14.09)

# merge with data
misr.08.14.09$predPM25<-predicted.pm25.08.14.09
write.csv(misr.08.14.09,"predicted_pm25_misr_08_14_09.csv")



misr.02.19.09<-misr.08.09[misr.08.09$year==2009 & misr.08.09$month==2 & misr.08.09$day==19,]
misr.02.19.09$julian2<-misr.02.19.09$julian/10000
misr.02.19.09$x.1<-misr.02.19.09$x
misr.02.19.09$y.1<-misr.02.19.09$y
misr.02.19.09$dow<-(weekdays(as.Date(misr.02.19.09$date,"%m/%d/%y")))

predicted.pm25.02.19.09<-predict.gam(gam.st.pm25, newdata=misr.02.19.09)

# merge with data
misr.02.19.09$predPM25<-predicted.pm25.02.19.09
misr.02.19.09<-misr.02.19.09[misr.02.19.09$predPM25>0,]
write.csv(misr.02.19.09,"predicted_pm25_misr_02_19_09.csv")


# PM10 model
gam.st.pm10<-gam(Daily.Mean.PM10.Concentration~s(AODlarge)+s(x.1,y.1,k=12)+s(julian2)+s(month,bs="cc")+as.factor(dow), na.action=na.exclude,data=misr.aqspm10.ss)

predicted.pm10.04.21.08<-predict.gam(gam.st.pm10, newdata=misr.04.21.08)
# merge with data
misr.04.21.08$predPM10<-predicted.pm10.04.21.08
write.csv(misr.04.21.08,"predicted_pm_misr_04_21_08.csv")
