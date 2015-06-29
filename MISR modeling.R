##############################################
# MISR 2008-2009 4km data analysis
# February-June 2015
# Meredith Franklin
##############################################
library(mgcv)
library(ggplot2)
setwd("/Users/mf/Documents/MISR/Reports")
# Matched MISR-AQS datasets
misr.aqspm25<-read.csv("/Users/mf/Documents/MISR/Data/misr_aqspm25.csv")
misr.aqspm10<-read.csv("/Users/mf/Documents/MISR/Data/misr_aqspm10.csv")
misr.stn<-read.csv("/Users/mf/Documents/MISR/Data/misr_aqsstn.csv")

# Matched MISR-AQS-MET datasets
misr.aqspm25.met<-read.csv("/Users/mf/Documents/MISR/Data/misr_aqspm25_met.csv")
misr.aqspm10.met<-read.csv("/Users/mf/Documents/MISR/Data/misr_aqspm10_met.csv")
misr.stn.met<-read.csv("/Users/mf/Documents/MISR/Data/misr_aqsstn_met.csv")

# Visualizations and regressions

# Remove AOD greater than 1
misr.aqspm25.ss<-misr.aqspm25[misr.aqspm25$AOD<1,]
misr.aqspm25.ss2<-misr.aqspm25.ss[misr.aqspm25.ss$AODlargefrac>0,]
misr.aqspm10.ss<-misr.aqspm10[misr.aqspm10$AOD<1,]
misr.aqspm10.ss2<-misr.aqspm10.ss[misr.aqspm10.ss$AODlargefrac>0,]
# Create new Julian date for time indexing, divide by 10000
misr.aqspm25.ss$julian2<-misr.aqspm25.ss$julian/10000
misr.aqspm10.ss$julian2<-misr.aqspm10.ss$julian/10000
misr.stn$julian2<-misr.stn$julian/10000

# Remove AOD greater than 1
misr.aqspm25.met.ss<-misr.aqspm25.met[misr.aqspm25.met$AOD<1,]
misr.aqspm25.met.ss2<-misr.aqspm25.met.ss[misr.aqspm25.met.ss$AODlargefrac>0,]
misr.aqspm10.met.ss<-misr.aqspm10.met[misr.aqspm10.met$AOD<1,]
misr.aqspm10.met.ss2<-misr.aqspm10.met.ss[misr.aqspm10.met.ss$AODlargefrac>0,]
# Create new Julian date for time indexing, divide by 10000
misr.aqspm25.met.ss$julian2<-misr.aqspm25.met.ss$julian/10000
misr.aqspm10.met.ss$julian2<-misr.aqspm10.met.ss$julian/10000
misr.aqsstn.met$julian2<-misr.aqsstn.met$julian/10000


#### MISR AOD and AQS PM2.5 ####
# Summary statistics
# Title (writes new file)
cat("PM25 Summary Stats", file = "SummaryStatsPM25.txt")
# add new lines
cat("\n", file = "SummaryStatsPM25.txt", append = TRUE)
# export anova test output
cat("Median AOD\n", file = "SummaryStatsPM25.txt", append = TRUE)
capture.output(median(misr.aqspm25.ss$AOD), file = "SummaryStatsPM25.txt", append = TRUE)
cat("IQR AOD\n", file = "SummaryStatsPM25.txt", append = TRUE)
capture.output(IQR(misr.aqspm25.ss$AOD), file = "SummaryStatsPM25.txt", append = TRUE)
cat("Mean PM25\n", file = "SummaryStatsPM25.txt", append = TRUE)
capture.output(mean(misr.aqspm25.ss$Daily.Mean.PM2.5.Concentration), file = "SummaryStatsPM25.txt", append = TRUE)
cat("St Dev PM25\n", file = "SummaryStatsPM25.txt", append = TRUE)
capture.output(sd(misr.aqspm25.ss$Daily.Mean.PM2.5.Concentration), file = "SummaryStatsPM25.txt", append = TRUE)
cat("Cor AOD PM25\n", file = "SummaryStatsPM25.txt", append = TRUE)
capture.output(cor(misr.aqspm25.ss$AOD,misr.aqspm25.ss$Daily.Mean.PM2.5.Concentration), file = "SummaryStatsPM25.txt", append = TRUE)
cat("Cor AODlarge PM25\n", file = "SummaryStatsPM25.txt", append = TRUE)
capture.output(cor(misr.aqspm25.ss2$AODlarge,misr.aqspm25.ss2$Daily.Mean.PM2.5.Concentration), file = "SummaryStatsPM25.txt", append = TRUE)
cat("Cor AODsmall PM25\n", file = "SummaryStatsPM25.txt", append = TRUE)
capture.output(cor(misr.aqspm25.ss$AODsmall,misr.aqspm25.ss$Daily.Mean.PM2.5.Concentration), file = "SummaryStatsPM25.txt", append = TRUE)
cat("Cor AOD PM25 met\n", file = "SummaryStatsPM25.txt", append = TRUE)
capture.output(cor(misr.aqspm25.met.ss$AOD,misr.aqspm25.met.ss$Daily.Mean.PM2.5.Concentration), file = "SummaryStatsPM25.txt", append = TRUE)
cat("Cor AODlarge PM25 met\n", file = "SummaryStatsPM25.txt", append = TRUE)
capture.output(cor(misr.aqspm25.met.ss$AODlarge,misr.aqspm25.met.ss$Daily.Mean.PM2.5.Concentration), file = "SummaryStatsPM25.txt", append = TRUE)
cat("Cor AODsmall PM25 met\n", file = "SummaryStatsPM25.txt", append = TRUE)
capture.output(cor(misr.aqspm25.met.ss$AODsmall,misr.aqspm25.met.ss$Daily.Mean.PM2.5.Concentration), file = "SummaryStatsPM25.txt", append = TRUE)

# Univariate Linear models
cat("linear mod AOD PM25\n", file = "SummaryStatsPM25.txt", append = TRUE)
capture.output(summary(lm(misr.aqspm25.ss$Daily.Mean.PM2.5.Concentration~(misr.aqspm25.ss$AOD))), file = "SummaryStatsPM25.txt", append = TRUE)
cat("linear mod logAOD PM25\n", file = "SummaryStatsPM25.txt", append = TRUE)
capture.output(summary(lm(misr.aqspm25.ss$Daily.Mean.PM2.5.Concentration~log(misr.aqspm25.ss$AOD))), file = "SummaryStatsPM25.txt", append = TRUE)

cat("linear mod AODlarge PM25\n", file = "SummaryStatsPM25.txt", append = TRUE)
capture.output(summary(lm(misr.aqspm25.ss2$Daily.Mean.PM2.5.Concentration~(misr.aqspm25.ss2$AODlarge))), file = "SummaryStatsPM25.txt", append = TRUE)
cat("linear mod logAODlarge PM25\n", file = "SummaryStatsPM25.txt", append = TRUE)
capture.output(summary(lm(misr.aqspm25.ss2$Daily.Mean.PM2.5.Concentration~log(misr.aqspm25.ss2$AODlarge))), file = "SummaryStatsPM25.txt", append = TRUE)

cat("linear mod AODsmall PM25\n", file = "SummaryStatsPM25.txt", append = TRUE)
capture.output(summary(lm(misr.aqspm25.ss$Daily.Mean.PM2.5.Concentration~(misr.aqspm25.ss$AODsmall))), file = "SummaryStatsPM25.txt", append = TRUE)
cat("linear mod logAODsmall PM25\n", file = "SummaryStatsPM25.txt", append = TRUE)
capture.output(summary(lm(misr.aqspm25.ss$Daily.Mean.PM2.5.Concentration~log(misr.aqspm25.ss$AODsmall))), file = "SummaryStatsPM25.txt", append = TRUE)

# Univariate GAM models
cat("GAM mod AOD PM25\n", file = "SummaryStatsPM25.txt", append = TRUE)
capture.output(summary(gam(misr.aqspm25.ss$Daily.Mean.PM2.5.Concentration~s(misr.aqspm25.ss$AOD))), file = "SummaryStatsPM25.txt", append = TRUE)
cat("GAM mod logAOD PM25\n", file = "SummaryStatsPM25.txt", append = TRUE)
capture.output(summary(gam(misr.aqspm25.ss$Daily.Mean.PM2.5.Concentration~s(log(misr.aqspm25.ss$AOD)))), file = "SummaryStatsPM25.txt", append = TRUE)

cat("GAM mod AODlarge PM25\n", file = "SummaryStatsPM25.txt", append = TRUE)
capture.output(summary(gam(Daily.Mean.PM2.5.Concentration~s(AODlarge), data=misr.aqspm25.ss2),na.action='na.exclude'), file = "SummaryStatsPM25.txt", append = TRUE)
cat("GAM mod logAODlarge PM25\n", file = "SummaryStatsPM25.txt", append = TRUE)
capture.output(summary(gam(Daily.Mean.PM2.5.Concentration~s(log(AODlarge)), data=misr.aqspm25.ss2),na.action='na.exclude'), file = "SummaryStatsPM25.txt", append = TRUE)

cat("GAM mod AODsmall PM25\n", file = "SummaryStatsPM25.txt", append = TRUE)
capture.output(summary(gam(Daily.Mean.PM2.5.Concentration~s(AODsmall), data=misr.aqspm25.ss2)), file = "SummaryStatsPM25.txt", append = TRUE)
cat("GAM mod logAODsmall PM25\n", file = "SummaryStatsPM25.txt", append = TRUE)
capture.output(summary(gam(Daily.Mean.PM2.5.Concentration~s(log(AODsmall)), data=misr.aqspm25.ss2)), file = "SummaryStatsPM25.txt", append = TRUE)

#### MISR AOD and AQS PM10 ####
# Summary statistics
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

# Univariate Linear models
cat("linear mod AOD PM10\n", file = "SummaryStatsPM10.txt", append = TRUE)
capture.output(summary(lm(misr.aqspm10.ss$Daily.Mean.PM10.Concentration~(misr.aqspm10.ss$AOD))), file = "SummaryStatsPM10.txt", append = TRUE)
cat("linear mod logAOD PM10\n", file = "SummaryStatsPM10.txt", append = TRUE)
capture.output(summary(lm(misr.aqspm10.ss$Daily.Mean.PM10.Concentration~log(misr.aqspm10.ss$AOD))), file = "SummaryStatsPM10.txt", append = TRUE)

cat("linear mod AODlarge PM10\n", file = "SummaryStatsPM10.txt", append = TRUE)
capture.output(summary(lm(misr.aqspm10.ss2$Daily.Mean.PM10.Concentration~(misr.aqspm10.ss2$AODlarge))), file = "SummaryStatsPM10.txt", append = TRUE)
cat("linear mod logAODlarge PM10\n", file = "SummaryStatsPM10.txt", append = TRUE)
capture.output(summary(lm(misr.aqspm10.ss2$Daily.Mean.PM10.Concentration~log(misr.aqspm10.ss2$AODlarge))), file = "SummaryStatsPM10.txt", append = TRUE)

cat("linear mod AODsmall PM10\n", file = "SummaryStatsPM10.txt", append = TRUE)
capture.output(summary(lm(misr.aqspm10.ss$Daily.Mean.PM10.Concentration~(misr.aqspm10.ss$AODsmall))), file = "SummaryStatsPM10.txt", append = TRUE)
cat("linear mod logAODsmall PM10\n", file = "SummaryStatsPM10.txt", append = TRUE)
capture.output(summary(lm(misr.aqspm10.ss$Daily.Mean.PM10.Concentration~log(misr.aqspm10.ss$AODsmall))), file = "SummaryStatsPM10.txt", append = TRUE)

# Univariate GAM models
cat("GAM mod AOD PM10\n", file = "SummaryStatsPM10.txt", append = TRUE)
capture.output(summary(gam(misr.aqspm10.ss$Daily.Mean.PM10.Concentration~s(misr.aqspm10.ss$AOD))), file = "SummaryStatsPM10.txt", append = TRUE)
cat("GAM mod logAOD PM10\n", file = "SummaryStatsPM10.txt", append = TRUE)
capture.output(summary(gam(misr.aqspm10.ss$Daily.Mean.PM10.Concentration~s(log(misr.aqspm10.ss$AOD)))), file = "SummaryStatsPM10.txt", append = TRUE)

cat("GAM mod AODlarge PM10\n", file = "SummaryStatsPM10.txt", append = TRUE)
capture.output(summary(gam(Daily.Mean.PM10.Concentration~s(AODlarge, k=4), data=misr.aqspm10.ss2),na.action='na.exclude'), file = "SummaryStatsPM10.txt", append = TRUE)
cat("GAM mod logAODlarge PM10\n", file = "SummaryStatsPM10.txt", append = TRUE)
capture.output(summary(gam(Daily.Mean.PM10.Concentration~s(log(AODlarge)), data=misr.aqspm10.ss2),na.action='na.exclude'), file = "SummaryStatsPM10.txt", append = TRUE)

cat("GAM mod AODsmall PM10\n", file = "SummaryStatsPM10.txt", append = TRUE)
capture.output(summary(gam(Daily.Mean.PM10.Concentration~s(AODsmall), data=misr.aqspm10.ss2)), file = "SummaryStatsPM10.txt", append = TRUE)
cat("GAM mod logAODsmall PM10\n", file = "SummaryStatsPM10.txt", append = TRUE)
capture.output(summary(gam(Daily.Mean.PM10.Concentration~s(log(AODsmall)), data=misr.aqspm10.ss2)), file = "SummaryStatsPM10.txt", append = TRUE)

# Plots
png('MISR.AOD.PM25.png')
p<-qplot(AOD,Daily.Mean.PM2.5.Concentration, data=misr.aqspm25.ss,xlab="MISR AOD",ylab="AQS PM2.5 (ug/m3)")
p+stat_smooth(method="gam",formula=y~s(x)) +stat_smooth(method='lm',formula=y~x,col='red')
dev.off()
png('MISR.AOD.PM10.png')
p<-qplot(AOD,Daily.Mean.PM10.Concentration, data=misr.aqspm10.ss2,xlab="MISR AOD",ylab="AQS PM10 (ug/m3)")
p+stat_smooth(method="gam",formula=y~s(x,k=4)) +stat_smooth(method='lm',formula=y~x,col='red')
dev.off()

# Spatio-temporal regression on matched data
gam.st.aod.pm25<-gam(Daily.Mean.PM2.5.Concentration~s(AOD)+s(x.1,y.1,k=16)+s(julian2), na.action=na.exclude,data=misr.aqspm25.ss)
gam.st.aodlarge.pm25<-gam(Daily.Mean.PM2.5.Concentration~s(AODlarge)+s(x.1,y.1,k=16)+s(julian2), na.action=na.exclude,data=misr.aqspm25.ss2)

gam.st.aod.pm10<-gam(Daily.Mean.PM10.Concentration~s(AOD)+s(x.1,y.1,k=12)+s(julian2), na.action=na.exclude,data=misr.aqspm10.ss)
gam.st.aodlarge.pm10<-gam(Daily.Mean.PM10.Concentration~AODlarge+s(x.1,y.1,k=12)+s(julian2), na.action=na.exclude,data=misr.aqspm10.ss2)

# Spatio-temporal regression on matched data adjustment for meteorology
gam.st.aod.pm25.met<-gam(Daily.Mean.PM2.5.Concentration~s(AOD)+s(x.1,y.1,k=16)+s(julian2)+dew.point+wind.sp, na.action=na.exclude,data=misr.aqspm25.met.ss)
gam.st.aodlarge.pm25.met<-gam(Daily.Mean.PM2.5.Concentration~s(AODlarge)+s(x.1,y.1,k=12)+s(julian2)+dew.point+wind.sp, na.action=na.exclude,data=misr.aqspm25.met.ss2)

gam.st.aod.pm10.met<-gam(Daily.Mean.PM10.Concentration~log(AOD)+s(x.1,y.1,k=11)+s(julian2)+wind.sp+atm.press, na.action=na.exclude,data=misr.aqspm10.met.ss)
gam.st.aodlarge.pm10.met<-gam(Daily.Mean.PM10.Concentration~log(AODlarge)+s(x.1,y.1,k=11)+s(julian2)+wind.sp+atm.press, na.action=na.exclude,data=misr.aqspm10.met.ss2)

# MISR AOD and STN
# Remove AOD greater than 1
MISR.STN.ss<-MISR.STN[MISR.STN$AOD<1,]
# Create new Julian date for time indexing, divide by 10000
MISR.STN.ss$julian2<-MISR.STN.ss$julian/10000

plot(MISR.STN.ss$AOD,MISR.STN.ss$OC,xlab='MISR AOD',ylab='STN OC')
abline(lm(OC~AOD, data=MISR.STN.ss), col="red")

plot(MISR.STN.ss$AOD,MISR.STN.ss$EC,xlab='MISR AOD',ylab='STN EC')
abline(lm(EC~AOD, data=MISR.STN.ss), col="red")

plot(MISR.STN.ss$AOD,MISR.STN.ss$SO2,xlab='MISR AOD',ylab='STN SO4')
abline(lm(SO2~AOD, data=MISR.STN.ss), col="red")

plot(MISR.STN.ss$AOD,MISR.STN.ss$NH4,xlab='MISR AOD',ylab='STN NH4')
abline(lm(NH4~AOD, data=MISR.STN.ss), col="red")

OC.mod<-gam(OC~AOD, data=MISR.STN.ss)
SO4.mod<-gam(SO2~AOD, data=MISR.STN.ss)
NH4.mod<-gam(NH4~AOD, data=MISR.STN.ss)

# Regressions
OCmod<-gam(OC~AOD+s(x,y,k=10), data=MISR.STN.ss)
ECmod<-gam(EC~s(AOD)+julian, data=MISR.STN.ss)
SO4mod<-gam(SO2~s(AOD), data=MISR.STN.ss)
NH4mod<-gam(NH4~s(AOD), data=MISR.STN.ss)
# Cross validation


# Use s-t model to predict PM2.5 from full AOD
misr.04.21.08<-misr.08.09[misr.08.09$year==2008 & misr.08.09$month==4 & misr.08.09$day==21,]
misr.04.21.08$julian2<-misr.04.21.08$julian/10000
misr.04.21.08<-misr.04.21.08[,-1]
predicted.pm25.04.21.08<-predict.gam(gam.st.MISR.AOD2, newdata=misr.04.21.08)
# merge with data
misr.04.21.08$predPM25<-predicted.pm25.04.21.08

gam.st.MISR.AOD2log<-gam(log(Daily.Mean.PM2.5.Concentration)~s(AOD,k=10)+s(x,y)+s(julian2), na.action=na.exclude,data=MISR.AQS.ss)
predicted.pm25.04.21.08<-predict.gam(gam.st.MISR.AOD2log, newdata=misr.04.21.08)
# merge with data
misr.04.21.08$predPM25log<-exp(predicted.pm25.04.21.08)



 # Cross Validation PM25
gam.pred.list<-vector('list',length(10))
gam.cor.list<-vector('list',length(10))
gam.resid.list<-vector('list',length(20))
for (i in 1:20){
  location.sample<-MISR.AQS.met.ss[sample(nrow(MISR.AQS.met.ss),1),]
  #don't want to sample a background location
  print(location.sample)
  train<-MISR.AQS.met.ss[!(MISR.AQS.met.ss$x %in% location.sample$x
                           & MISR.AQS.met.ss$y %in% location.sample$y), ]
  test<-MISR.AQS.met.ss[(MISR.AQS.met.ss$x %in% location.sample$x
                         & MISR.AQS.met.ss$y %in% location.sample$y), ]
  #gam.st<-gam(Daily.Mean.PM2.5.Concentration~s(x,y)+s(AOD,k=6)+s(julian2),method='REML',data=train)
  gam.st<-gam(Daily.Mean.PM2.5.Concentration~s(AOD)+s(x,y)+s(julian2)+dew.point, na.action=na.exclude,data=train)
  #print(summary(gam.st))
  gam.st.pred<-predict.gam(gam.st,newdata=test)
  gam.st.pred.merge<-cbind(gam.st.pred,test)
  gam.st.resid<-data.frame(resid=gam.st$residuals, y=gam.st$y,fitted=gam.st$fitted.values)
  RMSE<-sqrt(sum((gam.st.resid$fitted-gam.st.resid$y)^2)/dim(gam.st.resid)[1])
  #print(RMSE)
  #print(mse(gam.st.pred.merge$PM25, gam.st.pred.merge$gam.st.pred))
  gam.cor.list[[i]]<-cor(gam.st.pred.merge$Daily.Mean.PM2.5.Concentration, gam.st.pred.merge$gam.st.pred)
  gam.pred.list[[i]]<-gam.st.pred.merge
  gam.resid.list[[i]]<-gam.st.resid
  #print(summary(gam.st)$r.sq)
}
unlist(gam.cor.list)
#RMSE for the model 
RMSE<-sqrt(sum((gam.resid.list[[3]]$fitted.values-gam.resid.list[[3]]$y)^2)/dim(gam.resid.list[[3]])[1])

i=1
View(gam.pred.list[[i]])
plot(gam.pred.list[[i]]$julian2,gam.pred.list[[i]]$Daily.Mean.PM2.5.Concentration,
     xlab="Time",main="PM2.5 observed (black) and predicted (red)",ylab="PM2.5,ug/m3",
     pch=19,cex=0.5)
points(gam.pred.list[[i]]$julian2,gam.pred.list[[i]]$gam.st.pred,col="red",pch=19,cex=0.5)
lines(gam.pred.list[[i]]$julian2,gam.pred.list[[i]]$gam.st.pred,col="red")
lines(gam.pred.list[[i]]$julian2,gam.pred.list[[i]]$Daily.Mean.PM2.5.Concentration,col="black")

# Spatio-temporal regression on matched data with meteorology
lm.MISR.AOD<-lm(Daily.Mean.PM2.5.Concentration.x~AOD, data=MISR.AQS.met.ss)
gam.st.MISR.AOD1<-gam(Daily.Mean.PM2.5.Concentration.x~s(AOD),na.action=na.exclude,data=MISR.AQS.met.ss)
gam.st.MISR.AOD.met<-gam(Daily.Mean.PM2.5.Concentration~s(AOD)+s(x,y)+s(julian2), na.action=na.exclude,data=MISR.AQS.met.ss)
gam.st.MISR.AOD.met2<-gam(Daily.Mean.PM2.5.Concentration~s(AOD)+s(x,y)+s(julian2)+dew.point, na.action=na.exclude,data=MISR.AQS.met.ss)
# best met model includes dew point and wind dir
gam.st.MISR.AOD.met3<-gam(Daily.Mean.PM2.5.Concentration~s(AOD)+s(x,y)+s(julian2)+dew.point+wind.sp, na.action=na.exclude,data=MISR.AQS.met.ss)


gam.st.MISR.AOD.met4<-gam(AOD~dew.point+atm.press+ceiling.ht+s(julian2), na.action=na.exclude,data=MISR.AQS.met.ss)
met.subset<-MISR.AQS.met.ss[,47:52]
cor(met.subset, use='complete')


gam.st.MISR.AODsmall.met3<-gam(Daily.Mean.PM2.5.Concentration~s(AODsmall)+s(x,y)+s(julian2)+dew.point+wind.sp, na.action=na.exclude,data=MISR.AQS.met.ss)

gam.st.MISR.AODlarge.met3<-gam(Daily.Mean.PM2.5.Concentration~s(AODlarge)+s(x,y)+s(julian2)+dew.point+wind.sp, na.action=na.exclude,data=MISR.AQS.met.ss)

plot(MISR.AQS.met.ss$AOD,MISR.AQS.met.ss$ceiling.ht)
plot(MISR.AQS.met.ss$Daily.Mean.PM2.5.Concentration,MISR.AQS.met.ss$temp)
hist(met.08.09$ceiling.ht,breaks=50)

