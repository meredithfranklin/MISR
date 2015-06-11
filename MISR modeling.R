##############################################
# MISR 2008-2009 4km data analysis
# February, March 2015
# Meredith Franklin
##############################################
library(mgcv)
library(ggplot2)
setwd("/Users/mf/Documents/MISR/Reports")
# Matched MISR-AQS data
MISR.AQS<-read.csv("/Users/mf/Documents/MISR/Data/MISR.AQS.csv")
MISR.AQS.met<-read.csv("/Users/mf/Documents/MISR/Data/MISR.AQS.met.csv")
MISR.STN<-read.csv("/Users/mf/Documents/MISR/Data/MISR.STN.csv")
MISR.ICV<-read.csv("/Users/mf/Documents/MISR/Data/MISR.ICV.csv")
# MISR data
misr.08.09<-read.csv("/Users/mf/Documents/MISR/Data/misr.08.09.csv")
# Visualizations and regressions

# Remove AOD greater than 1
MISR.AQS.ss<-MISR.AQS[MISR.AQS$AOD<1,]
# Create new Julian date for time indexing, divide by 10000
MISR.AQS.ss$julian2<-MISR.AQS.ss$julian/10000

MISR.AQS.met.ss<-MISR.AQS.met[MISR.AQS.met$AOD<1,]
MISR.AQS.met.ss$julian2<-MISR.AQS.met.ss$julian/10000
# Remove multiple observations in matched met-aqs-misr dataset
# MISR.AQS.met.ss$PMdiff<-MISR.AQS.met.ss$Daily.Mean.PM2.5.Concentration.x-MISR.AQS.met.ss$Daily.Mean.PM2.5.Concentration.y
MISR.AQS.met.ss<-MISR.AQS.met.ss[MISR.AQS.met.ss$PMdiff==0,]


# MISR AOD and AQS 
# Univariate relationship between AOD and PM25
cor(MISR.AQS.ss$AOD,MISR.AQS.ss$Daily.Mean.PM2.5.Concentration)
lm.MISR.AOD<-lm(Daily.Mean.PM2.5.Concentration~AOD, data=MISR.AQS.ss)
gam.MISR.AOD<-gam(Daily.Mean.PM2.5.Concentration~s(AOD), data=MISR.AQS.ss)

# Spatio-temporal regression on matched data
gam.st.MISR.AOD2<-gam(Daily.Mean.PM2.5.Concentration~s(AOD)+s(x,y)+s(julian2), na.action=na.exclude,data=MISR.AQS.ss)
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

png('MISR.AOD.PM25.png')
p<-qplot(AOD,Daily.Mean.PM2.5.Concentration, data=MISR.AQS.ss,xlab="MISR AOD",ylab="AQS PM2.5 (ug/m3)")
p+stat_smooth(method="gam",formula=y~s(x)) +stat_smooth(method='lm',formula=y~x,col='red')
dev.off()

# MISR AOD Small and AQS PM2.5
plot(MISR.AQS$AODsmall2,MISR.AQS$Daily.Mean.PM2.5.Concentration,xlab='MISR AOD small',ylab='AQS PM2.5')
abline(lm(Daily.Mean.PM2.5.Concentration~AODsmall, data=MISR.AQS), col="red")
cor(MISR.AQS$AODsmall,MISR.AQS$Daily.Mean.PM2.5.Concentration)
lm.MISR.AOD.small<-lm(Daily.Mean.PM2.5.Concentration~AODsmall, data=MISR.AQS)
gam.MISR.AOD.small<-gam(Daily.Mean.PM2.5.Concentration~s(AODsmall), data=MISR.AQS.ss)
gam.st.MISR.small<-gam(Daily.Mean.PM2.5.Concentration~s(AODsmall)+s(x,y)+s(julian2), na.action=na.exclude,data=MISR.AQS.ss)

pdf('MISR.AODsmall.PM25.pdf')
p<-qplot(AODsmall,log(Daily.Mean.PM2.5.Concentration), data=MISR.AQS.ss,xlab="MISR AOD small",ylab="AQS PM2.5 (ug/m3)")
p+stat_smooth(method="gam",formula=y~s(x,k=4)) +stat_smooth(method='lm',formula=y~x,col='red')
dev.off()

# MISR AOD Medium and AQS PM2.5
plot(MISR.AQS$AODmed,MISR.AQS$Daily.Mean.PM2.5.Concentration,xlab='MISR AOD',ylab='AQS PM2.5')
abline(lm(Daily.Mean.PM2.5.Concentration~AODmed, data=MISR.AQS), col="red")
cor(MISR.AQS$AODmed,MISR.AQS$Daily.Mean.PM2.5.Concentration)
lm.MISR.AOD.med<-lm(Daily.Mean.PM2.5.Concentration~AODmed, data=MISR.AQS)

# MISR AOD Large and AQS PM2.5
plot(MISR.AQS$AODlarge,MISR.AQS$Daily.Mean.PM2.5.Concentration,xlab='MISR AOD',ylab='AQS PM2.5')
abline(lm(Daily.Mean.PM2.5.Concentration~AODlarge, data=MISR.AQS), col="red")
cor(MISR.AQS$AODlarge,MISR.AQS$Daily.Mean.PM2.5.Concentration)
lm.MISR.AOD.large<-lm(Daily.Mean.PM2.5.Concentration~AODlarge, data=MISR.AQS)

gam.MISR.AOD.large<-gam(Daily.Mean.PM2.5.Concentration~s(AODlarge), data=MISR.AQS.ss)
gam.st.MISR.large<-gam(Daily.Mean.PM2.5.Concentration~s(AODlarge)+s(x,y)+s(julian2), na.action=na.exclude,data=MISR.AQS.ss)

pdf('MISR.AODlarge.PM25.pdf')
p<-qplot(AODlarge,log(Daily.Mean.PM2.5.Concentration), data=MISR.AQS.ss,xlab="MISR AOD large",ylab="AQS PM2.5 (ug/m3)")
p+stat_smooth(method="gam",formula=y~s(x,k=4)) +stat_smooth(method='lm',formula=y~x,col='red')
dev.off()

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


# MISR-ICV models
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

