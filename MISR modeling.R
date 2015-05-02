##############################################
# MISR 2008-2009 4km data analysis
# February, March 2015
# Meredith Franklin
##############################################
library(mgcv)
library(ggplot2)
# Matched MISR-AQS data
MISR.AQS<-read.csv("/Users/mf/Documents/MISR/Data/MISR.AQS.csv")
# MISR data
misr.08.09<-read.csv("/Users/mf/Documents/MISR/Data/misr.08.09.csv")
# Visualizations and regressions

# MISR AOD and AQS PM2.5
plot(MISR.AQS$AOD,MISR.AQS$Daily.Mean.PM2.5.Concentration,xlab='MISR AOD',ylab='AQS PM2.5')
abline(lm(Daily.Mean.PM2.5.Concentration~AOD, data=MISR.AQS), col="red")

# Remove AOD greater than 1
MISR.AQS.ss<-MISR.AQS[MISR.AQS$AOD<1,]
# Create new Julian date for time indexing, divide by 10000
MISR.AQS.ss$julian2<-MISR.AQS.ss$julian/10000

cor(MISR.AQS.ss$AOD,MISR.AQS.ss$Daily.Mean.PM2.5.Concentration)
lm.MISR.AOD<-lm(Daily.Mean.PM2.5.Concentration~AOD, data=MISR.AQS.ss)
gam.MISR.AOD<-gam(Daily.Mean.PM2.5.Concentration~s(AOD,k=10), data=MISR.AQS.ss)

# Spatio-temporal regression on matched data
gam.st.MISR.AOD2<-gam(Daily.Mean.PM2.5.Concentration~s(AOD,k=10)+s(x,y)+s(julian2), na.action=na.exclude,data=MISR.AQS.ss)
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


pdf('MISR.AOD.plot1.pdf')
p<-qplot(AOD,Daily.Mean.PM2.5.Concentration, data=MISR.AQS.ss,xlab="MISR AOD",ylab="AQS PM2.5 (ug/m3)")
p+stat_smooth(method="gam",formula=y~s(x,k=10)) +stat_smooth(method='lm',formula=y~x,col='red')
dev.off()

# MISR AOD Small and AQS PM2.5
plot(MISR.AQS$AODsmall2,MISR.AQS$Daily.Mean.PM2.5.Concentration,xlab='MISR AOD small',ylab='AQS PM2.5')
abline(lm(Daily.Mean.PM2.5.Concentration~AODsmall, data=MISR.AQS), col="red")
cor(MISR.AQS$AODsmall,MISR.AQS$Daily.Mean.PM2.5.Concentration)
lm.MISR.AOD.small<-lm(Daily.Mean.PM2.5.Concentration~AODsmall, data=MISR.AQS)
gam.MISR.AOD.small<-gam(Daily.Mean.PM2.5.Concentration~AODsmall, data=MISR.AQS)


pdf('MISR.AOD.plot2.pdf')
p<-qplot(AODsmall,log(Daily.Mean.PM2.5.Concentration), data=MISR.AQS,xlab="MISR AOD",ylab="AQS PM2.5 (ug/m3)")
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



# Cross validation

# Cross Validation PM10 (5 year models)
gam.pred.list<-vector('list',length(10))
gam.cor.list<-vector('list',length(10))
gam.resid.list<-vector('list',length(20))
for (i in 1:10){
  location.sample<-MISR.AQS[sample(nrow(MISR.AQS),1),]
  #don't want to sample a background location
  #print(location.sample)
  train<-MISR.AQS.ss[!(MISR.AQS.ss$x %in% location.sample$x
                           & MISR.AQS.ss$y %in% location.sample$y), ]
  test<-MISR.AQS.ss[(MISR.AQS.ss$x %in% location.sample$x
                         & MISR.AQS.ss$y %in% location.sample$y), ]
  gam.st<-gam(Daily.Mean.PM2.5.Concentration~s(x,y)+s(AOD,k=6)+s(julian2),method='REML',data=train)
  print(summary(gam.st))
  gam.st.pred<-predict.gam(gam.st,newdata=test)
  gam.st.pred.merge<-cbind(gam.st.pred,test)
  gam.st.resid<-data.frame(resid=gam.st$residuals, y=gam.st$y,fitted=gam.st$fitted.values)
  RMSE<-sqrt(sum((gam.st.resid$fitted-gam.st.resid$y)^2)/dim(gam.st.resid)[1])
  print(RMSE)
  #print(mse(gam.st.pred.merge$PM25, gam.st.pred.merge$gam.st.pred))
  gam.cor.list[[i]]<-cor(gam.st.pred.merge$Daily.Mean.PM2.5.Concentration, gam.st.pred.merge$gam.st.pred)
  gam.pred.list[[i]]<-gam.st.pred.merge
  gam.resid.list[[i]]<-gam.st.resid
  #print(summary(gam.st)$r.sq)
}
mean(unlist(gam.cor.list))
#RMSE for the model 
RMSE<-sqrt(sum((gam.resid.list[[3]]$fitted.values-gam.resid.list[[3]]$y)^2)/dim(gam.resid.list[[3]])[1])

gam.cor.list[[3]]
plot(gam.pred.list[[3]]$julian2,gam.pred.list[[3]]$Daily.Mean.PM2.5.Concentration,
     xlab="Time",main="PM2.5 observed (black) and predicted (red)",ylab="PM2.5,ug/m3",
     pch=19,cex=0.5)
points(gam.pred.list[[3]]$julian2,gam.pred.list[[3]]$gam.st.pred,col="red",pch=19,cex=0.5)
lines(gam.pred.list[[3]]$julian2,gam.pred.list[[3]]$gam.st.pred,col="red")
lines(gam.pred.list[[3]]$julian2,gam.pred.list[[3]]$Daily.Mean.PM2.5.Concentration,col="black")

