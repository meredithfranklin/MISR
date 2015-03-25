##############################################
# MISR 2008-2009 4km data analysis
# February, March 2015
# Meredith Franklin
##############################################
library(mgcv)
library(ggplot2)
# Matched MISR-AQS data
MISR.AQS<-read.csv("/Users/mf/Documents/MISR/Data/MISR.AQS.csv")
MISR.AQS$AODsmall2<-MISR.AQS$AODsmall*MISR.AQS$AOD
# Visualizations and regressions

# MISR AOD and AQS PM2.5
plot(MISR.AQS$AOD,MISR.AQS$Daily.Mean.PM2.5.Concentration,xlab='MISR AOD',ylab='AQS PM2.5')
abline(lm(Daily.Mean.PM2.5.Concentration~AOD, data=MISR.AQS), col="red")

cor(MISR.AQS$AOD,MISR.AQS$Daily.Mean.PM2.5.Concentration)
lm.MISR.AOD<-lm(Daily.Mean.PM2.5.Concentration~AOD, data=MISR.AQS)
gam.MISR.AOD<-gam(Daily.Mean.PM2.5.Concentration~s(AOD,k=10), data=MISR.AQS)

pdf('MISR.AOD.plot1.pdf')
p<-qplot(AOD,Daily.Mean.PM2.5.Concentration, data=MISR.AQS,xlab="MISR AOD",ylab="AQS PM2.5 (ug/m3)")
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
