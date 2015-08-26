##############################################
# MISR 2008-2009 4km data analysis
# February-June 2015
# Meredith Franklin
##############################################
library(mgcv)
library(ggplot2)
library(gridExtra)

setwd("/Users/mf/Documents/MISR/Reports")
# Matched MISR-AQS datasets
misr.aqspm25<-read.csv("/Users/mf/Documents/MISR/Data/misr_aqspm25.csv")
#misr.aqspm10<-read.csv("/Users/mf/Documents/MISR/Data/misr_aqspm10.csv")
misr.aqspm10<-read.csv("/Users/mf/Documents/MISR/Data/misr_aqspm10_new.csv")
misr.stn<-read.csv("/Users/mf/Documents/MISR/Data/misr_stn.csv")

# Matched MISR-AQS-MET datasets
misr.aqspm25.met<-read.csv("/Users/mf/Documents/MISR/Data/misr_aqspm25_met.csv")
#misr.aqspm10.met<-read.csv("/Users/mf/Documents/MISR/Data/misr_aqspm10_met.csv")
misr.aqspm10.met<-read.csv("/Users/mf/Documents/MISR/Data/misr_aqspm10_met_new.csv")
misr.stn.met<-read.csv("/Users/mf/Documents/MISR/Data/misr_aqsstn_met.csv")

# Create new Julian date for time indexing, divide by 10000
misr.aqspm25$julian2<-misr.aqspm25$julian/10000
misr.aqspm10$julian2<-misr.aqspm10$julian/10000
misr.stn$julian2<-misr.stn$julian/10000

misr.aqspm25.met$julian2<-misr.aqspm25.met$julian/10000
misr.aqspm10.met$julian2<-misr.aqspm10.met$julian/10000
misr.stn.met$julian2<-misr.stn.met$julian/10000

misr.aqspm25$dow<-(weekdays(as.Date(misr.aqspm25$date,"%m/%d/%y")))
misr.aqspm10$dow<-(weekdays(as.Date(misr.aqspm10$date2,"%Y-%m-%d")))
misr.stn$dow<-(weekdays(as.Date(misr.stn$date,"%m/%d/%y")))

misr.aqspm25.met$dow<-(weekdays(as.Date(misr.aqspm25.met$date,"%m/%d/%y")))
misr.aqspm10.met$dow<-(weekdays(as.Date(misr.aqspm10.met$date2,"%Y-%m-%d")))
misr.stn.met$dow<-(weekdays(as.Date(misr.stn.met$date,"%m/%d/%y")))

# Remove AOD greater than 1 and AODlarge = 0
misr.aqspm25.ss<-misr.aqspm25[misr.aqspm25$AOD<1,]
misr.aqspm25.ss2<-misr.aqspm25.ss[misr.aqspm25.ss$AODlargefrac>0,]
misr.aqspm10.ss<-misr.aqspm10[misr.aqspm10$AOD<1,]
misr.aqspm10.ss2<-misr.aqspm10.ss[misr.aqspm10.ss$AODlargefrac>0,]

misr.aqspm25.met.ss<-misr.aqspm25.met[misr.aqspm25.met$AOD<1,]
misr.aqspm25.met.ss2<-misr.aqspm25.met.ss[misr.aqspm25.met.ss$AODlargefrac>0,]
misr.aqspm10.met.ss<-misr.aqspm10.met[misr.aqspm10.met$AOD<1,]
misr.aqspm10.met.ss2<-misr.aqspm10.met.ss[misr.aqspm10.met.ss$AODlargefrac>0,]


#### MISR AOD and AQS PM2.5 ####
# Summary statistics
cor.test(misr.aqspm25.ss$AOD,misr.aqspm25.ss$Daily.Mean.PM2.5.Concentration)
cor.test(misr.aqspm25.ss$AODsmall,misr.aqspm25.ss$Daily.Mean.PM2.5.Concentration)
cor.test(misr.aqspm25.ss$AODlarge,misr.aqspm25.ss$Daily.Mean.PM2.5.Concentration)

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
capture.output(summary(gam(Daily.Mean.PM10.Concentration~s(AOD), data=misr.aqspm10.ss)), file = "SummaryStatsPM10.txt", append = TRUE)

cat("GAM mod AODlarge PM10\n", file = "SummaryStatsPM10.txt", append = TRUE)
capture.output(summary(gam(Daily.Mean.PM10.Concentration~s(AODlarge, k=4), data=misr.aqspm10.ss2),na.action='na.exclude'), file = "SummaryStatsPM10.txt", append = TRUE)

cat("GAM mod AODsmall PM10\n", file = "SummaryStatsPM10.txt", append = TRUE)
capture.output(summary(gam(Daily.Mean.PM10.Concentration~s(AODsmall), data=misr.aqspm10.ss2)), file = "SummaryStatsPM10.txt", append = TRUE)
cat("GAM mod logAODsmall PM10\n", file = "SummaryStatsPM10.txt", append = TRUE)
capture.output(summary(gam(Daily.Mean.PM10.Concentration~s(log(AODsmall)), data=misr.aqspm10.ss2)), file = "SummaryStatsPM10.txt", append = TRUE)

# Plots
pdf('MISR.AOD.PM25.pdf')
p<-qplot(AOD,Daily.Mean.PM2.5.Concentration, data=misr.aqspm25.ss,xlab="MISR AOD",ylab=expression("AQS PM"[2.5]*", ug/m"^3))
p+stat_smooth(method="gam",formula=y~s(x)) +stat_smooth(method='lm',formula=y~x,col='red')
dev.off()
pdf('MISR.AOD.PM10.pdf')
p<-qplot(AOD,Daily.Mean.PM10.Concentration, data=misr.aqspm10.ss,xlab="MISR AOD",ylab=expression("AQS PM"[10]*", ug/m"^3))
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

# Univariate Linear models with species
cat("linear mod AOD STNPM25\n", file = "SummaryStatsPMSpeciation.txt", append = TRUE)
capture.output(summary(lm(misr.stn$PM25~(misr.stn$AOD))), file = "SummaryStatsPMSpeciation.txt", append = TRUE)
cat("linear mod AOD STNEC\n", file = "SummaryStatsPMSpeciation.txt", append = TRUE)
capture.output(summary(lm(misr.stn$EC~(misr.stn$AOD))), file = "SummaryStatsPMSpeciation.txt", append = TRUE)
cat("linear mod AOD STNOC\n", file = "SummaryStatsPMSpeciation.txt", append = TRUE)
capture.output(summary(lm(misr.stn$OC~(misr.stn$AOD))), file = "SummaryStatsPMSpeciation.txt", append = TRUE)
cat("linear mod AOD STNSO4\n", file = "SummaryStatsPMSpeciation.txt", append = TRUE)
capture.output(summary(lm(misr.stn$SO4~(misr.stn$AOD))), file = "SummaryStatsPMSpeciation.txt", append = TRUE)
cat("linear mod AOD STNNH4\n", file = "SummaryStatsPMSpeciation.txt", append = TRUE)
capture.output(summary(lm(misr.stn$NH4~(misr.stn$AOD))), file = "SummaryStatsPMSpeciation.txt", append = TRUE)


cat("linear mod AODlarge STNPM25\n", file = "SummaryStatsPMSpeciation.txt", append = TRUE)
capture.output(summary(lm(misr.stn$PM25~(misr.stn$AODlarge))), file = "SummaryStatsPMSpeciation.txt", append = TRUE)
cat("linear mod AODlarge STNEC\n", file = "SummaryStatsPMSpeciation.txt", append = TRUE)
capture.output(summary(lm(misr.stn$EC~(misr.stn$AODlarge))), file = "SummaryStatsPMSpeciation.txt", append = TRUE)
cat("linear mod AODlarge STNOC\n", file = "SummaryStatsPMSpeciation.txt", append = TRUE)
capture.output(summary(lm(misr.stn$OC~(misr.stn$AODlarge))), file = "SummaryStatsPMSpeciation.txt", append = TRUE)
cat("linear mod AODlarge STNSO4\n", file = "SummaryStatsPMSpeciation.txt", append = TRUE)
capture.output(summary(lm(misr.stn$SO4~(misr.stn$AODlarge))), file = "SummaryStatsPMSpeciation.txt", append = TRUE)
cat("linear mod AODlarge STNNH4\n", file = "SummaryStatsPMSpeciation.txt", append = TRUE)
capture.output(summary(lm(misr.stn$NH4~(misr.stn$AODlarge))), file = "SummaryStatsPMSpeciation.txt", append = TRUE)

cat("linear mod AODsmall STNPM25\n", file = "SummaryStatsPMSpeciation.txt", append = TRUE)
capture.output(summary(lm(misr.stn$PM25~(misr.stn$AODsmall))), file = "SummaryStatsPMSpeciation.txt", append = TRUE)
cat("linear mod AODsmall STNEC\n", file = "SummaryStatsPMSpeciation.txt", append = TRUE)
capture.output(summary(lm(misr.stn$EC~(misr.stn$AODsmall))), file = "SummaryStatsPMSpeciation.txt", append = TRUE)
cat("linear mod AODsmall STNOC\n", file = "SummaryStatsPMSpeciation.txt", append = TRUE)
capture.output(summary(lm(misr.stn$OC~(misr.stn$AODsmall))), file = "SummaryStatsPMSpeciation.txt", append = TRUE)
cat("linear mod AODsmall STNSO4\n", file = "SummaryStatsPMSpeciation.txt", append = TRUE)
capture.output(summary(lm(misr.stn$SO4~(misr.stn$AODsmall))), file = "SummaryStatsPMSpeciation.txt", append = TRUE)
cat("linear mod AODsmall STNNH4\n", file = "SummaryStatsPMSpeciation.txt", append = TRUE)
capture.output(summary(lm(misr.stn$NH4~(misr.stn$AODsmall))), file = "SummaryStatsPMSpeciation.txt", append = TRUE)

# Univariate GAM models
cat("GAM mod AOD STNPM25\n", file = "SummaryStatsPMSpeciation.txt", append = TRUE)
capture.output(summary(gam(PM25~s(AOD),data=misr.stn)), file = "SummaryStatsPMSpeciation.txt", append = TRUE)
cat("GAM mod AOD STNEC\n", file = "SummaryStatsPMSpeciation.txt", append = TRUE)
capture.output(summary(gam(EC~s(AOD),data=misr.stn)), file = "SummaryStatsPMSpeciation.txt", append = TRUE)
cat("GAM mod AOD STNOC\n", file = "SummaryStatsPMSpeciation.txt", append = TRUE)
capture.output(summary(gam(OC~s(AOD),data=misr.stn)), file = "SummaryStatsPMSpeciation.txt", append = TRUE)
cat("GAM mod AOD STN5O4\n", file = "SummaryStatsPMSpeciation.txt", append = TRUE)
capture.output(summary(gam(SO4~s(AOD),data=misr.stn)), file = "SummaryStatsPMSpeciation.txt", append = TRUE)
cat("GAM mod AOD STNNH4\n", file = "SummaryStatsPMSpeciation.txt", append = TRUE)
capture.output(summary(gam(NH4~s(AOD),data=misr.stn)), file = "SummaryStatsPMSpeciation.txt", append = TRUE)

cat("GAM mod AODlarge STNPM25\n", file = "SummaryStatsPMSpeciation.txt", append = TRUE)
capture.output(summary(gam(PM25~s(AODlarge),data=misr.stn)), file = "SummaryStatsPMSpeciation.txt", append = TRUE)
cat("GAM mod AODlarge STNEC\n", file = "SummaryStatsPMSpeciation.txt", append = TRUE)
capture.output(summary(gam(EC~s(AODlarge),data=misr.stn)), file = "SummaryStatsPMSpeciation.txt", append = TRUE)
cat("GAM mod AODlarge STNOC\n", file = "SummaryStatsPMSpeciation.txt", append = TRUE)
capture.output(summary(gam(OC~s(AODlarge),data=misr.stn)), file = "SummaryStatsPMSpeciation.txt", append = TRUE)
cat("GAM mod AODlarge STN5O4\n", file = "SummaryStatsPMSpeciation.txt", append = TRUE)
capture.output(summary(gam(SO4~s(AODlarge),data=misr.stn)), file = "SummaryStatsPMSpeciation.txt", append = TRUE)
cat("GAM mod AODlarge STNNH4\n", file = "SummaryStatsPMSpeciation.txt", append = TRUE)
capture.output(summary(gam(NH4~s(AODlarge),data=misr.stn)), file = "SummaryStatsPMSpeciation.txt", append = TRUE)

cat("GAM mod AODsmall STNPM25\n", file = "SummaryStatsPMSpeciation.txt", append = TRUE)
capture.output(summary(gam(PM25~s(AODsmall,k=4),data=misr.stn)), file = "SummaryStatsPMSpeciation.txt", append = TRUE)
cat("GAM mod AODsmall STNEC\n", file = "SummaryStatsPMSpeciation.txt", append = TRUE)
capture.output(summary(gam(EC~s(AODsmall),data=misr.stn)), file = "SummaryStatsPMSpeciation.txt", append = TRUE)
cat("GAM mod AODsmall STNOC\n", file = "SummaryStatsPMSpeciation.txt", append = TRUE)
capture.output(summary(gam(OC~s(AODsmall),data=misr.stn)), file = "SummaryStatsPMSpeciation.txt", append = TRUE)
cat("GAM mod AODsmall STN5O4\n", file = "SummaryStatsPMSpeciation.txt", append = TRUE)
capture.output(summary(gam(SO4~s(AODsmall),data=misr.stn)), file = "SummaryStatsPMSpeciation.txt", append = TRUE)
cat("GAM mod AODsmall STNNH4\n", file = "SummaryStatsPMSpeciation.txt", append = TRUE)
capture.output(summary(gam(NH4~s(AODsmall),data=misr.stn)), file = "SummaryStatsPMSpeciation.txt", append = TRUE)


# Plots
p1<-qplot(AOD,PM25, data=misr.stn,xlab="MISR AOD",ylab="STN PM2.5 (ug/m3)")
plot1<-p1 +stat_smooth(method='lm',formula=y~x,col='red')
#plot1<-p1+stat_smooth(method="gam",formula=y~s(x,k=4)) +stat_smooth(method='lm',formula=y~x,col='red')

p2<-qplot(AOD,OC, data=misr.stn,xlab="MISR AOD",ylab="STN OC (ug/m3)")
plot2<-p2 +stat_smooth(method='lm',formula=y~x,col='red')
#plot2<-p2+stat_smooth(method="gam",formula=y~s(x,k=4)) +stat_smooth(method='lm',formula=y~x,col='red')

p3<-qplot(AOD,SO4, data=misr.stn,xlab="MISR AOD",ylab="STN SO4 (ug/m3)")
plot3<-p3+stat_smooth(method='lm',formula=y~x,col='red')
#plot3<-p3+stat_smooth(method="gam",formula=y~s(x,k=4)) +stat_smooth(method='lm',formula=y~x,col='red')

p4<-qplot(AOD,NH4, data=misr.stn,xlab="MISR AOD",ylab="STN NH4 (ug/m3)")
plot4<-p4+stat_smooth(method='lm',formula=y~x,col='red')
#plot4<-p4+stat_smooth(method="gam",formula=y~s(x,k=4)) +stat_smooth(method='lm',formula=y~x,col='red')

png('MISR.STN.png')
grid.arrange(plot1,plot2,plot3,plot4,nrow=2,ncol=2)
dev.off()

#### Spatio-temporal regression with and without meteorology#####
# PM2.5
# Title (writes new file)
cat("PM AOD Spatio-Temporal Models", file = "SpatioTemporalModels2.txt")
# add new lines
cat("\n", file = "SpatioTemporalModels2.txt", append = TRUE)
# ST PM25-AOD
cat("ST model PM25 AOD\n", file = "SpatioTemporalModels2.txt", append = TRUE)
cat("\n", file = "SpatioTemporalModels2.txt", append = TRUE)
capture.output(summary(gam(Daily.Mean.PM2.5.Concentration~s(AOD)+s(x.1,y.1,k=16)+s(julian2)+s(month,bs="cc")+as.factor(dow), na.action=na.exclude,data=misr.aqspm25.ss)), file = "SpatioTemporalModels2.txt", append = TRUE)
cat("ST model PM25 AOD\n version2", file = "SpatioTemporalModels2.txt", append = TRUE)
cat("\n", file = "SpatioTemporalModels2.txt", append = TRUE)
capture.output(summary(gam(Daily.Mean.PM2.5.Concentration~s(AOD)+s(x.1,y.1,k=16)+s(julian2,k=6)+s(month,bs="cc")+as.factor(dow), na.action=na.exclude,data=misr.aqspm25.ss)), file = "SpatioTemporalModels2.txt", append = TRUE)
cat("ST model PM25 AODlarge\n", file = "SpatioTemporalModels2.txt", append = TRUE)
cat("\n", file = "SpatioTemporalModels2.txt", append = TRUE)
capture.output(summary(gam(Daily.Mean.PM2.5.Concentration~s(AODlarge)+s(x.1,y.1,k=16)+s(julian2)+s(month,bs="cc")+as.factor(dow), na.action=na.exclude,data=misr.aqspm25.ss2)), file = "SpatioTemporalModels2.txt", append = TRUE)
#ST PM10-AOD
cat("ST model PM10 AOD\n", file = "SpatioTemporalModels2.txt", append = TRUE)
capture.output(summary(gam(Daily.Mean.PM10.Concentration~s(AOD)+s(x.1,y.1,k=12)+s(julian2)+s(month,bs="cc")+as.factor(dow), na.action=na.exclude,data=misr.aqspm10.ss)), file = "SpatioTemporalModels2.txt", append = TRUE)
cat("ST model PM10 AOD version 2\n", file = "SpatioTemporalModels2.txt", append = TRUE)
capture.output(summary(gam(Daily.Mean.PM10.Concentration~s(AOD)+s(x.1,y.1,k=12)+s(julian2,k=4)+s(month,bs="cc")+as.factor(dow), na.action=na.exclude,data=misr.aqspm10.ss)), file = "SpatioTemporalModels2.txt", append = TRUE)
cat("ST model PM10 AOD version 3\n", file = "SpatioTemporalModels2.txt", append = TRUE)
capture.output(summary(gam(Daily.Mean.PM10.Concentration~s(AOD)+s(x.1,y.1,k=12)+s(julian2,k=3)+s(month,bs="cc")+as.factor(dow), na.action=na.exclude,data=misr.aqspm10.ss)), file = "SpatioTemporalModels2.txt", append = TRUE)

cat("ST model PM10 AODlarge\n", file = "SpatioTemporalModels2.txt", append = TRUE)
capture.output(summary(gam(Daily.Mean.PM10.Concentration~s(AODlarge)+s(x.1,y.1,k=12)+s(julian2)+s(month,bs="cc")+as.factor(dow), na.action=na.exclude,data=misr.aqspm10.ss)), file = "SpatioTemporalModels2.txt", append = TRUE)
cat("ST model PM10 AODlarge version 2 \n", file = "SpatioTemporalModels2.txt", append = TRUE)
capture.output(summary(gam(Daily.Mean.PM10.Concentration~s(AODlarge)+s(x.1,y.1,k=12)+s(julian2,k=4)+s(month,bs="cc",k=4)+as.factor(dow), na.action=na.exclude,data=misr.aqspm10.ss)), file = "SpatioTemporalModels2.txt", append = TRUE)
cat("ST model PM10 AODlarge version 3 \n", file = "SpatioTemporalModels2.txt", append = TRUE)
capture.output(summary(gam(Daily.Mean.PM10.Concentration~s(AODlarge)+s(x.1,y.1,k=12)+s(julian2,k=3)+s(month,bs="cc")+as.factor(dow), na.action=na.exclude,data=misr.aqspm10.ss)), file = "SpatioTemporalModels2.txt", append = TRUE)


# ST PM25-AOD with met
cat("ST model PM25 AOD met\n", file = "SpatioTemporalModels2.txt", append = TRUE)
capture.output(summary(gam(Daily.Mean.PM2.5.Concentration~s(AOD)+s(x.1,y.1,k=16)+s(julian2)+s(month, bs="cc")+as.factor(dow)+dew.point+wind.sp, na.action=na.exclude,data=misr.aqspm25.met.ss)), file = "SpatioTemporalModels2.txt", append = TRUE)
cat("ST model PM25 AODlarge met\n", file = "SpatioTemporalModels2.txt", append = TRUE)
capture.output(summary(gam(Daily.Mean.PM2.5.Concentration~s(AODlarge)+s(x.1,y.1,k=16)+s(julian2)+s(month, bs="cc")+as.factor(dow)+dew.point+wind.sp, na.action=na.exclude,data=misr.aqspm25.met.ss2)), file = "SpatioTemporalModels2.txt", append = TRUE)
# ST PM10-AOD with met
cat("ST model PM10 AOD met\n", file = "SpatioTemporalModels2.txt", append = TRUE)
capture.output(summary(gam(Daily.Mean.PM10.Concentration~s(AOD)+s(x.1,y.1,k=10)+s(julian2,k=5)+s(month, bs="cc")+as.factor(dow)+wind.sp+atm.press, na.action=na.exclude,data=misr.aqspm10.met)), file = "SpatioTemporalModels2.txt", append = TRUE)
cat("ST model PM10 AODlarge met\n", file = "SpatioTemporalModels2.txt", append = TRUE)
capture.output(summary(gam(Daily.Mean.PM10.Concentration~s(AODlarge)+s(x.1,y.1,k=10)+s(julian2,k=5)+s(month, bs="cc")+as.factor(dow)+wind.sp+atm.press, na.action=na.exclude,data=misr.aqspm10.met)), file = "SpatioTemporalModels2.txt", append = TRUE)
# ST PM10 linear AOD with met
# cat("ST model PM10 linear AOD met\n", file = "SpatioTemporalModels.txt", append = TRUE)
# capture.output(summary(gam(Daily.Mean.PM10.Concentration~s(AOD)+s(x.1,y.1,k=11)+s(julian2)+wind.sp+atm.press, na.action=na.exclude,data=misr.aqspm10.met.ss)), file = "SpatioTemporalModels.txt", append = TRUE)
# cat("ST model PM10 linear AODlarge met version 2\n", file = "SpatioTemporalModels.txt", append = TRUE)
# capture.output(summary(gam(Daily.Mean.PM10.Concentration~s(AODlarge)+s(x.1,y.1,k=11)+s(julian2,k=4)+wind.sp+atm.press, na.action=na.exclude,data=misr.aqspm10.met.ss)), file = "SpatioTemporalModels.txt", append = TRUE)
# cat("ST model PM10 linear AODlarge met version 3\n", file = "SpatioTemporalModels.txt", append = TRUE)
# capture.output(summary(gam(Daily.Mean.PM10.Concentration~s(AODlarge)+s(x.1,y.1,k=11)+s(julian2,k=5)+wind.sp+atm.press, na.action=na.exclude,data=misr.aqspm10.met.ss)), file = "SpatioTemporalModels.txt", append = TRUE)
# cat("ST model PM10 linear AODlarge met version 4\n", file = "SpatioTemporalModels.txt", append = TRUE)
# capture.output(summary(gam(Daily.Mean.PM10.Concentration~s(AODlarge)+s(x.1,y.1,k=11)+julian2+wind.sp+atm.press, na.action=na.exclude,data=misr.aqspm10.met.ss)), file = "SpatioTemporalModels.txt", append = TRUE)

#### Cross-Validation ####

misr.aqspm25.points<-unique(misr.aqspm25.met.ss[,35:36])

gam.pred.pm25.list<-vector('list',dim(misr.aqspm25.points)[1])
gam.resid.pm25.list<-vector('list',dim(misr.aqspm25.points)[1])
gam.pred.pm25.met.list<-vector('list',dim(misr.aqspm25.points)[1])
gam.resid.pm25.met.list<-vector('list',dim(misr.aqspm25.points)[1])

gam.pred.pm25.list2<-vector('list',dim(misr.aqspm25.points)[1])
gam.resid.pm25.list2<-vector('list',dim(misr.aqspm25.points)[1])
gam.pred.pm25.met.list2<-vector('list',dim(misr.aqspm25.points)[1])
gam.resid.pm25.met.list2<-vector('list',dim(misr.aqspm25.points)[1])

# PM2.5 cross validation
for (i in 1:dim(misr.aqspm25.points)[1]){
  location.sample1<-misr.aqspm25.points[i,]

  train1<-misr.aqspm25.met.ss[!(misr.aqspm25.met.ss$x.1 %in% location.sample1$x.1
                           & misr.aqspm25.met.ss$y.1 %in% location.sample1$y.1), ]
  test1<-misr.aqspm25.met.ss[(misr.aqspm25.met.ss$x.1 %in% location.sample1$x.1
                         & misr.aqspm25.met.ss$y.1 %in% location.sample1$y.1), ]

  gam.st.pm25<-gam(Daily.Mean.PM2.5.Concentration~s(AOD)+s(x.1,y.1,k=16)+s(julian2)+s(month,bs='cc')+as.factor(dow), na.action=na.exclude,data=train1)
  gam.st.pm25.met<-gam(Daily.Mean.PM2.5.Concentration~s(AOD)+s(x.1,y.1,k=16)+s(julian2)+s(month,bs='cc')+as.factor(dow)+dew.point+wind.sp, na.action=na.exclude,data=train1)
  
  gam.st2.pm25<-gam(Daily.Mean.PM2.5.Concentration~s(AODlarge)+s(x.1,y.1,k=16)+s(julian2)+s(month,bs='cc')+as.factor(dow), na.action=na.exclude,data=train1)
  gam.st2.pm25.met<-gam(Daily.Mean.PM2.5.Concentration~s(AODlarge)+s(x.1,y.1,k=16)+s(julian2)+s(month,bs='cc')+as.factor(dow)+dew.point+wind.sp, na.action=na.exclude,data=train1)
  
  gam.st.pred.pm25<-predict.gam(gam.st.pm25,newdata=test1)
  gam.st.pred.pm25.merge<-cbind(gam.st.pred.pm25,test1)
  gam.st.resid.pm25<-data.frame(resid=gam.st.pm25$residuals, y=gam.st.pm25$y,fitted=gam.st.pm25$fitted.values)

  gam.st.pred.pm25.met<-predict.gam(gam.st.pm25.met,newdata=test1)
  gam.st.pred.pm25.met.merge<-cbind(gam.st.pred.pm25.met,test1)
  gam.st.resid.pm25.met<-data.frame(resid=gam.st.pm25.met$residuals, y=gam.st.pm25.met$y,fitted=gam.st.pm25.met$fitted.values)
  
  gam.st2.pred.pm25<-predict.gam(gam.st2.pm25,newdata=test1)
  gam.st2.pred.pm25.merge<-cbind(gam.st2.pred.pm25,test1)
  gam.st2.resid.pm25<-data.frame(resid=gam.st2.pm25$residuals, y=gam.st2.pm25$y,fitted=gam.st2.pm25$fitted.values)
  
  gam.st2.pred.pm25.met<-predict.gam(gam.st2.pm25.met,newdata=test1)
  gam.st2.pred.pm25.met.merge<-cbind(gam.st2.pred.pm25.met,test1)
  gam.st2.resid.pm25.met<-data.frame(resid=gam.st2.pm25.met$residuals, y=gam.st2.pm25.met$y,fitted=gam.st2.pm25.met$fitted.values)
  
  gam.pred.pm25.list[[i]]<-gam.st.pred.pm25.merge
  gam.pred.pm25.met.list[[i]]<-gam.st.pred.pm25.met.merge
  gam.resid.pm25.list[[i]]<-gam.st.resid.pm25
  gam.resid.pm25.met.list[[i]]<-gam.st.resid.pm25.met
  
  gam.pred.pm25.list2[[i]]<-gam.st2.pred.pm25.merge
  gam.pred.pm25.met.list2[[i]]<-gam.st2.pred.pm25.met.merge
  gam.resid.pm25.list2[[i]]<-gam.st2.resid.pm25
  gam.resid.pm25.met.list2[[i]]<-gam.st2.resid.pm25.met

}

gam.pred.pm25 <- do.call("rbind", gam.pred.pm25.list) 
gam.pred.pm25.met <- do.call("rbind", gam.pred.pm25.met.list) 
gam.pred2.pm25 <- do.call("rbind", gam.pred.pm25.list2) 
gam.pred2.pm25.met <- do.call("rbind", gam.pred.pm25.met.list2) 


obs.pred.AOD<-lm(Daily.Mean.PM2.5.Concentration~gam.st.pred.pm25, data=gam.pred.pm25)
obs.pred.AODmet<-lm(Daily.Mean.PM2.5.Concentration~gam.st.pred.pm25.met, data=gam.pred.pm25.met)

obs.pred.AODlarge<-lm(Daily.Mean.PM2.5.Concentration~gam.st2.pred.pm25, data=gam.pred2.pm25)
obs.pred.AODlargemet<-lm(Daily.Mean.PM2.5.Concentration~gam.st2.pred.pm25.met, data=gam.pred2.pm25.met)

cor.test(gam.pred.pm25$gam.st.pred.pm25,gam.pred.pm25$Daily.Mean.PM2.5.Concentration)

p5<-qplot(gam.st.pred.pm25,Daily.Mean.PM2.5.Concentration, data=gam.pred.pm25,xlab=expression('Predicted PM'[2.5]*', ug/m'^3),ylab=expression('Observed PM'[2.5]*', ug/m'^3))
plot5<-p5+stat_smooth(method='lm',formula=y~x-1,col='red')+stat_smooth(method='lm',formula=y~x,col='blue')
plot5

p6<-qplot(gam.st.pred.pm25.met,Daily.Mean.PM2.5.Concentration, data=gam.pred.pm25.met,xlab=expression('Predicted PM'[2.5]*', ug/m'^3),ylab=expression('Observed PM'[2.5]*', ug/m'^3))
plot6<-p6+stat_smooth(method='lm',formula=y~x-1,col='red')+stat_smooth(method='lm',formula=y~x,col='blue')
plot6

p7<-qplot(gam.st2.pred.pm25,Daily.Mean.PM2.5.Concentration, data=gam.pred2.pm25,xlab="Predicted",ylab="Observed",xlim=c(0,30))
plot7<-p7+stat_smooth(method='lm',formula=y~x-1,col='red')+stat_smooth(method='lm',formula=y~x,col='blue')
plot7

p8<-qplot(gam.st2.pred.pm25.met,Daily.Mean.PM2.5.Concentration, data=gam.pred2.pm25.met,xlab=expression('Predicted PM'[2.5]*', ug/m'^3),ylab=expression('Observed PM'[2.5]*', ug/m'^3))
plot8<-p8+stat_smooth(method='lm',formula=y~x-1,col='red')+stat_smooth(method='lm',formula=y~x,col='blue')
plot8
p8<-qplot(gam.st2.pred.pm25.met,Daily.Mean.PM2.5.Concentration, data=gam.pred2.pm25.met,xlab="Predicted PM2.5",ylab=expression('hi'[5]*'there'[6]^8*'you'['down here']*'and'^'up'*'there'))

tiff('MISR.PM25.CV.tiff')
grid.arrange(plot6,plot8,nrow=1,ncol=2)
dev.off()

# Plot prediction over time with observed gam.pred.pm25 and gam.pred.pm25.met
# Site with max latitude (remote, low concentration) pred.site1, site in LA with dense monitoring pred.site2
pred.site1<-gam.pred.pm25[gam.pred.pm25$AQS_SITE_ID=="06-037-4002",] # use 06-037-4002
pred.site2<-gam.pred.pm25[gam.pred.pm25$AQS_SITE_ID=="06-071-9004",]

pdf('MISR.PM25.CV.LA.pdf')
ggplot(data = pred.site1, aes(as.Date(date2,"%Y-%m-%d"))) + 
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


# Cross validation PM10
misr.aqspm10.points<-unique(misr.aqspm10.met[,26:27])
gam.pred.pm10.list<-vector('list',length(misr.aqspm10.points))
gam.resid.pm10.list<-vector('list',length(misr.aqspm10.points))
gam.pred.pm10.met.list<-vector('list',length(misr.aqspm10.points))
gam.resid.pm10.met.list<-vector('list',length(misr.aqspm10.points))

gam.pred.pm10.list2<-vector('list',length(misr.aqspm10.points))
gam.resid.pm10.list2<-vector('list',length(misr.aqspm10.points))
gam.pred.pm10.met.list2<-vector('list',length(misr.aqspm10.points))
gam.resid.pm10.met.list2<-vector('list',length(misr.aqspm10.points))

for (i in 1:dim(misr.aqspm10.points)[1]){
  location.sample2<-misr.aqspm10.points[i,]

  train2<-misr.aqspm10.met.ss[!(misr.aqspm10.met.ss$x.1 %in% location.sample2$x.1
                              & misr.aqspm10.met.ss$y.1 %in% location.sample2$y.1), ]

  test2<-misr.aqspm10.met.ss[(misr.aqspm10.met.ss$x.1 %in% location.sample2$x.1
                            & misr.aqspm10.met.ss$y.1 %in% location.sample2$y.1), ]


  gam.st.pm10<-gam(Daily.Mean.PM10.Concentration~s(AOD)+s(x.1,y.1,k=11)+s(julian2)+s(month,bs='cc')+as.factor(dow), na.action=na.exclude,data=train2)
  gam.st.pm10.met<-gam(Daily.Mean.PM10.Concentration~s(AOD)+s(x.1,y.1,k=11)+s(julian2)+s(month,bs='cc')+as.factor(dow)+dew.point+wind.sp, na.action=na.exclude,data=train2)

  gam.st2.pm10<-gam(Daily.Mean.PM10.Concentration~s(AODlarge)+s(x.1,y.1,k=11)+s(julian2)+s(month,bs='cc')+as.factor(dow), na.action=na.exclude,data=train2)
  gam.st2.pm10.met<-gam(Daily.Mean.PM10.Concentration~s(AODlarge)+s(x.1,y.1,k=11)+s(julian2)+s(month,bs='cc')+as.factor(dow)+dew.point+wind.sp, na.action=na.exclude,data=train2)

  gam.st.pred.pm10<-predict.gam(gam.st.pm10,newdata=test2)
  gam.st.pred.pm10.merge<-cbind(gam.st.pred.pm10,test2)
  gam.st.resid.pm10<-data.frame(resid=gam.st.pm10$residuals, y=gam.st.pm10$y,fitted=gam.st.pm10$fitted.values)

  gam.st.pred.pm10.met<-predict.gam(gam.st.pm10.met,newdata=test2)
  gam.st.pred.pm10.met.merge<-cbind(gam.st.pred.pm10.met,test2)
  gam.st.resid.pm10.met<-data.frame(resid=gam.st.pm10.met$residuals, y=gam.st.pm10.met$y,fitted=gam.st.pm10.met$fitted.values)

  gam.st2.pred.pm10<-predict.gam(gam.st2.pm10,newdata=test2)
  gam.st2.pred.pm10.merge<-cbind(gam.st2.pred.pm10,test2)
  gam.st2.resid.pm10<-data.frame(resid=gam.st2.pm10$residuals, y=gam.st2.pm10$y,fitted=gam.st2.pm10$fitted.values)

  gam.st2.pred.pm10.met<-predict.gam(gam.st2.pm10.met,newdata=test2)
  gam.st2.pred.pm10.met.merge<-cbind(gam.st2.pred.pm10.met,test2)
  gam.st2.resid.pm10.met<-data.frame(resid=gam.st2.pm10.met$residuals, y=gam.st2.pm10.met$y,fitted=gam.st2.pm10.met$fitted.values)

  gam.pred.pm10.list[[i]]<-gam.st.pred.pm10.merge
  gam.pred.pm10.met.list[[i]]<-gam.st.pred.pm10.met.merge
  gam.resid.pm10.list[[i]]<-gam.st.resid.pm10
  gam.resid.pm10.met.list[[i]]<-gam.st.resid.pm10.met

  gam.pred.pm10.list2[[i]]<-gam.st2.pred.pm10.merge
  gam.pred.pm10.met.list2[[i]]<-gam.st2.pred.pm10.met.merge
  gam.resid.pm10.list2[[i]]<-gam.st2.resid.pm10
  gam.resid.pm10.met.list2[[i]]<-gam.st2.resid.pm10.met

}
gam.pred.pm10 <- do.call("rbind", gam.pred.pm10.list) 
gam.pred.pm10.met <- do.call("rbind", gam.pred.pm10.met.list) 
gam.pred2.pm10 <- do.call("rbind", gam.pred.pm10.list2) 
gam.pred2.pm10.met <- do.call("rbind", gam.pred.pm10.met.list2) 


obs.pred.AOD<-lm(Daily.Mean.PM10.Concentration~gam.st.pred.pm10, data=gam.pred.pm10)
obs.pred.AODmet<-lm(Daily.Mean.PM10.Concentration~gam.st.pred.pm10.met, data=gam.pred.pm10.met)

obs.pred.AODlarge<-lm(Daily.Mean.PM10.Concentration~gam.st2.pred.pm10, data=gam.pred2.pm10)
obs.pred.AODlargemet<-lm(Daily.Mean.PM10.Concentration~gam.st2.pred.pm10.met, data=gam.pred2.pm10.met)

p9<-qplot(gam.st.pred.pm10,Daily.Mean.PM10.Concentration, data=gam.pred.pm10,xlab="Predicted",ylab="Observed")
plot9<-p9+stat_smooth(method='lm',formula=y~x-1,col='red')+stat_smooth(method='lm',formula=y~x,col='blue')
plot9

p10<-qplot(gam.st.pred.pm10.met,Daily.Mean.PM10.Concentration, data=gam.pred.pm10.met,xlab="Predicted",ylab="Observed")
plot10<-p10+stat_smooth(method='lm',formula=y~x-1,col='red')+stat_smooth(method='lm',formula=y~x,col='blue')
plot10

p11<-qplot(gam.st2.pred.pm10,Daily.Mean.PM10.Concentration, data=gam.pred2.pm10,xlab=expression('Predicted PM'[10]*', ug/m'^3),ylab=expression('Observed PM'[10]*', ug/m'^3))
plot11<-p11+stat_smooth(method='lm',formula=y~x-1,col='red')+stat_smooth(method='lm',formula=y~x,col='blue')
plot11

p12<-qplot(gam.st2.pred.pm10.met,Daily.Mean.PM10.Concentration, data=gam.pred2.pm10.met,xlab="Predicted",ylab="Observed")
plot12<-p12+stat_smooth(method='lm',formula=y~x-1,col='red')+stat_smooth(method='lm',formula=y~x,col='blue')
plot12

pdf('MISR.PM25.PM10.CV.pdf')
grid.arrange(plot6,plot11,nrow=2,ncol=1)
dev.off()

pred.site3<-gam.pred.pm10[gam.pred.pm10$AQS_SITE_ID=="06-037-0016",] # "06-071-9004" San Bernadino
pred.site3b<-gam.pred2.pm10[gam.pred2.pm10$AQS_SITE_ID=="06-065-0004",]

pdf('MISR.PM10.AODlarge.CV.OC.pdf')
ggplot(data = pred.site3b, aes(as.Date(date2,"%Y-%m-%d"))) + 
  geom_line(aes(y= gam.st2.pred.pm10, color="red")) + 
  geom_line(aes(y=Daily.Mean.PM10.Concentration)) +
  geom_point(aes(y=Daily.Mean.PM10.Concentration)) +
  geom_point(aes(y= gam.st2.pred.pm10, color="red"))+
  theme(legend.position="none",
        axis.line = element_line(colour = "black"),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())+
  labs(x = "Date",y=expression('PM'[10]*', ug/m'^3))
dev.off()


##### Use s-t model to predict PM2.5 and PM10 from full AOD ###

# PM2.5 model
gam.st.pm25<-gam(Daily.Mean.PM2.5.Concentration~s(AOD)+s(x.1,y.1,k=16)+s(julian2)+s(month,bs="cc")+as.factor(dow), na.action=na.exclude,data=misr.aqspm25.ss)

misr.04.21.08<-misr.08.09[misr.08.09$year==2008 & misr.08.09$month==4 & misr.08.09$day==21,]
misr.04.21.08$julian2<-misr.04.21.08$julian/10000
misr.04.21.08$x.1<-misr.04.21.08$x
misr.04.21.08$y.1<-misr.04.21.08$y
misr.04.21.08$dow<-(weekdays(as.Date(misr.04.21.08$date,"%m/%d/%y")))

predicted.pm25.04.21.08<-predict.gam(gam.st.pm25, newdata=misr.04.21.08)
# merge with data
misr.04.21.08$predPM25<-predicted.pm25.04.21.08
write.csv(misr.04.21.08,"predicted_pm25_misr_04_21_08.csv")


# PM10 model
gam.st.pm10<-gam(Daily.Mean.PM10.Concentration~s(AODlarge)+s(x.1,y.1,k=12)+s(julian2)+s(month,bs="cc")+as.factor(dow), na.action=na.exclude,data=misr.aqspm10.ss)

predicted.pm10.04.21.08<-predict.gam(gam.st.pm10, newdata=misr.04.21.08)
# merge with data
misr.04.21.08$predPM10<-predicted.pm10.04.21.08
write.csv(misr.04.21.08,"predicted_pm_misr_04_21_08.csv")
