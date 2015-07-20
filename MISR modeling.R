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
misr.aqspm10<-read.csv("/Users/mf/Documents/MISR/Data/misr_aqspm10.csv")
misr.stn<-read.csv("/Users/mf/Documents/MISR/Data/misr_stn.csv")

# Matched MISR-AQS-MET datasets
misr.aqspm25.met<-read.csv("/Users/mf/Documents/MISR/Data/misr_aqspm25_met.csv")
misr.aqspm10.met<-read.csv("/Users/mf/Documents/MISR/Data/misr_aqspm10_met.csv")
misr.stn.met<-read.csv("/Users/mf/Documents/MISR/Data/misr_aqsstn_met.csv")

# Create new Julian date for time indexing, divide by 10000
misr.aqspm25$julian2<-misr.aqspm25$julian/10000
misr.aqspm10$julian2<-misr.aqspm10$julian/10000
misr.stn$julian2<-misr.stn$julian/10000

misr.aqspm25.met$julian2<-misr.aqspm25.met$julian/10000
misr.aqspm10.met$julian2<-misr.aqspm10.met$julian/10000
misr.stn.met$julian2<-misr.stn.met$julian/10000

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
png('MISR.AOD.PM25.png')
p<-qplot(AOD,Daily.Mean.PM2.5.Concentration, data=misr.aqspm25.ss,xlab="MISR AOD",ylab="AQS PM2.5 (ug/m3)")
p+stat_smooth(method="gam",formula=y~s(x)) +stat_smooth(method='lm',formula=y~x,col='red')
dev.off()
png('MISR.AOD.PM10.png')
p<-qplot(AOD,Daily.Mean.PM10.Concentration, data=misr.aqspm10.ss2,xlab="MISR AOD",ylab="AQS PM10 (ug/m3)")
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
plot1<-p1+stat_smooth(method="gam",formula=y~s(x)) +stat_smooth(method='lm',formula=y~x,col='red')

p2<-qplot(AOD,OC, data=misr.stn,xlab="MISR AOD",ylab="STN OC (ug/m3)")
plot2<-p2+stat_smooth(method="gam",formula=y~s(x,k=4)) +stat_smooth(method='lm',formula=y~x,col='red')

p3<-qplot(AOD,SO4, data=misr.stn,xlab="MISR AOD",ylab="STN SO4 (ug/m3)")
plot3<-p3+stat_smooth(method="gam",formula=y~s(x,k=4)) +stat_smooth(method='lm',formula=y~x,col='red')

p4<-qplot(AOD,NH4, data=misr.stn,xlab="MISR AOD",ylab="STN NH4 (ug/m3)")
plot4<-p4+stat_smooth(method="gam",formula=y~s(x,k=4)) +stat_smooth(method='lm',formula=y~x,col='red')

png('MISR.STN.png')
grid.arrange(plot1,plot2,plot3,plot4,nrow=2,ncol=2)
dev.off()

#### Spatio-temporal regression with and without meteorology#####
# PM2.5
# Title (writes new file)
cat("PM AOD Spatio-Temporal Models", file = "SpatioTemporalModels.txt")
# add new lines
cat("\n", file = "SpatioTemporalModels.txt", append = TRUE)
# ST PM25-AOD
cat("ST model PM25 AOD\n", file = "SpatioTemporalModels.txt", append = TRUE)
cat("\n", file = "SpatioTemporalModels.txt", append = TRUE)
capture.output(summary(gam(Daily.Mean.PM2.5.Concentration~s(AOD)+s(x.1,y.1,k=16)+s(julian2), na.action=na.exclude,data=misr.aqspm25.ss)), file = "SpatioTemporalModels.txt", append = TRUE)
cat("ST model PM25 AOD\n version2", file = "SpatioTemporalModels.txt", append = TRUE)
cat("\n", file = "SpatioTemporalModels.txt", append = TRUE)
capture.output(summary(gam(Daily.Mean.PM2.5.Concentration~s(AOD)+s(x.1,y.1,k=16)+s(julian2), na.action=na.exclude,data=misr.aqspm25.ss)), file = "SpatioTemporalModels.txt", append = TRUE)
cat("ST model PM25 AODlarge\n", file = "SpatioTemporalModels.txt", append = TRUE)
cat("\n", file = "SpatioTemporalModels.txt", append = TRUE)
capture.output(summary(gam(Daily.Mean.PM2.5.Concentration~s(AODlarge)+s(x.1,y.1,k=16)+s(julian2), na.action=na.exclude,data=misr.aqspm25.ss2)), file = "SpatioTemporalModels.txt", append = TRUE)
#ST PM10-AOD
cat("ST model PM10 AOD\n", file = "SpatioTemporalModels.txt", append = TRUE)
capture.output(summary(gam(Daily.Mean.PM10.Concentration~s(AOD)+s(x.1,y.1,k=12)+s(julian2), na.action=na.exclude,data=misr.aqspm10.ss)), file = "SpatioTemporalModels.txt", append = TRUE)
cat("ST model PM10 AOD version 2\n", file = "SpatioTemporalModels.txt", append = TRUE)
capture.output(summary(gam(Daily.Mean.PM10.Concentration~s(AOD)+s(x.1,y.1,k=12)+s(julian2,k=4), na.action=na.exclude,data=misr.aqspm10.ss)), file = "SpatioTemporalModels.txt", append = TRUE)
cat("ST model PM10 AOD version 3\n", file = "SpatioTemporalModels.txt", append = TRUE)
capture.output(summary(gam(Daily.Mean.PM10.Concentration~s(AOD)+s(x.1,y.1,k=12)+s(julian2,k=3), na.action=na.exclude,data=misr.aqspm10.ss)), file = "SpatioTemporalModels.txt", append = TRUE)

cat("ST model PM10 AODlarge\n", file = "SpatioTemporalModels.txt", append = TRUE)
capture.output(summary(gam(Daily.Mean.PM10.Concentration~s(AODlarge)+s(x.1,y.1,k=12)+s(julian2), na.action=na.exclude,data=misr.aqspm10.ss)), file = "SpatioTemporalModels.txt", append = TRUE)
cat("ST model PM10 AODlarge version 2 \n", file = "SpatioTemporalModels.txt", append = TRUE)
capture.output(summary(gam(Daily.Mean.PM10.Concentration~s(AODlarge)+s(x.1,y.1,k=12)+s(julian2,k=4), na.action=na.exclude,data=misr.aqspm10.ss)), file = "SpatioTemporalModels.txt", append = TRUE)
cat("ST model PM10 AODlarge version 3 \n", file = "SpatioTemporalModels.txt", append = TRUE)
capture.output(summary(gam(Daily.Mean.PM10.Concentration~s(AODlarge)+s(x.1,y.1,k=12)+s(julian2,k=3), na.action=na.exclude,data=misr.aqspm10.ss)), file = "SpatioTemporalModels.txt", append = TRUE)


# ST PM25-AOD with met
cat("ST model PM25 AOD met\n", file = "SpatioTemporalModels.txt", append = TRUE)
capture.output(summary(gam(Daily.Mean.PM2.5.Concentration~s(AOD)+s(x.1,y.1,k=16)+s(julian2)+dew.point+wind.sp, na.action=na.exclude,data=misr.aqspm25.met.ss)), file = "SpatioTemporalModels.txt", append = TRUE)
cat("ST model PM25 AODlarge met\n", file = "SpatioTemporalModels.txt", append = TRUE)
capture.output(summary(gam(Daily.Mean.PM2.5.Concentration~s(AODlarge)+s(x.1,y.1,k=16)+s(julian2)+dew.point+wind.sp, na.action=na.exclude,data=misr.aqspm25.met.ss2)), file = "SpatioTemporalModels.txt", append = TRUE)
# ST PM10-AOD with met
cat("ST model PM10 AOD met\n", file = "SpatioTemporalModels.txt", append = TRUE)
capture.output(summary(gam(Daily.Mean.PM10.Concentration~s(AOD)+s(x.1,y.1,k=11)+s(julian2,k=5)+wind.sp+atm.press, na.action=na.exclude,data=misr.aqspm10.met.ss)), file = "SpatioTemporalModels.txt", append = TRUE)
cat("ST model PM10 AODlarge met\n", file = "SpatioTemporalModels.txt", append = TRUE)
capture.output(summary(gam(Daily.Mean.PM10.Concentration~s(AODlarge)+s(x.1,y.1,k=11)+s(julian2)+wind.sp+atm.press, na.action=na.exclude,data=misr.aqspm10.met.ss2)), file = "SpatioTemporalModels.txt", append = TRUE)
# ST PM10 linear AOD with met
cat("ST model PM10 linear AOD met\n", file = "SpatioTemporalModels.txt", append = TRUE)
capture.output(summary(gam(Daily.Mean.PM10.Concentration~s(AOD)+s(x.1,y.1,k=11)+s(julian2)+wind.sp+atm.press, na.action=na.exclude,data=misr.aqspm10.met.ss)), file = "SpatioTemporalModels.txt", append = TRUE)
cat("ST model PM10 linear AODlarge met version 2\n", file = "SpatioTemporalModels.txt", append = TRUE)
capture.output(summary(gam(Daily.Mean.PM10.Concentration~s(AODlarge)+s(x.1,y.1,k=11)+s(julian2,k=4)+wind.sp+atm.press, na.action=na.exclude,data=misr.aqspm10.met.ss)), file = "SpatioTemporalModels.txt", append = TRUE)
cat("ST model PM10 linear AODlarge met version 3\n", file = "SpatioTemporalModels.txt", append = TRUE)
capture.output(summary(gam(Daily.Mean.PM10.Concentration~s(AODlarge)+s(x.1,y.1,k=11)+s(julian2,k=5)+wind.sp+atm.press, na.action=na.exclude,data=misr.aqspm10.met.ss)), file = "SpatioTemporalModels.txt", append = TRUE)
cat("ST model PM10 linear AODlarge met version 4\n", file = "SpatioTemporalModels.txt", append = TRUE)
capture.output(summary(gam(Daily.Mean.PM10.Concentration~s(AODlarge)+s(x.1,y.1,k=11)+julian2+wind.sp+atm.press, na.action=na.exclude,data=misr.aqspm10.met.ss)), file = "SpatioTemporalModels.txt", append = TRUE)

#### Cross-Validation ####
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

