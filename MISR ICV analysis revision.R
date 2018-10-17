library(geoR)
library(fields)
library(dplyr)

# Match ALL ICV with MISR grid, not just when there is a MISR overpass
icv_new<-read.csv("/Users/mf/Documents/MISR/Data/icv_new2.csv")
icv_new2<-icv_new[!is.na(icv_new$x),]
icv_new2$PM25_ug<-icv_new2$PM25/1000
icv_new2$coarse_ug<-icv_new2$Coarse/1000

# MISR predictions to get the grid cells
misr_06_11_pred_pm25<-read.csv("/Users/mf/Documents/MISR/Data/predicted_pm25_2006_2011.csv")
misr_08_09_pred_pm25<-misr_06_11_pred_pm25[misr_06_11_pred_pm25$year %in% c(2008,2009),-1]
misr_08_09_pred_pm25$MISR_x<-round(misr_08_09_pred_pm25$x.1,6)
misr_08_09_pred_pm25$MISR_y<-round(misr_08_09_pred_pm25$y.1,6)

#create 2008-2009 average predPM for ID
# subgrid analysis
misr_08_09<-misr_08_09_pred_pm25 %>% group_by(MISR_x,MISR_y) %>% mutate(AOD = mean(AOD),MISRpredPM25=mean(predicted.pm25))

#misr_08_09 <- ddply(misr_08_09_pred_pm25, .(x,y), summarise, AOD=mean(AOD), MISRpredPM25=mean(predicted.pm25))
#colnames(misr_08_09)<-c("MISR_x","MISR_y","AOD","MISRpredPM25")

MISR_grid<-unique(misr_08_09[,28:29])

# distances between misr grid and icv
dist.misr.icv<-rdist(cbind(MISR_grid$MISR_x,MISR_grid$MISR_y),cbind(icv_new2$x,icv_new2$y))

# match ICV to MISR grid cell
MISR_ICV_match_list<-vector('list',length(dist.misr.icv[1,]))
    
  for (j in 1:length(dist.misr.icv[1,])){
    if (min(dist.misr.icv[,j])<=4.3){
      MISR_ICV_match_list[[j]]<-data.frame(misr_08_09[which.min(dist.misr.icv[,j]),],icv_new2[j,]) # identifies misr pixel close to ICV site
    }
}

MISRGRID_ICV <- do.call("rbind", MISR_ICV_match_list)
# x,y are the ICV locations, MISR_x, MISR_y are the grid cells

# subgrid analysis
MISRGRID_ICV2<-MISRGRID_ICV %>% group_by(MISR_x,MISR_y,season) %>% mutate(count = n())
# remove grid cells with only one ICV
MISRGRID_ICV3<-MISRGRID_ICV2[MISRGRID_ICV2$count>1,]

MISRGRID_ICV3_w<-MISRGRID_ICV3[MISRGRID_ICV3$season=="warm",]
MISRGRID_ICV3_c<-MISRGRID_ICV3[MISRGRID_ICV3$season=="cool",]

s2<-MISRGRID_ICV3_w %>%
  group_by(townname) %>%
  summarise(mean=mean(count,na.rm=TRUE),  meanPM=mean((PM25/1000)),sdPM=sd(PM25/1000),meanPMC=mean(Coarse/1000),sdPMC=sd(Coarse/1000),n=n())
s2<-data.frame(s2)


# counts and variance by MISR grid cell

pm25freq_means_sd_cool<-ddply(MISRGRID_ICV3_c, .(MISR_x,MISR_y), summarise, town=town.x[1], avgPMc=mean(PM25/1000,na.rm=TRUE), varPMc=(sd(PM25/1000,na.rm=TRUE))^2,freqPMc=length(PM25))
pm25freq_means_sd_warm<-ddply(MISRGRID_ICV3_w, .(MISR_x,MISR_y), summarise, town=town.x[1], avgPMw=mean(PM25/1000,na.rm=TRUE), varPMw=(sd(PM25/1000,na.rm=TRUE))^2,freqPMw=length(PM25))

pmcoarsefreq_means_sd_cool<-ddply(MISRGRID_ICV3_c, .(MISR_x,MISR_y), summarise, town=town.x[1], avgCoarsec=mean(Coarse/1000,na.rm=TRUE), varCoarsec=(sd(Coarse/1000,na.rm=TRUE))^2,freqCoarsec=length(Coarse))
pmcoarsefreq_means_sd_warm<-ddply(MISRGRID_ICV3_w, .(MISR_x,MISR_y), summarise, town=town.x[1], avgCoarsew=mean(Coarse/1000,na.rm=TRUE), varCoarsew=(sd(Coarse/1000,na.rm=TRUE))^2,freqCoarsew=length(Coarse))

PMvar_all1<-merge(pm25freq_means_sd_cool, pm25freq_means_sd_warm, by = c('MISR_x', 'MISR_y'),all=TRUE)
PMvar_all2<-merge(pmcoarsefreq_means_sd_cool, pmcoarsefreq_means_sd_warm, by = c('MISR_x', 'MISR_y'),all=TRUE)
PMvar_all<-merge(PMvar_all1,PMvar_all2,by=c('MISR_x','MISR_y'),all=TRUE)


write.csv(PMvar_all,"/Users/mf/Documents/MISR/Papers and Reports/MISR Subgrid/subgridvarianceICV_resubmission.csv",row.names = FALSE)

library(lme4)
# treat misr grid as a grouping variable (random effect)
MISRGRID_ICV3$MISRpredPM25<-as.factor(MISRGRID_ICV3$MISRpredPM25)
MISRGRID_ICV3_w<-MISRGRID_ICV3[MISRGRID_ICV3$season=="warm",]
lme.pm.w<-lmer((PM25_ug)~ (1|MISRpredPM25),data=MISRGRID_ICV3_w)
summary(lme.pm.w)
CV.PM25.w=100*1.451/15.1031
ICC.PM25.w=4.572/(4.572+2.105)


lme.pmc.w<-lmer((coarse_ug)~ (1|MISRpredPM25),data=MISRGRID_ICV3_w)
summary(lme.pmc.w)
CV.PMC.w=100*2.932/14.1369
ICC.PMC.w=9.970/(9.970+8.596)

MISRGRID_ICV3_c<-MISRGRID_ICV3[MISRGRID_ICV3$season=="cool",]
lme.pm.c<-lmer((PM25_ug)~ (1|MISRpredPM25),data=MISRGRID_ICV3_c)
summary(lme.pm.c)
CV.PM25.c=100*1.523/14.3663
ICC.PM25.c=24.903/(24.903+2.319)


lme.pmc.c<-lmer((coarse_ug)~ (1|MISRpredPM25),data=MISRGRID_ICV3_c)
summary(lme.pmc.c)
CV.PMC.c=100*1.727/12.2150
ICC.PMC.c=19.723/(19.723+2.982)



# Spatial analysis of ICV data on its own (no matching with MISR grid)

icv_new_w<-icv_new2[icv_new2$season=="warm", ]
# Summary statistics on ALL ICV data seasonally
s<-icv_new_w %>%
  group_by(townname) %>%
  summarise(mean=mean(PM25_ug,na.rm=TRUE),median=median(PM25_ug,na.rm=TRUE),min=min(PM25_ug,na.rm=TRUE),max=max(PM25_ug,na.rm=TRUE),sd=sd(PM25_ug,na.rm=TRUE),n=n())
s2<-data.frame(s)
# overall 
mean(icv_new_w$PM25_ug)
sd(icv_new_w$PM25_ug)

s<-icv_new_w %>%
  group_by(townname) %>%
  summarise(mean=mean(coarse_ug,na.rm=TRUE),median=median(coarse_ug,na.rm=TRUE),min=min(coarse_ug,na.rm=TRUE),max=max(coarse_ug,na.rm=TRUE),sd=sd(coarse_ug,na.rm=TRUE),n=n())
s2<-data.frame(s)
mean(icv_new_w$coarse_ug)
var(icv_new_w$coarse_ug)

icv_new_c<-icv_new2[icv_new2$season=="cool", ]
s<-icv_new_c %>%
  group_by(townname) %>%
  summarise(mean=mean(PM25_ug,na.rm=TRUE),median=median(PM25_ug,na.rm=TRUE),min=min(PM25_ug,na.rm=TRUE),max=max(PM25_ug,na.rm=TRUE),sd=sd(PM25_ug,na.rm=TRUE),n=n())
s2<-data.frame(s)
mean(icv_new_c$PM25_ug)
var(icv_new_c$PM25_ug)

s<-icv_new_c %>%
  group_by(townname) %>%
  summarise(mean=mean(coarse_ug,na.rm=TRUE),median=median(coarse_ug,na.rm=TRUE),min=min(coarse_ug,na.rm=TRUE),max=max(coarse_ug,na.rm=TRUE),sd=sd(coarse_ug,na.rm=TRUE),n=n())
s2<-data.frame(s)
mean(icv_new_c$coarse_ug)
var(icv_new_c$coarse_ug)

######## SPATIAL ANALYSIS SENSITIVITY ########
## PM2.5 ##
## WARM ##

icv_new3<-icv_new2[icv_new2$town.x!="SB", ]
icv_new3_w<-icv_new3[icv_new3$season=="warm", ]

icv.pm25.geow<-as.geodata(icv_new3_w,coords.col=c(33,34),data.col=35)
plot(icv.pm25.geow)


# larger scale
icv.pm25.variog.w<-variog(icv.pm25.geow,max.dist=15, uvec=seq(0,15,l=15),estimator.type="modulus",bin.cloud = TRUE)

pdf("/Users/mf/Documents/MISR/Papers and Reports/MISR Subgrid/Remote Sensing Submission/Revision 2/Vario_boxplot_warm_allICV.pdf")
plot(icv.pm25.variog.w,bin.cloud=TRUE, cex = 2, pch= 16, col = "black",xlab="Distance (Km)", ylab = ~ "Semivariance ( " *( mu^2 * g / m^6) * ")", font.main = 1, cex.lab = 2)
dev.off()
median(icv.pm25.variog.w$bin.cloud[[1]]) #1km= 0.95 2km=0.95 3km=1.0 4km=1.1 5km= 1.1 ug/m3


#multiple comparison test icv.pm25.variog.w$bin.cloud[[15]]
for(i in 1:length(icv.pm25.variog.w$bin.cloud)){ 
  icv.pm25.variog.w$bin.cloud[[i]] <- cbind(icv.pm25.variog.w$bin.cloud[[i]], 
                                            id=rep(i, length(icv.pm25.variog.w$bin.cloud[[i]])))}
icv.pm25.variog.w<-do.call("rbind", icv.pm25.variog.w$bin.cloud)
colnames(icv.pm25.variog.w)<-c("pm","id")
pairwise.t.test(as.vector(icv.pm25.variog.w[,1]), as.vector(icv.pm25.variog.w[,2]), pool.sd=TRUE,p.adj="bonferroni")



# smaller scale
icv.pm25.variog.w2<-variog(icv.pm25.geow,uvec=seq(0,4.4,l=18),estimator.type="modulus",bin.cloud = TRUE)
pdf("/Users/mf/Documents/MISR/Papers and Reports/MISR Subgrid/Vario_subgrid_boxplot_warm_allICV.pdf")
plot(icv.pm25.variog.w2,bin.cloud=TRUE,xlab="Distance, km",ylab="Semivariance")
dev.off()

#multiple comparison test icv.pmc.variog.c$bin.cloud[[15]]
for(i in 1:length(icv.pm25.variog.w2$bin.cloud)){ 
  icv.pm25.variog.w2$bin.cloud[[i]] <- cbind(icv.pm25.variog.w2$bin.cloud[[i]], 
                                           id=rep(i, length(icv.pm25.variog.w2$bin.cloud[[i]])))}
icv.pm25.variog.w2<-do.call("rbind", icv.pm25.variog.w2$bin.cloud)
colnames(icv.pm25.variog.w2)<-c("pm","id")
pairwise.t.test(as.vector(icv.pm25.variog.w2[,1]), as.vector(icv.pm25.variog.w2[,2]), pool.sd=TRUE,p.adj="bonferroni")



## COOL ##

icv_new3_c<-icv_new3[icv_new3$season=="cool", ]

icv.pm25.geoc<-as.geodata(icv_new3_c,coords.col=c(33,34),data.col=35)
plot(icv.pm25.geoc)

# larger scale
icv.pm25.variog.c<-variog(icv.pm25.geoc,max.dist=15, uvec=seq(0,15,l=15),estimator.type="modulus",bin.cloud = TRUE)

pdf("/Users/mf/Documents/MISR/Papers and Reports/MISR Subgrid/Remote Sensing Submission/Revision 2/Vario_boxplot_cool_allICV.pdf")
plot(icv.pm25.variog.c,bin.cloud=TRUE, cex = 1.5, pch= 16, col = "black",xlab="Distance (Km)", ylab = ~ "Semivariance ( " *( mu^2 * g / m^6) * ")", font.main = 1, cex.lab = 2)
dev.off()
median(icv.pm25.variog.c$bin.cloud[[5]]) #1km= 0.95 2km=0.95 3km=1.0 4km=1.1 5km= 1.1 ug/m3

# multiple comparisons
for(i in 1:length(icv.pm25.variog.c$bin.cloud)){ 
  icv.pm25.variog.c$bin.cloud[[i]] <- cbind(icv.pm25.variog.c$bin.cloud[[i]], 
                                            id=rep(i, length(icv.pm25.variog.c$bin.cloud[[i]])))}
icv.pm25.variog.c<-do.call("rbind", icv.pm25.variog.c$bin.cloud)
colnames(icv.pm25.variog.c)<-c("pm","id")
pairwise.t.test(as.vector(icv.pm25.variog.c[,1]), as.vector(icv.pm25.variog.c[,2]), pool.sd=TRUE,p.adj="bonferroni")


# smaller scale
icv.pm25.variog.c2<-variog(icv.pm25.geoc,uvec=seq(0,4.4,l=18),estimator.type="modulus",bin.cloud = TRUE)

pdf("/Users/mf/Documents/MISR/Papers and Reports/MISR Subgrid/Remote Sensing Submission/Revision 3/Vario_subgrid_boxplot_cool_allICV.pdf", pointsize = 15)
plot(icv.pm25.variog.c2,bin.cloud=TRUE,xlab="Distance, km",ylab="Semivariance")
dev.off()

median(icv.pm25.variog.c2$bin.cloud[[2]]) #1km= 0.95 2km=0.95 3km=1.0 4km=1.1 5km= 1.1 ug/m3

for(i in 1:length(icv.pm25.variog.c2$bin.cloud)){ 
  icv.pm25.variog.c2$bin.cloud[[i]] <- cbind(icv.pm25.variog.c2$bin.cloud[[i]], 
                                             id=rep(i, length(icv.pm25.variog.c2$bin.cloud[[i]])))}
icv.pm25.variog.c2<-do.call("rbind", icv.pm25.variog.c2$bin.cloud)
colnames(icv.pm25.variog.c2)<-c("pm","id")
pairwise.t.test(as.vector(icv.pm25.variog.c2[,1]), as.vector(icv.pm25.variog.c2[,2]), pool.sd=FALSE,p.adj="bonferroni")

## COARSE PM ##

## warm ##

icv.pmc.geow<-as.geodata(icv_new3_w,coords.col=c(33,34),data.col=36)
plot(icv.pmc.geow)


icv.pmc.variog.w<-variog(icv.pmc.geow,uvec=seq(0,15,l=15),estimator.type="modulus",bin.cloud = TRUE)

pdf("/Users/mf/Documents/MISR/Papers and Reports/MISR Subgrid/Remote Sensing Submission/Revision 2/Vario_boxplot_Coarsewarm_allICV.pdf")
plot(icv.pmc.variog.w,bin.cloud=TRUE,xlab="Distance, km",ylab="Semivariance")
dev.off()
median(icv.pmc.variog.w$bin.cloud[[1]]) #1km= 0.95 2km=0.95 3km=1.0 4km=1.1 5km= 1.1 ug/m3


#multiple comparison test icv.pmc.variog.c$bin.cloud[[15]]
for(i in 1:length(icv.pmc.variog.w$bin.cloud)){ 
  icv.pmc.variog.w$bin.cloud[[i]] <- cbind(icv.pmc.variog.w$bin.cloud[[i]], 
                                           id=rep(i, length(icv.pmc.variog.w$bin.cloud[[i]])))}
icv.pmc.variog.w<-do.call("rbind", icv.pmc.variog.w$bin.cloud)
colnames(icv.pmc.variog.w)<-c("pm","id")
pairwise.t.test(as.vector(icv.pmc.variog.w[,1]), as.vector(icv.pmc.variog.w[,2]), pool.sd=TRUE,p.adj="bonferroni")


# subgrid
icv.pmc.variog.w2<-variog(icv.pmc.geow,uvec=seq(0,4.4,l=18),estimator.type="modulus",bin.cloud = TRUE)
plot(icv.pmc.variog.w2,bin.cloud=TRUE,xlab="Distance, km",ylab="Semivariance")


for(i in 1:length(icv.pmc.variog.w2$bin.cloud)){ 
  icv.pmc.variog.w2$bin.cloud[[i]] <- cbind(icv.pmc.variog.w2$bin.cloud[[i]], 
                                            id=rep(i, length(icv.pmc.variog.w2$bin.cloud[[i]])))}
icv.pmc.variog.w2<-do.call("rbind", icv.pmc.variog.w2$bin.cloud)
colnames(icv.pmc.variog.w2)<-c("pm","id")
pairwise.t.test(as.vector(icv.pmc.variog.w2[,1]), as.vector(icv.pmc.variog.w2[,2]), pool.sd=TRUE,p.adj="bonferroni")





## cool ##

icv.pmc.geoc<-as.geodata(icv_new3_c,coords.col=c(33,34),data.col=36)
plot(icv.pm25.geoc)

# larger scale coarse cool
icv.pmc.variog.c<-variog(icv.pmc.geoc,max.dist=15, uvec=seq(0,15,l=15),estimator.type="modulus",bin.cloud = TRUE)

pdf("/Users/mf/Documents/MISR/Papers and Reports/MISR Subgrid/Remote Sensing Submission/Revision 2/Vario_boxplot_Coarsecool_allICV.pdf")
plot(icv.pmc.variog.c,bin.cloud=TRUE,xlab="Distance, km",ylab="Semivariance")
dev.off()
median(icv.pmc.variog.c$bin.cloud[[5]]) 


for(i in 1:length(icv.pmc.variog.c$bin.cloud)){ 
  icv.pmc.variog.c$bin.cloud[[i]] <- cbind(icv.pmc.variog.c$bin.cloud[[i]], 
                                            id=rep(i, length(icv.pmc.variog.c$bin.cloud[[i]])))}
icv.pmc.variog.c<-do.call("rbind", icv.pmc.variog.c$bin.cloud)
colnames(icv.pmc.variog.c)<-c("pm","id")
pairwise.t.test(as.vector(icv.pmc.variog.c[,1]), as.vector(icv.pmc.variog.c[,2]), pool.sd=TRUE,p.adj="bonferroni")


# small scale coarse cool
icv.pmc.variog.c2<-variog(icv.pmc.geoc,uvec=seq(0,4.4,l=18),estimator.type="modulus",bin.cloud = TRUE)

pdf("/Users/mf/Documents/MISR/Papers and Reports/MISR Subgrid/Remote Sensing Submission/Revision 2/Vario_subgrid_boxplot_Coarsecool_allICV.pdf")
plot(icv.pmc.variog.c2,bin.cloud=TRUE,xlab="Distance, km",ylab="Semivariance")
dev.off()
median(icv.pmc.variog.c2$bin.cloud[[4]]) #1km= 0.95 2km=0.95 3km=1.0 4km=1.1 5km= 1.1 ug/m3

#multiple comparison test icv.pmc.variog.c$bin.cloud[[15]]
for(i in 1:length(icv.pmc.variog.c2$bin.cloud)){ 
  icv.pmc.variog.c2$bin.cloud[[i]] <- cbind(icv.pmc.variog.c2$bin.cloud[[i]], 
                                            id=rep(i, length(icv.pmc.variog.c2$bin.cloud[[i]])))}
icv.pmc.variog.c2<-do.call("rbind", icv.pmc.variog.c2$bin.cloud)
colnames(icv.pmc.variog.c2)<-c("pm","id")
pairwise.t.test(as.vector(icv.pmc.variog.c2[,1]), as.vector(icv.pmc.variog.c2[,2]), pool.sd=TRUE,p.adj="bonferroni")

