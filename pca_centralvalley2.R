bakers10<-read.csv("/Volumes/Projects/Satellite/MISR/STN/bakers_STN_MISR_Low10Chisq.csv")
fresno10<-read.csv("/Volumes/Projects/Satellite/MISR/STN/fresno_STN_MISR_Low10Chisq.csv")
modesto10<-read.csv("/Volumes/Projects/Satellite/MISR/STN/modest_STN_MISR_Low10Chisq.csv")
visali10<-read.csv("/Volumes/Projects/Satellite/MISR/STN/visali_STN_MISR_Low10Chisq.csv")

# coincident pixel with STN site
# choose smallest distance between MISR and STN

bakers10$dist <- sapply(1:nrow(bakers10),function(i)
  spDistsN1(as.matrix(bakers10[i,3:4]),as.matrix(bakers10[i,37:38]),longlat=T))

fresno10$dist <- sapply(1:nrow(fresno10),function(i)
  spDistsN1(as.matrix(fresno10[i,3:4]),as.matrix(fresno10[i,37:38]),longlat=T))

modesto10$dist <- sapply(1:nrow(modesto10),function(i)
  spDistsN1(as.matrix(modesto10[i,3:4]),as.matrix(modesto10[i,37:38]),longlat=T))

visali10$dist <- sapply(1:nrow(visali10),function(i)
  spDistsN1(as.matrix(visali10[i,3:4]),as.matrix(visali10[i,37:38]),longlat=T))

bakers10.matched<-bakers10 %>% 
          group_by(date) %>% 
              slice(which.min(dist))

fresno10.matched<-fresno10 %>% 
  group_by(date) %>% 
  slice(which.min(dist))

modesto10.matched<-modesto10 %>% 
  group_by(date) %>% 
  slice(which.min(dist))

visali10.matched<-visali10 %>% 
          group_by(date) %>% 
                slice(which.min(dist))

all.aod10.matched<-data.frame(rbind(bakers10.matched, fresno10.matched, modesto10.matched, visali10.matched))

hist(c(all.aod10.matched$mixid.min01,all.aod10.matched$mixid.min02,all.aod10.matched$mixid.min03,all.aod10.matched$mixid.min04,
       all.aod10.matched$mixid.min05,all.aod10.matched$mixid.min06,all.aod10.matched$mixid.min07,all.aod10.matched$mixid.min08,
       all.aod10.matched$mixid.min09,all.aod10.matched$mixid.min10),breaks=30, xlab="MISR Mixture #", main="")

table(c(all.aod10.matched$mixid.min01,all.aod10.matched$mixid.min02,all.aod10.matched$mixid.min03,all.aod10.matched$mixid.min04,
       all.aod10.matched$mixid.min05,all.aod10.matched$mixid.min06,all.aod10.matched$mixid.min07,all.aod10.matched$mixid.min08,
       all.aod10.matched$mixid.min09,all.aod10.matched$mixid.min10))

RowVar <- function(x) {
  rowSums((x - rowMeans(x))^2)/(dim(x)[2] - 1)
}

mix.num.mean<-rowMeans(all.aod10.matched[,c(8,11,14,17,20,23,26,29,32,35)])
all.aod10.matched$mix.num.mean<-mix.num.mean
hist(mix.num.mean,breaks=30,xlab="MISR Mixture # Mean Lowest 10 ChiSq",main="")
mix.num.median<-apply(all.aod10.matched[,c(8,11,14,17,20,23,26,29,32,35)], 1, median, na.action=na.omit)
hist(mix.num.median,breaks=30,xlab="MISR Mixture # Median Lowest 10 ChiSq",main="")


mix.num<-all.aod10.matched[,c(8,11,14,17,20,23,26,29,32,35)]
mix.num.var<-RowVar(mix.num)
hist(RowVar(mix.num),breaks=20)

all.aod10.matched$mix.num.var<-mix.num.var

aod.pca<-prcomp(~ ., bakers10.matched[,c(6,9,12,15,18,21,24,27,30,33)],center=TRUE,scale.=TRUE)
summary(aod.pca)
biplot(aod.pca,choices=2:3)
