### Using FEM for smoothing satellite and surface data ###
### script ###
library(geometry)
library(Matrix)
library(mgcv)
library(rgdal)

setwd('/Users/mf/Documents/Finite Element/fem2/fem')
#load("omi.summer.rdata")
source("fem_func.R")

# day1.tri <- delaunayn(day1.coord)
# tri.pt <- day1.tri

#MISR data
MISR.08.09<-read.csv(misr.08.09,"/Users/mf/Documents/MISR/Data/misr.08.09.csv")
# Choose 1 day for testing
day1.coord <- cbind(omi.summer[[1]]$Longitude, omi.summer[[1]]$Latitude)
new.coord <- day1.coord / 100
new.x <- seq(-4.3, 6, length.out=50)
new.y <- seq(-6.6, 5.1, length.out=50)

# MISR data September 2009 (monthly average)
# See MISR Data Extract.R
proj.albers<-"+proj=aea +lat_1=34.0 +lat_2=40.5 +lon_0=-120.0 +x_0=0 +y_0=-4000000 +ellps=GRS80 +datum=NAD83 +units=m"
newcoords.MISR<-project(as.matrix(cbind(AOD.dat.sep.09$lon, AOD.dat.sep.09$lat)), proj=proj.albers)
AOD.dat.sep.09$x<-newcoords.MISR[,1]/1000
AOD.dat.sep.09$y<-newcoords.MISR[,2]/1000

MISR.coord <- cbind(AOD.dat.sep.09$x, AOD.dat.sep.09$y)

# transform coord to enhance stability
new.MISR.coord <- MISR.coord / 100
new.x <- seq(-1, 4, length.out=50)
new.y <- seq(-5, 1, length.out=50)

## Use regular mesh
new.mesh <- tri.uniform.mesh(new.x, new.y)
new.mesh$get.tri(new.MISR.coord[428,])

new.mat <- tri.mat(new.mesh, new.MISR.coord)

# turn them to sparse matrix
lambda <- exp(-10:1)
#new.result <- tri.regular.solve(new.mat, omi.summer[[1]]$Log.ColumnAmountNO2Trop, lambda)
# best lambda acound 0.02
lambda <- exp(2:2.5)
new.result <- tri.regular.solve(new.mat, AOD.dat.sep.09$AOD.month, lambda)

## Use innate mesh
org.mesh <- tri.local.mesh(new.coord)
org.mat <- tri.mat(org.mesh, new.coord, phi=F)

org.result <- tri.local.solve(org.mat, omi.summer[[1]]$Log.ColumnAmountNO2Trop, lambda)
# best lambda acound 0.02



### break ###
## plot
library(fields)
new.f <- tri.regular.solve(new.mat, AOD.dat.sep.09$AOD.month, 0.02, f.out=T)
res <- 100
new.p.x <- seq(-4.2, 5.9, length.out=res)
new.p.y <- seq(-6.5, 5.0, length.out=res)

new.p.loc <- cbind(rep(new.p.x, res), rep(new.p.y, each=res))
new.p.phi <- tri.mat(new.mesh, new.p.loc)$phi
image.plot(new.p.x, new.p.y, matrix(new.p.phi %*% new.f$f, nrow=res, byrow=T), xlab="", ylab="", main="")
tri.plot(new.mesh, frame=F, lwd=0.5)


org.p.phi <- tri.mat(org.mesh, new.p.loc)$phi



### get the estimate for all the days using FEM
new.y.hat <- list()
new.f <- list()
for(i in 1:length(omi.summer)){
  new.coord <- cbind(omi.summer[[i]]$Longitude, omi.summer[[i]]$Latitude) / 100
  new.response <- omi.summer[[i]]$Log.ColumnAmountNO2Trop
  new.mat <- tri.mat(new.mesh, new.coord)
  lambda <- 0.02
  new.result <- tri.regular.solve(new.mat, new.response, lambda, f.out=T)
  new.y.hat[[i]] <- new.result[[1]]$y.hat
  new.f[[i]] <- new.result$f
  cat("Day: ", i, "\n")
}

org.mesh.list <- list()
org.f <- list()
lambda <- 0.02
for(i in 1:length(omi.summer)){
  cat("Day: ", i, "\n")
  org.coord <- cbind(omi.summer[[i]]$Longitude, omi.summer[[i]]$Latitude) / 100
  org.response <- omi.summer[[i]]$Log.ColumnAmountNO2Trop
  org.mesh.list[[i]] <- tri.local.mesh(org.coord)
  org.mat <- tri.mat(org.mesh.list[[i]], org.coord, phi=F)
  org.result <- tri.local.solve(org.mat, org.response, lambda) 
  org.f[[i]] <- org.result$y.hat
}


### For presentation
library(maps)
library(rgdal)
png("aug-1-uniform.png", width=800, height=800)
image.plot(new.p.x, new.p.y, matrix(new.p.phi %*% new.f[[18]], nrow=res), xlab="", ylab="", main="", zlim=c(31.5, 37.2))
ca.map <- map("state", "California", plot=F)
proj.albers<-"+proj=aea +lat_1=34.0 +lat_2=40.5 +lon_0=-120.0 +x_0=0 +y_0=-4000000 +ellps=GRS80 +datum=NAD83 +units=m"
ca.newcoords <- project(as.matrix(cbind(ca.map$x, ca.map$y)), proj=proj.albers)
ca.coords <- ca.newcoords / 100000
title("FE Smoothing on Aug 1st")
lines(ca.coords, lty=2, lwd=2)
#tri.plot(new.mesh, frame=F, lwd=0.5)
dev.off()

png("aug-1-uniform-w-mesh.png", width=800, height=800)
image.plot(new.p.x, new.p.y, matrix(new.p.phi %*% new.f[[18]], nrow=res), xlab="", ylab="", main="", zlim=c(31.5, 37.2))
ca.map <- map("state", "California", plot=F)
proj.albers<-"+proj=aea +lat_1=34.0 +lat_2=40.5 +lon_0=-120.0 +x_0=0 +y_0=-4000000 +ellps=GRS80 +datum=NAD83 +units=m"
ca.newcoords <- project(as.matrix(cbind(ca.map$x, ca.map$y)), proj=proj.albers)
ca.coords <- ca.newcoords / 100000
title("FE Smoothing on Aug 1st")
tri.plot(new.mesh, frame=F, lwd=0.1)
lines(ca.coords, lty=2, lwd=2)
dev.off()

load("../epa.site.summer.rdata")
epa.coord <- do.call(rbind, lapply(epa.site.summer, function(x){x$coord})) / 100
epa.phi <- tri.mat(new.mesh, epa.coord)$phi
epa.y.hat <- do.call(c, lapply(new.f, function(x){as.numeric(epa.phi %*% x)}))
epa.y <- as.numeric(do.call(rbind, lapply(epa.site.summer, function(x){x$Log.Mean.Value.in.ppb})))
summary(lm(epa.y~epa.y.hat))
temp <- lm(epa.y~epa.y.hat)

png("mesh.png", width=800, height=800)
tri.plot(new.mesh, frame=T, lwd=0.1)
dev.off()


ind <- sample(1:78, 78)
train.ind <- ind[1:61]
test.ind <- ind[62:78]
train.y.hat <- do.call(c, lapply(new.f[train.ind], function(x){as.numeric(epa.phi %*% x)}))
train.y <- as.numeric(do.call(rbind, lapply(epa.site.summer, function(x){x$Log.Mean.Value.in.ppb[train.ind]})))
temp <- lm(train.y~train.y.hat)
