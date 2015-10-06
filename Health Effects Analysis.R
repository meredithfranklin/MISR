library(foreign)
library(plyr)
as.numeric.factor <- function(x) {as.numeric(levels(x))[x]}


hosp.adm<-read.csv("/Volumes/projects_cen/NIS/Paper/Meredith Franklin/hosp_ca_08_09.csv")
keep.vars<-c("AGE","AMONTH","YEAR","HOSPZIP","FEMALE","RACE","ischemic","MS","Bronchitis","ari","Pneumonia","Influenza","copd","Asthma","HOSPADDR","HOSPCITY","HOSPNAME")
hosp.adm<-hosp.adm[keep.vars]
write.csv(hosp.adm,"/Volumes/projects_cen/NIS/Paper/Meredith Franklin/hosp_ca_ss_08_09.csv")

# zip code data for LA to subset hosp adm
zip<-read.dbf("/Users/mf/Documents/MISR/Data/Health/LA_zip.dbf")
zip$HOSPZIP<-as.numeric.factor(zip$ZCTA5CE10)
zip$lat<-as.numeric.factor(zip$INTPTLAT10)
zip$lon<-as.numeric.factor(zip$INTPTLON10)

# subset hosp admit 
hosp.adm<-read.csv("/Volumes/projects_cen/NIS/Paper/Meredith Franklin/hosp_ca_ss_08_09.csv")
hosp.adm$age2<-as.numeric.factor(hosp.adm$AGE)


# merge zip data with hosp admit
zip.hosp.adm<-join(zip, hosp.adm, by='HOSPZIP')
child.hosp.adm<-zip.hosp.adm[zip.hosp.adm$age2<18,]
summary(child.hosp.adm)
count(child.hosp.adm, c("ari"))
table(child.hosp.adm$ari)
count(child.hosp.adm, c("Bronchitis"))
count(child.hosp.adm, c("Pneumonia"))
count(child.hosp.adm, c("Influenza"))

# take counts by zip code
log.mod<-glm(ari~as.factor(RACE)+FEMALE+ age2,data=child.hosp.adm)
# match zip lat/lon with MISR pixel


