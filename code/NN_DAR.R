##NN DAR LF prediction
##(nearest neighbor drainage area ratio prediction)
##July 15, 2016

library(geoR) 
library(fossil)
library(plyr)

load("output/AGBSites.rdata")
load("output/fish_sitesLL.rdata")
fishsites <- read.csv("YK/WSarea_forKanno.csv")

load("output/SitesHUC2LL.rdata")
load("output/sflow.rdata")

#merge Lat and Long with seasonal min flows to standardize them by sq miles
sum(as.numeric(SitesHUC2$DRAIN_SQKM)<50) #15 are under 50km2 which ideally would be the only ones used for regional regression
SitesHUC2$DRAIN_SQMI<-as.numeric(SitesHUC2$DRAIN_SQKM)/(1.60934^2)

Sflows<-merge(SitesHUC2,sflow,by="site_no")
Sflows$Smin7day<-Sflows$min7day/Sflows$DRAIN_SQMI
Sflows<-Sflows[c("site_no","LAT_GAGE","LNG_GAGE","DRAIN_SQMI","year","season","Smin7day")]
GAGEDLL<-SitesHUC2LL[c("LONG","LAT")]
FishLL<-fish_sitesLL[c("LONG","LAT")]

#to find the nearest USGS gage to the fish sampling sites:
library(rgeos)
library(sp)
fish1sp <- SpatialPoints(FishLL)
usgs2sp <- SpatialPoints(GAGEDLL)
FishLL$nearest_usgsG <- apply(gDistance(usgs2sp, fish1sp, byid=TRUE), 1, which.min) #ordering confusing...
GAGEDLL$NDX<-1:nrow(GAGEDLL)
names(GAGEDLL)<-c("G_LNG","G_LAT","nearest_usgsG")

#merge these together
NN<-merge(FishLL,GAGEDLL,by="nearest_usgsG")

#merge back in the site numbers and other info:
fishDA<-fishsites[c("SiteID","WatershedArea_ha","lattitude","longitude")]
fishDA$DA_SQMI<-fishDA$WatershedArea_ha/258.999
fishDA<-fishDA[c("SiteID","DA_SQMI")]
names(fishDA)[1]<-"site_no"

fishDA2<-merge(fish_sitesLL,NN,by=c("LAT","LONG"))
fishDA3<-merge(fishDA2,fishDA,by="site_no") #merge in DA of sites

USGSsites<-SitesHUC2[c("site_no","DRAIN_SQMI","LAT_GAGE","LNG_GAGE")]
names(USGSsites)<-c("site_no","G_DA_SQMI","G_LAT","G_LNG")

ALL<-merge(USGSsites,fishDA3,by=c("G_LAT",  "G_LNG"))table(as.factor(ALL$site_no.x)) #there are 4 sites
unique(ALL$G_DA_SQMI) #with DAs 94.78855 114.51847  14.63334  22.70292

NNmatched<-ALL[c("site_no.x", "G_DA_SQMI", "site_no.y",  "DA_SQMI")]
names(NNmatched)<-c("site_no", "G_DA_SQMI", "fish_site",  "F_DA_SQMI")

#Now take the season/year/site min7flow/DA from the nearest gaged site and apply to fish site*DA
NNpreds<-merge(NNmatched,sflow,by="site_no")

NNpreds$NN_pred<-(merge$min7day/merge$G_DA_SQMI)*merge$F_DA_SQMI #predict seasonal min7day flow


##map these...
library("ggmap")
library("ggplot2")

summary(NN$LONG); summary(NN$LAT)
summary(NN$G_LNG); summary(NN$G_LAT)

myLocation<-"Shenandoah, Virginia"

myMap<- get_map(location=myLocation, source="google", maptype="terrain", crop=FALSE,zoom=7)
pdf(file="output/site_map.pdf")
ggmap(myMap)+geom_point(aes(x = LONG, y = LAT), data = NN, color="darkred",size = 3)+
  geom_point(aes(x = G_LNG, y = G_LAT), data = NN, color="black",size = 2)
dev.off()
