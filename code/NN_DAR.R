##NN DAR (nearest neighbor drainage area ratio prediction) daily flows prediction
###Annalise Blum
#Created:July 15, 2016  Updated: Oct 20,2016
##Data NEEDED for this file: "output/gagedsites_BC.rdata"
##Data sets created in this file:

library(geoR);library(fossil); library(plyr)

#### 1 - Load data sets


#### 1 - Load data sets ####
load("output/UVA_BC.rdata") #UVA gages Basin Chars
load("output/USGS_BC.rdata")#USGS gages Basin Chars

# load("output/AGBSites.rdata")
# load("output/fish_sitesLL.rdata")
# fishsites <- read.csv("YK/WSarea_forKanno.csv")
# load("output/SitesHUC2LL.rdata")
# load("output/sflow.rdata")

#### 2 - Prep data sets ####

#pull Lat and Long
UVA_LL<-UVA_BC[c("LAT_GAGE","LNG_GAGE")]
USGS_LL<-USGS_BC[c("LAT_GAGE","LNG_GAGE")]

# Sflows<-merge(SitesHUC2,sflow,by="site_no")
# Sflows$Smin7day<-Sflows$min7day/Sflows$DRAIN_SQMI
# Sflows<-Sflows[c("site_no","LAT_GAGE","LNG_GAGE","DRAIN_SQMI","year","season","Smin7day")]

#to find the nearest USGS gage to the fish sampling sites:
library(rgeos)
library(sp)
UVA_sp <- SpatialPoints(UVA_LL)
USGS_sp <- SpatialPoints(USGS_LL)
UVA_LL$nearest_usgsG <- apply(gDistance(USGS_sp, UVA_sp, byid=TRUE), 1, which.min) #returns entry number of closest
USGS_LL$NDX<-1:nrow(USGS_LL)
names(USGS_LL)<-c("USGS_LNG","USGS_LAT","nearest_usgsG")

#merge these together
NN<-merge(UVA_LL,USGS_LL,by="nearest_usgsG")

#merge back in the site numbers and other info:
#fishDA<-UVA_BC[c("site_no","DA_SQKM","LAT_GAGE","LNG_GAGE")]
UVA_BC<-UVA_DA[c("site_no","DA_SQKM")]

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
