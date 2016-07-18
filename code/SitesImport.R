##sites
##July 5, 2016
##Annalise Blum annaliseblum@gmail.com

library(stringr)
library("dataRetrieval") #to pull data from USGS site

####Import USGS gage site numbers and basin characteristics####
#Import list of sites in the eastern highland
EastHi <- read.csv("data/RefEastHi.csv",colClasses="character") #7 variables
EastHi$site_no<-str_pad(EastHi$STAID, 8, side="left", pad = "0") #pad with leading zeros until length is at least 8

#Import sheet of basin characteristics to get HUCs
GAGESIIBasinID <- read.csv("data/GAGEIIBasinID.csv",colClasses="character") #7 variables, don't lose leading 0 by importing as chars
names(GAGESIIBasinID)[1]<-"site_no"

#merge with sheet of basin characteristics to get HUCs
EastHiBC<-merge(EastHi,GAGESIIBasinID,by="site_no")

#drop extra vars
Sitelist<-EastHiBC[c("site_no","DRAIN_SQKM","HUC02","LAT_GAGE","LNG_GAGE","STATE")]

summary(as.numeric(Sitelist$HUC02)) #53 sites in huc 02, 73 in huc 07

Sitelist$HUC02[as.numeric(Sitelist$HUC02)==2]

SitesHUC2<-Sitelist[as.numeric(Sitelist$HUC02)==2,]
SitesHUC2<-SitesHUC2[!is.na(SitesHUC2$site_no),]

SitesHUC2$LAT_GAGE<-as.numeric(SitesHUC2$LAT_GAGE)
SitesHUC2$LNG_GAGE<-as.numeric(SitesHUC2$LNG_GAGE)

####Import streamflow data from USGS NWIS ####
# Sites<-unique(SitesHUC2$site_no)
# parameterCd<-"00060" #set the parameter of interest at cfs
# rawDailyData<-readNWISdv(Sites,parameterCd, startDate = "1994-01-01", endDate = "2010-12-31") #statCd = "00003 defaults

##pulled USGS NWIS data on desktop because I couldn't get the R package to work on the server
load("~/flows_fish/rawDailyData.rdata")
length(unique(rawDailyData$site_no)) #there are 47 sites with flow data from these days (from the 53 originally)

####USGS sites basin characteristics from GAGESII ####
#annoying that dataRetriveal won't install on server
GAGESII_Hydro <- read.csv("data/GAGESII_Hydro.csv",colClasses=c("character",rep("numeric",33))) #fix classes
ncol(GAGESII_Hydro); head(GAGESII_Hydro)
names(GAGESII_Hydro)[1]<-"site_no"

GAGESII_Topo <- read.csv("data/GAGESII_Topo.csv",colClasses=c("character",rep("numeric",12))) #
ncol(GAGESII_Topo); head(GAGESII_Topo)
names(GAGESII_Topo)[1]<-"site_no"

#Pull just the variables for which I have for the fish sites also
names(GAGESII_Topo)
GAGESII_TopoC<-GAGESII_Topo[c("site_no","SLOPE_PCT","ASPECT_DEGREES","ELEV_SITE_M","ELEV_MEAN_M_BASIN")]
names(GAGESII_Hydro)
GAGESII_HydroC<-GAGESII_Hydro[c("site_no","REACHCODE","BFI_AVE","TOPWET")]

USGS_BC<-merge(SitesHUC2,GAGESII_HydroC,by="site_no")

USGS_BC<-merge(USGS_BC,GAGESII_TopoC)
save(USGS_BC,file="output/USGS_BC.rdata")

####Import fish site info from Kanno et al sites####
fishsiteDf <- read.csv("YK/siteDf.csv",colClasses="character") #7 variables, don't lose leading 0 by importing as chars
names(fishsiteDf)
fishsiteDf
fishsiteDf$Lat_n83<-as.numeric(fishsiteDf$Lat_n83)
fishsiteDf$Lon_n83<-as.numeric(fishsiteDf$Lon_n83)

fishsites <- read.csv("YK/WSarea_forKanno.csv") #7 variables, don't lose leading 0 by importing as chars


#figure out which 30 sites are the ones with 10 years of 3 passes 1994-2010

#### MAPS####

library("ggmap")
library("ggplot2")

summary(SitesHUC2$LAT_GAGE)
summary(SitesHUC2$LNG_GAGE)

summary(fishsiteDf$Lat_n83)
summary(fishsiteDf$Lon_n83)

# lat <- c(37, 41.5) #define our map's ylim
# lon <- c(-81, -74.1) #define our map's xlim
# center = c(mean(lat), mean(lon))  #tell what point to center on
# myLocation<-c(mean(lat), mean(lon))
# myLocation<-c(40, -82, 49,-67.1)
myLocation<-"Shenandoah, Virginia"

myMap<- get_map(location=myLocation, source="google", maptype="terrain", crop=FALSE,zoom=7)
pdf(file="output/site_map.pdf")
ggmap(myMap)+geom_point(aes(x = LNG_GAGE, y = LAT_GAGE), data = SitesHUC2, color="darkred",size = 3)+
  geom_point(aes(x = Lon_n83, y = Lat_n83), data = fishsiteDf, color="black",size = 2)
dev.off()

#### For Kyle - Pull Lat and Longs of the selected sites ####

load("~/flows_fish/YK/countArray 115 sites.rdata")

#pull YOY data pass 1
YOYcountArP1<-as.data.frame(countAr[,,1,1]) #just YOY and pass 1
YOYcountArP1<-YOYcountArP1[,13:29] #just get 1994-2010
YOYcountArP1$yr.rec<-rowSums(!is.na(YOYcountArP1))
summary(YOYcountArP1$yr.rec)
sum(YOYcountArP1$yr.rec>9) #sites with at least 10 years of fish counts (1st pass)
fishSites<-YOYcountArP1[YOYcountArP1$yr.rec>9,]

fishSites$SiteID<-rownames(fishSites)
fishSites$SiteID<-substr(fishSites$SiteID, 3, 7)
fishSitesSub<-fishSites

fishSites<-merge(fishSites,fishsiteDf,by="SiteID")

fish_sitesLL<-fishSites[c("SiteID","Lon_n83","Lat_n83")]
names(fish_sitesLL)<-c("site_no","LONG","LAT")
Fish_sites<-fish_sitesLL$site_no
save(fish_sitesLL,file="output/fish_sitesLL.rdata")

SitesHUC2LL<-SitesHUC2[c("site_no","LNG_GAGE","LAT_GAGE")]
names(SitesHUC2LL)<-c("site_no","LONG","LAT")
save(SitesHUC2LL,file="output/SitesHUC2LL.rdata")

AGBSites<-rbind(fish_sitesLL,SitesHUC2LL)

save(AGBSites,file="output/AGBSites.rdata")
