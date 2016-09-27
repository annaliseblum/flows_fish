##sites
##Created: July 5, 2016 Updated: Sept 27,2016
##Annalise Blum annaliseblum@gmail.com
#rm(list=ls())

library(stringr)
library("dataRetrieval") #to pull data from USGS site

####1 - Import USGS gage site numbers and basin characteristics####
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

####2 - Import streamflow data from USGS NWIS ####
Sites<-unique(SitesHUC2$site_no)
parameterCd<-"00060" #set the parameter of interest at cfs
#rawDailyData<-readNWISdv(Sites,parameterCd, startDate = "1994-01-01", endDate = "2010-12-31") #statCd = "00003 defaults
save(rawDailyData, file="data/rawDailyData.rdata")

load("data/rawDailyData.rdata")
length(unique(rawDailyData$site_no)) #there are 47 sites with flow data from these days (from the 53 originally)

####3 - USGS sites basin characteristics from GAGESII ####
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

####4 - Import fish data and site info from Kanno et al sites####

#sites
fishsiteDf <- read.csv("YK/siteDf.csv")
save(fishsiteDf,file="output/fishsiteDf.rdata")
names(fishsiteDf)
fishsiteDf
fishsiteDf$Lat_n83<-as.numeric(fishsiteDf$Lat_n83)
fishsiteDf$Lon_n83<-as.numeric(fishsiteDf$Lon_n83)

#data
#load array with fish data from kanno folder
load("~/flows_fish/YK/countArray 115 sites.rdata")
save(countAr,file="output/countAr.rdata")


####5 - Import UVA site info ####
UVAstreamsSC <- ("data/UVAstreamsites.csv") #import site characteristics
save(UVAstreamsSC,file="data/UVAstreamsSC.rdata")
UVA_Discharge <- ("output/SWAS_data.csv") #import discharge data
save(UVA_Discharge,file="output/UVA_Discharge.rdata")

#### For Kyle to get DAYMET - Pull Lat and Longs of the selected sites ####

load("~/flows_fish/YK/countArray 115 sites.rdata")

#pull YOY data pass 1
YOYcountArP1<-as.data.frame(countAr[,,1,1]) #just YOY and pass 1
YOYcountArP1<-YOYcountArP1[,13:29] #just get 1994-2010
YOYcountArP1$yr.rec<-rowSums(!is.na(YOYcountArP1))
summary(YOYcountArP1$yr.rec)
sum(YOYcountArP1$yr.rec>9) #sites with at least 10 years of fish counts (1st pass)
fishSites<-YOYcountArP1[YOYcountArP1$yr.rec>9,]

fishSites$SiteID<-rownames(fishSites)
fishSites$SiteID<-substr(fishSites$SiteID, 3, 9)
fishSitesSub<-fishSites
fishSites34<-fishSites$SiteID
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

##FROM KYLE ROUND 2
R1sites<-fishSites$SiteID

#find positions
match(R1sites,Allsites)
length(match(R1sites,Allsites))
Allsites<-rownames(countAr)

Newsites<-Allsites[-match(R1sites,Allsites)]
Newsites_df<-data.frame(Newsites)
names(Newsites_df)<-"SiteID"
#both are factors, won't work for merging:
Newsites_df$SiteID<-as.character(Newsites_df$SiteID);fishsiteDf$SiteID<-as.character(fishsiteDf$SiteID)

#need to remove leading F_
Newsites_df$SiteID<-substr(Newsites_df$SiteID, 3, 9)

NEWfishSites<-merge(Newsites_df,fishsiteDf,by="SiteID")

NEWAGBSites<-NEWfishSites[c("SiteID","Lon_n83","Lat_n83")]
names(NEWAGBSites)<-c("site_no","LONG","LAT")

#Pull in uva sites
UVASitesLL <- read.csv("data/UVAsitesLL.csv")
UVASitesLL$SITE_ID<-as.character(UVASitesLL$site)
names(UVASitesLL)<-c("site_no","LONG","LAT")  

NEWSites<-rbind(UVASitesLL,NEWAGBSites)
NEWAGBSites<-NEWSites
save(NEWAGBSites,file="output/NEWAGBSites.rdata")
