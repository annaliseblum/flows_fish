##Import Raw Data and save as r data sets for fish and extreme flows project
##Created: July 5, 2016 Updated: Dec 5,2016
##Annalise Blum annaliseblum@gmail.com

#rm(list=ls())

#data sets created in this file (And section):
# 2 load("data/rawDailyData.rdata") #flows for 47 USGS sites in HUC2 and GAGESII EasternMts
# 3 load("output/USGS_BC.rdata") #basin characteristics from GAGESII
# 4 load("output/UVAstreamsSC.rdata") #UVA sites characteristics
# 4 load("output/UVA_Discharge.rdata") #UVA flow data
# 5 load("output/fishsiteDf.rdata") #fish site characteristics from Kanno et al (2016)
# 5 load("output/countAr.rdata") #fish DATA array from Kanno et al (2016)
# 6 load("output/DAYMET.rdata") # DAYMET data ####

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

#summary(as.numeric(Sitelist$HUC02)) #53 sites in huc 02, 73 in huc 07

Sitelist$HUC02[as.numeric(Sitelist$HUC02)==2]

SitesHUC2<-Sitelist[as.numeric(Sitelist$HUC02)==2,]
SitesHUC2<-SitesHUC2[!is.na(SitesHUC2$site_no),]

SitesHUC2$LAT_GAGE<-as.numeric(SitesHUC2$LAT_GAGE)
SitesHUC2$LNG_GAGE<-as.numeric(SitesHUC2$LNG_GAGE)

####2 - Import streamflow data from USGS NWIS ####
# Sites<-unique(SitesHUC2$site_no)
# parameterCd<-"00060" #set the parameter of interest at cfs
# rawDailyData<-readNWISdv(Sites,parameterCd, startDate = "1981-01-01", endDate = "2010-12-31") #statCd = "00003 defaults
# save(rawDailyData, file="data/rawDailyData.rdata")

load("data/rawDailyData.rdata")
#length(unique(rawDailyData$site_no)) #there are 49 sites with flow data from these days (from the 53 originally) -now 51?

####3 - USGS sites basin characteristics from GAGESII ####
GAGESII_Hydro <- read.csv("data/GAGESII_Hydro.csv",colClasses=c("character",rep("numeric",33))) #fix classes
#ncol(GAGESII_Hydro); head(GAGESII_Hydro)
names(GAGESII_Hydro)[1]<-"site_no"

GAGESII_Topo <- read.csv("data/GAGESII_Topo.csv",colClasses=c("character",rep("numeric",12))) #
#ncol(GAGESII_Topo); head(GAGESII_Topo)
names(GAGESII_Topo)[1]<-"site_no"

#Pull just the variables for which I have for the fish sites also
names(GAGESII_Topo)
GAGESII_TopoC<-GAGESII_Topo[c("site_no","SLOPE_PCT","ASPECT_DEGREES","ELEV_SITE_M","ELEV_MEAN_M_BASIN")]
names(GAGESII_Hydro)
GAGESII_HydroC<-GAGESII_Hydro[c("site_no","REACHCODE","BFI_AVE","TOPWET")]

rawUSGS_BC<-merge(SitesHUC2,GAGESII_HydroC,by="site_no")

rawUSGS_BC<-merge(rawUSGS_BC,GAGESII_TopoC)
save(rawUSGS_BC,file="output/rawUSGS_BC.rdata")

####4 - Import UVA site info ####
rawUVA_BC <- read.csv("data/UVAstreamsites.csv") #import site characteristics
names(rawUVA_BC)[2]<-"site_no" #rename site variable to be consistent with other data sets
#for some reason, it is the 2nd variable...
rawUVA_BC$site_no<-as.character(rawUVA_BC$site_no)
save(rawUVA_BC,file="output/rawUVA_BC.rdata")

UVA_Discharge <- read.csv("data/SWAS_data.csv") #import discharge data
save(UVA_Discharge,file="output/UVA_Discharge.rdata")

####5 - Import fish data and site info from Kanno et al sites####

#sites
fishsiteDf <- read.csv("YK/siteDf.csv")
# names(fishsiteDf)
fishsiteDf$Lat_n83<-as.numeric(fishsiteDf$Lat_n83)
fishsiteDf$Lon_n83<-as.numeric(fishsiteDf$Lon_n83)
save(fishsiteDf,file="output/fishsiteDf.rdata")

#data
#load array with fish data from kanno folder
load("~/flows_fish/YK/countArray 115 sites.rdata")
save(countAr,file="output/countAr.rdata")

#information on Julian day of sampling
Stats_Rev2 <- read.csv("YK/W_FI_MICOFISH_Stats_Rev2.csv")
JulianDayraw<-Stats_Rev2[c("SiteID","Year","Month","Day")]

## add Julian date
JulianDayraw$julian <- ifelse(JulianDayraw$Month == 6, 151+JulianDayraw$Day, 
                           ifelse(JulianDayraw$Month == 7, 181+JulianDayraw$Day, 212+JulianDayraw$Day)) #in leap years, dec 31 is removed so this will be off by a day
JulianDayraw$summerday <- ifelse(JulianDayraw$Month == 6, JulianDayraw$Day, 
                              ifelse(JulianDayraw$Month == 7, 30+JulianDayraw$Day, 31+JulianDayraw$Day))

#for some reason, some of the data is triplicated

JulianDay <- ddply(JulianDayraw, .(SiteID,Year), summarize,
                          julian = mean(julian, na.rm = T),
                     summerday = mean(summerday, na.rm = T)
)


JulianDay$Nyear<-JulianDay$Year-1981
JulianDay$site_no<-substr(JulianDay$SiteID,3,9)
JulianDay<-JulianDay[c("site_no","Nyear","summerday","julian")]
JulianDay<-JulianDay[JulianDay$Nyear<30,]
save(JulianDay,file="output/JulianDay.rdata")

####6 - Import DAYMET data ####

#first for the original 34 sites I was looking at:
#daymetRecord.csv is 1994-2010 for 53 USGS and 34 fish sites
#DaymetAGBSites.csv is 1982-1993 for 53 USGS and 34 fish sites
DAYMET1 <- read.csv("data/daymetRecord.csv") #6 variables ,colClasses=c("character",rep("numeric",5))
DAYMET2 <- read.csv("data/DaymetAGBSites.csv") #This one has 4 additional variables (daylight, radiation, vp and swe)
DAYMET2s<-DAYMET2[c("site_no", "featureid","date","tmax","tmin", "prcp")]
DAYMETpart1<-rbind(DAYMET1,DAYMET2s)
#str(DAYMETpart1) ; head(DAYMETpart1); tail(DAYMETpart1) #do spot checks

#create date variable in date format for these sites its: %Y-%m-%d
DAYMETpart1$Date<-as.Date(DAYMETpart1$date, "%Y-%m-%d")
DAYMETpart1<-DAYMETpart1[c("site_no", "featureid","Date","tmax","tmin", "prcp")] #just variables I'm using

##Second for the Additional Fish sites: NEWAGBSites - had to break it up into 4 chunks
DAYMET86_1 <- read.csv("data/Daymet86_1.csv")
DAYMET86_2 <- read.csv("data/Daymet86_2.csv")
DAYMET86_3 <- read.csv("data/Daymet86_3.csv")
DAYMET86_4 <- read.csv("data/Daymet86_4.csv")
#str(DAYMET86_4) ; head(DAYMET86_4); tail(DAYMET86_4) #do spot checks

DAYMET86<-rbind(DAYMET86_1,DAYMET86_2,DAYMET86_3,DAYMET86_4) #rbind these back into one dataset

#create date variable in date format for these sites its: %m/%d/%y
DAYMET86$Date<-as.Date(DAYMET86$date, "%m/%d/%y")
DAYMET86s<-DAYMET86[c("site_no", "featureid","Date","tmax","tmin", "prcp")] #just variables I'm using

#combine datasets
#names(DAYMETpart1); names(DAYMET86s) #match, good
DAYMET<-rbind(DAYMETpart1,DAYMET86s)
#unique(DAYMET$site_no) #173 sites: 115 fish + 5 in SNP +53 USGS = 173
save(DAYMET,file="output/DAYMET.rdata")

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
