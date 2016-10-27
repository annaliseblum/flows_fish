##NN DAR (nearest neighbor drainage area ratio prediction) daily flows prediction
###Annalise Blum
#Created:July 15, 2016  Updated: Oct 25,2016
##Data NEEDED for this file: "output/gagedsites_BC.rdata"
##Data sets created in this file:

#### 1 - Load data sets
#### 2 - Prep data sets
#### 3 Predict daily flow time series using USGS NNs
#### use only UVA sites

#### 1 - Load data sets ####
load("output/UVA_BC.rdata") #UVA gages Basin Chars
load("output/USGS_BC.rdata")#USGS gages Basin Chars
load("output/gagedsites_BC.rdata")# both
load("data/r.rdata")
load("output/USGSdaily.rdata")

#install Will's package
#install_github("wfarmer-usgs/PUBAD")
#The functions you will want are indexNN and estDAR.

#### 2 - Prep data sets ####

#create index number for list of USGS gages
USGS_BC$nearest_usgsG<-1:nrow(USGS_BC) #number the USGS gages to match nearest_usgsG in UVA_LL

#pull Lat and Long
UVA_LL<-UVA_BC[c("LAT_GAGE","LNG_GAGE")]
USGS_LL<-USGS_BC[c("LAT_GAGE","LNG_GAGE")]

USGSdailyNN<-USGSdaily[c("site_no","Date","cfs")] #pull the useful columns
names(USGSdailyNN)<-c("USGS_site_no","Date","USGS_cfs") #rename

#create USGS data set of only sites with full 29 years of record
rec.lengths<-aggregate(USGSdailyNN$USGS_cfs,by=list(USGSdailyNN$USGS_site_no),length)
names(rec.lengths)<-c("USGS_site_no","daysofflow")
USGSdailyNNrecs<-data.frame(merge(USGSdailyNN, rec.lengths, by = c('USGS_site_no'))) #merge with flow data
#drop sites with less than 3 years of flow data
USGSdailyNN29yr<-USGSdailyNNrecs[USGSdailyNNrecs$daysofflow>28*365,]
USGS29yrsites<-as.data.frame(unique(USGSdailyNN29yr$USGS_site_no))
names(USGS29yrsites)<-"site_no"
USGS_BC29<-merge(USGS_BC,USGS29yrsites,by="site_no") #only full record sites
USGS_LL29<-USGS_BC29[c("LAT_GAGE","LNG_GAGE")]

#add "UVA_" to UVA site numbers
head(UVA_daily)
UVA_daily<-UVA_daily[,1:3] #just pull first 3 columns
names(UVA_daily)[3]<-"UVAobs_cfs"
UVA_daily$site_no<-paste("UVA_",UVA_daily$site_no,sep="")
class(UVA_daily$Date);class(NN_USGSdata$Date)

#### 3 - USGS for UVA sites - Predict daily flow time series ####
#to find the nearest USGS gage to the UVA sampling sites:
library(rgeos);library(sp)
UVA_sp <- SpatialPoints(UVA_LL)
USGS_sp <- SpatialPoints(USGS_LL)
UVA_LL$nearest_usgsG <- apply(gDistance(USGS_sp, UVA_sp, byid=TRUE), 1, which.min) #returns entry number of closest

#merge back in the site numbers and other info:
UVA_DA<-UVA_BC[c("site_no","DA_SQKM","LAT_GAGE","LNG_GAGE")]

UVA_DA<-merge(UVA_DA,UVA_LL,by=c("LAT_GAGE","LNG_GAGE")) #merge in the nearest USGS gage index

USGSsites<-USGS_BC[c("site_no","DA_SQKM","LAT_GAGE","LNG_GAGE","nearest_usgsG")]
names(USGSsites)<-c("USGS_site_no","USGS_DA_SQKM","USGS_LAT","USGS_LNG","nearest_usgsG")

NN_UVA_USGS<-merge(USGSsites,UVA_DA,by=c("nearest_usgsG"))

NN_UVA_USGSd<-merge(NN_UVA_USGS,USGSdailyNN,by="USGS_site_no") #add in the daily flow data for the 3 USGS sites

# #how many days of USGS data per UVA site?
# table(as.factor(NN_UVA_USGSd$site_no))
# #UVA_NFDR and UVA_STAN only have 7.4 years; #UVA_PAIN and UVA_WOR1 have 29 years; #UVA_STAN has 25 years

NN_ALL<-merge(UVA_daily,NN_UVA_USGSd,by=c("site_no","Date")) #merge in observed flows at UVA sites
NN_ALL$cfsperKM2<-NN_ALL$USGS_cfs/NN_ALL$USGS_DA_SQKM
NN_ALL$NNpredcfs<-NN_ALL$cfsperKM2*NN_ALL$DA_SQKM

save(NN_ALL, file="output/NN_ALLrdata")



NSE(NN_ALL$NNpredcfs, NN_ALL$UVAobs_cfs) #0.36
plot(NN_ALL$NNpredcfs, NN_ALL$UVAobs_cfs,xlim=c(0,600))
abline(0,1)

NSE(log(NN_ALL$NNpredcfs), log(NN_ALL$UVAobs_cfs+.0001)) #0.52 n=24,623
plot(log(NN_ALL$NNpredcfs), log(NN_ALL$UVAobs_cfs+.0001))
abline(0,1)

#### 4 UVA for UVA sites - Predict daily flow time series ####
dist<-gDistance(UVA_sp, UVA_sp, byid=TRUE) #create matrix of distances
dist[dist==0] <- NA #replace 0x with NA
UVA_DAnew<-UVA_DA
UVA_DAnew$nearest_uvaG <- apply(dist, 1, which.min) #returns entry number of closest

#merge back in the site numbers and other info:
UVA_DAUVA<-merge(UVA_DA,UVA_LL,by=c("LAT_GAGE","LNG_GAGE"))

#create 2nd data set to get the DA of the NN
UVA_DA3<-UVA_DA
names(UVA_DA3)<-c("NNsite_no", "NNDA_SQKM", "NNLAT_GAGE", "NNLNG_GAGE")
UVA_DA3$nearest_uvaG<-row.names(UVA_DA3)
UVA_DAUVA<-merge(UVA_DAnew,UVA_DA3,by="nearest_uvaG")

load("output/UVA_daily.rdata")
head(UVA_daily)
UVA_daily<-UVA_daily[,1:3] #just pull first 3 columns
names(UVA_daily)[3]<-"UVAobs_cfs"
UVA_daily$site_no<-paste("UVA_",UVA_daily$site_no,sep="")

NN_UVAUVA<-merge(UVA_daily,UVA_DAUVA,by=c("site_no"))

#make a second UVA_daily dataset for NN"
UVA_dailynew<-UVA_daily
names(UVA_dailynew)<-c("NNsite_no", "Date", "NNcfs")

NN_UVAUVA<-merge(NN_UVAUVA,UVA_dailynew,by=c("NNsite_no","Date"))
NN_UVAUVA$cfsperKM2<-NN_UVAUVA$NNcfs/NN_UVAUVA$NNDA_SQKM

NN_UVAUVA$NNpredcfs<-NN_UVAUVA$cfsperKM2*NN_UVAUVA$DA_SQKM
NSE(NN_UVAUVA$NNpredcfs, NN_UVAUVA$UVAobs_cfs) #0.3 -  n=33,359
plot(NN_UVAUVA$NNpredcfs, NN_UVAUVA$UVAobs_cfs,xlim=c(0,600))
abline(0,1)

NSE(log(NN_UVAUVA$NNpredcfs+.0001), log(NN_UVAUVA$UVAobs_cfs+.0001)) #0.42
plot(log(NN_UVAUVA$NNpredcfs+.0001), log(NN_UVAUVA$UVAobs_cfs+.0001))
abline(0,1)

#### 5 USGS and UVA for fish sites - Predict daily flow time series ####

load("output/fishSC.rdata")
head(fishSC)
fishLL<-fishSC[c("LAT_GAGE","LNG_GAGE")]

head(UVA_LL);head(USGS_LL)
UVA_LL$nearest_usgsG<-NULL
gaged_LL<-rbind(UVA_LL,USGS_LL29)

gaged_sp <- SpatialPoints(gaged_LL)
fish_sp <- SpatialPoints(fishLL)
fishLL$nearest_gage <- apply(gDistance(gaged_sp, fish_sp, byid=TRUE), 1, which.min) #returns entry number of closest
gaged_LL$NDX<-1:nrow(gaged_LL); head(gaged_LL)
names(gaged_LL)<-c("gaged_LAT","gaged_LNG","nearest_gage")

#merge fish site info with nearest gage numbers
fishDA<-fishSC[c("site_no","DA_SQKM","LAT_GAGE","LNG_GAGE")]
fishDA2<-merge(fishDA,fishLL,by=c("LAT_GAGE","LNG_GAGE"))

#pull gaged site characteristics to merge those in
gagedsites<-gagedsites_BC[c("site_no","DA_SQKM","LAT_GAGE","LNG_GAGE")]
names(gagedsites)<-c("gaged_site_no","gaged_DA_SQKM","gaged_LAT","gaged_LNG")

#merge gaged site info into data set with index (called "nearest gage" for later)
gagedsites<-merge(gagedsites,gaged_LL,by=c("gaged_LAT","gaged_LNG"))

#now merge gaged lat and long into master fish data set by nearest gage
fish_gaged_list<-merge(fishDA2,gagedsites,by="nearest_gage") #all UVA sites
save(fish_gaged_list,file="output/fish_gaged_list.rdata")

#now merge in gaged flow data
##create daily flows data set of both USGS and UVA data
load("output/USGSdaily.rdata")

USGSdaily<-USGSdaily[,c("site_no","Date", "cfs")]
names(USGSdaily)[1]<-"gaged_site_no"
names(UVA_daily)[3]<-"cfs"
names(UVA_daily)[1]<-"gaged_site_no"
gaged_daily<-rbind(USGSdaily,UVA_daily)

fish_gagedflows<-merge(fish_gaged_list,gaged_daily,by="gaged_site_no")

table(as.factor(fish_gagedflows$site_no)) #how many flows available by site?
rec.length<-aggregate(fish_gagedflows$site_no,by=list(fish_gagedflows$site_no),length)
summary(rec.length) #7-23 years, 5 sites with only 7 years, most with 18 years, some with 23
names(rec.length)<-c("site_no","days")
rec.length$years<-rec.length$days/365

head(fish_gagedflows)
fish_gagedflows$cfsperKM2<-fish_gagedflows$cfs/fish_gagedflows$gaged_DA_SQKM
fish_gagedflows$NNpredcfs<-fish_gagedflows$cfsperKM2*fish_gagedflows$DA_SQKM

dNN.Fish<-fish_gagedflows[c("site_no","Date","NNpredcfs")]

save(dNN.Fish,file="output/dNN.Fish.rdata")
