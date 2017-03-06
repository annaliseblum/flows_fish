##NEW - NN DAR (nearest neighbor drainage area ratio prediction) daily flows prediction; compare UVA to USGS sites
###Annalise Blum
#Created:Dec 5, 2016  Updated: Dec 5,2016
##Data sets created in this file: "output/fish_UVA4gaged_list.rdata"; "output/fish_USGSgaged_list.rdata"

#Purpose: Match gages to fish sites using NN
#Use SWAS (UVA) sites for 1993-2010 and USGS sites from 1982-1992

#rm(list=ls())

#### 1 - Load data sets
#### 2 - Prep UVA data
#### 3 - Prep USGS data
#### 4 - NN for fish sites from UVA and USGS sites
#### 5 - find NN for UVA (1993-2010) for fish sites

library(sp);library(rgeos)

#### 1 - Load data sets ####
load("output/UVA_BC.rdata") #UVA gages Basin Chars
load("output/USGS_BC.rdata")#USGS gages Basin Chars
load("output/USGSdaily.rdata") #flows
load("output/UVA_daily.rdata")#flows
load("output/fishSC.rdata") #fish basin characteristics

#### 2 - UVA data prep 1992-2010 ####
#pull Lat and Long
UVA_LL<-UVA_BC[c("LAT_GAGE","LNG_GAGE")]

##Pull the split sample of years for the two sites
#Use just 1993-2010 for UVA
UVAdailyNN93<-UVA_daily[UVA_daily$Date>"1992-12-31",]
UVArec.lengths<-aggregate(UVAdailyNN93$cfs,by=list(UVAdailyNN93$site_no),length)
#summary(UVArec.lengths) #good all equal

#head(UVAdailyNN93)
UVAdailyNN93<-UVAdailyNN93[,1:3] #just pull first 3 columns
names(UVAdailyNN93)[3]<-"UVAobs_cfs"

#### 3 - Prep USGS data for fish 1982-1992 ####
USGSdailyNN<-USGSdaily[c("site_no","Date","cfs")] #pull the useful columns
names(USGSdailyNN)<-c("USGS_site_no","Date","USGS_cfs") #rename

#create index number for list of USGS gages
USGS_BC$nearest_usgsG<-1:nrow(USGS_BC) #number the USGS gages to match nearest_usgsG in UVA_LL

USGSdailyNN82<-USGSdailyNN[USGSdailyNN$Date<"1993-01-01",]
#summary(USGSdailyNN82)
rec.lengths<-aggregate(USGSdailyNN82$USGS_cfs,by=list(USGSdailyNN82$USGS_site_no),length)
names(rec.lengths)<-c("USGS_site_no","daysofflow"); summary(rec.lengths)
USGSrec.lengths<-rec.lengths[rec.lengths$daysofflow==4383,] #37 sites
USGSdailyNNrecs<-data.frame(merge(USGSdailyNN82, USGSrec.lengths, by = c('USGS_site_no'))) #merge with flow data

SiteList82<-as.data.frame(unique(USGSdailyNNrecs$USGS_site_no))
names(SiteList82)<-"site_no"
USGS_BC82<-merge(USGS_BC,SiteList82,by="site_no") #only full record sites
USGS_LL82<-USGS_BC82[c("LAT_GAGE","LNG_GAGE")]

#### 4  find NN for USGS (1982-1992) for fish sites ####
#head(fishSC)
fishLL<-fishSC[c("LAT_GAGE","LNG_GAGE")]

#first do all predictions for USGS 1982-1992: USGS_LL82; USGS_BC82
gaged_sp <- SpatialPoints(USGS_LL82)
fish_sp <- SpatialPoints(fishLL)
fishLL$nearest_gage <- apply(gDistance(gaged_sp, fish_sp, byid=TRUE), 1, which.min) #returns entry number of closest
USGS_LL82$nearest_gage<-1:nrow(USGS_LL82); head(USGS_LL82)
names(USGS_LL82)<-c("gaged_LAT","gaged_LNG","nearest_gage")

#merge fish site info with nearest gage numbers
fishDA<-fishSC[c("site_no","DA_SQKM","LAT_GAGE","LNG_GAGE")]
fishDA2<-merge(fishDA,fishLL,by=c("LAT_GAGE","LNG_GAGE"))

#pull gaged site characteristics to merge those in
gagedsites<-USGS_BC82[c("site_no","DA_SQKM","LAT_GAGE","LNG_GAGE")]
names(gagedsites)<-c("gaged_site_no","gaged_DA_SQKM","gaged_LAT","gaged_LNG")

#merge gaged site info into data set with index (called "nearest gage" for later)
gagedsites2<-merge(gagedsites,USGS_LL82,by=c("gaged_LAT","gaged_LNG"))

#now merge gaged lat and long into master fish data set by nearest gage
fish_USGSgaged_list<-merge(fishDA2,gagedsites2,by="nearest_gage") #all USGS sites

save(fish_USGSgaged_list,file="output/fish_USGSgaged_list.rdata")

#### 5 - find NN for UVA (1993-2010) for fish sites ####
UVA_BC4<-UVA_BC[UVA_BC$DA_SQKM>5,] #REMOVE TINY 2km2 site
UVA_LL<-UVA_BC4[c("LAT_GAGE","LNG_GAGE")]

gaged_sp <- SpatialPoints(UVA_LL)
fish_sp <- SpatialPoints(fishLL)
fishLL$nearest_gage <- apply(gDistance(gaged_sp, fish_sp, byid=TRUE), 1, which.min) #returns entry number of closest
UVA_LL$nearest_gage<-1:nrow(UVA_LL); head(UVA_LL)
names(UVA_LL)<-c("gaged_LAT","gaged_LNG","nearest_gage")

#merge fish site info with nearest gage numbers
fishDA<-fishSC[c("site_no","DA_SQKM","LAT_GAGE","LNG_GAGE")]
fishDA2<-merge(fishDA,fishLL,by=c("LAT_GAGE","LNG_GAGE"))

#pull gaged site characteristics to merge those in
gagedsites<-UVA_BC[c("site_no","DA_SQKM","LAT_GAGE","LNG_GAGE")]
names(gagedsites)<-c("gaged_site_no","gaged_DA_SQKM","gaged_LAT","gaged_LNG")

#merge gaged site info into data set with index (called "nearest gage" for later)
gagedsites<-merge(gagedsites,UVA_LL,by=c("gaged_LAT","gaged_LNG"))

#now merge gaged lat and long into master fish data set by nearest gage
fish_UVA4gaged_list<-merge(fishDA2,gagedsites,by="nearest_gage") #all UVA sites
save(fish_UVA4gaged_list,file="output/fish_UVA4gaged_list.rdata")
