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
#load("data/r.rdata") #what was here?? UVA daily data
load("output/USGSdaily.rdata") 
load("output/UVA_daily.rdata")

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
gaged_LL<-USGS_LL29

gaged_sp <- SpatialPoints(gaged_LL)
UVA_sp <- SpatialPoints(UVA_LL)
UVA_LL$nearest_gage <- apply(gDistance(gaged_sp, UVA_sp, byid=TRUE), 1, which.min) #returns entry number of closest
gaged_LL$NDX<-1:nrow(gaged_LL); head(gaged_LL)
names(gaged_LL)<-c("gaged_LAT","gaged_LNG","nearest_gage")

#merge fish site info with nearest gage numbers
UVA_DA<-UVA_BC[c("site_no","DA_SQKM","LAT_GAGE","LNG_GAGE")]
UVA_DA2<-merge(UVA_DA,UVA_LL,by=c("LAT_GAGE","LNG_GAGE"))

#pull gaged site characteristics to merge those in
gagedsites<-gagedsites_BC[c("site_no","DA_SQKM","LAT_GAGE","LNG_GAGE")]
names(gagedsites)<-c("gaged_site_no","gaged_DA_SQKM","gaged_LAT","gaged_LNG")

#merge gaged site info into data set with index (called "nearest gage" for later)
gagedsites<-merge(gagedsites,gaged_LL,by=c("gaged_LAT","gaged_LNG"))

#now merge gaged lat and long into master fish data set by nearest gage
UVA_gaged_list<-merge(UVA_DA2,gagedsites,by="nearest_gage") #all UVA sites

#now merge in gaged flow data
##create daily flows data set of both USGS and UVA data
load("output/USGSdaily.rdata")
USGSdaily<-USGSdaily[,c("site_no","Date", "cfs")]
names(USGSdaily)[1]<-"gaged_site_no"
gaged_daily<-USGSdaily

UVA_gagedflows<-merge(UVA_gaged_list,gaged_daily,by="gaged_site_no")

table(as.factor(UVA_gagedflows$site_no)) #how many flows available by site?
rec.length<-aggregate(UVA_gagedflows$site_no,by=list(UVA_gagedflows$site_no),length)
summary(rec.length) #Full record for all of them
names(rec.length)<-c("site_no","days")
rec.length$years<-rec.length$days/365

head(UVA_gagedflows)
unique(UVA_gagedflows$gaged_site_no) #only 2 USGS sites used

UVA_gagedflows$cfsperKM2<-UVA_gagedflows$cfs/UVA_gagedflows$gaged_DA_SQKM
UVA_gagedflows$NNpredcfs<-UVA_gagedflows$cfsperKM2*UVA_gagedflows$DA_SQKM

dNN.USGS.UVA<-UVA_gagedflows[c("site_no","Date","NNpredcfs")]

##add UVA_ to begining of UVA_daily site_nos
UVA_daily$site_no<-paste("UVA_",UVA_daily$site_no,sep="")

NN.USGS.UVA<-merge(UVA_daily,dNN.USGS.UVA,by=c("site_no","Date")) #merge in observed flows at UVA sites

save(NN.USGS.UVA, file="output/NN.USGS.UVA.rdata")

NSE(NN.USGS.UVA$NNpredcfs, NN.USGS.UVA$cfs) #0.37
plot(NN.USGS.UVA$cfs,NN.USGS.UVA$NNpredcfs,xlim=c(0,600))
abline(0,1)

NSE(log(NN.USGS.UVA$NNpredcfs), log(NN.USGS.UVA$cfs+.0001)) #0.47 n=35,552
plot(log(NN.USGS.UVA$NNpredcfs), log(NN.USGS.UVA$cfs+.0001))
abline(0,1)

UUGSUVAsiteNSE<-rep(NA,5)
USGSUVAsiteLNSE<-rep(NA,5)
#NSE by site
for (i in 1:5){
  idata<-NN.USGS.UVA[NN.USGS.UVA$site_no==unique(NN.USGS.UVA$site_no)[i],]
  UUGSUVAsiteNSE[i]<-NSE(idata$NNpredcfs, idata$cfs)
  USGSUVAsiteLNSE[i]<-NSE(log(idata$NNpredcfs+.0001), log(idata$cfs+.0001))
}

#### 4 UVA for UVA sites - Predict daily flow time series ####

#remove NFDR with DA of 2km2
UVA_BC4<-UVA_BC[UVA_BC$site_no!="UVA_NFDR",]
UVA_LL4<-UVA_BC4[c("LAT_GAGE","LNG_GAGE")]
UVA4_sp <- SpatialPoints(UVA_LL4)

# gaged_LL$NDX<-1:nrow(gaged_LL); head(gaged_LL)
# names(gaged_LL)<-c("gaged_LAT","gaged_LNG","nearest_gage")

dist<-gDistance( UVA4_sp,UVA_sp, byid=TRUE) #create matrix of distances
dist[dist==0] <- NA #replace 0x with NA
UVA_DAnew<-UVA_DA
UVA_DAnew$nearest_uvaG <- apply(dist, 1, which.min) #returns entry number of closest

UVA_LL4$nearest_uvaG<-1:4

#merge back in the site numbers and other info:
UVA_DAUVA<-merge(UVA_DA,UVA_LL4,by=c("LAT_GAGE","LNG_GAGE"))
UVA_DAUVA<-UVA_DAUVA[c("site_no", "DA_SQKM","LAT_GAGE","LNG_GAGE")]
names(UVA_DAUVA)<-c("NNsite_no", "NNDA_SQKM", "NNLAT_GAGE", "NNLNG_GAGE")

# #create 2nd data set to get the DA of the NN
# UVA_DA3<-UVA_DA
# UVA_DA3$nearest_uvaG<-row.names(UVA_DA3)
# UVA_DAUVA<-merge(UVA_DAnew,UVA_DA3,by="nearest_uvaG")

load("output/UVA_daily.rdata")
head(UVA_daily)
UVA_daily<-UVA_daily[,1:3] #just pull first 3 columns
names(UVA_daily)[3]<-"UVAobs_cfs"
UVA_daily$site_no<-paste("UVA_",UVA_daily$site_no,sep="")

#make a second UVA_daily dataset for NN"
UVA_dailynew<-UVA_daily
names(UVA_dailynew)<-c("NNsite_no", "Date", "NNcfs")

NN_UVAUVA<-merge(UVA_dailynew,UVA_DAUVA,by=c("NNsite_no"))

##NEED TO MERGE WITH ACTUAL SITES AND NEAREST GAGE - START HERE

NN_UVAUVA$cfsperKM2<-NN_UVAUVA$NNcfs/NN_UVAUVA$NNDA_SQKM

NN_UVAUVA$NNpredcfs<-NN_UVAUVA$cfsperKM2*NN_UVAUVA$DA_SQKM

save(NN_UVAUVA,file="output/NN_UVAUVA.rdata")

NSE(NN_UVAUVA$NNpredcfs, NN_UVAUVA$UVAobs_cfs) #0.3 -  n=33,359
plot(NN_UVAUVA$NNpredcfs, NN_UVAUVA$UVAobs_cfs,xlim=c(0,600))
abline(0,1)

NSE(log(NN_UVAUVA$NNpredcfs+.0001), log(NN_UVAUVA$UVAobs_cfs+.0001)) #0.42
plot(log(NN_UVAUVA$NNpredcfs+.0001), log(NN_UVAUVA$UVAobs_cfs+.0001))
abline(0,1)

UVAUVAsiteNSE<-rep(NA,5)
UVAUVAsiteLNSE<-rep(NA,5)
#NSE by site
for (i in 1:5){
  idata<-NN_UVAUVA[NN_UVAUVA$site_no==unique(NN_UVAUVA$site_no)[i],]
  UVAUVAsiteNSE[i]<-NSE(idata$NNpredcfs, idata$UVAobs_cfs)
  UVAUVAsiteLNSE[i]<-NSE(log(idata$NNpredcfs+.0001), log(idata$UVAobs_cfs+.0001))
}

##Compare to USGS for UVA:
par(mfrow=c(1,1))
pdf(file="plots/NSE_by_UAsite.pdf")
boxplot(UVAUVAsiteNSE,UUGSUVAsiteNSE,UVAUVAsiteLNSE,USGSUVAsiteLNSE,names=c("UVA NSE","USGS NSE","UVA LNSE","USGS LNSE"),
        main="NSE by 5 UVA sites using other sites (UVA vs USGS)",ylab="NSE and LNSE") #,ylim=c(-1,1)
abline(0,0,lty=3)
abline(v=2.5)
dev.off()

#### 5 Just UVA for fish sites - Predict daily flow time series ####

load("output/fishSC.rdata")
head(fishSC)
fishLL<-fishSC[c("LAT_GAGE","LNG_GAGE")]

head(UVA_LL);head(USGS_LL)
UVA_LL$nearest_gage<-NULL

gaged_sp <- SpatialPoints(gaged_LL)
fish_sp <- SpatialPoints(fishLL)
fishLL$nearest_gage <- apply(gDistance(gaged_sp, fish_sp, byid=TRUE), 1, which.min) #returns entry number of closest
UVA_LL$nearest_gage<-1:nrow(UVA_LL); head(UVA_LL)
names(UVA_LL)<-c("gaged_LAT","gaged_LNG","nearest_gage")

#merge fish site info with nearest gage numbers
fishDA<-fishSC[c("site_no","DA_SQKM","LAT_GAGE","LNG_GAGE")]
fishDA2<-merge(fishDA,fishLL,by=c("LAT_GAGE","LNG_GAGE"))

#pull gaged site characteristics to merge those in
gagedsites<-gagedsites_BC[c("site_no","DA_SQKM","LAT_GAGE","LNG_GAGE")]
names(gagedsites)<-c("gaged_site_no","gaged_DA_SQKM","gaged_LAT","gaged_LNG")

#merge gaged site info into data set with index (called "nearest gage" for later)
gagedsites<-merge(gagedsites,UVA_LL,by=c("gaged_LAT","gaged_LNG"))

#now merge gaged lat and long into master fish data set by nearest gage
fish_UVAgaged_list<-merge(fishDA2,gagedsites,by="nearest_gage") #all UVA sites
save(fish_UVAgaged_list,file="output/fish_UVAgaged_list.rdata")

#now merge in gaged flow data
##create daily flows data set of both USGS and UVA data
load("output/UVA_daily.rdata")
head(UVA_daily)
UVA_daily$site_no<-paste("UVA_",UVA_daily$site_no,sep="")

table(as.factor(UVA_daily$site_no)) #how many flows available by site?
rec.length<-aggregate(UVA_daily$site_no,by=list(UVA_daily$site_no),length)
summary(rec.length) #7-23 years, 5 sites with only 7 years, most with 18 years, some with 23
names(rec.length)<-c("site_no","days")
rec.length$years<-rec.length$days/365

##merge in UVA sites flows
UVA_dailyNN<-UVA_daily
names(UVA_dailyNN)[1]<-"gaged_site_no"
UVA_dailyNN<-merge(fish_UVAgaged_list, UVA_dailyNN,by="gaged_site_no")

head(UVA_dailyNN)
UVA_dailyNN$cfsperKM2<-UVA_dailyNN$cfs/UVA_dailyNN$gaged_DA_SQKM
UVA_dailyNN$NNpredcfs<-UVA_dailyNN$cfsperKM2*UVA_dailyNN$DA_SQKM

dNNUVA.Fish<-UVA_dailyNN[c("site_no","Date","NNpredcfs")]

save(dNNUVA.Fish,file="output/dNNUVA.Fish.rdata")

#### 6 Just USGS with full 29 year record for fish sites - Predict daily flow time series ####

load("output/fishSC.rdata")
head(fishSC)
fishLL<-fishSC[c("LAT_GAGE","LNG_GAGE")]

gaged_LL<-USGS_LL29

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
FRfish_gaged_list<-merge(fishDA2,gagedsites,by="nearest_gage") #all UVA sites
save(FRfish_gaged_list,file="output/FRfish_gaged_list.rdata")

#now merge in gaged flow data
##create daily flows data set of both USGS and UVA data
load("output/USGSdaily.rdata")
USGSdaily<-USGSdaily[,c("site_no","Date", "cfs")]
names(USGSdaily)[1]<-"gaged_site_no"
gaged_daily<-USGSdaily

fish_gagedflows<-merge(fish_gaged_list,gaged_daily,by="gaged_site_no")

table(as.factor(fish_gagedflows$site_no)) #how many flows available by site?
rec.length<-aggregate(fish_gagedflows$site_no,by=list(fish_gagedflows$site_no),length)
summary(rec.length) #Full record for all of them
names(rec.length)<-c("site_no","days")
rec.length$years<-rec.length$days/365

head(fish_gagedflows)
fish_gagedflows$cfsperKM2<-fish_gagedflows$cfs/fish_gagedflows$gaged_DA_SQKM
fish_gagedflows$NNpredcfs<-fish_gagedflows$cfsperKM2*fish_gagedflows$DA_SQKM

dNN.USGS.Fish<-fish_gagedflows[c("site_no","Date","NNpredcfs")]

save(dNN.USGS.Fish,file="output/dNN.USGS.Fish.rdata")

#### Use USGS NN preds for 1981-1992 and UVA for 1993-2010 ####
#sum(UVA_daily$Date=="1992-09-01") #there are 5 observations starting Oct 1, 1992

#subset the USGS NN preds to just be 1981-1992
dNN.USGS.Fish82_93<-dNN.USGS.Fish[dNN.USGS.Fish$Date<"1993-01-01",]

#subset the UVA NN preds to just be 1993-2010
dNNUVA.Fish93_10<-dNNUVA.Fish[dNNUVA.Fish$Date>"1992-12-31",]

NN.USGS.UVA.fish<-rbind(dNN.USGS.Fish82_93,dNNUVA.Fish93_10)

##Compare predictions from UVA sites and USGS sites for fish sites 1993-2010
#load("output/dNN.Fish.rdata") #flows predicted in NN_DAR.R

load("output/dNNUVA.Fish.rdata")
head(dNNUVA.Fish)
names(dNNUVA.Fish)<-c("site_no","Date","UVA_NNpredcfs")

load("output/dNN.USGS.Fish.rdata")
head(dNN.USGS.Fish)
names(dNN.USGS.Fish)<-c("site_no","Date","USGS_NNpredcfs")

USGS_UVAcomp<-merge(dNN.USGS.Fish,dNNUVA.Fish,by=c("site_no","Date"))

plot(USGS_UVAcomp$USGS_NNpredcfs,USGS_UVAcomp$UVA_NNpredcfs)
abline(0,1)

plot(USGS_UVAcomp$USGS_NNpredcfs[USGS_UVAcomp$site_no=="3F101"],USGS_UVAcomp$UVA_NNpredcfs[USGS_UVAcomp$site_no=="3F101"])
abline(0,1)

NSE(USGS_UVAcomp$USGS_NNpredcfs,USGS_UVAcomp$UVA_NNpredcfs)#if UVA is "observed" NSE =.25
NSE(USGS_UVAcomp$UVA_NNpredcfs,USGS_UVAcomp$USGS_NNpredcfs) #if USGS is "observed" NSE =-3.66

#plot time series
test<-USGS_UVAcomp[USGS_UVAcomp$site_no=="3F101",]
testUSGS<-test[c("Date","USGS_NNpredcfs")]
names(testUSGS)<-c("Date","NNpredcfs")
testUSGS$type<-"USGS"
testUVA<-test[c("Date","UVA_NNpredcfs")]
names(testUVA)<-c("Date","NNpredcfs")
testUVA$type<-"UVA"
testall<-rbind(testUVA,testUSGS)
  
ggplot(testall, aes(x=Date, y=NNpredcfs))+
  geom_line(aes(y = NNpredcfs, colour = as.factor(testall$type)))+
  theme(legend.position = "bottom")+
  scale_y_continuous(limits = c(0,50))
  scale_x_date(limits = c(as.Date("2000-01-01"), as.Date("2002-01-01")))

