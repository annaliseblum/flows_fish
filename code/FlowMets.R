#Flow metrics estimation

#Purpose: Predict daily flow and other high and low flow statistics using NN-DAR method
#Use SWAS (UVA) sites for 1993-2010 and USGS sites from 1982-1992

#### 1 -Load data sets
#### 2 - 
#### 3 
#### 4

#### 1 - Load data sets ####
#rm(list=ls()) #clear variables before running analysis

#> load("~/flows_fish/output/FishPredscomb.rdata") #older version of this same file...
#oh with tiny UVA site still probably

#NN matches
load("output/fish_UVA4gaged_list.rdata")
load("output/fish_USGSgaged_list.rdata") 

#UVA
load("output/UVA_daily.rdata")
UVAdaily93<-UVA_daily[UVA_daily$Date>"1992-12-31",]

#USGS
load("output/USGSdaily.rdata")

#find the USGS sites of interest
USGS_NNsites<-as.data.frame(unique(fish_USGSgaged_list$gaged_site_no)) #USGS sites "01632900" "01634500" "01665500" "02028500"
names(USGS_NNsites)<-"site_no"

USGSdaily82<-USGSdaily[USGSdaily$Date<"1993-01-01",]
USGSdaily82<-merge(USGSdaily82,USGS_NNsites,by="site_no")

###STart with USGS, then UVA, only site_no, Date, cfs, day, month, year ####
#DAILY<-USGSdaily82
#DAILY<-UVAdaily93 
#DAILY<-df_preds #load("output/df_preds.rdata") #from PredFlowMets.R


#add Nyear and Nseason variables
#Create new season and year variables
#DAILY$Nyear<-ifelse(DAILY$month<6,DAILY$year-1981,DAILY$year-1980) #to get years 1-29

#create season variable
##Kanno defines seasons as fall=Aug-Nov, winter=dec-feb, spring=march-may, summer=june-aug
DAILY$season[DAILY$month==12|DAILY$month==01|DAILY$month==02]<-"winter"
DAILY$season[DAILY$month==03|DAILY$month==04|DAILY$month==05]<-"spring"
DAILY$season[DAILY$month==06|DAILY$month==07|DAILY$month==08]<-"summer"
DAILY$season[DAILY$month==09|DAILY$month==10|DAILY$month==11]<-"fall"

#create a numeric season variable 1-4 starting with summer as 1
DAILY$Nseason[DAILY$season=="summer"]<- 1
DAILY$Nseason[DAILY$season=="fall"]<-2
DAILY$Nseason[DAILY$season=="winter"]<-3
DAILY$Nseason[DAILY$season=="spring"]<-4

#### 3 - Metric 1: Average seasonal flows  ####
avgflow <- aggregate(DAILY$cfs,by=list(DAILY$site_no, DAILY$year,DAILY$Nseason),FUN=mean)
names(avgflow)<-c("site_no","year","Nseason","avgSNNflow")
summary(avgflow$avgSNNflow)
#check average seasonal average flows by season
#avgseasonalflows<-aggregate(USGSavgflow$avgSNNflow,by=list(USGSavgflow$Nseason),mean); avgseasonalflows

#### 4 - Metric 2: Magnitude (Intensity/Severity) LF and HFs  ####
#find by site, year, and season
SMag.05<-aggregate(DAILY$cfs,by=list(DAILY$site_no,DAILY$year,DAILY$Nseason),
                        FUN=quantile,probs=0.05,type=6)
names(SMag.05)<-c("site_no","year","Nseason","p5") #Q95 is the same as p.05
SMag.95<-aggregate(DAILY$cfs,by=list(DAILY$site_no,DAILY$year,DAILY$Nseason),FUN=quantile,probs=0.95,type=6)
names(SMag.95)<-c("site_no","year","Nseason","p95") #Q5 is the same as p.95

#merge datasets
Mag.05p.95p<-merge(SMag.05,SMag.95,by=c("site_no","year","Nseason"))

#### 5 - Metric 3: Duration: days below .25, .05 percentile MA-FDC or above .75, .95 flows in season, all years  ####

#aggregate by site and season DAILY
Dur.05<-aggregate(DAILY$cfs,by=list(DAILY$site_no,DAILY$Nseason),FUN=quantile,probs=0.05,type=6)
names(Dur.05)<-c("site_no","Nseason","Pp05") #Period of record for season, p05

Dur.95<-aggregate(DAILY$cfs,by=list(DAILY$site_no,DAILY$Nseason),FUN=quantile,probs=0.95,type=6)
names(Dur.95)<-c("site_no","Nseason","Pp95") #Period of record for season, p95

#merge datasets
DAILY<-merge(DAILY,Dur.05,by=c("site_no","Nseason"))
DAILY<-merge(DAILY,Dur.95,by=c("site_no","Nseason"))

#dummy variable coding for a Low or High Flow day
DAILY$LFD05<-ifelse(DAILY$Pp05>=DAILY$cfs, 1, 0)

#CHECK: sum(DAILY$LFD05)/length(DAILY$LFD05) = .049 
DAILY$HFD95<-ifelse(DAILY$Pp95<=DAILY$cfs, 1, 0)
#CHECK: sum(DAILY$HFD95)/length(DAILY$HFD95) = .050

#Collapse to seasonal level:
S_Duration <- ddply(DAILY, .(site_no, year,Nseason), summarize, 
                    #daysperseason=sum(tally),
                    LF05_days = sum(LFD05, na.rm = T),
                    HF95_days = sum(HFD95, na.rm = T)
)

# siteS_Duration <- ddply(S_Duration, .(site_no,Nseason), summarize, 
#                     #daysperseason=sum(tally),
#                     LF05_daysSite = mean(LF05_days, na.rm = T),
#                     HF95_daysSite = mean(HF95_days, na.rm = T)
# )


# #aggregate by site and season DAILY
# Dur.25<-aggregate(DAILY$cfs,by=list(DAILY$site_no,DAILY$Nseason),FUN=quantile,probs=0.25,type=6)
# names(Dur.25)<-c("site_no","Nseason","Pp25") #Period of record for season, p05
# 
# Dur.75<-aggregate(DAILY$cfs,by=list(DAILY$site_no,DAILY$Nseason),FUN=quantile,probs=0.75,type=6)
# names(Dur.75)<-c("site_no","Nseason","Pp75") #Period of record for season, p95
# 
# #merge datasets
# DAILY<-merge(DAILY,Dur.25,by=c("site_no","Nseason"))
# DAILY<-merge(DAILY,Dur.75,by=c("site_no","Nseason"))
# 
# #dummy variable coding for a Low or High Flow day
# DAILY$LFD25<-ifelse(DAILY$Pp25>DAILY$cfs, 1, 0)
# 
# #CHECK: sum(DAILY$LFD05)/length(DAILY$LFD05) = .049 
# DAILY$HFD75<-ifelse(DAILY$Pp75<DAILY$cfs, 1, 0)
# #CHECK: sum(DAILY$HFD95)/length(DAILY$HFD95) = .050

# #Collapse to seasonal level:
# S_Duration <- ddply(DAILY, .(site_no, year,Nseason), summarize, 
#                     #daysperseason=sum(tally),
#                     LF25_days = sum(LFD25, na.rm = T),
#                     HF75_days = sum(HFD75, na.rm = T)
# )

##Merge the 3 metrics data sets
merge1<-merge(avgflow,Mag.05p.95p,by=c("site_no","year","Nseason"))
merge2<-merge(merge1,S_Duration,by=c("site_no","year","Nseason"))

#UVA_metrics<-merge2
#save(UVA_metrics,file="output/UVA_metrics.rdata")

#USGS_metrics<-merge2
#save(USGS_metrics,file="output/USGS_metrics.rdata")

#Pred_fromDF<-merge2
#save(Pred_fromDF,file="output/Pred_fromDF.rdata")
#save(Pred_fromDF,file="output/Pred_fromDFNEW.rdata")
