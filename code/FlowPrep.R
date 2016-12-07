##Flow data prep and explore
###Impact of Extreme Streamflows on Brook Trout Young-of-Year Abundance
### Annalise G Blum; Modified Dec 5,2016
##Data sets created in this file: "output/S.FB.rdata" Seasonal Flow and Basin characteristics;  "output/gagedsites_BC.data"

#rm(list=ls())

library(DataCombine)
library(ggplot2)
#### 1 - clean data and bind USGS and UVA data together
#### 2 - Subset for sites with long enough records and find MA-FDCs for all of the sites
#### 3 - Metric 1: Average seasonal flows
#### 4 - Metric 2: Dummy variable for drought or flood
#### 5 - Metric 3: Magnitude (Intensity/Severity) LF and HFs
#### 6 - Metric 4: Duration: days below .05 percentile MA-FDC or above .95 MA-FDC
#### 7 - Merge Metrics datasets
#### 8 - Basin Characteristic cleaning
#### 9 - Merge flows and Basin Characteristics

load("data/rawDailyData.rdata") #flows for 47 USGS sites in HUC2 and GAGESII EasternMts
USGSdaily<-rawDailyData

#### 1 - clean data and bind USGS and UVA data together ####
USGSdaily$X_00060_00003_cd<-NULL
USGSdaily$agency_cd<-NULL
names(USGSdaily)[3]<-"cfs"

#evaluate record lengths of sites
USGSdaily$day<-as.integer(format(USGSdaily$Date, "%d")) #extract day variable 
USGSdaily$month<-as.integer(format(USGSdaily$Date, "%m")) #extract month variable 
USGSdaily$year<-as.integer(format(USGSdaily$Date, "%Y")) #extract year variable 

save(USGSdaily,file="output/USGSdaily.rdata")

#how many zero flows?
sum(USGSdaily$cfs==0)/length(USGSdaily$cfs) # 0.00067

# # UVA flow data prep ##
load("output/UVA_Discharge.rdata") #flows for 5 sites in SNP from UVA SWAS team
#aggregate to daily values
UVA_daily<-aggregate(UVA_Discharge$cfs,by=list(UVA_Discharge$StationID,UVA_Discharge$year,
                                               UVA_Discharge$month,UVA_Discharge$day),mean)
names(UVA_daily)<-c("site_no","year","month","day","cfs")

UVA_daily$site_no<-paste("UVA_",UVA_daily$site_no,sep="")

#need this dataset to match daily - get date variable into date format
str(USGSdaily); names(USGSdaily) #site_no" "Date"    "cfs"     "month"   "year" 
str(UVA_daily); names(UVA_daily) 
UVA_daily$Date<-as.Date(paste (str_pad(as.character(UVA_daily$day),width=2,side="left",pad=0),
                               str_pad(as.character(UVA_daily$month),width=2,side="left",pad=0),
                               as.character(UVA_daily$year),
                               sep = "/"),"%d/%m/%Y")

#re-order to match daily dataset of USGS flows
UVA_daily<-UVA_daily[,c("site_no","Date","cfs","day","month","year")]
names(UVA_daily);names(USGSdaily)

#drop data after 2010
UVA_daily<-UVA_daily[UVA_daily$year<2011,]
save(UVA_daily,file="output/UVA_daily.rdata")

#sum(UVA_daily$cfs==0)/length(UVA_daily$cfs) #=0.0162 are zeros

#Combine USGS and UVA sites
USGSdaily$type<-"USGS"
UVA_daily$type<-"UVA"

#rbind USGS data (USGSdaily) and UVA data (UVA_daily) - why am I doing this??
daily<-rbind(USGSdaily,UVA_daily)
length(unique(daily$site_no)) #49 USGS sites + 5 UVA sites =54

#Create new season and year variables
daily$Nyear<-ifelse(daily$month<6,daily$year-1981,daily$year-1980) #to get years 1-29

#remove years without all 365 days of flow data
YearTally <- aggregate(daily$cfs,by=list(daily$site_no, daily$Nyear),FUN=length) #drop years without 365 days of data
names(YearTally)<-c("site_no","Nyear","daysperNyr")
#sum(YearTally$daysperNyr<365) #44 years with <365 flow values
df1<-data.frame(merge(daily, YearTally, by = c('site_no','Nyear'))) #merge with flow data
df2<-df1[df1$daysperNyr>364,] #remove years with less than 365 days of data
length(unique(df2$site_no)) #53 sites (lost 1 USGS one that didn't have any complete flow years)

#create season variable
##Kanno defines seasons as fall=Aug-Nov, winter=dec-feb, spring=march-may, summer=june-aug
df2$season[df2$month==12|df2$month==01|df2$month==02]<-"winter"
df2$season[df2$month==03|df2$month==04|df2$month==05]<-"spring"
df2$season[df2$month==06|df2$month==07|df2$month==08]<-"summer"
df2$season[df2$month==09|df2$month==10|df2$month==11]<-"fall"

#create a numeric season variable 1-4 starting with summer as 1
df2$Nseason[df2$season=="summer"]<- 1
df2$Nseason[df2$season=="fall"]<-2
df2$Nseason[df2$season=="winter"]<-3
df2$Nseason[df2$season=="spring"]<-4
#df2[300:450,] #check

#### 2 - Subset for sites with long enough records and find MA-FDCs for all of the sites####
#how many years of data per site?
rec.lengths<-aggregate(df2$cfs,by=list(df2$site_no),length)
names(rec.lengths)<-c("site_no","daysofflow")
sort(rec.lengths$daysofflow/365)
df3<-data.frame(merge(df2, rec.lengths, by = c('site_no'))) #merge with flow data

#drop sites with less than 3 years of flow data
df4<-df3[df3$daysofflow>3*365,] #remove sites with less than 3 (before 15 but lost too many) years of data
length(unique(df4$site_no)) #50 sites remain!!

#create a list of sites
USGS_sites<-as.data.frame(unique(df4$site_no[df4$type=="USGS"])); names(USGS_sites)<-"site_no"

#how many zero flows across both USGS and UVA sites?
sum(df4$cfs==0)/length(df4$cfs) # 0.0022

###MA-FDC for each site - STILL USING CALENDAR YEARS FOR THESE
#drop day days Feb 29th so all years n=365
forMAFDC<-df4[-which(df4$month==02 & df4$day==29),]
forMAFDC<-forMAFDC[c("site_no","year", "cfs")] #pull relevant variables

L_forMAFDC<-split(forMAFDC[,2:3],forMAFDC[1]) #make into list of sites

#one site as test example
# test<-L_forMAFDC[[1]]
# test2<-data.frame(split(test[,2],test[1])) #make into matrix
# test3<-apply(test2,2,sort)
# MAFDCtest<-apply(test3,1,median)

createMAFDC<-function(x) { #x=dataframe with 2 columns: year and cfs
  asmatrix<-data.frame(split(x[,2],x[1]))
  sortedmatrix<-apply(asmatrix,2,sort) #sort the flows with in each year in increasing order
  MAFDC<-apply(sortedmatrix,1,median) #then take median of each "day-tile" column
  return(MAFDC)
}
#createMAFDC(test)
#then apply function to list to make each into a matrix of years X days of the year
MAFDCs<-t(as.data.frame(lapply(L_forMAFDC,createMAFDC))) #transpose application of createMAFDC

#loop through site_nos to remove leading X for USGS sites:
for (i in 1:length(rownames(MAFDCs))){
  if (nchar(rownames(MAFDCs)[i])==9)
    rownames(MAFDCs)[i]=substr(rownames(MAFDCs)[i],2,9)
  else
    rownames(MAFDCs)[i]=rownames(MAFDCs)[i]
}
#alt method using strsplit: but how to save just site numbers? strsplit(rownames(MAFDCs),"X")

#### 3 - Metric 1: Average seasonal flows  ####
avgflow <- aggregate(df4$cfs,by=list(df4$site_no, df4$Nyear,df4$Nseason),FUN=mean)
names(avgflow)<-c("site_no","Nyear","Nseason","avgSflow")
summary(avgflow$avgSflow)
#check average seasonal average flows by season
#avgseasonalflows<-aggregate(avgflow$avgSflow,by=list(avgflow$Nseason),mean); avgseasonalflows

#### 4 - Metric 2: Dummy variable for drought or flood  ####
#Probably should use Palmer Drought index for drought
#for now can define as any event below 0.01 percentile MA-FDC= drought
#for now can define as any event above .99 percentile MA-FDC= flood
MAFDC.01p <- apply(MAFDCs,1,FUN=quantile,probs=0.01,type=6)
MAFDC.99p <- apply(MAFDCs,1,FUN=quantile,probs=0.99,type=6) #type 6: m = p. p[k] = k / (n + 1).

#make into dataframes
MAFDC.01p_df<-data.frame(site_no=names(MAFDC.01p), MAFDC.01p=MAFDC.01p, row.names=NULL)
MAFDC.01p_df$site_no<-as.character(MAFDC.01p_df$site_no)

MAFDC.99p_df<-data.frame(site_no=names(MAFDC.99p), MAFDC.99p=MAFDC.99p, row.names=NULL)
MAFDC.99p_df$site_no<-as.character(MAFDC.99p_df$site_no)

#merge datasets
df4_MAFDC<-merge(df4,MAFDC.01p_df,by="site_no")
df4_MAFDC<-merge(df4_MAFDC,MAFDC.99p_df,by="site_no")

#dummy variable coding
df4_MAFDC$DroughtD<-ifelse(df4_MAFDC$MAFDC.01p>df4_MAFDC$cfs, 1, 0)
#CHECK: sum(df4_MAFDC$DroughtD)/length(df4_MAFDC$DroughtD) = 0.039 - why not closer to 0.01? maybe because of ties??
df4_MAFDC$FloodD<-ifelse(df4_MAFDC$MAFDC.99p<df4_MAFDC$cfs, 1, 0)
#CHECK: sum(df4_MAFDC$FloodD)/length(df4_MAFDC$DroughtD) = 0.01

#Collapse data to monthly level
df4_MAFDC$tally<-1
month_FDDummy <- ddply(df4_MAFDC, .(site_no, Nyear,month), summarize, 
                 dayspermonth=sum(tally),
                 Drought_days = sum(DroughtD, na.rm = T),
                 Flood_days = sum(FloodD, na.rm = T)
)

#Collapse to seasonal level:
S_FDDummy <- ddply(df4_MAFDC, .(site_no, Nyear,Nseason), summarize, 
                    #daysperseason=sum(tally),
                    Drought_days = sum(DroughtD, na.rm = T),
                    Flood_days = sum(FloodD, na.rm = T)
)
S_FDDummy$Drought<-ifelse(S_FDDummy$Drought_days>0,1,0)
S_FDDummy$Flood<-ifelse(S_FDDummy$Flood_days>0,1,0)

#### 5 - Metric 3: Magnitude (Intensity/Severity) LF and HFs  ####

##LOW FLOWS
#create 7day rolling averages
df4S<-df4[order(df4$site_no,as.Date(df4$Date, format="%Y-%m-%d ")),] #make sure order is correct
#df4S[1:100,]; tail(df4S) #spot check

#slide data
df4S<-slide(df4S, Var= "cfs", GroupVar= "site_no", NewVar= "cfsL1", slideBy = -1)
df4S<-slide(df4S, Var= "cfs", GroupVar= "site_no", NewVar= "cfsL2", slideBy = -2)
df4S<-slide(df4S, Var= "cfs", GroupVar= "site_no", NewVar= "cfsL3", slideBy = -3)
df4S<-slide(df4S, Var= "cfs", GroupVar= "site_no", NewVar= "cfsF1", slideBy = 1)
df4S<-slide(df4S, Var= "cfs", GroupVar= "site_no", NewVar= "cfsF2", slideBy = 2)
df4S<-slide(df4S, Var= "cfs", GroupVar= "site_no", NewVar= "cfsF3", slideBy = 3)

#head(df4S);  tail(df4S) #spot check

df5<-df4S #rename
#calculate average of 7 days ahead and 7 behind around each date (and 3)
df5$avg7day<-rowMeans(subset(df5, select = c("cfs","cfsL1","cfsL2","cfsL3","cfsF1","cfsF2","cfsF3")), na.rm = TRUE)
df5$avg3day<-rowMeans(subset(df5, select = c("cfs","cfsL1","cfsF1")), na.rm = TRUE)

##Collapse to seasonal level to find min7day and min3day flow for each season, year and site
minflow7 <- aggregate(df5$avg7day,by=list(df5$site_no, df5$Nyear,df5$Nseason),FUN=min)
names(minflow7)<-c("site_no","Nyear","Nseason","min7day")
summary(minflow7)
minflow3 <- aggregate(df5$avg3day,by=list(df5$site_no, df5$Nyear,df5$Nseason),FUN=min)
names(minflow3)<-c("site_no","Nyear","Nseason","min3day")
minflow1 <- aggregate(df5$cfs,by=list(df5$site_no, df5$Nyear,df5$Nseason),FUN=min)
names(minflow1)<-c("site_no","Nyear","Nseason","min1day")

#merge
minflow<-merge(minflow7, minflow3, by=c("site_no","Nyear","Nseason"))
minflow<-merge(minflow, minflow1, by=c("site_no","Nyear","Nseason"))

# ##Collapse to monthly level to find min7day flow for each MONTH, NNyear and site
# minflowMon7 <- aggregate(df5$avg7day,by=list(df5$site_no, df5$Nyear,df5$month),FUN=min)
# names(minflowMon7)<-c("site_no","Nyear","month","min7day")
# summary(minflowMon7)
# minflowMon3 <- aggregate(df5$avg3day,by=list(df5$site_no, df5$Nyear,df5$month),FUN=min)
# names(minflowMon3)<-c("site_no","Nyear","month","min3day")
# minflowMon<-merge(minflowMon7, minflowMon3, by=c("site_no","Nyear","month"))

####HIGH FLOWS
#1 day max
maxflow1 <- aggregate(df5$cfs,by=list(df5$site_no, df5$Nyear,df5$Nseason),FUN=max)
names(maxflow1)<-c("site_no","Nyear","Nseason","maxdayflow")
summary(maxflow1$maxdayflow)

#3 day max
maxflow3 <- aggregate(df5$avg3day,by=list(df5$site_no, df5$Nyear,df5$Nseason),FUN=max)
names(maxflow3)<-c("site_no","Nyear","Nseason","max3dayflow")
summary(maxflow3$max3dayflow)

#merge these
maxflow<-merge(maxflow1,maxflow3,by=c("site_no","Nyear","Nseason"))

# #check: all max 1 day flow must be higher than max 3 day
# check1 <- aggregate(maxflow$maxdayflow,by=list(maxflow$site_no),FUN=max)
# check2 <- aggregate(maxflow$max3dayflow,by=list(maxflow$site_no),FUN=max)
# sum(check1$x<check2$x) #0, good

##Merge max and min flow data sets
minmaxflow<-merge(minflow,maxflow,by=c("site_no","Nyear","Nseason"))

#### 6 - Metric 4: Duration: days below .05 percentile MA-FDC or above .95 MA-FDC  ####

##Low Flow duration - how many days flows are below 0.05 percentile within each season: MAFDCs
##High Flow duration - how many days flows are above 0.95 percentile within each season: MAFDCs
MAFDC.05p <- apply(MAFDCs,1,FUN=quantile,probs=0.05,type=6)
MAFDC.95p <- apply(MAFDCs,1,FUN=quantile,probs=0.95,type=6)

#make into dataframes
MAFDC.05p_df<-data.frame(site_no=names(MAFDC.05p), MAFDC.05p=MAFDC.05p, row.names=NULL)
MAFDC.05p_df$site_no<-as.character(MAFDC.05p_df$site_no)

MAFDC.95p_df<-data.frame(site_no=names(MAFDC.95p), MAFDC.95p=MAFDC.95p, row.names=NULL)
MAFDC.95p_df$site_no<-as.character(MAFDC.95p_df$site_no)

#merge datasets
df4_Duration<-merge(df4,MAFDC.05p_df,by="site_no")
df4_Duration<-merge(df4_Duration,MAFDC.95p_df,by="site_no")

#dummy variable coding for a Low or High Flow day
df4_Duration$LFD<-ifelse(df4_Duration$MAFDC.05p>df4_Duration$cfs, 1, 0)
#CHECK: sum(df4_Duration$LFD)/length(df4_Duration$LFD) = .072
df4_Duration$HFD<-ifelse(df4_Duration$MAFDC.95p<df4_Duration$cfs, 1, 0)
#CHECK: sum(df4_Duration$HFD)/length(df4_Duration$HFD) = .057

#df4_Duration$tally=1
#Collapse to seasonal level:
S_Duration <- ddply(df4_Duration, .(site_no, Nyear,Nseason), summarize, 
                   #daysperseason=sum(tally),
                   LF_days = sum(LFD, na.rm = T),
                   HF_days = sum(HFD, na.rm = T)
)

#### 7 - Merge Metrics datasets  ####
# avgflow
# S_FDDummy
# minmaxflow
# S_Duration

sdata0<-merge(avgflow,S_FDDummy,by=c("site_no", "Nyear","Nseason"))
sdata0<-merge(sdata0,minmaxflow,by=c("site_no", "Nyear","Nseason"))
sdata<-merge(sdata0,S_Duration,by=c("site_no", "Nyear","Nseason"))

head(sdata); tail(sdata); dim(sdata); str(sdata) #summary(sdata)

#add site type and UVA to those site names
sdata$type<-ifelse(nchar(sdata$site_no)==4,"UVA","USGS")
#loop through site_nos to add "UVA_" to UVA sites:
for (i in 1:length(sdata$site_no)){
  if (nchar(sdata$site_no[i])==4) #(sdata$type=="UVA)
    sdata$site_no[i]=paste("UVA_",sdata$site_no[i],sep="")
}

sflow<-sdata
save(sflow,file="output/sflow.rdata")

#### 8 - Basin Characteristic cleaning ####
#USGS
load("output/rawUSGS_BC.rdata")#USGS gages Basin Chars
#rename site characteristics variables to match fish sample sites
#USGS: first get in correct order and pull necessary vars
USGS_BC<-rawUSGS_BC[c("site_no","DRAIN_SQKM","HUC02","LAT_GAGE","LNG_GAGE","REACHCODE",
                   "SLOPE_PCT","ASPECT_DEGREES", "ELEV_SITE_M",
                   "BFI_AVE","TOPWET")] #these aren't in fish
names(USGS_BC)<-c("site_no","DA_SQKM","HUC02","LAT_GAGE","LNG_GAGE","REACH_CODE",
                  "Slope_pct","Aspect_deg","Elev_m","BFI_AVE","TOPWET")
USGS_BC$DA_SQKM<-as.numeric(USGS_BC$DA_SQKM)
USGS_BC$type<-"USGS"
USGS_BC<-merge(USGS_sites,USGS_BC,by="site_no") #just keep the 45 sites with at least 3 years of full daily data
save(USGS_BC,file="output/USGS_BC.rdata")#USGS gages Basin Chars - cleaned

#UVA
load("output/rawUVA_BC.rdata") #UVA gages Basin Chars

#first get in correct order and add necessary vars
rawUVA_BC$HUC02<-NA
rawUVA_BC$REACHCODE<-NA
rawUVA_BC$SLOPE_PCT<-NA
rawUVA_BC$ASPECT_DEGREES<-NA
rawUVA_BC$BFI_AVE<-NA
rawUVA_BC$TOPWET<-NA
#convert DA to km2 from ha: 100 ha/km2
rawUVA_BC$DA_SQKM<-rawUVA_BC$Basin.Area..ha./100
UVA_BC<-rawUVA_BC[c("site_no","DA_SQKM","HUC02","Latitude","Longitude","REACHCODE",
                 "SLOPE_PCT","ASPECT_DEGREES", "Elev..m.","BFI_AVE","TOPWET")]

names(UVA_BC)<-c("site_no","DA_SQKM","HUC02","LAT_GAGE","LNG_GAGE","REACH_CODE",
                  "Slope_pct","Aspect_deg","Elev_m","BFI_AVE","TOPWET")

#Add "UVA_" to UVA sites
UVA_BC$site_no<-paste("UVA_",UVA_BC$site_no,sep="")
UVA_BC$type<-"UVA"
save(UVA_BC,file="output/UVA_BC.rdata") #UVA gages Basin Chars - cleaned

#### 9 - Merge flows and Basin Characteristics ####
#rbind basin characteristics:
gagedExtrasites_BC<-rbind(USGS_BC,UVA_BC)
class(gagedExtrasites_BC$site_no);class(sflow$site_no)

#Gaged sites (USGS and UVA) - get site list of characteristics only sites we are using
Gagedsitelist<-as.data.frame(unique(sflow$site_no))
names(Gagedsitelist)<-"site_no"; Gagedsitelist$site_no<-as.character(Gagedsitelist$site_no)
gagedsites_BC<-merge(gagedExtrasites_BC,Gagedsitelist,by="site_no")

save(gagedsites_BC,file="output/gagedsites_BC.rdata")

#merge to get Seasonal Flow and Basin characteristics
S.FB<-merge(gagedsites_BC,sflow,by=c("site_no","type")) 

# # Check distributions of variables; transform ones with negs
# summary(S.FB) #look for negative values which will cause trouble
# #hist(log(S.FB$maxdayflow)) #lognormal-ish
# #hist(log(S.FB$min7day+.001))
# summary(S.FB$min7day)
# sum(S.FB$min7day==0) #32 zeros...
# sort(S.FB$min7day[S.FB$min7day<.05]) #next lowest is 0.004 some how? should be .01 - UVA sites??
# S.FB$site_no[S.FB$min7day<.01] #mostly UVA sites, but also 3 USGS sites
# summary(S.FB$min3day)
# sum(S.FB$min3day==0) #40 of zero
# sum(S.FB$min1day==0) #47 of zero

#replace zeros in LFs
S.FB$min7day[S.FB$min7day<.001]<- .001 #add .001 to the zeros so i can log
S.FB$min3day[S.FB$min3day<.001]<- .001 #add .001 to the zeros so i can log
S.FB$min1day[S.FB$min1day<.001]<- .001 #add .001 to the zeros so i can log

S.FB$LNG_GAGE.T<- S.FB$LNG_GAGE+100 #summary(S.FB)

save(S.FB,file="output/S.FB.rdata")
