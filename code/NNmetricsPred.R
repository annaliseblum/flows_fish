#Estimate extreme flow statistics with predicted daily streamflows
###Impact of Extreme Streamflows on Brook Trout Young-of-Year Abundance
### Annalise G Blum
##Created:Oct 24, 2016, last modified: Oct 24,2016
##Data NEEDED for this file: "output/dNN.Fish.rdata", "output/fishSC.rdata"
#TO DO: Section 3 Magnitude - trying to convert to Q95 and Q5 by season/year

##Data sets created in this file: 

library(DataCombine); library(ggplot2)

#### 1 - Clean and drop
#### 2 - Find MA-FDCs for all of the sites 
#### 3 - Metric 1: Average seasonal flows
#### 4 - Metric 2: Dummy variable for drought or flood
#### 5 - Metric 3: Magnitude (Intensity/Severity) LF and HFs
#### 6 - Metric 4: Duration: days below .05 percentile MA-FDC or above .95 MA-FDC
#### 7 - Merge Metrics datasets
#### 8 - Collapse to Annual level
#### 9 - Merge FISH SITE flow metrics and site characteristics

#load("output/dNN.USGS.Fish.rdata") #flows predicted in NN_DAR.R

#dNN.Fish<-dNN.USGS.Fish #all USGS sites

dNN.Fish<-NN.USGS.UVA.fish #also UVA sites

#load("output/dNN.Fish.rdata") #flows predicted in NN_DAR.R

#### 1 - Clean and drop ####
head(dNN.Fish)
#how many zero flows?
#sum(dNN.Fish$NNpredcfs==0)/length(dNN.Fish$NNpredcfs) # No zero flows predicted using the USGS sites

#create day, month and year variables
dNN.Fish$day<-as.integer(format(dNN.Fish$Date, "%d")) #extract day variable 
dNN.Fish$month<-as.integer(format(dNN.Fish$Date, "%m")) #extract month variable 
dNN.Fish$year<-as.integer(format(dNN.Fish$Date, "%Y")) #extract year variable

#Create new season and year variables
dNN.Fish$Nyear<-ifelse(dNN.Fish$month<6,dNN.Fish$year-1981,dNN.Fish$year-1980) #to get years 1-29

#drop day days Feb 29th so all years n=365
dNN.Fish<-dNN.Fish[-which(dNN.Fish$month==02 & dNN.Fish$day==29),]

#remove FISH years without all 365 days of flow data
YearTally <- aggregate(dNN.Fish$NNpredcfs,by=list(dNN.Fish$site_no, dNN.Fish$Nyear),FUN=length)
names(YearTally)<-c("site_no","Nyear","daysperFyr")
sum(YearTally$daysperFyr<365) #230 years with <365 flow values (only 120 sites for calendar years...)
dNN.Fish1<-data.frame(merge(dNN.Fish, YearTally, by = c('site_no','Nyear'))) #merge with flow data
NN.Fish<-dNN.Fish1[dNN.Fish1$daysperFyr>364,] #remove years with less than 365 days of data
length(unique(NN.Fish$site_no))

#how many years per site?
SiteTally <- aggregate(NN.Fish$NNpredcfs,by=list(NN.Fish$site_no),FUN=length)
names(SiteTally)<-c("site_no","daysofrecord")
SiteTally$years<-SiteTally$daysofrecord/365
#summary(SiteTally$years)

#create season variable
##Kanno defines seasons as fall=Aug-Nov, winter=dec-feb, spring=march-may, summer=june-aug
NN.Fish$season[NN.Fish$month==12|NN.Fish$month==01|NN.Fish$month==02]<-"winter"
NN.Fish$season[NN.Fish$month==03|NN.Fish$month==04|NN.Fish$month==05]<-"spring"
NN.Fish$season[NN.Fish$month==06|NN.Fish$month==07|NN.Fish$month==08]<-"summer"
NN.Fish$season[NN.Fish$month==09|NN.Fish$month==10|NN.Fish$month==11]<-"fall"

#create a numeric season variable 1-4 starting with summer as 1
NN.Fish$Nseason[NN.Fish$season=="summer"]<- 1
NN.Fish$Nseason[NN.Fish$season=="fall"]<-2
NN.Fish$Nseason[NN.Fish$season=="winter"]<-3
NN.Fish$Nseason[NN.Fish$season=="spring"]<-4
#NN.Fish[300:450,] #check

#### 2 - Find MA-FDCs for all of the sites ####
###FISH YEARS
NforMAFDC<-NN.Fish[c("site_no","Nyear", "NNpredcfs")] #pull relevant variables
NforMAFDC$Nyear<-as.integer(NforMAFDC$Nyear)

NL_forMAFDC<-split(NforMAFDC[,2:3],NforMAFDC[1]) #make into list of sites

#one site as test example
# test<-NL_forMAFDC[[1]]
# test2<-data.frame(split(test[,2],test[1])) #make into matrix of flows by year
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
MAFDCs<-t(as.data.frame(lapply(NL_forMAFDC,createMAFDC))) #transpose application of createMAFDC
rownames(MAFDCs)<-substr(rownames(MAFDCs),2,9)

# #loop through site_nos to remove leading X for USGS sites:
# for (i in 1:length(rownames(MAFDCs))){
#   if (nchar(rownames(MAFDCs)[i])==9)
#     rownames(MAFDCs)[i]=substr(rownames(MAFDCs)[i],2,9)
#   else
#     rownames(MAFDCs)[i]=rownames(MAFDCs)[i]
# }
# #alt method using strsplit: but how to save just site numbers? strsplit(rownames(MAFDCs),"X")

#### 3 - Metric 1: Average seasonal flows  ####
avgflow <- aggregate(NN.Fish$NNpredcfs,by=list(NN.Fish$site_no, NN.Fish$Nyear,NN.Fish$Nseason),FUN=mean)
names(avgflow)<-c("site_no","Nyear","Nseason","avgSNNflow")
summary(avgflow$avgSNNflow)
#check average seasonal average flows by season
#avgseasonalflows<-aggregate(avgflow$avgSNNflow,by=list(avgflow$Nseason),mean); avgseasonalflows

#### 4 - Metric 2: Magnitude (Intensity/Severity) LF and HFs  ####
#find by site, year, and season
SMag.05<-aggregate(NN.Fish$NNpredcfs,by=list(NN.Fish$site_no,NN.Fish$Nyear,NN.Fish$Nseason),FUN=quantile,probs=0.05,type=6)
names(SMag.05)<-c("site_no","Nyear","Nseason","p5") #Q95 is the same as p.05
SMag.95<-aggregate(NN.Fish$NNpredcfs,by=list(NN.Fish$site_no,NN.Fish$Nyear,NN.Fish$Nseason),FUN=quantile,probs=0.95,type=6)
names(SMag.95)<-c("site_no","Nyear","Nseason","p95") #Q5 is the same as p.95

#merge datasets
Mag.05p.95p<-merge(SMag.05,SMag.95,by=c("site_no","Nyear","Nseason"))

###ALTERNATIVELY, 7DAYMIN, 3DAY MAX NN.Fish$NNpredcfs
##LOW FLOWS
#create 7day rolling averages
NN.FishS<-NN.Fish[order(NN.Fish$site_no,NN.Fish$Dat),] #make sure order is correct
#NN.FishS[1:100,]; tail(NN.FishS) #spot check

#slide data
NN.FishS<-slide(NN.FishS, Var= "NNpredcfs", GroupVar= "site_no", NewVar= "NNpredcfsL1", slideBy = -1)
NN.FishS<-slide(NN.FishS, Var= "NNpredcfs", GroupVar= "site_no", NewVar= "NNpredcfsL2", slideBy = -2)
NN.FishS<-slide(NN.FishS, Var= "NNpredcfs", GroupVar= "site_no", NewVar= "NNpredcfsL3", slideBy = -3)
NN.FishS<-slide(NN.FishS, Var= "NNpredcfs", GroupVar= "site_no", NewVar= "NNpredcfsF1", slideBy = 1)
NN.FishS<-slide(NN.FishS, Var= "NNpredcfs", GroupVar= "site_no", NewVar= "NNpredcfsF2", slideBy = 2)
NN.FishS<-slide(NN.FishS, Var= "NNpredcfs", GroupVar= "site_no", NewVar= "NNpredcfsF3", slideBy = 3)

#head(NN.FishS);  tail(NN.FishS) #spot check

#calculate average of 7 days ahead and 7 behind around each date (and 3)
NN.FishS$avg7day<-rowMeans(subset(NN.FishS, select = c("NNpredcfs","NNpredcfsL1","NNpredcfsL2","NNpredcfsL3",
                                                       "NNpredcfsF1","NNpredcfsF2","NNpredcfsF3")), na.rm = TRUE)
NN.FishS$avg3day<-rowMeans(subset(NN.FishS, select = c("NNpredcfs","NNpredcfsL1","NNpredcfsF3")), na.rm = TRUE)

##Collapse to seasonal level to find min7day and min3day flow for each season, year and site
minflow7 <- aggregate(NN.FishS$avg7day,by=list(NN.FishS$site_no, NN.FishS$Nyear,NN.FishS$Nseason),FUN=min)
names(minflow7)<-c("site_no","Nyear","Nseason","Pmin7day")
summary(minflow7)
minflow3 <- aggregate(NN.FishS$avg3day,by=list(NN.FishS$site_no, NN.FishS$Nyear,NN.FishS$Nseason),FUN=min)
names(minflow3)<-c("site_no","Nyear","Nseason","Pmin3day")
minflow1 <- aggregate(NN.FishS$NNpredcfs,by=list(NN.FishS$site_no, NN.FishS$Nyear,NN.FishS$Nseason),FUN=min)
names(minflow1)<-c("site_no","Nyear","Nseason","min1day")

#merge
minflow<-merge(minflow7, minflow3, by=c("site_no","Nyear","Nseason"))
minflow<-merge(minflow, minflow1, by=c("site_no","Nyear","Nseason"))

##MAX
maxflow3 <- aggregate(NN.FishS$avg3day,by=list(NN.FishS$site_no, NN.FishS$Nyear,NN.FishS$Nseason),FUN=max)
names(maxflow3)<-c("site_no","Nyear","Nseason","maxflow3")
maxflow1 <- aggregate(NN.FishS$NNpredcfs,by=list(NN.FishS$site_no, NN.FishS$Nyear,NN.FishS$Nseason),FUN=max)
names(maxflow1)<-c("site_no","Nyear","Nseason","maxflow1")

#merge
maxflow<-merge(maxflow3, maxflow1, by=c("site_no","Nyear","Nseason"))

#### 5 - Metric 3: Duration: days below .05 percentile MA-FDC or above .95 MA-FDC  ####
names(NNfish_MAFDC)
#for now can define as any event above .99 percentile MA-FDC= flood
MAFDC.05p <- apply(MAFDCs,1,FUN=quantile,probs=0.05,type=6)
MAFDC.95p <- apply(MAFDCs,1,FUN=quantile,probs=0.95,type=6) #type 6: m = p. p[k] = k / (n + 1).

#make into dataframes
MAFDC.05p_df<-data.frame(site_no=names(MAFDC.05p), MAFDC.05p=MAFDC.05p, row.names=NULL)
MAFDC.05p_df$site_no<-as.character(MAFDC.05p_df$site_no)

MAFDC.95p_df<-data.frame(site_no=names(MAFDC.95p), MAFDC.95p=MAFDC.95p, row.names=NULL)
MAFDC.95p_df$site_no<-as.character(MAFDC.95p_df$site_no)

#merge datasets
NNfish_p5p95<-merge(NN.Fish,MAFDC.05p_df,by=c("site_no"))
NNfish_p5p95<-merge(NNfish_p5p95,MAFDC.95p_df,by=c("site_no"))

#dummy variable coding for a Low or High Flow day
NNfish_p5p95$LFD<-ifelse(NNfish_p5p95$MAFDC.05p>NNfish_p5p95$NNpredcfs, 1, 0)
#CHECK: sum(NNfish_MAFDC3$LFD)/length(NNfish_MAFDC3$LFD) = .097
NNfish_p5p95$HFD<-ifelse(NNfish_p5p95$MAFDC.95p<NNfish_p5p95$NNpredcfs, 1, 0)
#CHECK: sum(NNfish_MAFDC3$HFD)/length(NNfish_MAFDC3$HFD) = .058

#Collapse to seasonal level:
S_Duration <- ddply(NNfish_p5p95, .(site_no, Nyear,Nseason), summarize, 
                    #daysperseason=sum(tally),
                    LF_days = sum(LFD, na.rm = T),
                    HF_days = sum(HFD, na.rm = T)
)

#### 6 - Metric 4: Dummy variable for drought or flood  ####
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
NNfish_MAFDC1<-merge(NN.Fish,MAFDC.01p_df,by=c("site_no"))
NNfish_MAFDC<-merge(NNfish_MAFDC1,MAFDC.99p_df,by=c("site_no"))

#dummy variable coding
NNfish_MAFDC$DroughtD<-ifelse(NNfish_MAFDC$MAFDC.01p>NNfish_MAFDC$NNpredcfs, 1, 0)
#CHECK: sum(NNfish_MAFDC$DroughtD)/length(NNfish_MAFDC$DroughtD) = 0.043 - why not closer to 0.01? maybe because of ties??
NNfish_MAFDC$FloodD<-ifelse(NNfish_MAFDC$MAFDC.99p<NNfish_MAFDC$NNpredcfs, 1, 0)
#CHECK: sum(df4_MAFDC$FloodD)/length(df4_MAFDC$DroughtD) = 0.01

#Collapse data to monthly level
NNfish_MAFDC$tally<-1
# month_FDDummy <- ddply(NNfish_MAFDC, .(site_no, Nyear,month), summarize, 
#                        dayspermonth=sum(tally),
#                        Drought_days = sum(DroughtD, na.rm = T),
#                        Flood_days = sum(FloodD, na.rm = T)
# )

#Collapse to seasonal level:
S_FDDummy <- ddply(NNfish_MAFDC, .(site_no, Nyear,Nseason), summarize, 
                   #daysperseason=sum(tally),
                   Drought_days = sum(DroughtD, na.rm = T),
                   Flood_days = sum(FloodD, na.rm = T)
)
S_FDDummy$Drought<-ifelse(S_FDDummy$Drought_days>0,1,0)
S_FDDummy$Flood<-ifelse(S_FDDummy$Flood_days>0,1,0)

#### 7 - Merge Metrics datasets  ####
# avgflow
# S_FDDummy
# NNfish_MAFDC3
# S_Duration
#maxflow minflow

merMetrics<-merge(avgflow,Mag.05p.95p,by=c("site_no", "Nyear","Nseason"))
merMetrics<-merge(merMetrics,S_Duration,by=c("site_no", "Nyear","Nseason"))
merMetrics<-merge(merMetrics,minflow,by=c("site_no", "Nyear","Nseason"))
merMetrics<-merge(merMetrics,maxflow,by=c("site_no", "Nyear","Nseason"))

#merMetrics<-merge(merMetrics,S_FDDummy,by=c("site_no", "Nyear","Nseason")) #skipping dummies for now

head(merMetrics); tail(merMetrics); dim(merMetrics); str(merMetrics) #summary(merMetrics)
length(unique(merMetrics$site_no))
#save(merMetrics,file="output/merMetrics.rdata")
merUVAMetrics<-merMetrics #when using UVA-USGS combined datasets
save(merUVAMetrics,file="output/merUVAMetrics.rdata")


#### 8 - Collapse to Annual level ####

#seasonal avg flows
Seasonalcast <- dcast(merMetrics, site_no + Nyear ~ Nseason,value.var = "avgSNNflow") 
#head(Seasonalcast) #summer = 1; fall=2, etc
names(Seasonalcast)<-c("site_no","Nyear","AvgQsummer","AvgQfall", "AvgQwinter","AvgQspring")
A.Fishmerge<-Seasonalcast

#Magnitude: p5 and p95
Magcast5 <- dcast(merMetrics, site_no + Nyear ~ Nseason,value.var = "p5") 
#head(Magcast5) #summer = 1; fall=2, etc
names(Magcast5)<-c("site_no","Nyear","p5summer","p5fall", "p5winter","p5spring")
A.Fishmerge<-merge(A.Fishmerge,Magcast5,by=c("site_no","Nyear"))

Magcast95 <- dcast(merMetrics, site_no + Nyear ~ Nseason,value.var = "p95") 
#head(Magcast95) #summer = 1; fall=2, etc
names(Magcast95)<-c("site_no","Nyear","p95summer","p95fall", "p95winter","p95spring")
A.Fishmerge<-merge(A.Fishmerge,Magcast95,by=c("site_no","Nyear"))

#Duration
DurLFcast <- dcast(merMetrics, site_no + Nyear ~ Nseason,value.var = "LF_days") 
#head(DurLFcast) #summer = 1; fall=2, etc
names(DurLFcast)<-c("site_no","Nyear","DurLFsummer","DurLFfall", "DurLFwinter","DurLFspring")
A.Fishmerge<-merge(A.Fishmerge,DurLFcast,by=c("site_no","Nyear"))

DurHFcast <- dcast(merMetrics, site_no + Nyear ~ Nseason,value.var = "HF_days") 
#head(DurHFcast) #summer = 1; fall=2, etc
names(DurHFcast)<-c("site_no","Nyear","DurHFsummer","DurHFfall", "DurHFwinter","DurHFspring")
A.Fishmerge<-merge(A.Fishmerge,DurHFcast,by=c("site_no","Nyear"))

#Dummy for drought
castDrought <- dcast(merMetrics, site_no + Nyear ~ Nseason,value.var = "Drought")
names(castDrought)<-c("site_no","Nyear", "DroPredsummer","DroPredfall","DroPredwin","DroPredspring") #DroPredwin DroPredspring all NANs
A.Fishmerge<-merge(A.Fishmerge,castDrought,by=c("site_no","Nyear"))

#Dummy for flood
castFlood <- dcast(merMetrics, site_no + Nyear ~ Nseason,value.var = "Flood")
names(castFlood)<-c("site_no","Nyear", "FloodPredsummer","FloodPredfall","FloodPredwinter","FloodPredspring")
A.Fishmerge<-merge(A.Fishmerge,castFlood,by=c("site_no","Nyear"))

#remove drought and flood variables I'm not using:
A.Fishmerge$DroPredwin<-NULL;A.Fishmerge$DroPredspring<-NULL
A.Fishmerge$FloodPredsummer<-NULL;A.Fishmerge$FloodPredfall<-NULL

#### 9 - Merge FISH SITE flow metrics and site characteristics - FOR FREQUENTIST MODELS ####

load("output/A.Fish.rdata")

#merge to get Seasonal Flow and Basin characteristics
str(A.Fish); str(A.Fishmerge)
UVAA.FishNNPreds<-merge(A.Fish,A.Fishmerge,by=c("site_no","Nyear")) 

save(UVAA.FishNNPreds,file="output/UVAA.FishNNPreds.rdata")

#save(A.FishNNPreds,file="output/A.FishNNPreds.rdata")


