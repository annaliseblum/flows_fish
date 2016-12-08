##Comparison of RR to NN-DAR methods
#Need: (1) actual UVA flow metrics (2) NN pred metrics and (3) RR predicted flow metrics for UVA sites 

#### 1 - actual UVA flow metrics  #### 
load("output/S.FB.rdata")
unique(S.FB$site_no) #this includes the UVA sites - actual flow metrics
UVAobsMetrics<-S.FB[S.FB$type=="UVA",]

#### 2 - regional regression models  #### 

#run PredFlows.R file to get S.Flow

#want names to match UVAobsMetrics
names(UVAobsMetrics)
names(S.Flow); unique(S.Flow$site_no)
S.FlowUVA<-S.Flow[S.Flow$type=="UVA",]

#keep actual to make sure that they match the UVAobsMetrics dataset
RRPreds<-S.FlowUVA[c("site_no","Nyear","Nseason","avgSflow","Drought_days","Flood_days","Drought","Flood",
                     "min7day","min3day","min1day","maxdayflow","max3dayflow", "LF_days","HF_days",
                     "AvgPreds","LF7Preds","HF3Preds","DroughtPreds","FloodPreds","DurLFPreds","DurHFPreds")]

#rename RR columns except site, Nyear and Nseason
colnames(RRPreds)[4:22] <- paste("RR", colnames(RRPreds)[4:22], sep = "_")

save(RRPreds,file="output/RRPreds.rdata")

#### 3 - NN predicted metrics  #### 
load("output/NN_ALLrdata")
head(NN_ALL)

NN_4comp<-NN_ALL[c("site_no","Date","NNpredcfs")] #"UVAobs_cfs",
names(NN_4comp)<-c("site_no","Date","cfs") #need it to match daily flows so that I can use the other code

#evaluate record lengths of sites
NN_4comp$day<-as.integer(format(NN_4comp$Date, "%d")) #extract day variable 
NN_4comp$month<-as.integer(format(NN_4comp$Date, "%m")) #extract month variable 
NN_4comp$year<-as.integer(format(NN_4comp$Date, "%Y")) #extract year variable 

#Create new season and year variables
NN_4comp$Nyear<-ifelse(NN_4comp$month<6,NN_4comp$year-1981,NN_4comp$year-1980) #to get years 1-29

####RENAME TO MATCH
daily<-NN_4comp

#remove years without all 365 days of flow data
YearTally <- aggregate(daily$cfs,by=list(daily$site_no, daily$Nyear),FUN=length) #drop years without 365 days of data
names(YearTally)<-c("site_no","Nyear","daysperNyr")
sum(YearTally$daysperNyr<365) #12 years with <365 flow values
df1<-data.frame(merge(daily, YearTally, by = c('site_no','Nyear'))) #merge with flow data
df2<-df1[df1$daysperNyr>364,] #remove years with less than 365 days of data
length(unique(df2$site_no)) #5 still

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

#### 2 - Subset for sites with long enough records and find MA-FDCs for all of the sites
#how many years of data per site?
rec.lengths<-aggregate(df2$cfs,by=list(df2$site_no),length)
names(rec.lengths)<-c("site_no","daysofflow")
sort(rec.lengths$daysofflow/365) #7.421918  7.421918 15.087671 18.263014 19.265753

###MA-FDC for each site - SWTICH TO

#drop day days Feb 29th so all years n=365
forMAFDC<-df2[-which(df2$month==02 & df2$day==29),]
forMAFDC<-forMAFDC[c("site_no","Nyear", "cfs")] #pull relevant variables

L_forMAFDC<-split(forMAFDC[,2:3],forMAFDC[1]) #make into list of sites

# #one site as test example
# test<-L_forMAFDC[[1]]
# aggregate(test$cfs,by=list(test$Nyear),length)
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

df4<-df2
#### 3 - NN predicted flows - estimated Metrics
#1: Average seasonal flows 
avgflow <- aggregate(df4$cfs,by=list(df4$site_no, df4$Nyear,df4$Nseason),FUN=mean)
names(avgflow)<-c("site_no","Nyear","Nseason","avgSflow")
summary(avgflow$avgSflow)
#check average seasonal average flows by season
#avgseasonalflows<-aggregate(avgflow$avgSflow,by=list(avgflow$Nseason),mean); avgseasonalflows

#### 4 - Metric 2: Dummy variable for drought or flood
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

#### 5 - Metric 3: Magnitude (Intensity/Severity) LF and HFs

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

#### 6 - Metric 4: Duration: days below .05 percentile MA-FDC or above .95 MA-FDC

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

#### 7 - Merge Metrics datasets
sdata0<-merge(avgflow,S_FDDummy,by=c("site_no", "Nyear","Nseason"))
sdata0<-merge(sdata0,minmaxflow,by=c("site_no", "Nyear","Nseason"))
sdata<-merge(sdata0,S_Duration,by=c("site_no", "Nyear","Nseason"))

head(sdata); tail(sdata); dim(sdata); str(sdata) #summary(sdata)

NNsdata<-sdata

#rename NN columns except site, Nyear and Nseason
colnames(NNsdata)[4:15] <- paste("NN", colnames(NNsdata)[4:15], sep = "_")

save(NNsdata,file="output/NNsdata.rdata")

#### Merge NN to observed metrics NNsdata to S.Flow ####
load("output/NNsdata.rdata")
load("output/RRPreds.rdata")

str(UVAobsMetrics)
str(NNsdata) 
str(RRPreds) 

##merge NN predictions and observed by site, Nyear, Nseason
ObsNNMetrics<-merge(UVAobsMetrics,NNsdata,by=c("site_no","Nyear","Nseason"))
ObsNNRRMet<-merge(ObsNNMetrics,RRPreds,by=c("site_no","Nyear","Nseason"))
dim(ObsNNRRMet);str(ObsNNRRMet)
names(ObsNNRRMet)
summary(ObsNNRRMet)

#Compare:

##7day LFs
plot(ObsNNRRMet$min7day,ObsNNRRMet$NN_min7day)
points(ObsNNRRMet$min7day,ObsNNRRMet$RR_LF7Preds,col="blue")
abline(0,1)
NSE(ObsNNRRMet$NN_min7day,ObsNNRRMet$min7day)
NSE(ObsNNRRMet$RR_LF7Preds,ObsNNRRMet$min7day)
NSE(log(ObsNNRRMet$NN_min7day),log(ObsNNRRMet$min7day)) #LNSE
NSE(log(ObsNNRRMet$RR_LF7Preds),log(ObsNNRRMet$min7day))

##3day max flows
plot(ObsNNRRMet$max3dayflow,ObsNNRRMet$NN_max3dayflow)
points(ObsNNRRMet$max3dayflow,ObsNNRRMet$RR_HF3Preds,col="blue")
abline(0,1)
NSE(ObsNNRRMet$NN_max3dayflow,ObsNNRRMet$max3dayflow)
NSE(ObsNNRRMet$RR_HF3Preds,ObsNNRRMet$max3dayflow)
NSE(log(ObsNNRRMet$NN_max3dayflow),log(ObsNNRRMet$max3dayflow)) #LNSE
NSE(log(ObsNNRRMet$RR_HF3Preds),log(ObsNNRRMet$max3dayflow))

##Seasonal flows
plot(ObsNNRRMet$avgSflow,ObsNNRRMet$NN_avgSflow)
points(ObsNNRRMet$avgSflow,ObsNNRRMet$RR_AvgPreds,col="blue")
abline(0,1)

NSE(ObsNNRRMet$NN_avgSflow,ObsNNRRMet$avgSflow)
NSE(ObsNNRRMet$RR_AvgPreds,ObsNNRRMet$avgSflow)
NSE(log(ObsNNRRMet$NN_avgSflow),log(ObsNNRRMet$avgSflow)) #LNSE
NSE(log(ObsNNRRMet$RR_AvgPreds),log(ObsNNRRMet$avgSflow))
