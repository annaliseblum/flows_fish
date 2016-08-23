##Flow data prep and explore
#Goal: predict LFs = annual 7day min flows

library(DataCombine)
library(ggplot2)

load("~/flows_fish/rawDailyData.rdata")
daily<-rawDailyData

#clean data up
daily$X_00060_00003_cd<-NULL
daily$agency_cd<-NULL
names(daily)[3]<-"cfs"

#evaluate record lengths of sites
daily$month<-format(daily$Date, "%m") #extract month variable 
daily$year<-format(daily$Date, "%Y") #extract year variable 

#remove years without all 365 days of flow data
YearTally <- aggregate(daily$cfs,by=list(daily$site_no, daily$year),FUN=length) #need to drop years without 365 days of data
names(YearTally)<-c("site_no","year","daysperyr")
sum(YearTally$daysperyr<365) #27 years with <365 flow values
df1<-data.frame(merge(daily, YearTally, by = c('site_no','year'))) #merge with flow data
df2<-df1[df1$daysperyr>364,] #remove years with less than 365 days of data
length(unique(df2$site_no)) #47 sites

#how many years of data per site?
rec.lengths<-aggregate(df2$cfs,by=list(df2$site_no),length)
names(rec.lengths)<-c("site_no","daysofflow")
df3<-data.frame(merge(df2, rec.lengths, by = c('site_no'))) #merge with flow data

#drop sites with less than 3 years of flow data
df4<-df3[df3$daysofflow>3*365,] #remove sites with less than 3 (before 15 but lost too many) years of data
length(unique(df4$site_no)) #45 sites remain!!

#create 7day rolling averages
#sort data
df4S<-df4[order(df4$site_no,as.Date(df4$Date, format="%Y-%m-%d ")),] #make sure order is correct
#df4S[1:100,]; tail(df4S) #spot check

#slide data
df4S<-slide(df4S, Var= "cfs", GroupVar= "site_no", NewVar= "cfsL1", slideBy = -1)
df4S<-slide(df4S, Var= "cfs", GroupVar= "site_no", NewVar= "cfsL2", slideBy = -2)
df4S<-slide(df4S, Var= "cfs", GroupVar= "site_no", NewVar= "cfsL3", slideBy = -3)
df4S<-slide(df4S, Var= "cfs", GroupVar= "site_no", NewVar= "cfsF1", slideBy = 1)
df4S<-slide(df4S, Var= "cfs", GroupVar= "site_no", NewVar= "cfsF2", slideBy = 2)
df4S<-slide(df4S, Var= "cfs", GroupVar= "site_no", NewVar= "cfsF3", slideBy = 3)

head(df4S);  tail(df4S) #spot check

df5<-df4S #rename
#calculate average of 3 days ahead and 3 behind around each date

df5$avg7day<-rowMeans(subset(df5, select = c("cfs","cfsL1","cfsL2","cfsL3","cfsF1","cfsF2","cfsF3")), na.rm = TRUE)
df5$avg3day<-rowMeans(subset(df5, select = c("cfs","cfsL1","cfsF1")), na.rm = TRUE)

##Kanno defines seasons as fall=Aug-Nov, winter=dec-feb, spring=march-may, summer=june-aug
df5$season[df5$month=="12"|df5$month=="01"|df5$month=="02"]<-"winter"
df5$season[df5$month=="03"|df5$month=="04"|df5$month=="05"]<-"spring"
df5$season[df5$month=="06"|df5$month=="07"|df5$month=="08"]<-"summer"
df5$season[df5$month=="09"|df5$month=="10"|df5$month=="11"]<-"fall"

df5$seasonf<-as.factor(df5$season)

##Collapse to seasonal level to find min7day and min3day flow for each season, year and site
minflow7 <- aggregate(df5$avg7day,by=list(df5$site_no, df5$year,df5$season),FUN=min)
names(minflow7)<-c("site_no","year","season","min7day")
summary(minflow7)
minflow3 <- aggregate(df5$avg3day,by=list(df5$site_no, df5$year,df5$season),FUN=min)
names(minflow3)<-c("site_no","year","season","min3day")
minflow<-merge(minflow, minflow3, by=c("site_no","year","season"))

##Collapse to monthly level to find min7day flow for each season, year and site
minflowMon7 <- aggregate(df5$avg7day,by=list(df5$site_no, df5$year,df5$month),FUN=min)
names(minflowMon7)<-c("site_no","year","month","min7day")
summary(minflowMon7)
minflowMon3 <- aggregate(df5$avg3day,by=list(df5$site_no, df5$year,df5$month),FUN=min)
names(minflowMon3)<-c("site_no","year","month","min3day")
minflowMon<-merge(minflowMon7, minflowMon3, by=c("site_no","year","month"))

##Find how many days flows are below 0.01 percentile within each season
flow.01p <- aggregate(df5$cfs,by=list(df5$site_no),FUN=quantile,probs=0.01)
names(flow.01p)<-c("site_no","cfs.01p")
#merge back into df5 to get number of days in each season with flows below
df6<-merge(df5,flow.01p,by="site_no")
df6$below.01p<-ifelse(df6$cfs.01p>=df6$cfs,1,0)

# #test to make sure it makes sense
# agg1 <- aggregate(df6$below.01p,by=list(df5$site_no),FUN=sum) #
# agg2 <- aggregate(df6$below.01p,by=list(df5$site_no),FUN=length) #,df5$month
# cbind(agg1,agg2$x*.01)

Sdays.01 <- aggregate(df6$below.01p,by=list(df5$site_no,df5$year,df5$season),FUN=sum) #
names(Sdays.01)<-c("site_no","year","season","days.01p")
  
#merge drought and min flow
lowflow<-merge(Sdays.01,minflow,by=c("site_no","year","season"))

####HIGH FLOWS
#1 day max
maxflow1 <- aggregate(df5$cfs,by=list(df5$site_no, df5$year,df5$season),FUN=max)
names(maxflow1)<-c("site_no","year","season","maxdayflow")
summary(maxflow1$maxdayflow)

#3 day max
maxflow3 <- aggregate(df5$avg3day,by=list(df5$site_no, df5$year,df5$season),FUN=max)
names(maxflow3)<-c("site_no","year","season","max3dayflow")
summary(maxflow3$max3dayflow)

#merge these
maxflow<-merge(maxflow1,maxflow3,by=c("site_no","year","season"))

# #check: all max 1 day flow must be higher than max 3 day
# check1 <- aggregate(maxflow$maxdayflow,by=list(maxflow$site_no),FUN=max)
# check2 <- aggregate(maxflow$max3dayflow,by=list(maxflow$site_no),FUN=max)
# sum(check1$x<check2$x) #0, good

#number of days above 98%ile
flow.98p <- aggregate(df5$cfs,by=list(df5$site_no),FUN=quantile,probs=0.98)
names(flow.98p)<-c("site_no","cfs.98p")
#merge back into df5 to get number of days in each season with flows below
df7<-merge(df5,flow.98p,by="site_no")
df7$above.98p<-ifelse(df7$cfs.98p<df6$cfs,1,0)

# #test to make sure it makes sense - matches even better than with low flows
# agg3 <- aggregate(df7$above.98p,by=list(df5$site_no),FUN=sum) #
# agg4 <- aggregate(df7$above.98p,by=list(df5$site_no),FUN=length) #,df5$month
# cbind(agg3,agg4$x*.02)

Sdays.98 <- aggregate(df7$cfs.98p,by=list(df5$site_no,df5$year,df5$season),FUN=sum) #
names(Sdays.98)<-c("site_no","year","season","days.98p")

#merge Sdays.98 and max flow
highflow<-merge(Sdays.98,maxflow,by=c("site_no","year","season"))

##Average seasonal flow
avgflow <- aggregate(df5$cfs,by=list(df5$site_no, df5$year,df5$season),FUN=mean)
names(avgflow)<-c("site_no","year","season","avgSflow")
summary(avgflow$avgSflow)

##merge average, min and high flow data sets
sdata0<-merge(lowflow,highflow,by=c("site_no", "year","season"))
sdata<-merge(avgflow,sdata0,by=c("site_no", "year","season"))

head(sdata)

#create fish year variable
sdata$year<-as.integer(sdata$year)
sdata$year.f <- ifelse(sdata$season=="winter"|sdata$season=="spring", sdata$year, sdata$year+1) 
sflow<-sdata
save(sflow,file="output/sflow.rdata")

#### UVA flow data prep ####
UVAstreamsSC <- read.csv("data/UVAstreamsites.csv") #import site characteristics
UVA_Discharge <- read.csv("data/SWAS_data.csv") #import discharge data

#aggregate to daily values
UVA_daily<-aggregate(UVA_Discharge$cfs,by=list(UVA_Discharge$StationID,UVA_Discharge$year,UVA_Discharge$month,UVA_Discharge$day),mean)
names(UVA_daily)<-c("UVAsite","year","month","day","daily_cfs")

save(UVA_daily,file="data/UVA_daily.rdata")
