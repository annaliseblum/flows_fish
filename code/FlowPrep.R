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

#drop sites with less than 15 years of flow data
df4<-df3[df3$daysofflow>15*365,] #remove sites with less than 15 years of data
length(unique(df4$site_no)) #29 sites remain

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

##Kanno defines seasons as fall=Aug-Nov, winter=dec-feb, spring=march-may, summer=june-aug
df5$season[df5$month=="12"|df5$month=="01"|df5$month=="02"]<-"winter"
df5$season[df5$month=="03"|df5$month=="04"|df5$month=="05"]<-"spring"
df5$season[df5$month=="06"|df5$month=="07"|df5$month=="08"]<-"summer"
df5$season[df5$month=="09"|df5$month=="10"|df5$month=="11"]<-"fall"

df5$seasonf<-as.factor(df5$season)

##Collapse to seasonal level to find min7day flow for each season, year and site
minflow <- aggregate(df5$avg7day,by=list(df5$site_no, df5$year,df5$season),FUN=min)
names(minflow)<-c("site_no","year","season","min7day")
summary(minflow$min7day)

#define drought - MAKE SURE THIS IS CORRECT
drought1 <- aggregate(df5$avg7day,by=list(df5$site_no,df5$season),FUN=quantile,probs=0.01)
names(drought1)<-c("site_no","season","drough.01ps")
summary(drought1$drough.01ps)

#merge drought and min flow
lowflow<-merge(drought1,minflow,by=c("site_no","season"))
sum(lowflow$drough.01ps>lowflow$min7day) #40% of seasons have a "flood"
lowflow$drought<-ifelse(lowflow$drough.01ps>lowflow$min7day,1,0) #13.6% of seasons have a "drought"

#define flood - MAKE SURE THIS IS CORRECT
flood1 <- aggregate(df5$cfs,by=list(df5$site_no,df5$season),FUN=quantile,probs=.99) #100 year flood?"
names(flood1)<-c("site_no","season","flood.99ps")
summary(flood1$flood.99ps)

maxflow <- aggregate(df5$cfs,by=list(df5$site_no, df5$year,df5$season),FUN=max)
names(maxflow)<-c("site_no","year","season","maxdayflow")
summary(maxflow$maxdayflow)

#merge flood1 and max flow
highflow<-merge(flood1,maxflow,by=c("site_no","season"))
sum(highflow$flood.99ps<highflow$maxdayflow) #40% of seasons have a "flood"
highflow$flood<-ifelse(highflow$flood.99ps<highflow$maxdayflow,1,0) #40% of seasons have a "flood"

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

##collapse by year and season across sites
avgFlowSites <- ddply(sflow, .(year.f,season), summarize, 
                  Avgdrought=mean(drought),
                  Avgflood=mean(flood)
                  #, avgSflow=mean(avgSflow) #need to make this per DA at the least
)

droughtS <- dcast(avgFlowSites, year.f ~ season,value.var = "Avgdrought")
names(droughtS)<-c("year.f","Drfall", "Drspring", "Drsummer", "Drwinter")

FloodS <- dcast(avgFlowSites, year.f ~ season,value.var = "Avgflood")
names(FloodS)<-c("year.f","Flfall", "Flspring", "Flsummer", "Flwinter")

Extremes<-merge(droughtS,FloodS,by="year.f")

##look at weather before floods and droughts to understand relationship at these sites


##Collapse to ANNUAL level to get which seasonal min7day is the minimum among the seasons
aflow <- aggregate(sflow$min7day,by=list(sflow$site_no, sflow$year),FUN=min)
names(aflow)<-c("site_no","year","min7day")
Aflow<-merge(aflow,sflow,by=c("site_no","year","min7day"))
  