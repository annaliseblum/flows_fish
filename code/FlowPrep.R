##Flow data prep and explore
#Goal: predict LFs = annual 7day min flows

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

#map these sites
plot(df4$site_no, df4$year)

#create 7day rolling averages
#sort data
dailydata0<-dailydata[order(dailydata$site_no,as.Date(dailydata$Date, format="%Y-%m-%d ")),] #make sure order is correct
#dailydata[1:100,]; dailydata[600:900,] #spot check

#slide data
dailydata1<-slide(dailydata0, Var= "Flow", GroupVar= "site_no", NewVar= "FlowL1", slideBy = -1)
dailydata2<-slide(dailydata1, Var= "Flow", GroupVar= "site_no", NewVar= "FlowL2", slideBy = -2)
dailydata3<-slide(dailydata2, Var= "Flow", GroupVar= "site_no", NewVar= "FlowL3", slideBy = -3)
dailydata4<-slide(dailydata3, Var= "Flow", GroupVar= "site_no", NewVar= "FlowF1", slideBy = 1)
dailydata5<-slide(dailydata4, Var= "Flow", GroupVar= "site_no", NewVar= "FlowF2", slideBy = 2)
dailydata6<-slide(dailydata5, Var= "Flow", GroupVar= "site_no", NewVar= "FlowF3", slideBy = 3)

head(dailydata6);  tail(dailydata6) #spot check

#calculate average of 3 days ahead and 3 behind around each date

my_df$RA7day<-rowMeans(subset(dailydata6, select = c("Flow","FlowL1","FlowL2","FlowL3","FlowF1","FlowF2","FlowF3")), na.rm = TRUE)
