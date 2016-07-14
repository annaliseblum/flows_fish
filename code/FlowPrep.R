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
rec.lengths<-aggregate(daily$cfs,by=list(daily$site_no),length)
sum(rec.lengths$x/365<17) #19 of the 47 sites have not the full record length
sort(rec.lengths$x/365) # start with just the sites with at
