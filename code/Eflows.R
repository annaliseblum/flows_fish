##Check out E-Flows package
###Annalise Blum
#Created: Oct 20,2016  Updated: Oct 20,2016
##Data sets need to run: 
##Data sets created in this file: 

## Install E-Flow package
#install.packages("EflowStats",repos=c("http://owi.usgs.gov/R","http://cran.us.r-project.org"))
library(EflowStats)

#### 1 - Learn to use EflowStats package ####
#Test E-Flows on Flow data set
# calculate stats for a USGS streamgage
sites <- c("01451800","01539000")
startdate <- "2000"
enddate <- "2010"
stats="magnifSeven,magStat,flowStat,durStat,timStat,rateStat,otherStat"
# calculate stats for USGS streamgage(s)
statsout <- ObservedStatsUSGS(sites,startdate,enddate,stats) 
#for some reaons it's doing all 191 stats not the 7 listed above 
names(statsout); View(statsout)

# save statsout to a tab-delimited file
output = "output/EflowsStatsTEST.txt"
write.table(statsout, file = output, col.names = TRUE,
            row.names = FALSE, quote = FALSE, sep = "\t")

# Load sample data included with package:
daily_data<-dailyData1
#This dataframe has two columns, the first named date, containing dates as characters in the format
#"YYYY-MM-DD" and the second column, discharge, containing the numeric discharge

#Get my UVA sites
load("output/UVA_daily.rdata"); load("output/UVA_BC.rdata")
head(UVA_daily); UVA_BC # UVA_NFDR has DA 2.35km2
dailyData1<-UVA_daily[UVA_daily$site_no=="NFDR",]
dailyDataNFDR<- dailyData1[c("Date","cfs")]; head(dailyDataNFDR)
names(dailyDataNFDR)<-c("date","discharge")

# calculate stats for data from your own data file
drain_area=UVA_BC$DA_SQKM[UVA_BC$site_no=="UVA_NFDR"]
site_id="NFDR"
daily_data<-dailyDataNFDR
stats="magnifSeven,magStat,flowStat,durStat,timStat,rateStat,otherStat"
statsout <- ObservedStatsOther(daily_data,drain_area,site_id,stats)

#The user may wish to generate statistics for a group of sites with locally-stored data p 5

# plot monthly means for a daily discharge timeseries
qfiletempf<-sampleData
meanmonts<-monthlyMeanTs(qfiletempf)
plotMonthlyMean(meanmonts,'02178400')

#to compare sites
# NWIS-local
sites <- c("02186000","02192000","02219000","02317500","02329600")
startDt <- "1990"
endDt <- "1999"
stats="magnifSeven,magStat,flowStat,durStat,timStat,rateStat,otherStat"
dataPath="C:/Users/jlthomps/Documents/R/JData/modeled/" #SET THIS
DiffStats <- CompareStats(stats,sites=sites,dataPath=dataPath,
                          startDt=startDt,endDt=endDt)
stats1 <- DiffStats[[1]]
stats2 <- DiffStats[[2]]
Diffstats <- DiffStats[[3]]
RegGoFstats <- DiffStats[[4]]
GoFstats <- DiffStats[[5]]

#can also compare stats for the same site but different time periods
# NWIS-NWIS
sites <- c("02186000","02192000","02219000","02317500","02329600")
startDt <- "1990"
endDt <- "1999"
startDt2 <- "2000"
endDt2 <- "2008"
stats="magnifSeven,magStat,flowStat,durStat,timStat,rateStat,otherStat"
DiffStats <- CompareStats(stats,sites=sites,startDt=startDt,
                          endDt=endDt,startDt2=startDt2,endDt2=endDt2)




#### 2 ####
