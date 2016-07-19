##Weather prep

DAYMET <- read.csv("data/daymetRecord.csv") #6 variables ,colClasses=c("character",rep("numeric",5))
str(DAYMET) ; head(DAYMET)
DAYMET$Date<-as.Date(DAYMET$date, "%Y-%m-%d")
DAYMET$site_no<-as.character(DAYMET$site_no)
unique(DAYMET$site_no)

DAYMET$month<-format(DAYMET$Date, "%m") #extract month variable 
DAYMET$year<-format(DAYMET$Date, "%Y") #extract year variable 
DAYMET$season[DAYMET$month=="12"|DAYMET$month=="01"|DAYMET$month=="02"]<-"winter"
DAYMET$season[DAYMET$month=="03"|DAYMET$month=="04"|DAYMET$month=="05"]<-"spring"
DAYMET$season[DAYMET$month=="06"|DAYMET$month=="07"|DAYMET$month=="08"]<-"summer"
DAYMET$season[DAYMET$month=="09"|DAYMET$month=="10"|DAYMET$month=="11"]<-"fall"

#collapse to seasonal level
DAYMET$tally<-1
SDAYMET <- ddply(DAYMET, .(site_no, year,season), summarize, 
                daysperseason=sum(tally),
                totprecip = sum(prcp, na.rm = T),
                avgtmax = mean(tmax, na.rm = T),
                avgtmin = mean(tmin, na.rm = T)
)

head(SDAYMET)
save(SDAYMET,file="output/SDAYMET.rdata")

#need to get weather data for year starting the summer before sampling
#so each fall and summer temps need to match with sampling a year later (year+1)
SDAYMET$year<-as.integer(SDAYMET$year)
SDAYMET$year.f <- ifelse(SDAYMET$season=="winter"|SDAYMET$season=="spring", SDAYMET$year, SDAYMET$year+1) 

#get weather on annual level to match fish data #how to do this better??
SDAYMETM <- dcast(SDAYMET, site_no + year.f ~ season,value.var = "totprecip") #need to get wide format
names(SDAYMETM)<-c("site_no","year.f","Pfall", "Pspring", "Psummer", "Pwinter")

SDAYMETM2 <- dcast(SDAYMET, site_no + year.f ~ season,value.var = "avgtmax") #need to get wide format
names(SDAYMETM2)<-c("site_no","year.f","MaxTfall", "MaxTspring", "MaxTsummer", "MaxTwinter")

SDAYMETM3 <- dcast(SDAYMET, site_no + year.f ~ season,value.var = "avgtmin") #need to get wide format
names(SDAYMETM3)<-c("site_no","year.f","MinTfall", "MinTspring", "MinTsummer", "MinTwinter")

annual.DAYMET<-merge(SDAYMETM,SDAYMETM2,by=c("site_no","year.f"))
annual.DAYMET<-merge(annual.DAYMET,SDAYMETM3,by=c("site_no","year.f"))
save(annual.DAYMET,file="output/annual.DAYMET.rdata")
