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

#add F_ back to the begining of the site numbers - CANT GET THIS TO WORK
#test<-ifelse(nchar(DAYMET$site_no==8),paste(DAYMET$site_no),paste("F_",DAYMET$site_no,sep=""))

#collapse to seasonal level
DAYMET$tally<-1
SDAYMET <- ddply(DAYMET, .(site_no, year,season), summarize, 
                daysperseason=sum(tally),
                totprecip = sum(prcp, na.rm = T),
                avgtmax = mean(tmax, na.rm = T),
                avgtmin = mean(tmin, na.rm = T)
)

head(SDAYMET)

##Lagged seasonal weather: 1 year of lagged P and T
#first create season variable that is a factor and numbers in chronological order
SDAYMET$season.f<- as.factor(ifelse(SDAYMET$season=="winter",1,ifelse(SDAYMET$season=="spring",2,
                                                                      ifelse(SDAYMET$season=="summer",3,4))))

#order by site, year, season
slide0<-SDAYMET[order(SDAYMET$site_no,SDAYMET$year,SDAYMET$season.f),] #make sure order is correct
head(slide0);tail(slide0)
#slide data
slide1<-slide(slide0, Var= "totprecip", GroupVar= "site_no", NewVar= "L1totprecip", slideBy = -1)
slide2<-slide(slide1, Var= "totprecip", GroupVar= "site_no", NewVar= "L2totprecip", slideBy = -2)
slide3<-slide(slide2, Var= "totprecip", GroupVar= "site_no", NewVar= "L3totprecip", slideBy = -3)
slide4<-slide(slide3, Var= "totprecip", GroupVar= "site_no", NewVar= "L4totprecip", slideBy = -4)

slide5<-slide(slide4, Var= "avgtmax", GroupVar= "site_no", NewVar= "L1avgtmax", slideBy = -1)
slide6<-slide(slide5, Var= "avgtmax", GroupVar= "site_no", NewVar= "L2avgtmax", slideBy = -2)
slide7<-slide(slide6, Var= "avgtmax", GroupVar= "site_no", NewVar= "L3avgtmax", slideBy = -3)
slide8<-slide(slide7, Var= "avgtmax", GroupVar= "site_no", NewVar= "L4avgtmax", slideBy = -4)

head(slide8); tail(slide8)
SDAYMET<-slide8

#need to get weather data for year starting the summer before sampling
#so each fall and summer temps need to match with sampling a year later (year+1)
SDAYMET$year<-as.integer(SDAYMET$year)
SDAYMET$year.f <- ifelse(SDAYMET$season=="winter"|SDAYMET$season=="spring", SDAYMET$year, SDAYMET$year+1) 

save(SDAYMET,file="output/SDAYMET.rdata")

#get weather on annual level to match fish data
SDAYMETM <- dcast(SDAYMET, site_no + year.f ~ season,value.var = "totprecip") #need to get wide format
names(SDAYMETM)<-c("site_no","year.f","Pfall", "Pspring", "Psummer", "Pwinter")

SDAYMETM2 <- dcast(SDAYMET, site_no + year.f ~ season,value.var = "avgtmax") #need to get wide format
names(SDAYMETM2)<-c("site_no","year.f","MaxTfall", "MaxTspring", "MaxTsummer", "MaxTwinter")

SDAYMETM3 <- dcast(SDAYMET, site_no + year.f ~ season,value.var = "avgtmin") #need to get wide format
names(SDAYMETM3)<-c("site_no","year.f","MinTfall", "MinTspring", "MinTsummer", "MinTwinter")

annual.DAYMET<-merge(SDAYMETM,SDAYMETM2,by=c("site_no","year.f"))
annual.DAYMET<-merge(annual.DAYMET,SDAYMETM3,by=c("site_no","year.f"))
save(annual.DAYMET,file="output/annual.DAYMET.rdata")
