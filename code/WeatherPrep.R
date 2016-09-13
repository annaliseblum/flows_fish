##Weather prep

DAYMET1 <- read.csv("data/daymetRecord.csv") #6 variables ,colClasses=c("character",rep("numeric",5))
str(DAYMET1) ; head(DAYMET1)

DAYMET2 <- read.csv("data/DaymetAGBSites.csv") #This one has 4 additional variables (daylight, radiation, vp and swe)
str(DAYMET2) ; head(DAYMET2)
DAYMET2s<-DAYMET2[c("site_no", "featureid","date","tmax","tmin", "prcp")]
str(DAYMET2s) ; head(DAYMET2s)

#combine datasets
names(DAYMET1); names(DAYMET2s); #match, good
DAYMET<-rbind(DAYMET1,DAYMET2s)
unique(DAYMET$site_no)

#prep variables
DAYMET$Date<-as.Date(DAYMET$date, "%Y-%m-%d")
DAYMET$site_no<-as.character(DAYMET$site_no)



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

#### Add monthly Precip, max daily precip and max 3 day precip ####
MDAYMET <- ddply(DAYMET, .(site_no, year, month), summarize, 
                 daysperseason=sum(tally),
                 totprecip = sum(prcp, na.rm = T)
)
head(MDAYMET)

##aggregate back to seasonal level, preserving monthly precips
w_MDAYMET <- dcast(MDAYMET, site_no + year ~ month ,value.var = "totprecip") #need to get wide format
names(w_MDAYMET)<-c("site_no","year","Pjan", "Pfeb", "Pmar", "Papril", "Pmay", "Pjune", "Pjuly", "Paug", "Psept", "Poct", "Pnov", "Pdec")

#find 3 day total P
#sort to make sure it's in order
sDAYMET<-DAYMET[order(DAYMET$site_no,as.Date(DAYMET$Date, format="%Y-%m-%d ")),] #make sure order is correct
#sDAYMET[1:100,]; tail(sDAYMET) #spot check

#slide data
sDAYMETS1<-slide(sDAYMET, Var= "prcp", GroupVar= "site_no", NewVar= "prcpL1", slideBy = -1)
sDAYMETS2<-slide(sDAYMETS1, Var= "prcp", GroupVar= "site_no", NewVar= "prcpF1", slideBy = 1)

#sum 3 days
sDAYMETS2$P3day<-rowSums(subset(sDAYMETS2, select = c("prcp","prcpL1","prcpF1")), na.rm = TRUE)

#aggregate down to seasonal level, selecting max sum3day precip
sP3day <- aggregate(sDAYMETS2$P3day,by=list(sDAYMETS2$site_no, sDAYMETS2$year, sDAYMETS2$season),FUN=max) #need to drop years without 365 days of data
names(sP3day)<-c("site_no","year","season","maxP3day")

##max
sP1day <- aggregate(DAYMET$prcp,by=list(DAYMET$site_no, DAYMET$year, DAYMET$season),FUN=max) #need to drop years without 365 days of data
names(sP1day)<-c("site_no","year","season","maxP1day")

### merge these datasets together: S_MDAYMET, sP3day, sP1day

sP13day<-merge(sP3day, sP1day,by=c("site_no","year","season"))
sum(sP13day$maxP1day> sP13day$maxP3day) #good, no 1 day maxes are higher than 3 day which wouldn't make sense

sPrec<-merge(sP13day,w_MDAYMET,by=c("site_no","year"))

SDAYMET<-merge(SDAYMET,sPrec,by=c("site_no","year","season"))
save(SDAYMET,file="output/SDAYMET.rdata")

#### ANNUAL for FLOW PREDICTIONS#### 
fSDAYMETM <- dcast(SDAYMET, site_no + year ~ season,value.var = "totprecip") #need to get wide format
names(fSDAYMETM)<-c("site_no","year","Pfall", "Pspring", "Psummer", "Pwinter")

fSDAYMETM2 <- dcast(SDAYMET, site_no + year ~ season,value.var = "avgtmax") #need to get wide format
names(fSDAYMETM2)<-c("site_no","year","MaxTfall", "MaxTspring", "MaxTsummer", "MaxTwinter")

fSDAYMETM3 <- dcast(SDAYMET, site_no + year ~ season,value.var = "avgtmin") #need to get wide format
names(fSDAYMETM3)<-c("site_no","year","MinTfall", "MinTspring", "MinTsummer", "MinTwinter")

sP3day_w <- dcast(sP13day, site_no + year ~ season,value.var = "maxP3day") #need to get wide format
names(sP3day_w)<-c("site_no","year","maxP3fall", "maxP3spring", "maxP3summer", "maxP3winter")

sP1day_w <- dcast(sP13day, site_no + year ~ season,value.var = "maxP1day") #need to get wide format
names(sP1day_w)<-c("site_no","year","maxP1fall", "maxP1spring", "maxP1summer", "maxP1winter")

aDAYMET<-merge(fSDAYMETM,fSDAYMETM2,by=c("site_no","year"))
aDAYMET<-merge(aDAYMET,fSDAYMETM3,by=c("site_no","year"))
aDAYMET<-merge(aDAYMET,w_MDAYMET,by=c("site_no","year"))
aDAYMET<-merge(aDAYMET,sP3day_w,by=c("site_no","year"))
aDAYMET<-merge(aDAYMET,sP1day_w,by=c("site_no","year"))

save(aDAYMET,file="output/aDAYMET.rdata")

#### ANNUAL get weather on annual level to match fish data #### 
SDAYMETM <- dcast(SDAYMET, site_no + year.f ~ season,value.var = "totprecip") #need to get wide format
names(SDAYMETM)<-c("site_no","year.f","Pfall", "Pspring", "Psummer", "Pwinter")

SDAYMETM2 <- dcast(SDAYMET, site_no + year.f ~ season,value.var = "avgtmax") #need to get wide format
names(SDAYMETM2)<-c("site_no","year.f","MaxTfall", "MaxTspring", "MaxTsummer", "MaxTwinter")

SDAYMETM3 <- dcast(SDAYMET, site_no + year.f ~ season,value.var = "avgtmin") #need to get wide format
names(SDAYMETM3)<-c("site_no","year.f","MinTfall", "MinTspring", "MinTsummer", "MinTwinter")

annual.DAYMET<-merge(SDAYMETM,SDAYMETM2,by=c("site_no","year.f"))
annual.DAYMET<-merge(annual.DAYMET,SDAYMETM3,by=c("site_no","year.f"))
annual.DAYMET1<-merge(annual.DAYMET,w_MDAYMET,by=c("site_no","year.f"))
save(annual.DAYMET,file="output/annual.DAYMET.rdata")
