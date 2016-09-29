##Weather prep: import and clean DAYMET datasets
###Impact of Extreme Streamflows on Brook Trout Young-of-Year Abundance
### Annalise G Blum
##Created:July 18, 2016, last modified: Sept 28,2016

#daymetRecord.csv is 1994-2010 for 53 USGS and 34 fish sites
#DaymetAGBSites.csv is 1982-1993 for 53 USGS and 34 fish sites

DAYMET1 <- read.csv("data/daymetRecord.csv") #6 variables ,colClasses=c("character",rep("numeric",5))
str(DAYMET1) ; head(DAYMET1); tail(DAYMET1)
DAYMET2 <- read.csv("data/DaymetAGBSites.csv") #This one has 4 additional variables (daylight, radiation, vp and swe)
str(DAYMET2) ; head(DAYMET2)
DAYMET2s<-DAYMET2[c("site_no", "featureid","date","tmax","tmin", "prcp")]
str(DAYMET2s) ; head(DAYMET2s); tail(DAYMET2s)

DAYMETpart1<-rbind(DAYMET1,DAYMET2s)
str(DAYMETpart1) ; head(DAYMETpart1); tail(DAYMETpart1)

#create date variable in date format for these sites its: %Y-%m-%d
DAYMETpart1$Date<-as.Date(DAYMETpart1$date, "%Y-%m-%d")
DAYMETpart1<-DAYMETpart1[c("site_no", "featureid","Date","tmax","tmin", "prcp")] #just variables I'm using

##Additional Fish sites: NEWAGBSites - had to break it up into 4 chunks
DAYMET86_1 <- read.csv("data/Daymet86_1.csv")
str(DAYMET86_1) ; head(DAYMET86_1); tail(DAYMET86_1)
DAYMET86_2 <- read.csv("data/Daymet86_2.csv")
str(DAYMET86_2)
DAYMET86_3 <- read.csv("data/Daymet86_3.csv")
str(DAYMET86_3)
DAYMET86_4 <- read.csv("data/Daymet86_4.csv")
str(DAYMET86_4) ; head(DAYMET86_4); tail(DAYMET86_4)

DAYMET86<-rbind(DAYMET86_1,DAYMET86_2,DAYMET86_3,DAYMET86_4) #rbind these back into one dataset

#create date variable in date format for these sites its: %m/%d/%y
DAYMET86$Date<-as.Date(DAYMET86$date, "%m/%d/%y")
DAYMET86s<-DAYMET86[c("site_no", "featureid","Date","tmax","tmin", "prcp")] #just variables I'm using

#combine datasets
names(DAYMETpart1); names(DAYMET86s) #match, good
DAYMET<-rbind(DAYMETpart1,DAYMET86s)
unique(DAYMET$site_no) #173 sites: 115 fish + 5 in SNP +53 USGS = 173
save(DAYMET,file="output/DAYMET.rdata")

#prep variables
names(DAYMET); head(DAYMET); tail(DAYMET); str(DAYMET)
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

head(SDAYMET);tail(SDAYMET); str(SDAYMET)

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

#find 3 day total Precip
#sort to make sure it's in order
sortDAYMET<-DAYMET[order(DAYMET$site_no,as.Date(DAYMET$Date, format="%Y-%m-%d ")),] #make sure order is correct
#sDAYMET[1:100,]; tail(sDAYMET) #spot check

#slide data forward and back 1 day to get 3 day totals around each day
sortDAYMETS1<-slide(sortDAYMET, Var= "prcp", GroupVar= "site_no", NewVar= "prcpL1", slideBy = -1)
sortDAYMETS2<-slide(sortDAYMETS1, Var= "prcp", GroupVar= "site_no", NewVar= "prcpF1", slideBy = 1)

#sum 3 days
sortDAYMETS2$P3day<-rowSums(subset(sortDAYMETS2, select = c("prcp","prcpL1","prcpF1")), na.rm = TRUE)

#aggregate down to seasonal level, selecting max sum3day precip
sP3day <- aggregate(sortDAYMETS2$P3day,by=list(sortDAYMETS2$site_no, sortDAYMETS2$year, sortDAYMETS2$season),FUN=max)
names(sP3day)<-c("site_no","year","season","maxP3day")

##max
sP1day <- aggregate(DAYMET$prcp,by=list(DAYMET$site_no, DAYMET$year, DAYMET$season),FUN=max)
names(sP1day)<-c("site_no","year","season","maxP1day")

### merge these datasets together to have a seasonal data set with monthly precip sums, 3 day max precip

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

#1 year lagged Precip - NEED TO USE FISH YEAR!
laggedPrecip <- aggregate(SDAYMET$totprecip,by=list(SDAYMET$site_no, SDAYMET$year),FUN=sum) #sum each year precip
names(laggedPrecip)<-c("site_no","year","totAnnPrecip") #name vars
str(laggedPrecip)
sortlaggedPrecip<-laggedPrecip[order(laggedPrecip$site_no,laggedPrecip$year),] #make sure order is correct; could be missing years!! DEAL WITH??
#sortlaggedPrecip[1:100,]; tail(sortlaggedPrecip) #spot check

#slide data forward and back 1 day to get 3 day totals around each day
laggedPreciplagged<-slide(sortlaggedPrecip, Var= "totAnnPrecip", GroupVar= "site_no", NewVar= "L1AnnPrec", slideBy = -1)
head(laggedPreciplagged);tail(laggedPreciplagged)
#sort data
#lag by one year

aDAYMET<-merge(fSDAYMETM,fSDAYMETM2,by=c("site_no","year"))
aDAYMET<-merge(aDAYMET,fSDAYMETM3,by=c("site_no","year"))
aDAYMET<-merge(aDAYMET,w_MDAYMET,by=c("site_no","year"))
aDAYMET<-merge(aDAYMET,sP3day_w,by=c("site_no","year"))
aDAYMET<-merge(aDAYMET,sP1day_w,by=c("site_no","year"))
aDAYMET<-merge(aDAYMET,laggedPreciplagged,by=c("site_no","year"))

save(aDAYMET,file="output/aDAYMET.rdata")

#### THIS SEEMS OLD?? ANNUAL get weather on annual level to match fish data #### 
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
