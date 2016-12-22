##Weather prep: import and clean DAYMET datasets
###Impact of Extreme Streamflows on Brook Trout Young-of-Year Abundance
### Annalise G Blum
##Created:July 18, 2016, last modified: Sept 28,2016
##Data sets created in this file: "output/SDAYMET.rdata"; "output/aDAYMET.rdata"

library(DataCombine)
##Need to keep everything consistent as "fish years" which are june - may 
#years are preceeding fish counts (summer and fall obs are used to predict next year's YOY)

load("output/DAYMET.rdata")
length(unique(DAYMET$site_no)) #173=53 USGS + 5 UVA + 115 Fish

#but only need for fish sites now
DAYMET$type<-ifelse(nchar(DAYMET$site_no)>7,"gage","fish") #both USGS and UVA have length 8, longest fish is 7
DAYMETf<-DAYMET[DAYMET$type=="fish",]
DAYMET<-DAYMETf #rename for ease of calculations below

#### 1 - Prep and collapse to Seasonal level #### 
names(DAYMET); head(DAYMET); tail(DAYMET); str(DAYMET)
DAYMET$site_no<-as.character(DAYMET$site_no)

DAYMET$month<-as.integer(format(DAYMET$Date, "%m")) #extract month variable 
DAYMET$year<-as.integer(format(DAYMET$Date, "%Y")) #extract year variable 

#create New year variable and drop weather data that won't be used
DAYMET$Nyear<-ifelse(DAYMET$month<6,DAYMET$year-1981,DAYMET$year-1980) #to get years 1-29
DAYMET<-DAYMET[DAYMET$Nyear<30,] #these are months 06-12 for 2010 but no fish data in summer 2011, so drop

DAYMET$season[DAYMET$month==12|DAYMET$month==01|DAYMET$month==02]<-"winter"
DAYMET$season[DAYMET$month==03|DAYMET$month==04|DAYMET$month==05]<-"spring"
DAYMET$season[DAYMET$month==06|DAYMET$month==07|DAYMET$month==08]<-"summer"
DAYMET$season[DAYMET$month==09|DAYMET$month==10|DAYMET$month==11]<-"fall"

#create a numeric season variable 1-4 starting with summer as 1
DAYMET$Nseason[DAYMET$season=="summer"]<- 1
DAYMET$Nseason[DAYMET$season=="fall"]<-2
DAYMET$Nseason[DAYMET$season=="winter"]<-3
DAYMET$Nseason[DAYMET$season=="spring"]<-4

#create an average daily temperature variable
DAYMET$tavg<-(DAYMET$tmax+DAYMET$tmin)/2

#make sure sorted to enable slides and 7day averages
DAYMET.s<-DAYMET[order(DAYMET$site_no,DAYMET$Date),] #make sure order is correct
#DAYMET.s[1:100,]; tail(DAYMET.s); DAYMET.s[1050:1200,]#spot check

##add rolling average 7-day max temperature value
#slide data forward and back 3 day to get 7 day totals around each day
sortDAYMETS<-slide(DAYMET.s, Var= "tmax", GroupVar= "site_no", NewVar= "tmaxL1", slideBy = -1)
sortDAYMETS<-slide(sortDAYMETS, Var= "tmax", GroupVar= "site_no", NewVar= "tmaxL2", slideBy = -2)
sortDAYMETS<-slide(sortDAYMETS, Var= "tmax", GroupVar= "site_no", NewVar= "tmaxL3", slideBy = -3)

sortDAYMETS<-slide(sortDAYMETS, Var= "tmax", GroupVar= "site_no", NewVar= "tmaxF1", slideBy = 1)
sortDAYMETS<-slide(sortDAYMETS, Var= "tmax", GroupVar= "site_no", NewVar= "tmaxF2", slideBy = 2)
sortDAYMETS<-slide(sortDAYMETS, Var= "tmax", GroupVar= "site_no", NewVar= "tmaxF3", slideBy = 3)

##add rolling average 7-day precip value

sortDAYMETS<-slide(sortDAYMETS, Var= "prcp", GroupVar= "site_no", NewVar= "prcpL1", slideBy = -1)
sortDAYMETS<-slide(sortDAYMETS, Var= "prcp", GroupVar= "site_no", NewVar= "prcpL2", slideBy = -2)
sortDAYMETS<-slide(sortDAYMETS, Var= "prcp", GroupVar= "site_no", NewVar= "prcpL3", slideBy = -3)

sortDAYMETS<-slide(sortDAYMETS, Var= "prcp", GroupVar= "site_no", NewVar= "prcpF1", slideBy = 1)
sortDAYMETS<-slide(sortDAYMETS, Var= "prcp", GroupVar= "site_no", NewVar= "prcpF2", slideBy = 2)
sortDAYMETS<-slide(sortDAYMETS, Var= "prcp", GroupVar= "site_no", NewVar= "prcpF3", slideBy = 3)

slidDAYMETS<-sortDAYMETS

#number of consecutive days with P< 1 mm and P <=1mm
slidDAYMETS$ZeroPrec<-(slidDAYMETS$prcp==0)
slidDAYMETS$U1Prec<-(slidDAYMETS$prcp<2)
# sum(slidDAYMETS$ZeroPrec)/length(slidDAYMETS$ZeroPrec) #0.695 of days have zero precip
# sum(slidDAYMETS$U1Prec)/length(slidDAYMETS$ZeroPrec) #7157 of days have 1 mm or less precip


#from http://stackoverflow.com/questions/5012516/count-how-many-consecutive-values-are-true
cumul_zeros <- function(x)  {
  x <- !x
  rl <- rle(x)
  len <- rl$lengths
  v <- rl$values
  cumLen <- cumsum(len)
  z <- x
  # replace the 0 at the end of each zero-block in z by the 
  # negative of the length of the preceding 1-block....
  iDrops <- c(0, diff(v)) < 0
  z[ cumLen[ iDrops ] ] <- -len[ c(iDrops[-1],FALSE) ]
  # ... to ensure that the cumsum below does the right thing.
  # We zap the cumsum with x so only the cumsums for the 1-blocks survive:
  x*cumsum(z)
}

slidDAYMETS$cum0prcp<-cumul_zeros(slidDAYMETS$prcp)
slidDAYMETS$cumU1prcp<-cumul_zeros(as.numeric((slidDAYMETS$prcp>1)))

# #was going to use find 5%ile flow by site, found have to just use non-zero precip days (or else 5%ile is 0 for all sites)
# DAYMETnon0prp<-slidDAYMETS[slidDAYMETS$prcp>0,]
# prcp.05<-aggregate(DAYMETnon0prp$prcp,by=list(DAYMETnon0prp$site_no),FUN=quantile,probs=0.05,type=6)
# names(prcp.05)<-c("site_no","nonzero5pPrec") #but most are 1 mm

#collapse to seasonal level
slidDAYMETS$tally<-1
SDAYMET <- ddply(slidDAYMETS, .(site_no, Nyear,season), summarize, 
                daysperseason=sum(tally),
                totprecip = sum(prcp, na.rm = T),
                avgtmax = mean(tmax, na.rm = T),
                avgtmin = mean(tmin, na.rm = T),
                avgtavg = mean(tavg, na.rm = T),
                Nseason = mean(Nseason, na.rm = T),
                maxP1day = max(prcp, na.rm = T),
                maxcons0p = max(cum0prcp, na.rm = T), #longest consecutive days 0 precip - change to under 1?
                maxconsU1p = max(cumU1prcp, na.rm = T), #longest consecutive days 1 mm or less precip
                year = max(year, na.rm = T) #to check - different years within seasons so this is chosing the year that goes with dec
)

#head(SDAYMET);tail(SDAYMET); str(SDAYMET); summary(SDAYMET)

save(SDAYMET,file="output/SDAYMET.rdata")

#### 2- Add monthly Precip, max daily precip and max 3 day precip ####
MDAYMET <- ddply(DAYMET, .(site_no, Nyear, month), summarize, 
                 dayspermonth=sum(tally),
                 totprecip = sum(prcp, na.rm = T))
head(MDAYMET)

##aggregate back to seasonal level, preserving monthly precips
w_MDAYMET <- dcast(MDAYMET, site_no + Nyear ~ month ,value.var = "totprecip") #need to get wide format
names(w_MDAYMET) #check this
names(w_MDAYMET)<-c("site_no","Nyear","Pjan", "Pfeb", "Pmar", "Papril", "Pmay", "Pjune", "Pjuly", "Paug", "Psept",
                    "Poct", "Pnov", "Pdec")
#reorder in fish year order
w_MDAYMET<-w_MDAYMET[c("site_no","Nyear","Pjune", "Pjuly", "Paug", "Psept",
                       "Poct", "Pnov", "Pdec","Pjan", "Pfeb", "Pmar", "Papril", "Pmay")]

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
sP3day <- aggregate(sortDAYMETS2$P3day,by=list(sortDAYMETS2$site_no, sortDAYMETS2$Nyear, sortDAYMETS2$Nseason),FUN=max)
names(sP3day)<-c("site_no","Nyear","Nseason","maxP3day")

##max
sP1day <- aggregate(DAYMET$prcp,by=list(DAYMET$site_no, DAYMET$Nyear, DAYMET$Nseason),FUN=max)
names(sP1day)<-c("site_no","Nyear","Nseason","maxP1day")

### merge these datasets together to have a seasonal data set with monthly precip sums, 3 day max precip
sP13day<-merge(sP3day, sP1day,by=c("site_no","Nyear","Nseason"))
#sum(sP13day$maxP1day> sP13day$maxP3day) #good, no 1 day maxes are higher than 3 day which wouldn't make sense

sPrec<-merge(sP13day,w_MDAYMET,by=c("site_no","Nyear"))

SDAYMET<-merge(SDAYMET,sPrec,by=c("site_no","Nyear","Nseason"))

#add 0.01 to Precip vars with zeros: Pmar, Psept, Poct
#summary(SDAYMET)
SDAYMET$Pmar[SDAYMET$Pmar==0]<- .01
SDAYMET$Psept[SDAYMET$Psept==0]<- .01
SDAYMET$Poct[SDAYMET$Poct==0]<- .01

sum(SDAYMET$avgtmax<0,na.rm=T) #11
sum(SDAYMET$avgtmaxL1<0,na.rm=T) #11
sum(SDAYMET$avgtmaxL2<0,na.rm=T) #8

SDAYMET$avgtmax.T<- SDAYMET$avgtmax+3
SDAYMET$avgtmaxL1.T<- SDAYMET$avgtmaxL1+3
SDAYMET$avgtmaxL2.T<- SDAYMET$avgtmaxL2+3

save(SDAYMET,file="output/SDAYMET.rdata")

#### 3 - Collapse to annual level for both flow and fish predictions - need to be using "fish" year #### 
#load("output/SDAYMET.rdata")
fSDAYMETM <- dcast(SDAYMET, site_no + Nyear ~ Nseason,value.var = "totprecip") #need to get wide format
names(fSDAYMETM)
names(fSDAYMETM)<-c("site_no","Nyear","Psummer","Pfall","Pwinter","Pspring")

fSDAYMETM2 <- dcast(SDAYMET, site_no + Nyear ~ Nseason,value.var = "avgtmax") #need to get wide format
names(fSDAYMETM2)<-c("site_no","Nyear", "MaxTsummer","MaxTfall", "MaxTwinter","MaxTspring")

fSDAYMETM3 <- dcast(SDAYMET, site_no + Nyear ~ Nseason,value.var = "avgtmin") #need to get wide format
names(fSDAYMETM3)<-c("site_no","Nyear","MinTsummer", "MinTfall", "MinTwinter", "MinTspring")

sP3day_w <- dcast(SDAYMET, site_no + Nyear ~ Nseason,value.var = "maxP3day") #need to get wide format
names(sP3day_w)<-c("site_no","Nyear","maxP3summer","maxP3fall","maxP3winter","maxP3spring")

sP1day_w <- dcast(SDAYMET, site_no + Nyear ~ Nseason,value.var = "maxP1day") #need to get wide format
names(sP1day_w)<-c("site_no","Nyear","maxP1summer", "maxP1fall","maxP1winter","maxP1spring")

#1 year lagged Precip
laggedPrecip <- aggregate(SDAYMET$totprecip,by=list(SDAYMET$site_no, SDAYMET$Nyear),FUN=sum) #sum each FISH year precip
names(laggedPrecip)<-c("site_no","Nyear","totAnnPrecip") #name vars
str(laggedPrecip)
sortlaggedPrecip<-laggedPrecip[order(laggedPrecip$site_no,laggedPrecip$Nyear),] #check for missing years! #DEAL WITH??
#sortlaggedPrecip[1:100,]; tail(sortlaggedPrecip) #spot check

#slide data forward and back 1 day to get 3 day totals around each day
laggedPreciplagged<-slide(sortlaggedPrecip, Var= "totAnnPrecip", GroupVar= "site_no", NewVar= "L1AnnPrec", slideBy = -1)
#head(laggedPreciplagged);tail(laggedPreciplagged)



#merge all these data sets together
aDAYMET<-merge(fSDAYMETM,fSDAYMETM2,by=c("site_no","Nyear"))
aDAYMET<-merge(aDAYMET,fSDAYMETM3,by=c("site_no","Nyear"))
aDAYMET<-merge(aDAYMET,w_MDAYMET,by=c("site_no","Nyear"))
aDAYMET<-merge(aDAYMET,sP3day_w,by=c("site_no","Nyear"))
aDAYMET<-merge(aDAYMET,sP1day_w,by=c("site_no","Nyear"))
aDAYMET<-merge(aDAYMET,laggedPreciplagged,by=c("site_no","Nyear"))

aDAYMET<-aDAYMET[order(aDAYMET$site_no,aDAYMET$Nyear),] #check for missing years! #DEAL WITH??

save(aDAYMET,file="output/aDAYMET.rdata")

# #### THIS SEEMS OLD?? ANNUAL get weather on annual level to match fish data #### 
# SDAYMETM <- dcast(SDAYMET, site_no + Nyear ~ season,value.var = "totprecip") #need to get wide format
# names(SDAYMETM)<-c("site_no","Nyear","Pfall", "Pspring", "Psummer", "Pwinter")
# 
# SDAYMETM2 <- dcast(SDAYMET, site_no + Nyear ~ season,value.var = "avgtmax") #need to get wide format
# names(SDAYMETM2)<-c("site_no","Nyear","MaxTfall", "MaxTspring", "MaxTsummer", "MaxTwinter")
# 
# SDAYMETM3 <- dcast(SDAYMET, site_no + Nyear ~ season,value.var = "avgtmin") #need to get wide format
# names(SDAYMETM3)<-c("site_no","Nyear","MinTfall", "MinTspring", "MinTsummer", "MinTwinter")
# 
# annual.DAYMET<-merge(SDAYMETM,SDAYMETM2,by=c("site_no","Nyear"))
# annual.DAYMET<-merge(annual.DAYMET,SDAYMETM3,by=c("site_no","Nyear"))
# annual.DAYMET1<-merge(annual.DAYMET,w_MDAYMET,by=c("site_no","Nyear"))
# save(annual.DAYMET,file="output/annual.DAYMET.rdata")
# 
# ##ALSO THIS??
# ##Lagged seasonal weather: 1 year of lagged P and T - AM I ACTUALLY USING THIS?
# #first create season variable that is a factor and numbers in chronological order
# SDAYMET$seasonf<- as.factor(ifelse(SDAYMET$season=="winter",1,ifelse(SDAYMET$season=="spring",2,
#                                                                      ifelse(SDAYMET$season=="summer",3,4))))
# 
# #order by site, FISH year, season
# slide0<-SDAYMET[order(SDAYMET$site_no,SDAYMET$year,SDAYMET$seasonf),] #make sure order is correct (of seasons...)
# head(slide0);tail(slide0)
# #slide data
# slide1<-slide(slide0, Var= "totprecip", GroupVar= "site_no", NewVar= "L1totprecip", slideBy = -1)
# slide2<-slide(slide1, Var= "totprecip", GroupVar= "site_no", NewVar= "L2totprecip", slideBy = -2)
# slide3<-slide(slide2, Var= "totprecip", GroupVar= "site_no", NewVar= "L3totprecip", slideBy = -3)
# slide4<-slide(slide3, Var= "totprecip", GroupVar= "site_no", NewVar= "L4totprecip", slideBy = -4)
# 
# slide5<-slide(slide4, Var= "avgtmax", GroupVar= "site_no", NewVar= "L1avgtmax", slideBy = -1)
# slide6<-slide(slide5, Var= "avgtmax", GroupVar= "site_no", NewVar= "L2avgtmax", slideBy = -2)
# slide7<-slide(slide6, Var= "avgtmax", GroupVar= "site_no", NewVar= "L3avgtmax", slideBy = -3)
# slide8<-slide(slide7, Var= "avgtmax", GroupVar= "site_no", NewVar= "L4avgtmax", slideBy = -4)
# 
# head(slide8); tail(slide8)
# SDAYMET<-slide8
