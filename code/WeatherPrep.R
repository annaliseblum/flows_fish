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
