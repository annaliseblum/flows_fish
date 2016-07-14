##Weather prep

DAYMET <- read.csv("data/daymetRecord.csv",colClasses="character") #6 variables
head(DAYMET)
DAYMET$Date<-as.Date(DAYMET$date, "%Y-%m-%d")

unique(DAYMET$site_no)
