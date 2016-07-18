##Merge all the data for the USGS gaged sites together
###Annalise Blum
#July 18,2016

#run SitesImport.R, FlowPrep.R, WeatherPrep.R, FishPrep.R,
#or just load the prepared data sets:
#flow
load("output/sflow.rdata") #flows
load("output/SDAYMET.rdata") #weather
load("output/USGS_BC.rdata")#USGS gages Basin Chars

#fish
load("output/fish_YP1.rdata") #took just first pass, YOY for the 34 sites with 3 passes 1994-2010
load("output/fishsiteDf.rdata") #all the site characteristics available
load("output/annual.DAYMET.rdata") #daymet weather melted wide


#### 1 - Merge USGS GAGED sites info ####
class(SDAYMET$site_no); class(sflow$site_no); class(USGS_BC$site_no)
class(SDAYMET$year); class(sflow$year); class(USGS_BC$year) #right, BCs are time-invariant
class(SDAYMET$season); class(sflow$season); class(USGS_BC$season)

S.WF<-merge(SDAYMET,sflow,by=c("site_no","year","season"))
S.WFB<-merge(S.WF,USGS_BC,by=c("site_no"))

#### Check distributions of variables; transform ones with negs ####
sum(S.WFB$avgtmax<0)
head(S.WFB)
hist(S.WFB$totprecip) #pretty normal
hist(S.WFB$avgtmax) #not so normal
hist(S.WFB$avgtmin) #even less normal
hist(S.WFB$min7day) #lognormal?
summary(S.WFB$min7day)
sum(S.WFB$min7day==0) #5 of zero... 
sort(S.WFB$min7day[S.WF$min7day<.05]) #next lowest is 0.01 

#replace zeros in LFs
S.WFB$min7day[S.WFB$min7day==0]<- .005 #add .005 to the zeros so i can log
hist(log(S.WFB$min7day)) #looks better!

#transform ones with negs
summary(S.WFB) #check for negatives: avgtmax, avgtmin,LNG_GAGE

S.WFB$avgtmax.T<- S.WFB$avgtmax+3
S.WFB$avgtmin.T<- S.WFB$avgtmin+10
S.WFB$LNG_GAGE.T<- S.WFB$LNG_GAGE+100
summary(S.WFB)

save(S.WFB,file="output/S.WFB.rdata")

#### 2 - Merge fish data with weather and basin characteristics ####
str(fish_YP1); str(fishsiteDf) #both first cols are "SiteID"
names(fish_YP1)[1]<-"site_no" #rename as site_no to be consistent
names(fishsiteDf)[1]<-"site_no" 
fishSC<-fishsiteDf[c("site_no","HUC8","REACH_CODE","County","Aspect_deg","Slope_deg",
                     "Elev_m","Lat_n83","Lon_n83")]  #pull just the vars we need from fishsiteDf (All in park)
class(annual.DAYMET$site_no); class(fish_YP1$site_no); class(fishSC$site_no) #need to be all character to merge
fishSC$site_no<-as.character(fishSC$site_no)

class(annual.DAYMET$year); class(fish_YP1$year) #need to be all character to merge

#merge at year and site
A.FW<-merge(annual.DAYMET,fish_YP1,by=c("site_no","year"))
str(A.FW)
summary(A.FW)
length(unique(A.FW$site_no)) #34 sites all made it!

#merge site characteristics in at site-level
A.FWC<-merge(A.FW,fishSC,by="site_no")
str(A.FWC)
summary(A.FWC)
length(unique(A.FWC$site_no)) #34 sites all made it! again!

#save
save(A.FWC,file="output/A.FWC.rdata")
