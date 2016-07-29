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
load("output/annual.DAYMET.rdata") #daymet weather melted wide
load("output/fishSC.rdata") #all the site characteristics available

#### 1 - Merge USGS GAGED sites info ####
class(SDAYMET$site_no); class(sflow$site_no); class(USGS_BC$site_no)
class(SDAYMET$year); class(sflow$year); class(USGS_BC$year) #right, BCs are time-invariant
class(SDAYMET$season); class(sflow$season); class(USGS_BC$season)

#rename site characteristics variables to match fish sample sites
USGS_BC<-USGS_BC[c("site_no","DRAIN_SQMI","HUC02","LAT_GAGE","LNG_GAGE","REACHCODE",
                   "SLOPE_PCT","ASPECT_DEGREES", "ELEV_SITE_M",
                   "BFI_AVE","TOPWET","ELEV_MEAN_M_BASIN")] #these aren't in fish
names(USGS_BC)<-c("site_no","DRAIN_SQMI","HUC02","LAT_GAGE","LNG_GAGE","REACH_CODE",
                  "Slope_pct","Aspect_deg","Elev_m","BFI_AVE","TOPWET","ELEV_MEAN_M_BASIN")

S.WF<-merge(SDAYMET,sflow,by=c("site_no","year","season"))
S.WFB<-merge(S.WF,USGS_BC,by=c("site_no"))

#USGS
Usitelist<-as.data.frame(unique(S.WFB$site_no))
names(Usitelist)<-"site_no"; Usitelist$site_no<-as.character(Usitelist$site_no)
#pull just 29 USGS sites from all 34
USGS_BC1<-merge(Usitelist,USGS_BC,by="site_no")

## Check distributions of variables; transform ones with negs
# sum(S.WFB$avgtmax<0)
# head(S.WFB)
# hist(S.WFB$totprecip) #pretty normal
# hist(S.WFB$avgtmax) #not so normal
# hist(S.WFB$avgtmin) #even less normal
# hist(S.WFB$min7day) #lognormal?
# summary(S.WFB$min7day)
# sum(S.WFB$min7day==0) #5 of zero... 
# sort(S.WFB$min7day[S.WF$min7day<.05]) #next lowest is 0.01 

#replace zeros in LFs
S.WFB$min7day[S.WFB$min7day==0]<- .005 #add .005 to the zeros so i can log
#hist(log(S.WFB$min7day)) #looks better!

#transform ones with negs
#summary(S.WFB) #check for negatives: avgtmax, avgtmin,LNG_GAGE

S.WFB$avgtmax.T<- S.WFB$avgtmax+3
S.WFB$L1avgtmax.T<- S.WFB$L1avgtmax+3
S.WFB$L2avgtmax.T<- S.WFB$L2avgtmax+3
S.WFB$L3avgtmax.T<- S.WFB$L3avgtmax+3
S.WFB$L4avgtmax.T<- S.WFB$L4avgtmax+3

#S.WFB$avgtmin.T<- S.WFB$avgtmin+10
S.WFB$LNG_GAGE.T<- S.WFB$LNG_GAGE+100
summary(S.WFB)

save(S.WFB,file="output/S.WFB.rdata")

#get merged data set at annual level for plotting
#need to reshape sflow data to be annual
aflow <- dcast(sflow, site_no + year.f ~ season,value.var = "min7day") #need to get wide format
names(aflow)<-c("site_no","year.f","LFfall", "LFspring", "LFsummer", "LFwinter")

#merge at year and site
A.Flows<-merge(annual.DAYMET,aflow,by=c("site_no","year.f"))
str(A.Flows)
summary(A.Flows) #lose the 2011s in DAYMET because no fish data for then...
length(unique(A.Flows$site_no)) #29

#### 2 - Merge fish data with weather and basin characteristics ####
names(fish_YP1)
names(fishSC) 
names(fish_YP1)[1]<-"site_no" #rename as site_no to be consistent

class(annual.DAYMET$site_no); class(fish_YP1$site_no); class(fishSC$site_no) #need to be all character to merge
fishSC$site_no<-as.character(fishSC$site_no)

fish_YP1$year.f<-as.numeric(as.character(fish_YP1$year)) #fish years are the same as years for the fish data (they are really summer-spring)
class(annual.DAYMET$year.f); class(fish_YP1$year.f) 

#merge at year and site
A.FW<-merge(annual.DAYMET,fish_YP1,by=c("site_no","year.f"))
str(A.FW)
summary(A.FW) #lose the 2011s in DAYMET because no fish data for then...
length(unique(A.FW$site_no)) #34 sites all made it!

#merge site characteristics in at site-level
A.FWC<-merge(A.FW,fishSC,by="site_no")
str(A.FWC)
summary(A.FWC)
length(unique(A.FWC$site_no)) #34 sites all made it! again!

#save
save(A.FWC,file="output/A.FWC.rdata")

##Fish sites at SEASONAL level (for predictions with regional regression need this)
class(SDAYMET$site_no);class(fishSC$site_no)
SDAYMET1<-SDAYMET
SDAYMET1$site_no<-paste("F_",SDAYMET1$site_no,sep="") #Fs missing
S.WFC<-merge(SDAYMET1,fishSC,by="site_no") #seasonal weather (W) with fish charactersitics (FC)
head(S.WFC)
length(unique(S.WFC$site_no)) #34!! good

#do same transformations as for the flow data
S.WFC$avgtmax.T<- S.WFC$avgtmax+3
S.WFC$L1avgtmax.T<- S.WFC$L1avgtmax+3
S.WFC$L2avgtmax.T<- S.WFC$L2avgtmax+3
S.WFC$L3avgtmax.T<- S.WFC$L3avgtmax+3
S.WFC$L4avgtmax.T<- S.WFC$L4avgtmax+3

S.WFC$avgtmin.T<- S.WFC$avgtmin+10
S.WFC$LNG_GAGE.T<- S.WFC$LNG_GAGE+100
summary(S.WFC)

#save
save(S.WFC,file="output/S.WFC.rdata")

#fish
Fsitelist<-as.data.frame(Fish_sites1$site_no)
names(Fsitelist)<-"site_no"; Fsitelist$site_no<-as.character(Fsitelist$site_no)
fishSC$site_no<-paste("F_",fishSC$site_no, sep = "")
#pull just 34 fish sites from all 115
fishSC1<-merge(Fsitelist,fishSC,by="site_no")
summary(fishSC1$DRAIN_SQMI)
