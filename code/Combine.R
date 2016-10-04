##Merge all the data for the USGS gaged sites together
###Annalise Blum
#July 18,2016

#run SitesImport.R, FlowPrep.R, WeatherPrep.R, FishPrep.R,
#or just load the prepared data sets:
load("output/S.FB.rdata") #flows with basin characteristics

#fish
load("output/fish_YAbu.rdata") #All fish data 1982-2010 for 115 sites
load("output/fishSC.rdata") #all the site characteristics available

#weather -WHICH OF THESE TO USE??
load("output/SDAYMET.rdata") #weather
load("output/aDAYMET.rdata") #weather - Annual
load("output/annual.DAYMET.rdata") #daymet weather melted wide

#### 1 - Merge GAGED sites with weather ####
#check for consistent variable types across data sets: SEASONAL DATA SET OR ANNUAL LEVEL (SEASONS WIDE??)
class(SDAYMET$site_no); class(S.FB$site_no)
class(SDAYMET$year); class(S.FB$year)
class(SDAYMET$season); class(S.FB$season)

S.WFB<-merge(aDAYMET,S.FB,by=c("site_no","year")) #using weather from annual dataset but melted wide-NOW ONLY YEAR.F IN ADAYMET

#USGS
Usitelist<-as.data.frame(unique(S.WFB$site_no))
names(Usitelist)<-"site_no"; Usitelist$site_no<-as.character(Usitelist$site_no)
#pull 45 USGS sites that are actually included
USGS_BC1<-merge(Usitelist,USGS_BC,by="site_no")

# Check distributions of variables; transform ones with negs
summary(S.WFB) #look for negative values which will cause trouble
sum(S.WFB$MaxTwinter<0) #28
hist(log(S.WFB$maxdayflow)) #lognormal
hist(log(S.WFB$min7day+.001))
summary(S.WFB$min7day)
sum(S.WFB$min7day==0) #33 zeros...
sort(S.WFB$min7day[S.WFB$min7day<.05]) #next lowest is 0.0004 some how? should be .01
summary(S.WFB$min3day)
sum(S.WFB$min3day==0) #41 of zero...

#replace zeros in LFs
S.WFB$min7day[S.WFB$min7day==0]<- .0001 #add .0001 to the zeros so i can log
S.WFB$min3day[S.WFB$min3day==0]<- .0001 #add .0001 to the zeros so i can log

#transform vars with negs
S.WFB$MaxTwinter.T<- S.WFB$MaxTwinter+3

#add 0.01 to Precip vars with zeros: Pmar, Psept, Poct 
S.WFB$Pmar[S.WFB$Pmar==0]<- .01
S.WFB$Psept[S.WFB$Psept==0]<- .01
S.WFB$Poct[S.WFB$Poct==0]<- .01

#S.WFB$avgtmin.T<- S.WFB$avgtmin+10
S.WFB$LNG_GAGE.T<- S.WFB$LNG_GAGE+100
summary(S.WFB)

#create fish year variable because looking at previous summer, fall, winter spring to each summer's fish counts
S.WFB$year<-as.integer(S.WFB$year)
S.WFB$year.f <- ifelse(S.WFB$season=="winter"|S.WFB$season=="spring", S.WFB$year, S.WFB$year+1) 

#need variable USGS, UVA or fish
S.WFB$site_no[is.na(S.WFB$REACH_CODE)] #only the fish sites!!
S.WFB$site_type<-ifelse(is.na(S.WFB$REACH_CODE)==T,"UVA","USGS")

save(S.WFB,file="output/S.WFB.rdata")

##FIGURE OUT WHAT THIS IS FOR
# #get merged data set at annual level for plotting
# #need to reshape sflow data to be annual
# annual7minflow <- dcast(sflow, site_no + year.f ~ season,value.var = "min7day") #need to get wide format
# names(annual7minflow)<-c("site_no","year.f","LFfall", "LFspring", "LFsummer", "LFwinter")
# 
# #merge at year and site
# A.Flows<-merge(annual.DAYMET,annual7minflow,by=c("site_no","year.f"))
# str(A.Flows)
# summary(A.Flows) #lose the 2011s in DAYMET because no fish data for then...
# length(unique(A.Flows$site_no)) #45

#### 2 - Merge fish data with weather and basin characteristics ####
names(fish_YAbu)
names(fishSC) 

class(aDAYMET$site_no); class(fish_YAbu$site_no); class(fishSC$site_no) #need to be all character to merge
fishSC$site_no<-as.character(fishSC$site_no)

class(aDAYMET$year.f); class(fish_YAbu$year.f) 

#merge at year and site
A.FW<-merge(aDAYMET,fish_YAbu,by=c("site_no","year.f"))
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
S.WFC<-merge(SDAYMET,fishSC,by="site_no") #seasonal weather (W) with fish charactersitics (FC)
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
# Fsitelist<-as.data.frame(Fish_sites1$site_no)
# names(Fsitelist)<-"site_no"; Fsitelist$site_no<-as.character(Fsitelist$site_no)
# fishSC$site_no<-paste("F_",fishSC$site_no, sep = "")
# #pull just 34 fish sites from all 115
# fishSC1<-merge(Fsitelist,fishSC,by="site_no")
# summary(fishSC1$DRAIN_SQMI)
