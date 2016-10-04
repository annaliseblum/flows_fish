##Merge all the data for the USGS gaged sites together
###Annalise Blum
#July 18,2016  Updated: Oct 4,2016
##Data sets created in this file: "output/

#run SitesImport.R, FlowPrep.R, WeatherPrep.R, FishPrep.R,
#or just load the prepared data sets:
load("output/S.FB.rdata") #flows with basin characteristics

#fish
load("output/fish_YAbu.rdata") #All fish data 1982-2010 for 115 sites
load("output/fishSC.rdata") #all the site characteristics available

#weather -WHICH OF THESE TO USE??
load("output/SDAYMET.rdata") #weather
load("output/aDAYMET.rdata") #weather - Annual
#load("output/annual.DAYMET.rdata") #daymet weather melted wide

#### 1 - Merge GAGED sites with weather ####
#check for consistent variable types across data sets: SEASONAL DATA SET OR ANNUAL LEVEL (SEASONS WIDE??)
class(SDAYMET$site_no); class(S.FB$site_no)
class(SDAYMET$Nyear); class(S.FB$Nyear)
class(SDAYMET$Nseason); class(S.FB$Nseason)

S.WFB<-merge(SDAYMET,S.FB,by=c("site_no","Nyear","Nseason")) #Should I be using weather from annual dataset but melted wide: aDAYMET

#need variable USGS, UVA or fish
S.WFB$site_no[is.na(S.WFB$REACH_CODE)] #only the fish sites!!
S.WFB$site_type<-ifelse(is.na(S.WFB$REACH_CODE)==T,"UVA","USGS")

save(S.WFB,file="output/S.WFB.rdata")

#USGS - get site list of characteristics only sites we are using
Usitelist<-as.data.frame(unique(S.WFB$site_no))
names(Usitelist)<-"site_no"; Usitelist$site_no<-as.character(Usitelist$site_no)
#pull 45 USGS sites that are actually included
USGS_BC1<-merge(Usitelist,USGS_BC,by="site_no")

#### 2 - Merge fish data with weather and basin characteristics ####
names(fish_YAbu)
names(fishSC) 

class(aDAYMET$site_no); class(fish_YAbu$site_no); class(fishSC$site_no) #need to be all character to merge
fishSC$site_no<-as.character(fishSC$site_no)

class(aDAYMET$Nyear); class(fish_YAbu$Nyear) 

#merge at year and site
A.FW<-merge(aDAYMET,fish_YAbu,by=c("site_no","Nyear"))
str(A.FW)
summary(A.FW) #lose the 2011s in DAYMET because no fish data for then...
length(unique(A.FW$site_no)) #Good, all 115 sites are there

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
head(S.WFC); names(S.WFC)
length(unique(S.WFC$site_no))

#compare data sets because need to be matching for predictions later
names(S.WFB) #flows
names(S.WFC) #fish
names(A.FWC) #compare to annual

#save
save(S.WFC,file="output/S.WFC.rdata")

#### OLD ####
#fish
# Fsitelist<-as.data.frame(Fish_sites1$site_no)
# names(Fsitelist)<-"site_no"; Fsitelist$site_no<-as.character(Fsitelist$site_no)
# fishSC$site_no<-paste("F_",fishSC$site_no, sep = "")
# #pull just 34 fish sites from all 115
# fishSC1<-merge(Fsitelist,fishSC,by="site_no")
# summary(fishSC1$DRAIN_SQMI)

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