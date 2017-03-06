##Merge all the data for the USGS gaged sites together
###Annalise Blum
#July 18,2016  Updated: Dec 5,2016
##Data sets created in this file: "output/S.Fish.rdata"; "output/A.Fish.rdata"; "output/S.Flow.rdata"

#run SitesImport.R, FlowPrep.R, WeatherPrep.R, FishPrep.R,
#or just load the prepared data sets:
#load("output/S.FB.rdata") #flows with basin characteristics
load("output/gagedsites_BC.rdata") #Just basin characteristics
load("output/A.Fishmerge.rdata") #predicted flow Metrics for fish sites - annual level

#fish
load("output/fish_YAbu.rdata") #All fish data 1982-2010 for 115 sites
load("output/fishSC.rdata") #all the site characteristics available

#weather
load("output/SDAYMET.rdata") #weather
load("output/aDAYMET.rdata") #weather - Annual
#load("output/annual.DAYMET.rdata") #daymet weather melted wide

#### 1 - Merge GAGED sites with weather ####
#check for consistent variable types across data sets: SEASONAL DATA SET OR ANNUAL LEVEL (SEASONS WIDE??)
class(SDAYMET$site_no); class(Preds_metrics$site_no)
class(SDAYMET$Nyear); class(Preds_metrics$Nyear)
class(SDAYMET$Nseason); class(Preds_metrics$Nseason)

S.Flow<-merge(Preds_metrics,SDAYMET,by=c("site_no","Nyear","Nseason"))

save(S.Flow,file="output/S.Flow.rdata")

#Make Annual level flow data set - merge at year and site
A.FlowW<-merge(aDAYMET,gagedsites_BC,by=c("site_no"))
str(A.FlowW)
summary(A.FlowW)
length(unique(A.FlowW$site_no)) 
save(A.FlowW,file="output/A.FlowW.rdata")

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
A.Fish<-merge(A.FW,fishSC,by="site_no")
str(A.Fish)
summary(A.Fish)
length(unique(A.Fish$site_no))

#save
save(A.Fish,file="output/A.Fish.rdata")

##Fish sites at SEASONAL level (for predictions with regional regression need this)
class(SDAYMET$site_no);class(fishSC$site_no)
S.Fish<-merge(SDAYMET,fishSC,by="site_no") #seasonal weather (W) with fish charactersitics (FC)
head(S.Fish); names(S.Fish)
length(unique(S.Fish$site_no))

#compare data sets because need to be matching for predictions later
names(S.Flow) #flows
names(S.Fish) #fish
names(A.Fish) #compare to annual

#save
save(S.Fish,file="output/S.Fish.rdata")

## Merge A.Fish and Flow predictions on annual level

#merge in the Julian Day that the sample was taken
# load("output/JulianDay.rdata")

All_fish<-merge(A.Fish,A.Fishmerge,by=c("site_no","Nyear"))
All_fish<-merge(All_fish,JulianDay,by=c("site_no","Nyear"))

save(All_fish,file="output/All_fish.rdata")



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