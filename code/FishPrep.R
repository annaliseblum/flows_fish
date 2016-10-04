##fish data prep
###Impact of Extreme Streamflows on Brook Trout Young-of-Year Abundance
### Annalise G Blum
##Created:July 18, 2016, last modified: Sept 28,2016
##Data set created in this file: "output/fishSC.rdata"

library(reshape)

#### 1 - Create Array of Fish Abundance from Counts ####
#load data
load("output/countAr.rdata")
#sort out structure of this multi-dimensional array
dim(countAr) #115 sites X 29 years X 2 agets X 3 passes

#extract YOY to get down to 3 dimensional array
YOYcountAr<-countAr[,,1,]
dim(YOYcountAr)
dimnames(countAr)

#sum the 3 passes to estimate abundance based on detection rate
YOYcountSum<-apply(YOYcountAr,c(1, 2),sum,na.rm=T)

#spot check
# YOYcountAr[1,1,1];YOYcountAr[1,1,2];YOYcountAr[1,1,3]
# YOYcountSum[1,1]

#are there any 0 values in first pass? Nope, so it's ok to replace 0s in sum matrix with NA
sum(YOYcountAr[,,1]==0,na.rm=T)

#replace 0s with NA because they are times no data was collected
YOYcountSum[YOYcountSum == 0] <- NA

#detection rate is 0.569 based on results of Kanno et al model analysis
YOY_Abun<-as.data.frame(YOYcountSum/0.569)

#create site number and remove leading "F_"
rownames(YOY_Abun)
YOY_Abun$site_no<-substr(rownames(YOY_Abun),3,9)

#melt into long format
fish_YAbu<-melt.data.frame(YOY_Abun,id.vars="site_no")
names(fish_YAbu)<-c("site_no","year","EstYOYAbu")

#create fish year variable
fish_YAbu$year.f<-as.numeric(as.character(fish_YAbu$year)) #fish years are the same as years for the fish data 
#(each year starts in starts fall, ends in summer)

save(fish_YAbu,file="output/fish_YAbu.rdata")

#length(unique(fish_YAbu$site_no)) # this has all 15 sites

#### 2 - Prep fish site characteristics data ####
fishsiteDf <- read.csv("YK/siteDf.csv") #this has lots of vars
fishsitesWS <- read.csv("YK/WSarea_forKanno.csv") #this has watershed area (missing from Df file)

names(fishsiteDf)
fishSC1<-fishsiteDf[c("SiteID","HUC8","REACH_CODE","County","Aspect_deg","Slope_deg",
                     "Elev_m","Lat_n83","Lon_n83")]  #pull just the vars we need from fishsiteDf (All in park)

names(fishsitesWS)
fishSC2<-fishsitesWS[c("SiteID","WatershedArea_ha")]  #pull just watershed area which wasn't in other data set
fishSC2$DA_SQKM<-fishSC2$WatershedArea_ha/100
fishSC2$WatershedArea_ha<-NULL

#merge to get one site characteristics data set
fishSC<-merge(fishSC2,fishSC1,by="SiteID")
names(fishSC)
fishSC<-fishSC[c("SiteID","DA_SQKM","HUC8","Lat_n83","Lon_n83","REACH_CODE",
                   "Slope_deg","Aspect_deg", "Elev_m")]
names(fishSC)<-c("site_no","DA_SQKM","HUC8","LAT_GAGE","LNG_GAGE","REACH_CODE",
                  "Slope_pct","Aspect_deg","Elev_m")

save(fishSC,file="output/fishSC.rdata")

# #### Old - subset of years and just first pass) ####
# ##pull out the 34 sites for prelim analysis; Fish_sites1 is the list of 34 sites
# matches<-match(YOY_Abun$site_no, fishSites34, nomatch=0)
# site.pos<-which(matches>0)
# YOY_Abun34<-YOY_Abun[site.pos,]
# 
# #extract years 1994-2010
# #countArS<-countAr[,13:29,,]
# 
# #how to extract the sites I'm using? a merge?
# apply(countArS[,,,],4,max,na.rm=T)
# alply(countArS,3)
# rownames(countArS)
# dimnames(countArS)
# 
# Fish_sites1<-paste("F_",Fish_sites,sep="")
# 
# ##Back to data.frame with just 1st pass YOY - fishSitesSub is from SitesImport
# head(fishSitesSub)
# dim(fishSitesSub) #this is my data set - need to make it long
# fishSitesSub$yr.rec<-NULL
# fish_YP1<-melt.data.frame(fishSitesSub,id.vars="SiteID")
# names(fish_YP1)<-c("site_no","year","YOY_P1")
# save(fish_YP1,file="output/fish_YP1.rdata")
# 
# fishsitenos<-unique(fish_YP1$site_no)
# 
# #why did I get rid of the F_? because of Df file 
# #NNpreds$Fish_site<-str_pad(NNpreds$fish_site,width=7, side="left",pad="F_")
# 
# ##Array subset - just the sites I'm using! ##
# 
# #want to just pull these site numbers from the array
# Fish_sites1
# 
# #just need to get a vector with the location of the 34 sites within the 115
# matches<-match(rownames(countAr), Fish_sites1, nomatch=0)
# site.pos<-which(matches>0)
# 
# dim(countAr[site.pos,,,]) #yesss
# 
# mycountAr<-countAr[site.pos,13:29,,]
