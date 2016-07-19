##fish data prep
##July 18, 2016
##annalise Blum
library(reshape)

#extract the fish sites I'm looking at (34 of them) and years 1994-2010
load("~/flows_fish/YK/countArray 115 sites.rdata")

#sort out structure of this multi-dimensional array
dim(countAr) #115 sites X 29 years X 2 agets X 3 passes

# #extract YOY to get down to 3 dimensional array
# YOYcountAr<-countAr[,,1,]
# dim(YOYcountAr)

#extract years 1994-2010
countArS<-countAr[,13:29,,]

#how to extract the sites I'm using? a merge?
apply(countArS[,,,],4,max,na.rm=T)
alply(countArS,3)
rownames(countArS)
##NEED TO LEARN HOW TO DEAL WITH ARRAYS

Fish_sites1<-paste("F_",Fish_sites,sep="")

merge(countArS)

##Back to data.frame with just 1st pass YOY
head(fishSitesSub)
dim(fishSitesSub) #this is my data set - need to make it long
fishSitesSub$yr.rec<-NULL
fish_YP1<-melt.data.frame(fishSitesSub,id.vars="SiteID")
names(fish_YP1)<-c("site_no","year","YOY_P1")
save(fish_YP1,file="output/fish_YP1.rdata")


#maybe I can just melt into long like this for all 3 passes! and take max

#why did I get rid of the F_? because of Df
#NNpreds$Fish_site<-str_pad(NNpreds$fish_site,width=7, side="left",pad="F_")

###Fish site Characteristics ####
fishsitesWS <- read.csv("YK/WSarea_forKanno.csv") #this has most
fishsiteDf <- read.csv("YK/siteDf.csv")
names(fishsiteDf)
fishSC1<-fishsiteDf[c("SiteID","HUC8","REACH_CODE","County","Aspect_deg","Slope_deg",
                     "Elev_m","Lat_n83","Lon_n83")]  #pull just the vars we need from fishsiteDf (All in park)

names(fishsiteDf)
fishSC2<-fishsitesWS[c("SiteID","WatershedArea_ha")]  #pull just the vars we need
fishSC2$DA_SQMI<-fishSC2$WatershedArea_ha/258.999

#merge
fishSC<-merge(fishSC2,fishSC1,by="SiteID")

fishSC<-fishSC[c("SiteID","DA_SQMI","HUC8","Lat_n83","Lon_n83","REACH_CODE",
                   "Slope_deg","Aspect_deg", "Elev_m")]
names(fishSC)<-c("site_no","DRAIN_SQMI","HUC8","LAT_GAGE","LNG_GAGE","REACH_CODE",
                  "Slope_pct","Aspect_deg","Elev_m")

save(fishSC,file="output/fishSC.rdata")
