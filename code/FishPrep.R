##fish data prep
##July 18, 2016
##annalise Blum
library(reshape)



#sort out structure of this multi-dimensional array
dim(countAr) #115 sites X 29 years X 2 agets X 3 passes

# #extract YOY to get down to 3 dimensional array
YOYcountAr<-countAr[,,1,]
dim(YOYcountAr)
dimnames(countAr)

#sum the 3 passes
YOYcountSum<-apply(YOYcountAr,c(1, 2),sum,na.rm=T)

#spot check
# YOYcountAr[1,1,1];YOYcountAr[1,1,2];YOYcountAr[1,1,3]
# YOYcountSum[1,1]
# YOYcountAr[2,1,1];YOYcountAr[2,1,2];YOYcountAr[2,1,3] #NAs go to 0s...hmm
# YOYcountSum[2,1]
# YOYcountAr[2,25,1]+YOYcountAr[2,25,2]+YOYcountAr[2,25,3]
# YOYcountSum[2,25]

#are there any 0 values in first pass? Nope, so it's ok to replace 0s in sum matrix with NA
sum(YOYcountAr[,,1]==0,na.rm=T)

#replace 0s with NA because they are times no data was collected
YOYcountSum[YOYcountSum == 0] <- NA

#detection rate is 0.569
YOY_Abun<-as.data.frame(YOYcountSum/0.569)

#create site number and remove leading "F_"
rownames(YOY_Abun)
YOY_Abun$site_no<-substr(rownames(YOY_Abun),3,9)

##pull out the 34 sites for prelim analysis; Fish_sites1 is the list of 34 sites
matches<-match(YOY_Abun$site_no, fishSites34, nomatch=0)
site.pos<-which(matches>0)
YOY_Abun34<-YOY_Abun[site.pos,]

#melt into long format
fish_YAbu34<-melt.data.frame(YOY_Abun34,id.vars="site_no")
names(fish_YAbu34)<-c("site_no","year","EstYOYAbu")
save(fish_YAbu34,file="output/fish_YAbu34.rdata")

#### Old - subset of years and just first pass) ####
#extract years 1994-2010
#countArS<-countAr[,13:29,,]

#how to extract the sites I'm using? a merge?
apply(countArS[,,,],4,max,na.rm=T)
alply(countArS,3)
rownames(countArS)
dimnames(countArS)

Fish_sites1<-paste("F_",Fish_sites,sep="")

merge(countArS)

##Back to data.frame with just 1st pass YOY - fishSitesSub is from SitesImport
head(fishSitesSub)
dim(fishSitesSub) #this is my data set - need to make it long
fishSitesSub$yr.rec<-NULL
fish_YP1<-melt.data.frame(fishSitesSub,id.vars="SiteID")
names(fish_YP1)<-c("site_no","year","YOY_P1")
save(fish_YP1,file="output/fish_YP1.rdata")

fishsitenos<-unique(fish_YP1$site_no)

#why did I get rid of the F_? because of Df file 
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

fishSC<-fishSC[c("SiteID","DA_SQMI","HUC8","LAT_GAGE","LNG_GAGE","REACH_CODE",
                   "Slope_deg","Aspect_deg", "Elev_m")]
names(fishSC)<-c("site_no","DRAIN_SQMI","HUC8","LAT_GAGE","LNG_GAGE","REACH_CODE",
                  "Slope_pct","Aspect_deg","Elev_m")

save(fishSC,file="output/fishSC.rdata")

####Array subset - just the sites I'm using! ####

#want to just pull these site numbers from the array
Fish_sites1

#just need to get a vector with the location of the 34 sites within the 115
matches<-match(rownames(countAr), Fish_sites1, nomatch=0)
site.pos<-which(matches>0)

dim(countAr[site.pos,,,]) #yesss

mycountAr<-countAr[site.pos,13:29,,]

