##Flow predictions prep
##Aug 19, 2016

#load dataset with flow predictions
load("output/A.FWC_RR.rdata")

#just get complete cases with no NAs
A.FWC_RR<-A.FWC_RR[complete.cases(A.FWC_RR),] #down to 411 from 578 obs
summary(A.FWC_RR)

#create rounded abundance measure
A.FWC_RR$RYOYAbu<-round(A.FWC_RR$EstYOYAbu)

#create dataframes by season to reshape and standardize
fall_preds<-A.FWC_RR[c("site_no", "year.f", "RRpredsfall")]
spring_preds<-A.FWC_RR[c("site_no", "year.f", "RRpredsspring")]
summer_preds<-A.FWC_RR[c("site_no", "year.f", "RRpredssummer")]
winter_preds<-A.FWC_RR[c("site_no", "year.f", "RRpredswinter")]

## reshape to wide, then Scale flow predictions by site (mean 0, sd 1)
fall_predsW <- dcast(fall_preds, site_no ~ year.f,value.var = "RRpredsfall") #need to get wide format
rownames(fall_predsW)<-fall_predsW$site_no
fall_predsW$site_no<-NULL
fallPred7minStd<-(fall_predsW-rep(rowMeans(fall_predsW,na.rm=T),ncol(fall_predsW)))/apply(fall_predsW,1, sd,na.rm=T)

spring_predsW <- dcast(spring_preds, site_no ~ year.f,value.var = "RRpredsspring") #need to get wide format
rownames(spring_predsW)<-spring_predsW$site_no
spring_predsW$site_no<-NULL
springPred7minStd<-(spring_predsW-rep(rowMeans(spring_predsW,na.rm=T),ncol(spring_predsW)))/apply(spring_predsW,1, sd,na.rm=T)

summer_predsW <- dcast(summer_preds, site_no ~ year.f,value.var = "RRpredssummer") #need to get wide format
rownames(summer_predsW)<-summer_predsW$site_no
summer_predsW$site_no<-NULL
summerPred7minStd<-(summer_predsW-rep(rowMeans(summer_predsW,na.rm=T),ncol(summer_predsW)))/apply(summer_predsW,1, sd,na.rm=T)

winter_predsW <- dcast(winter_preds, site_no ~ year.f,value.var = "RRpredswinter") #need to get wide format
rownames(winter_predsW)<-winter_predsW$site_no
winter_predsW$site_no<-NULL
winterPred7minStd<-(winter_predsW-rep(rowMeans(winter_predsW,na.rm=T),ncol(winter_predsW)))/apply(winter_predsW,1, sd,na.rm=T)

#spot check
rowMeans(fallPred7minStd,na.rm=T) #should all be very tiny ~0
apply(fallPred7minStd,1,sd,na.rm=T) #should all be 1
rowMeans(springPred7minStd,na.rm=T) #should all be very tiny ~0
apply(springPred7minStd,1,sd,na.rm=T) #should all be 1
rowMeans(summerPred7minStd,na.rm=T) #should all be very tiny ~0
apply(summerPred7minStd,1,sd,na.rm=T) #should all be 1
rowMeans(winterPred7minStd,na.rm=T) #should all be very tiny ~0
apply(winterPred7minStd,1,sd,na.rm=T) #should all be 1


###Just becasue I'm only using 34 sites now, 
##Now pull the 34 sites I'm using from the 115 Kanno used

## site covariate data
load("~/flows_fish/YK/standardized site cov 115 sites.rdata")

##pull out the 34 sites for prelim analysis; site.pos comes from FishPrep.R:
# matches<-match(YOY_Abun$site_no, fishSites34, nomatch=0)
# site.pos<-which(matches>0)

fallTempStd34<-as.data.frame(fallTempAryStd[site.pos,15:29]) #only 34 sites and only 1996-2010 (lost 1994 and 1995 because of lagged flows for prediction and fish years?)
winterTempStd34<-as.data.frame(winterTempAryStd[site.pos,15:29])
springTempStd34<-as.data.frame(springTempAryStd[site.pos,15:29] )
summerTempStd34<-as.data.frame(summerTempAryStd[site.pos,15:29])

#rename with actual years
names(fallTempStd34)<-c("1996","1997","1998","1999","2000","2001","2002","2003","2004","2005","2006","2007","2008","2009","2010")
names(winterTempStd34)<-c("1996","1997","1998","1999","2000","2001","2002","2003","2004","2005","2006","2007","2008","2009","2010")
names(springTempStd34)<-c("1996","1997","1998","1999","2000","2001","2002","2003","2004","2005","2006","2007","2008","2009","2010")
names(summerTempStd34)<-c("1996","1997","1998","1999","2000","2001","2002","2003","2004","2005","2006","2007","2008","2009","2010")

##need to add site numbers back and then reshape back to long
fallTempStd34$site_no<-fishSites34
fallTempStd34_L<-melt(fallTempStd34,id.var="site_no")
names(fallTempStd34_L)<-c("site_no","year","fallTempStd")
winterTempStd34$site_no<-fishSites34
winterTempStd34_L<-melt(winterTempStd34,id.var="site_no")
names(winterTempStd34_L)<-c("site_no","year","winterTempStd")

springTempStd34$site_no<-fishSites34
springTempStd34_L<-melt(springTempStd34,id.var="site_no")
names(springTempStd34_L)<-c("site_no","year","springTempStd")
summerTempStd34$site_no<-fishSites34
summerTempStd34_L<-melt(summerTempStd34,id.var="site_no")
names(summerTempStd34_L)<-c("site_no","year","summerTempStd")

#merge weather
merge1<-merge(fallTempStd34_L,winterTempStd34_L,by=c("site_no","year"))
merge2<-merge(merge1,springTempStd34_L,by=c("site_no","year"))
merge3<-merge(merge2,summerTempStd34_L,by=c("site_no","year"))

#for flow predictions
fallPred7minStd$site_no<-fishSites34
fallPred7minStd_L<-melt(fallPred7minStd,id.var="site_no")
names(fallPred7minStd_L)<-c("site_no","year","fallPmin7day")
winterPred7minStd$site_no<-fishSites34
winterPred7minStd_L<-melt(winterPred7minStd,id.var="site_no")
names(winterPred7minStd_L)<-c("site_no","year","winterPmin7day")
springPred7minStd$site_no<-fishSites34
springPred7minStd_L<-melt(springPred7minStd,id.var="site_no")
names(springPred7minStd_L)<-c("site_no","year","springPmin7day")
summerPred7minStd$site_no<-fishSites34
summerPred7minStd_L<-melt(summerPred7minStd,id.var="site_no")
names(summerPred7minStd_L)<-c("site_no","year","summerPmin7day")

#merge flow preds
merge4<-merge(fallPred7minStd_L,winterPred7minStd_L,by=c("site_no","year"))
merge5<-merge(merge4,springPred7minStd_L,by=c("site_no","year"))
merge6<-merge(merge5,summerPred7minStd_L,by=c("site_no","year"))

#merge flow and weather
merge3$year<-as.character(merge3$year)
merge6$year<-as.character(merge6$year)

merge8<-merge(merge3,merge6,by=c("site_no","year")) #year is actually fish year here! 
merge8$year.f<-as.numeric(merge8$year)
#merge back into original data set:
AbuPFlows<-merge(A.FWC_RR,merge8,by=c("site_no","year.f")) #year vs year.f!!

##maybe should check that this matches by aggregating temps by site and checking standardized values


