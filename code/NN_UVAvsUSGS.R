#Flow prediction comparison
#Systematically compare UVA and USGS NN-DAR preds for UVA sites

###Impact of Extreme Streamflows on Brook Trout Young-of-Year Abundance
### Annalise G Blum; Modified Dec 29,2016

#experiment with average of gage sites standardized flows?

load("output/NN.USGS.UVA.rdata")
load("output/NN_UVAUVA.rdata")

names(NN.USGS.UVA)
NN.USGS.UVA<-NN.USGS.UVA[c("site_no","Date","cfs","NNpredcfs")]
names(NN.USGS.UVA)<-c("site_no","Date","cfs","USGS_NNpredcfs")

names(NN_UVAUVA)
NN_UVAUVA<-NN_UVAUVA[c("site_no","Date","NNpredcfs")]
names(NN_UVAUVA)<-c("site_no","Date","UVA_NNpredcfs")

NNComparison<-merge(NN.USGS.UVA,NN_UVAUVA,by=c("site_no","Date"))

#get seasons
NNComparison$day<-as.integer(format(NNComparison$Date, "%d")) #extract day variable 
NNComparison$month<-as.integer(format(NNComparison$Date, "%m")) #extract month variable 
NNComparison$year<-as.integer(format(NNComparison$Date, "%Y")) #extract year variable 

#define seasons
NNComparison$season[NNComparison$month==12|NNComparison$month==01|NNComparison$month==02]<-"winter"
NNComparison$season[NNComparison$month==03|NNComparison$month==04|NNComparison$month==05]<-"spring"
NNComparison$season[NNComparison$month==06|NNComparison$month==07|NNComparison$month==08]<-"summer"
NNComparison$season[NNComparison$month==09|NNComparison$month==10|NNComparison$month==11]<-"fall"

save(NNComparison,file="output/NNComparison.rdata")
#Log space
ggplot(NNComparison, aes(x=log(cfs), y=log(USGS_NNpredcfs),color=season))+geom_point()+ facet_grid(~ site_no)+
  geom_abline(intercept = 0,slope=1)

ggplot(NNComparison, aes(x=log(cfs), y=log(UVA_NNpredcfs),color=season))+geom_point()+ facet_grid(~ site_no)+
  geom_abline(intercept = 0,slope=1)

#Real Space
ggplot(NNComparison, aes(x=cfs, y=USGS_NNpredcfs))+geom_point()+ facet_grid(~ site_no)+
geom_abline(intercept = 0,slope=1)

ggplot(NNComparison, aes(x=cfs, y=UVA_NNpredcfs))+geom_point()+ facet_grid(~ site_no)+
  geom_abline(intercept = 0,slope=1)

####NSE by site
UUGSUVAsiteNSE<-rep(NA,5)
USGSUVAsiteLNSE<-rep(NA,5)
UVAUVAsiteNSE<-rep(NA,5)
UVAUVAsiteLNSE<-rep(NA,5)

library(hydroGOF)

#NSE by site:  "UVA_NFDR" "UVA_PAIN" "UVA_PINE" "UVA_STAN" "UVA_WOR1"
for (i in 1:5){
  idata<-NNComparison[NNComparison$site_no==unique(NNComparison$site_no)[i],]
  UUGSUVAsiteNSE[i]<-NSE(idata$USGS_NNpredcfs, idata$cfs)
  USGSUVAsiteLNSE[i]<-NSE(log(idata$USGS_NNpredcfs+.0001), log(idata$cfs+.0001))
  UVAUVAsiteNSE[i]<-NSE(idata$UVA_NNpredcfs, idata$cfs)
  UVAUVAsiteLNSE[i]<-NSE(log(idata$UVA_NNpredcfs+.0001), log(idata$cfs+.0001))
}

UVAsites<-unique(NNComparison$site_no)
UVAsitesLNSE<-data.frame(UVAsites,round(UVAUVAsiteLNSE,3),round(USGSUVAsiteLNSE,3))
names(UVAsitesLNSE)<-c("site_no","UVApreds","USGSpreds")
UVAsitesLNSE

round(UVAUVAsiteNSE,3)
round(UUGSUVAsiteNSE,3)

round(UVAUVAsiteLNSE,3)
round(USGSUVAsiteLNSE,3)

##Omit summer
NNComparisonFWS<-NNComparison[NNComparison$season!="summer",]

UUGSUVAsiteNSE<-rep(NA,5)
USGSUVAsiteLNSE<-rep(NA,5)
UVAUVAsiteNSE<-rep(NA,5)
UVAUVAsiteLNSE<-rep(NA,5)

#NSE by site:  "UVA_NFDR" "UVA_PAIN" "UVA_PINE" "UVA_STAN" "UVA_WOR1"
for (i in 1:5){
  idata<-NNComparisonFWS[NNComparisonFWS$site_no==unique(NNComparisonFWS$site_no)[i],]
  UUGSUVAsiteNSE[i]<-NSE(idata$USGS_NNpredcfs, idata$cfs)
  USGSUVAsiteLNSE[i]<-NSE(log(idata$USGS_NNpredcfs+.0001), log(idata$cfs+.0001))
  UVAUVAsiteNSE[i]<-NSE(idata$UVA_NNpredcfs, idata$cfs)
  UVAUVAsiteLNSE[i]<-NSE(log(idata$UVA_NNpredcfs+.0001), log(idata$cfs+.0001))
}

UVAsites<-unique(NNComparisonFWS$site_no)
UVAsitesLNSE<-data.frame(UVAsites,round(UVAUVAsiteLNSE,3),round(USGSUVAsiteLNSE,3))
names(UVAsitesLNSE)<-c("site_no","UVApreds","USGSpreds")
UVAsitesLNSE

round(UVAUVAsiteNSE,3)
round(UUGSUVAsiteNSE,3)

round(UVAUVAsiteLNSE,3)
round(USGSUVAsiteLNSE,3)



