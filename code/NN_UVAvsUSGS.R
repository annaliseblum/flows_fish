
#Systematically compare UVA and USGS NN-DAR preds for UVA sites

load("output/NN.USGS.UVA.rdata")
load("output/NN_UVAUVA.rdata")

names(NN.USGS.UVA)
NN.USGS.UVA<-NN.USGS.UVA[c("site_no","Date","cfs","NNpredcfs")]
names(NN.USGS.UVA)<-c("site_no","Date","cfs","USGS_NNpredcfs")

names(NN_UVAUVA)
NN_UVAUVA<-NN_UVAUVA[c("site_no","Date","NNpredcfs")]
names(NN_UVAUVA)<-c("site_no","Date","UVA_NNpredcfs")

NNComparison<-merge(NN.USGS.UVA,NN_UVAUVA,by=c("site_no","Date"))

#Log space
ggplot(NNComparison, aes(x=log(cfs), y=log(USGS_NNpredcfs)))+geom_point()+ facet_grid(~ site_no)+
  geom_abline(intercept = 0,slope=1)

ggplot(NNComparison, aes(x=log(cfs), y=log(UVA_NNpredcfs)))+geom_point()+ facet_grid(~ site_no)+
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

#NSE by site:  "UVA_NFDR" "UVA_PAIN" "UVA_PINE" "UVA_STAN" "UVA_WOR1"
for (i in 1:5){
  idata<-NNComparison[NNComparison$site_no==unique(NNComparison$site_no)[i],]
  UUGSUVAsiteNSE[i]<-NSE(idata$USGS_NNpredcfs, idata$cfs)
  USGSUVAsiteLNSE[i]<-NSE(log(idata$USGS_NNpredcfs+.0001), log(idata$cfs+.0001))
  UVAUVAsiteNSE[i]<-NSE(idata$UVA_NNpredcfs, idata$cfs)
  UVAUVAsiteLNSE[i]<-NSE(log(idata$UVA_NNpredcfs+.0001), log(idata$cfs+.0001))
}

round(UVAUVAsiteNSE,3)
round(UUGSUVAsiteNSE,3)

round(UVAUVAsiteLNSE,3)
round(USGSUVAsiteLNSE,3)


