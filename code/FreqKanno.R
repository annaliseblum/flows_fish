# Can I replicate Kanno et al (2016) findings using frequentist regression (just YOY and first pass)

load("~/flows_fish/YK/Site_by_year seasonal climate var standardized 115.rdata")
load("~/flows_fish/YK/countArray 115 sites.rdata")


#countAr - just pull YOY and first pass
FcountAr<-as.data.frame(countAr[,,1,1])
fishsite<- rownames(FcountAr)
FcountAr<-data.frame(cbind(fishsite,FcountAr))
names(FcountAr)

##now reshape to long format
FcountAr_L<-reshape(FcountAr, direction="long", varying=list(names(FcountAr)[2:30]), v.names="YOY_P1", 
        idvar="fishsite", timevar="Year", times=1982:2010)

#merge in reshaped weather data
SumTemp<-cbind(data.frame(fishsite,summerTempAryStd))
SumTemp$fishsite<-as.character(SumTemp$fishsite)
SumTemp<-reshape(SumTemp, direction="long", varying=list(names(SumTemp)[2:30]), v.names="SumTemp", 
                    idvar="fishsite", timevar="Year", times=1982:2010)

FallTemp<-cbind(data.frame(fishsite,fallTempAryStd))
FallTemp$fishsite<-as.character(FallTemp$fishsite)
FallTemp<-reshape(FallTemp, direction="long", varying=list(names(FallTemp)[2:30]), v.names="FallTemp", 
                 idvar="fishsite", timevar="Year", times=1982:2010)

WinTemp<-cbind(data.frame(fishsite,winterTempAryStd))
WinTemp$fishsite<-as.character(WinTemp$fishsite)
WinTemp<-reshape(WinTemp, direction="long", varying=list(names(WinTemp)[2:30]), v.names="WinTemp", 
                  idvar="fishsite", timevar="Year", times=1982:2010)

SprTemp<-cbind(data.frame(fishsite,springTempAryStd))
SprTemp$fishsite<-as.character(SprTemp$fishsite)
SprTemp<-reshape(SprTemp, direction="long", varying=list(names(SprTemp)[2:30]), v.names="SprTemp", 
                  idvar="fishsite", timevar="Year", times=1982:2010)

SumPre<-cbind(data.frame(fishsite,summerPrcpAryStd))
SumPre$fishsite<-as.character(SumPre$fishsite)
SumPre<-reshape(SumPre, direction="long", varying=list(names(SumPre)[2:30]), v.names="SumPre", 
                  idvar="fishsite", timevar="Year", times=1982:2010)

FallPre<-cbind(data.frame(fishsite,fallPrcpAryStd))
FallPre$fishsite<-as.character(FallPre$fishsite)
FallPre<-reshape(FallPre, direction="long", varying=list(names(FallPre)[2:30]), v.names="FallPre", 
                  idvar="fishsite", timevar="Year", times=1982:2010)

WinPre<-cbind(data.frame(fishsite,winterPrcpAryStd))
WinPre$fishsite<-as.character(WinPre$fishsite)
WinPre<-reshape(WinPre, direction="long", varying=list(names(WinPre)[2:30]), v.names="WinPre", 
                  idvar="fishsite", timevar="Year", times=1982:2010)

SprPre<-cbind(data.frame(fishsite,springPrcpAryStd))
SprPre$fishsite<-as.character(SprPre$fishsite)
SprPre<-reshape(SprPre, direction="long", varying=list(names(SprPre)[2:30]), v.names="SprPre", 
                  idvar="fishsite", timevar="Year", times=1982:2010)
head(SprPre)

merges<-merge(FcountAr_L,SumTemp,by=c("fishsite","Year"))
merges<-merge(merges,FallTemp,by=c("fishsite","Year"))
merges<-merge(merges,WinTemp,by=c("fishsite","Year"))
merges<-merge(merges,SprTemp,by=c("fishsite","Year"))
merges<-merge(merges,SumPre,by=c("fishsite","Year"))
merges<-merge(merges,FallPre,by=c("fishsite","Year"))
merges<-merge(merges,WinPre,by=c("fishsite","Year"))
Fmerges<-merge(merges,SprPre,by=c("fishsite","Year"))

#to add in the site characteristics:
# elev=elev.std, lat=lat.std,
# julian=julian.std.ar, prcpTot=prcpTot.std.ar, area=area.std)

#### Regression experiments ####
fit<-lm(YOY_P1~SumPre + FallPre + WinPre + SprPre+SumTemp+FallTemp+WinTemp+SprTemp
        ,data=Fmerges)
summary(fit)

#### poisson regression ####
fit2<-glm(YOY_P1~SumPre + FallPre + WinPre + SprPre+SumTemp+FallTemp+WinTemp+SprTemp
             ,data=Fmerges,family=poisson)
summary(fit2)

#### mixed effects - RIpoisson regression ####
fit3<-glmer(YOY_P1~(1|fishsite)+SumPre + FallPre + WinPre + SprPre+SumTemp+FallTemp+WinTemp+SprTemp
            ,data=Fmerges,family=poisson)
summary(fit3)



