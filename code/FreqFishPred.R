##Frequentist fish predictions
##Annalise Blum
##Created Aug 24,2016

load("output/S.WFB.rdata") ##deal with year.f.x and year.f.y sum(SuFa.WFB$year.f.y-SuFa.WFB$year.f.x) they are the same

S.WFB<-S.WFB[complete.cases(S.WFB),]

#data chunks
SuFa.WFB<-S.WFB[S.WFB$season=="summer"|S.WFB$season=="fall",]
WinSp.WFB<-S.WFB[S.WFB$season=="winter"|S.WFB$season=="spring",]

#Final models for low and high flows:
##LOW
#7day min
SDLFs<-lm(log(min7day)~log(Pjune)+log(Pjuly)+log(Paug)+ #check that these are correct year
            log(totprecipL1)+log(totprecipL2)+ #in this case, totprecipL1 is spring Precip, L2 is winter
            log(DA_SQKM)+log(LAT_GAGE)+ log(LNG_GAGE.T)+log(Slope_pct)+log(Elev_m) #+ log(BFI_AVE)
          ,data=SuFa.WFB[SuFa.WFB$season=="summer",]); summary(SDLFs)

SDLFf<-lm(log(min7day)~log(Psept)+log(Poct)+log(Pnov)+ #check that these are correct year
            log(totprecipL1)+log(totprecipL2)+ #in this case, totprecipL1 is spring Precip, L2 is winter
            log(DA_SQKM)+log(LAT_GAGE)+ log(LNG_GAGE.T)+log(Slope_pct)+log(Elev_m) #+ log(BFI_AVE)
          ,data=SuFa.WFB[SuFa.WFB$season=="fall",]); summary(SDLFf)
#3day min
TDLFs<-lm(log(min3day)~log(Pmay)+log(Pjune)+log(Pjuly)+log(Paug)+
            log(Pwinter)+log(DA_SQKM)+log(LAT_GAGE)+ log(LNG_GAGE.T)+log(Slope_pct)+log(Elev_m) #+ log(BFI_AVE)
          ,data=SuFa.WFB[SuFa.WFB$season=="summer",]); summary(TDLFs)

TDLFf<-lm(log(min3day)~log(Pjune)+log(Pjuly)+log(Paug)+log(Psept)+log(Poct)+log(Pnov)+
            log(Pwinter)+log(DA_SQKM)+log(LAT_GAGE)+ log(LNG_GAGE.T)+log(Slope_pct)+log(Elev_m) #+ log(BFI_AVE)
          ,data=SuFa.WFB[SuFa.WFB$season=="fall",]); summary(TDLFf)

##HIGH
#3day max
TDHF3s<-lm(log(max3dayflow)~ log(Pwinter)+log(MaxTwinter+3)+log(Pspring) + 
             log(DA_SQKM)+log(Slope_pct)+log(LAT_GAGE)+ log(LNG_GAGE.T) #log(Aspect_deg)+log(Elev_m)
           , data=WinSp.WFB[WinSp.WFB$season=="spring",]); summary(TDHF3s)

##ME with site-specific intercepts
TDHF3s<-lmer(log(max3dayflow)~ (1|site_no)+log(Pwinter)+log(MaxTwinter+3)+log(Pspring) + 
               log(DA_SQKM)+log(Slope_pct)+log(LAT_GAGE)+ log(LNG_GAGE.T) #log(Aspect_deg)+log(Elev_m)
             , data=WinSp.WFB[WinSp.WFB$season=="spring",]); summary(TDHF3s)

#1day max
#similar to 3day high flows
TDHF1w<-lm(log(maxdayflow)~ log(Pwinter)+log(MaxTwinter+3)+ 
             log(DA_SQKM)+log(Slope_pct)+log(LAT_GAGE)+ log(LNG_GAGE.T)
           , data=WinSp.WFB[WinSp.WFB$season=="winter",]); summary(TDHF1w)

TDHF1s<-lm(log(maxdayflow)~ log(Pwinter)+log(MaxTwinter+3)+log(Pspring)+ 
             log(DA_SQKM)+log(Slope_pct)+log(LAT_GAGE)+ log(LNG_GAGE.T)
           , data=WinSp.WFB[WinSp.WFB$season=="spring",]); summary(TDHF1s)


#### Predict at fish sites ####
SuFa.WFB$PSDLFs
test<-exp(predict(SDLFs,S.WFC,allow.new.levels = T))

FishPredRR<-exp(predict(fitF,S.WFC,allow.new.levels = T)) #get correct fish data
names(S.WFC)
S.WFC$RRpreds<-FishPredRR

#collapse to annual level and merge in fish sample data ##Error in eval(expr, envir, enclos) : object 'year.f' not found ##HERE START
RRA.WFC <- dcast(S.WFC, site_no + year.f ~ season,value.var = "RRpreds") #need to get wide format
head(RRA.WFC)
names(RRA.WFC)<-c("site_no","year.f","RRpredsfall", "RRpredsspring", "RRpredssummer", "RRpredswinter")

#merge in with main data set
class(A.FWC$site_no);class(RRA.WFC$site_no)
class(A.FWC$year.f);class(RRA.WFC$year.f)
names(A.FWC)

A.FWC_RR<-merge(A.FWC,RRA.WFC,by=c("site_no","year.f"))

save(A.FWC_RR,file="output/A.FWC_RR.rdata")

