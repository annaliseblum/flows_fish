##Regional regression model to predict seasonal LFs
##Annalise Blum
##Created: July 18,2016
##Modified last: August 23,2016

library(lme4); library(MASS);library(hydroGOF)

#load cleaned data
load("output/S.WFB.rdata") ##deal with year.f.x and year.f.y sum(SuFa.WFB$year.f.y-SuFa.WFB$year.f.x) they are the same

S.WFB<-S.WFB[complete.cases(S.WFB),]

####Model Selection####
#Just focusing on LFs in summer and fall
SuFa.WFB<-S.WFB[S.WFB$season=="summer"|S.WFB$season=="fall",]
summary(SuFa.WFB)

#possible variables:
# > names(S.WFB)
# [1] "site_no"           "year"              "Pfall"     "Pspring"           "Psummer"           "Pwinter"     "MaxTfall"         
# [8] "MaxTspring"        "MaxTsummer"        "MaxTwinter"  "MinTfall"          "MinTspring"        "MinTsummer"  "MinTwinter"       
# [15] "Pjan"              "Pfeb"              "Pmar"      "Papril"            "Pmay"              "Pjune"      "Pjuly"            
# [22] "Paug"              "Psept"             "Poct"      "Pnov"              "Pdec"              "maxP3fall"  "maxP3spring"      
# [29] "maxP3summer"       "maxP3winter"       "maxP1fall"   "maxP1spring"       "maxP1summer"       "maxP1winter" "season"           
# [36] "avgSflow"          "days.01p"          "min7day"     "min3day"           "days.98p"          "maxdayflow"  "max3dayflow"      
# [43] "year.f"            "DRAIN_SQMI"        "HUC02"       "LAT_GAGE"          "LNG_GAGE"          "REACH_CODE" "Slope_pct"        
# [50] "Aspect_deg"        "Elev_m"            "BFI_AVE"     "TOPWET"            "ELEV_MEAN_M_BASIN" "LNG_GAGE.T"     

#combined summer and fall, which vars are useful? start with seasonal P and T and lags
SDLF1<-lm(log(min7day)~log(Pfall+Psummer)+log(MaxTsummer)+log(MaxTfall)+
           log(Pwinter)+log(Pspring)+log(MaxTwinter)+log(MaxTspring)+
           log(DRAIN_SQMI)+log(LAT_GAGE)+ log(LNG_GAGE.T)+log(Slope_pct)+log(Aspect_deg)+log(Elev_m)
           , data=SuFa.WFB)
summary(SDLF1)
ResidPlots(SDLF1) #lots of auto correlation, not normal with small resids

#remove non-sig vars: any temps,spring precip aspect degree; and try by summer and fall seperately
SDLF2<-lm(log(min7day)~log(Pfall)+log(Psummer)+
            log(Pwinter)+log(DRAIN_SQMI)+log(LAT_GAGE)+ log(LNG_GAGE.T)+log(Slope_pct)+log(Elev_m)
             , data=SuFa.WFB[SuFa.WFB$season=="summer",])
summary(SDLF2)
ResidPlots(SDLF2)

##good! fall precip doesn't explain previous summer flow at all

#try adding the variables I don't have for fish sites just to see how helpful: BFI increased adjR2 from .75 to .79 +log(TOPWET)
SDLF3<-lm(log(min7day)~log(Pfall)+log(Psummer)+
            log(Pwinter)+log(DRAIN_SQMI)+log(LAT_GAGE)+ log(LNG_GAGE.T)+log(Slope_pct)+log(Elev_m) + log(BFI_AVE)
          ,data=SuFa.WFB[SuFa.WFB$season=="summer",])
summary(SDLF3)
ResidPlots(SDLF3)

##try individual months P
SDLFs<-lm(log(min7day)~log(Pmay)+log(Pjune)+log(Pjuly)+log(Paug)+
            log(Pwinter)+log(DRAIN_SQMI)+log(LAT_GAGE)+ log(LNG_GAGE.T)+log(Slope_pct)+log(Elev_m) #+ log(BFI_AVE)
          ,data=SuFa.WFB[SuFa.WFB$season=="summer",])
summary(SDLFs)
ResidPlots(SDLFs)

SDLFf<-lm(log(min7day)~log(Pmay)+log(Pjune)+log(Pjuly)+log(Paug)+log(Psept)+log(Poct)+log(Pnov)+
            log(Pwinter)+log(DRAIN_SQMI)+log(LAT_GAGE)+ log(LNG_GAGE.T)+log(Slope_pct)+log(Elev_m) #+ log(BFI_AVE)
          ,data=SuFa.WFB[SuFa.WFB$season=="fall",])
summary(SDLFf)
ResidPlots(SDLFf)
#may and october P don't matter - maybe because there is so little precip in Oct??

##How well do these models work for 3day mins?
TDLFs<-lm(log(min3day)~log(Pmay)+log(Pjune)+log(Pjuly)+log(Paug)+
            log(Pwinter)+log(DRAIN_SQMI)+log(LAT_GAGE)+ log(LNG_GAGE.T)+log(Slope_pct)+log(Elev_m) # + log(BFI_AVE)
          ,data=SuFa.WFB[SuFa.WFB$season=="summer",])
summary(TDLFs)
ResidPlots(TDLFs)

TDLFf<-lm(log(min3day)~log(Pjune)+log(Pjuly)+log(Paug)+log(Psept)+log(Poct)+log(Pnov)+
            log(Pwinter)+log(DRAIN_SQMI)+log(LAT_GAGE)+ log(LNG_GAGE.T)+log(Slope_pct)+log(Elev_m) # + log(BFI_AVE)
          ,data=SuFa.WFB[SuFa.WFB$season=="fall",])
summary(TDLFf)
ResidPlots(TDLFf)

##How well do these models work for number of days below - something wrong with days.98p variable!!
TDLFs<-lm(log(XXXX)~log(Pmay)+log(Pjune)+log(Pjuly)+log(Paug)+
            log(Pwinter)+log(DRAIN_SQMI)+log(LAT_GAGE)+ log(LNG_GAGE.T)+log(Slope_pct)+log(Elev_m) #+ log(BFI_AVE)
          ,data=SuFa.WFB[SuFa.WFB$season=="summer",])
summary(TDLFs)
ResidPlots(TDLFs)

TDLFf<-lm(log(XXXX)~log(Pjune)+log(Pjuly)+log(Paug)+log(Psept)+log(Poct)+log(Pnov)+
            log(Pwinter)+log(DRAIN_SQMI)+log(LAT_GAGE)+ log(LNG_GAGE.T)+log(Slope_pct)+log(Elev_m) #+ log(BFI_AVE)
          ,data=SuFa.WFB[SuFa.WFB$season=="fall",])
summary(TDLFf)
ResidPlots(TDLFf)

####OLD ###

####Model selection - all, then stepwise - Pooled model ####
#Kanno used average max T, I find min to increase adjR2 by 0.0073
fit1<-lm(log(min7day)~log(totprecip)+log(avgtmax.T)+
           log(L1totprecip)+log(L2totprecip)+log(L3totprecip)+log(L4totprecip)+log(L1avgtmax.T)+log(L2avgtmax.T)
         +log(L3avgtmax.T)+log(L4avgtmax.T)+
           log(DRAIN_SQMI)+log(LAT_GAGE)+ log(LNG_GAGE.T)+log(Slope_pct)+log(Aspect_deg)+
           log(Elev_m), data=S.WFB)
summary(fit1)
ResidPlots(fit1)
#same season seasonal average flow (+log(avgSflow)) increases adjR2 to 84%

#stepwise
step <- stepAIC(fit1, direction="both")
step$anova # display results - just removes same season max T and longitude
(exp(var(residuals(fit1)))-1)^.5 #calculate SE-prediction

#final anova linear model
fit.an<-lm(log(min7day)~log(totprecip)+
           log(L1totprecip)+log(L2totprecip)+log(L3totprecip)+log(L4totprecip)+log(L1avgtmax.T)+log(L2avgtmax.T)
           +log(L3avgtmax.T)+log(L4avgtmax.T)+
           log(DRAIN_SQMI)+log(LAT_GAGE)+log(Slope_pct)+log(Aspect_deg)+
           log(Elev_m), data=S.WFB)
summary(fit.an)
ResidPlots(fit.an)
(exp(var(residuals(fit.an)))-1)^.5 #calculate SE-prediction

#check out FE with dummies
fitFE<-lm(log(min7day)~factor(site_no)+log(totprecip)+log(avgtmax.T)+
            log(L1totprecip)+log(L2totprecip)+log(L3totprecip)+log(L4totprecip)+log(L1avgtmax.T)+log(L2avgtmax.T)
          +log(L3avgtmax.T)+log(L4avgtmax.T)+
            log(DRAIN_SQMI)+log(LAT_GAGE)+ log(LNG_GAGE.T)+log(Slope_pct)+log(Aspect_deg)+
            log(Elev_m), data=S.WFB)
summary(fitFE)
(exp(var(residuals(fitFE)))-1)^.5 #calculate SE-prediction

#check out SEASONAL dummies
fitSeaD<-lm(log(min7day)~factor(season)+log(totprecip)+log(avgtmax.T)+
            log(L1totprecip)+log(L2totprecip)+log(L3totprecip)+log(L4totprecip)+log(L1avgtmax.T)+log(L2avgtmax.T)
            +log(L3avgtmax.T)+log(L4avgtmax.T)+
            log(DRAIN_SQMI)+log(LAT_GAGE)+ log(LNG_GAGE.T)+log(Slope_pct)+log(Aspect_deg)+
            log(Elev_m), data=S.WFB)
summary(fitSeaD)
(exp(var(residuals(fitFE)))-1)^.5 #calculate SE-prediction
ResidPlots(fitSeaD)

#try adding average BFI super significant, TOPWET isn't. Only increased adjR2 a little
fitBFI<-lm(log(min7day)~log(totprecip)+
             log(L1totprecip)+log(L2totprecip)+log(L3totprecip)+log(L4totprecip)+log(L1avgtmax.T)+log(L2avgtmax.T)
           +log(L3avgtmax.T)+log(L4avgtmax.T)+
             log(DRAIN_SQMI)+log(LAT_GAGE)+log(Slope_pct)+log(Aspect_deg)+
             log(Elev_m)+log(BFI_AVE), data=S.WFB)
summary(fitBFI)
(exp(var(residuals(fitBFI)))-1)^.5 #calculate SE-prediction
ResidPlots(fitBFI)

####ME - site-specific intercepts and slope on P, season-specific dummies
fit2<-lmer(log(min7day)~(1+log(totprecip)|site_no)+(1|season)+log(totprecip)+log(avgtmax.T)+
             log(L1totprecip)+log(L2totprecip)+log(L3totprecip)+log(L4totprecip)+log(L1avgtmax.T)+log(L2avgtmax.T)
           +log(L3avgtmax.T)+log(L4avgtmax.T)+
             log(DRAIN_SQMI)+log(LAT_GAGE)+ log(LNG_GAGE.T)+log(Slope_pct)+log(Aspect_deg)+
             log(Elev_m), data=S.WFB)
summary(fit2)
ResidPlots(fit2)
hist(resid(fit2))
residFit2<-resid(fit2)
(exp(var(residuals(fit2)))-1)^.5 #calculate SE-prediction

issuesS<-unique(S.WFB$site_no[S.WFB$residFit2< -2]) #8 (of 29) sites account for the 41 residuals under -2
length(unique(S.WFB$site_no))
summary(USGS_BC) #are these sites different somehow??
summary(S.WFB$ELEV_SITE_M[S.WFB$site_no==issuesS])
S.WFB2<-S.WFB[which(S.WFB$site_no!=issuesS),]

#try when not correlated
fit3<-lmer(log(min7day)~(1|season)+(0+log(totprecip)|season)+(0+log(avgtmax.T)|season)+
             log(totprecip)+log(avgtmax.T)+log(DRAIN_SQMI)+log(LAT_GAGE)+
             log(LNG_GAGE.T)+log(SLOPE_PCT)+log(ASPECT_DEGREES)+log(ELEV_SITE_M), data=S.WFB)
summary(fit3)
ResidPlots(fit3)
(exp(var(residuals(fit3)))-1)^.5 #calculate SE-prediction

fit4<-lmer(log(min7day)~(1|season)+(0+log(avgtmax.T)+log(totprecip)|season)+log(totprecip)+log(avgtmax.T)+
             log(DRAIN_SQMI)+log(LAT_GAGE)+
             log(LNG_GAGE.T)+log(SLOPE_PCT)+log(ASPECT_DEGREES)+log(ELEV_SITE_M),data=S.WFB,REML=F)
summary(fit4)
(exp(var(residuals(fit4)))-1)^.5 #calculate SE-prediction

### add lagged average seasonal flow
#same season average flow reduces pred-Cv to .68
#lag 7daymin and avgSflow flows

#sort data
S.WFBsort<-S.WFB[order(S.WFB$site_no,as.numeric(S.WFB$year),S.WFB$season.f),] #make sure order is correct
#S.WFBsort[1:30,]; tail(S.WFBsort) #spot check

#slide data
S.WFB_LF<-slide(S.WFBsort, Var= "avgSflow", GroupVar= "site_no", NewVar= "avgSflowL1", slideBy = -1)

fitLF<-lmer(log(min7day)~(1|site_no)+(0+log(totprecip)|site_no)+(1|season)+log(totprecip)+log(avgtmax.T)+
             log(L1totprecip)+log(L2totprecip)+log(L3totprecip)+log(L4totprecip)+log(L1avgtmax.T)
            log(L2avgtmax.T)+log(L3avgtmax.T)+log(L4avgtmax.T)+
             log(DRAIN_SQMI)+log(LAT_GAGE)+ log(LNG_GAGE.T)+log(Slope_pct)+log(Aspect_deg)+ log(avgSflow)+
             log(Elev_m), data=S.WFB_LF)
summary(fitLF)
#ResidPlots(fitLF)
(exp(var(residuals(fitLF)))-1)^.5 #same month seasonal flow much biger t (25) compared to lagged (t=8)

####final Model - ME ####
fitF<-lmer(log(min7day)~(1|site_no)+(0+log(totprecip)|site_no)+(1|season)+log(totprecip)+log(avgtmax.T)+
             log(L1totprecip)+log(L2totprecip)+log(L3totprecip)+log(L4totprecip)+log(L1avgtmax.T)
           +log(L2avgtmax.T)+log(L3avgtmax.T)+log(L4avgtmax.T)+
             log(DRAIN_SQMI)+log(LAT_GAGE)+ log(LNG_GAGE.T)+log(Slope_pct)+log(Aspect_deg)+
             log(Elev_m), data=S.WFB)
summary(fitF)

##neither Slope_pct nor Aspect_deg is statistically sig (t values < -.2)

#### GOF and Residual plots ##
ResidPlots(fitF)

#calculate SE-prediction
(exp(var(residuals(fitF)))-1)^.5 #.77 with corr intercept and slope for site; .79 with uncorr (was .85)

#not sure which or how many of the  random effects should be correlated
#somewhat arbitrarily chose this correlation between RE bc lowest (marginally  SE-pred)
#remaining issues:
#how to deal with repeated site-specific vars? 
#divide by DA and use BCs to predict the site-specific intercepts??

#first just check predictions
S.WFB$preds<-exp(predict(fitF)) #fit2 fitSD
plot(S.WFB$min7day,S.WFB$preds)
abline(0,1)
plot(log(S.WFB$min7day),log(S.WFB$preds)) #.8
abline(0,1)

NSE(S.WFB$preds,S.WFB$min7day) #.76 (was .67)
NSE(log(S.WFB$preds),log(S.WFB$min7day)) #.86 (was .8)

#resids - just a few sites? Yes! only 7 sites for the 22 weird residuals
S.WFB$resids<-residuals(fitF)
length(S.WFB$site_no[S.WFB$resids< -2.4])
length(unique(S.WFB$site_no[S.WFB$resids< -2.4]))
badsites<-unique(S.WFB$site_no[S.WFB$resids< -2.4])
USGS_BC[USGS_BC$site_no==badsites[5],] #two small sites, two average, one big

#remove these 5 sites?? ex: d<-d[!(d$A=="B" & d$E==0),]

####Just fall Low flows ####
fall.WFB<-S.WFB[S.WFB$season=="fall",] #pull just fall flows

lmfitfall<-lm(log(min7day)~log(totprecip)+log(avgtmax.T)+
                log(L1totprecip)+log(L2totprecip)+log(L3totprecip)+log(L4totprecip)+log(L1avgtmax.T)+
                log(L2avgtmax.T)+log(L3avgtmax.T)+log(L4avgtmax.T)+
                log(DRAIN_SQMI)+log(LAT_GAGE)+ log(LNG_GAGE.T)+log(Slope_pct)+log(Aspect_deg)+
                log(Elev_m), data=fall.WFB)
summary(lmfitfall)

##removing non-significant ones:
lmfitfall<-lm(log(min7day)~log(totprecip)+
                log(L1totprecip)+log(L3totprecip)+log(L4totprecip)+
                log(DRAIN_SQMI)+log(LAT_GAGE)+ log(LNG_GAGE.T)+log(Slope_pct)+
                log(Elev_m), data=fall.WFB)
summary(lmfitfall)
ResidPlots(lmfitfall)

#log(L1avgtmax.T)+  #log(L4avgtmax.T) significant, but because corr with L1maxT is .73

fitfall<-lmer(log(min7day)~(1|site_no)+(0+log(totprecip)|site_no)+log(totprecip)+log(avgtmax.T)+
                log(L1totprecip)+log(L2totprecip)+log(L3totprecip)+log(L4totprecip)+
                log(DRAIN_SQMI)+log(LAT_GAGE)+ log(LNG_GAGE.T)+log(Elev_m)
                , data=fall.WFB)

summary(fitfall)
ResidPlots(fitfall)

#### LOO-CV for the gaged sites ####
S.WFB$sNDX<-as.numeric(as.factor(S.WFB$site_no)) #site index
site_no<-unique(S.WFB$site_no) #list of sites

meNSE<-rep(NA,length(site_no)) #initalize vars
meLNSE<-rep(NA,length(site_no))
sNSE<-matrix(data=NA,nrow=length(site_no),4)
sLNSE<-matrix(data=NA,nrow=length(site_no),4)

for (i in 1:length(site_no)){
  #1 Subset data sets
  sitedata<-S.WFB[which(S.WFB$sNDX==i),]
  data_CV <- S.WFB[-which(S.WFB$sNDX==i),] #obtain data set with SITE i omitted

  #2 Estimate model
  CV_ME<- lmer(log(min7day)~(1+log(totprecip)|site_no)+(1|season)+log(totprecip)+log(avgtmax.T)+
                 log(L1totprecip)+log(L2totprecip)+log(L3totprecip)+log(L4totprecip)+log(L1avgtmax.T)+
                 log(L2avgtmax.T)+log(L3avgtmax.T)+log(L4avgtmax.T)+
                 log(DRAIN_SQMI)+log(LAT_GAGE)+ log(LNG_GAGE.T)+log(Slope_pct)+log(Aspect_deg)+
                 log(Elev_m), data=data_CV)
  
  #3 - Predict annual 7day min flows based on the 3 models and save
  sitedata$ME.pred<-exp(predict(CV_ME,sitedata,allow.new.levels = T))
  
  #4 - GOF
  meNSE[i]<-NSE(sitedata$ME.pred,sitedata$min7day)
  meLNSE[i]<-NSE(log(sitedata$ME.pred),log(sitedata$min7day))
}

boxplot(meLNSE,meNSE)
boxplot(meLNSE,meNSE,ylim=c(-1,1),names=c("LNSE","NSE"),col=c("lightblue","grey"))
abline(0,0)

sum(meNSE< -1)
sum(meLNSE< -1)
# 
# #are low flow predictions better at certain times of year??
# boxplot(S.LNSE[,1],S.LNSE[,2],S.LNSE[,3],S.LNSE[,4],ylim=c(-1,1))

##
USGS_BC1$meLNSE<-meLNSE
USGS_BC1$meNSE<-meNSE

plot(log(USGS_BC1$DRAIN_SQMI),USGS_BC1$meLNSE,ylab="Log-space NSE",xlab="log(DA in mi2)",main="DA vs LNSE",col="blue")

for (i in 1:length(fishSC1$DRAIN_SQMI)){
  abline(v=log(fishSC1$DRAIN_SQMI[i]),col="darkgrey")
}

USGS_BC1[USGS_BC1$meLNSE< -1,]
