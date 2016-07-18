##Regional regression model to predict seasonal LFs
##Annalise Blum
##July 18,2016

library(lme4); library(MASS);library(hydroGOF)

#load cleaned data
load("output/S.WFB.rdata")

####Model selection - all, then stepwise - Pooled model ####
#Kanno used average max T, I find min to increase adjR2 by 0.0073
fit1<-lm(log(min7day)~log(totprecip)+log(avgtmax.T)+log(DRAIN_SQMI)+log(LAT_GAGE)+
           log(LNG_GAGE.T)+log(SLOPE_PCT)+log(ASPECT_DEGREES)+log(ELEV_SITE_M), data=S.WFB)
summary(fit1)

# [1] "site_no"           "year"              "season"            "daysperseason"     "totprecip"        
# [6] "avgtmax"           "avgtmin"           "min7day"           "DRAIN_SQKM"        "HUC02"            
# [11] "LAT_GAGE"          "LNG_GAGE"          "STATE"             "DRAIN_SQMI"        "REACHCODE"        
# [16] "BFI_AVE"           "TOPWET"            "SLOPE_PCT"         "ASPECT_DEGREES"    "ELEV_SITE_M"      
# [21] "ELEV_MEAN_M_BASIN" "LNG_GAGE.T"        "avgtmax.T"         "avgtmin.T"  

#stepwise
step <- stepAIC(fit1, direction="both")
step$anova # display results - just removes site elevation...hmm that should prob stay in though...

#check out FE with dummies
fitFE<-lm(log(min7day)~factor(site_no)+log(totprecip)+log(avgtmax.T)+log(DRAIN_SQMI)+log(LAT_GAGE)+
           log(LNG_GAGE.T)+log(SLOPE_PCT)+log(ASPECT_DEGREES)+log(ELEV_SITE_M), data=S.WFB)
summary(fitFE)

#season dummies - average BFI super significant
fitSD<-lm(log(min7day)~factor(season)+log(totprecip)+log(avgtmax.T)+log(DRAIN_SQMI)+log(LAT_GAGE)+
            log(LNG_GAGE.T)+log(SLOPE_PCT)+log(ASPECT_DEGREES)+log(ELEV_SITE_M), data=S.WFB)
summary(fitSD)

####Model selection - stepwise - site-specific intercepts ####
#how to deal with repeated site-specific vars? 
#divide by DA and use BCs to predict the site-specific intercepts??

fit2<-lmer(log(min7day)~(1+log(totprecip)+log(avgtmax.T)|season)+log(totprecip)+log(avgtmax.T)+log(DRAIN_SQMI)+log(LAT_GAGE)+
             log(LNG_GAGE.T)+log(SLOPE_PCT)+log(ASPECT_DEGREES)+log(ELEV_SITE_M), data=S.WFB)
summary(fit2)
ResidPlots(fit2)

#try when not correlated
fit3<-lmer(log(min7day)~(1|season)+(0+log(totprecip)|season)+(0+log(avgtmax.T)|season)+
             log(totprecip)+log(avgtmax.T)+log(DRAIN_SQMI)+log(LAT_GAGE)+
             log(LNG_GAGE.T)+log(SLOPE_PCT)+log(ASPECT_DEGREES)+log(ELEV_SITE_M), data=S.WFB)
summary(fit3)
ResidPlots(fit3)

##Or just do season-specific regressions?
S.WFB$seasonfac[S.WFB$season=="winter"]<-1
S.WFB$seasonfac[S.WFB$season=="spring"]<-2
S.WFB$seasonfac[S.WFB$season=="summer"]<-3
S.WFB$seasonfac[S.WFB$season=="fall"]<-4

sNSE<-rep(NA,4)
sLNSE<-rep(NA,4)

for (i in 1:4){
  seasondata<- S.WFB[which(S.WFB$seasonfac==i),]
  Sfit<-lm(log(min7day)~log(totprecip)+log(avgtmax.T)+log(DRAIN_SQMI)+log(LAT_GAGE)+
             log(LNG_GAGE.T)+log(SLOPE_PCT)+log(ASPECT_DEGREES)+log(ELEV_SITE_M),
           data=seasondata)
  seasondata$preds<-exp(predict(Sfit))
  sNSE[i]<-NSE(seasondata$preds,seasondata$min7day)
  sLNSE[i]<-NSE(log(seasondata$preds),log(seasondata$min7day))
}

fitF<-lm(log(min7day)~log(totprecip)+log(avgtmax.T)+log(DRAIN_SQMI)+log(LAT_GAGE)+
            log(LNG_GAGE.T)+log(SLOPE_PCT)+log(ASPECT_DEGREES)+log(ELEV_SITE_M),
         data=S.WFB[S.WFB$season=="fall",])
summary(fitF)

falldata<-S.WFB[S.WFB$season=="fall",]
falldata$preds<-predict(fitF)
NSE(falldata$preds,falldata$min7day)
NSE(log(falldata$preds),log(falldata$min7day))

#### GOF and Residual plots ####
ResidPlots(fit2) # a lot of autocorrelation; use mean flow last season as X var??

#first just check predictions
S.WFB$preds<-exp(predict(fit2)) #fit2 fitSD
plot(S.WFB$min7day,S.WFB$preds)
abline(0,1)
NSE(S.WFB$preds,S.WFB$min7day) #.67
NSE(log(S.WFB$preds),log(S.WFB$min7day)) #.8

#### LOO-CV for the gaged sites ####
S.WFB$sNDX<-as.numeric(as.factor(x$site_no)) #site index
site_no<-unique(S.WFB$site_no) #list of sites

meNSE<-rep(NA,length(site_no)) #initalize vars
meLNSE<-rep(NA,length(site_no))
for (i in 1:length(site_no)){
  #1 Subset data sets
  sitedata<-S.WFB[which(S.WFB$sNDX==i),]
  data_CV <- S.WFB[-which(S.WFB$sNDX==i),] #obtain data set with SITE i omitted

  #2 Estimate model
  CV_ME<- lmer(log(min7day)~(1+log(totprecip)+log(avgtmax.T)|season)+log(totprecip)+log(avgtmax.T)+log(DRAIN_SQMI)+log(LAT_GAGE)+
                  log(LNG_GAGE.T)+log(SLOPE_PCT)+log(ASPECT_DEGREES)+log(ELEV_SITE_M),data=data_CV,REML=F)
  
  #3 - Predict annual 7day min flows based on the 3 models and save
  sitedata$ME.pred<-exp(predict(CV_ME,sitedata))
  
  #4 - GOF
  meNSE[i]<-NSE(sitedata$ME.pred,sitedata$min7day)
  meLNSE[i]<-NSE(log(sitedata$ME.pred),log(sitedata$min7day))
}
boxplot(meLNSE,fLNSE)
boxplot(meNSE,fNSE)

#### 4 seasonal model; LOO-CV for the gaged sites ####
S.WFB$sNDX<-as.numeric(as.factor(x$site_no)) #site index
site_no<-unique(S.WFB$site_no) #list of sites

S.NSE<-matrix(data=NA,length(site_no),4) #initalize vars
S.LNSE<-matrix(data=NA,length(site_no),4)
for (i in 1:length(site_no)){
  #1 Subset data sets
  sitedata<-S.WFB[which(S.WFB$sNDX==i),]
  data_CV <- S.WFB[-which(S.WFB$sNDX==i),] #obtain data set with SITE i omitted
  for (j in 1:4){
    Ssitedata<-sitedata[which(sitedata$seasonfac==j),] 
    Sdata_CV <- data_CV[which(data_CV$seasonfac==j),] 
    
    #2 Estimate model
    CV_S<- lm(log(min7day)~log(totprecip)+log(avgtmax.T)+log(DRAIN_SQMI)+log(LAT_GAGE)+
                   log(LNG_GAGE.T)+log(SLOPE_PCT)+log(ASPECT_DEGREES)+log(ELEV_SITE_M),data=Sdata_CV)
    #3 - Predict annual 7day min flows based on the seaonsal model
    Ssitedata$S.pred<-exp(predict(CV_S,Ssitedata)) #predict based on the seasonal model for the seasonal data
   #GOF
    S.NSE[i,j]<-NSE(Ssitedata$S.pred,Ssitedata$min7day)
    S.LNSE[i,j]<-NSE(log(Ssitedata$S.pred),log(Ssitedata$min7day))
     }
}

boxplot(S.LNSE[,1],S.LNSE[,2],S.LNSE[,3],S.LNSE[,4],ylim=c(-1,1))
#well that really didn't work...


#make S.WFB into a list of sites
#begin function here
CV_LFs<-function(x){ #x=sitedata
  x$yrNDX<-as.numeric(as.factor(x$year))
  adjR2_SS<-vector(mode = "numeric", length = length(x$yrNDX))
  SS.pred<-vector(mode = "numeric", length = length(x$yrNDX))
  FE.pred<-vector(mode = "numeric", length = length(x$yrNDX))
  ME.pred<-vector(mode = "numeric", length = length(x$yrNDX))
  site_no<-unique(x$site_no)
  for (i in 1:length(x$min7day)){
    #1 Subset data sets
    sitedata_CV <- x[-which(x$yrNDX==i),] #obtain data set with year i omitted
    #Get CV dataset without just that one site year:
    data_CV1 <- data[-which(data$site_no==site_no),] #obtain data set with year i omitted
    sitedata_CV$yrNDX<-NULL #drop to match dimensions of data (need it later??)
    data_CV<-rbind(data_CV1,sitedata_CV)
    #2 Estimate SS FE and ME models
    CV_SS.lm<-lm(log(min7day)~log(sum_precip)+log(maxT),data=sitedata_CV); #summary(CV_SS.lm) #SS
    adjR2_SS[i]<-summary(CV_SS.lm)$adj.r.squared
    #save adjR2
    CV_FE.lm<-lm(log(min7day)~log(sum_precip)+log(maxT)+factor(site_no),data=data_CV)
    CV_ME<-  lmer(log(min7day)~log.Forest +log(drain_area_va)+log(sum_precip)+log(avg_pet)+log.CONUSWetland+
                    log(maxT)+(1+log(sum_precip)|site_no),data=data_CV,REML=F)
    #3 - Predict annual 7day min flows based on the 3 models and save
    SS.pred[i]<-exp(predict(CV_SS.lm,x[which(x$yrNDX==i),]))
    FE.pred[i]<-exp(predict(CV_FE.lm,x[which(x$yrNDX==i),]))
    ME.pred[i]<-exp(predict(CV_ME,x[which(x$yrNDX==i),]))
  }
  
  #combine predictions into a dataframe
  yrs.rec<-length(x$min7day)
  Preds<-data.frame(x$site_no,x$min7day,SS.pred,FE.pred,ME.pred)
  names(Preds)[1]<-"site_no"; names(Preds)[2]<-"min7day"  #rename  cols in dataframe
  #calc NSE for each prediction
  SS.NSE<-NSE(Preds$SS.pred,Preds$min7day)
  FE.NSE<-NSE(Preds$FE.pred,Preds$min7day)
  ME.NSE<-NSE(Preds$ME.pred,Preds$min7day)
  #Median 7day LFs pred and obs
  Obs.MED<-median(Preds$min7day)
  SS.MED<-median(Preds$SS.pred)
  FE.MED<-median(Preds$FE.pred)
  ME.MED<-median(Preds$ME.pred)
  #average adjR2 of the SS models:
  adjR2_SS<-mean(adjR2_SS)
  #combine
  NSE_MED<-data.frame(site_no,yrs.rec,SS.NSE,FE.NSE,ME.NSE,Obs.MED,SS.MED,FE.MED,ME.MED,adjR2_SS)
  return(NSE_MED)
}

#test<-CV_LFs(x)

L_results<-lapply(list_LF,CV_LFs)