##Regional regression model to predict seasonal LFs
##Annalise Blum
##July 18,2016

library(lme4); library(MASS);library(hydroGOF)

#load cleaned data
load("output/S.WFB.rdata")

S.WFB<-S.WFB[complete.cases(S.WFB),]

####Model selection - all, then stepwise - Pooled model ####
#Kanno used average max T, I find min to increase adjR2 by 0.0073
fit1<-lm(log(min7day)~log(totprecip)+log(avgtmax.T)+
           log(L1totprecip)+log(L2totprecip)+log(L3totprecip)+log(L4totprecip)+log(L1avgtmax.T)+log(L2avgtmax.T)+log(L3avgtmax.T)+log(L4avgtmax.T)+
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
           log(L1totprecip)+log(L2totprecip)+log(L3totprecip)+log(L4totprecip)+log(L1avgtmax.T)+log(L2avgtmax.T)+log(L3avgtmax.T)+log(L4avgtmax.T)+
           log(DRAIN_SQMI)+log(LAT_GAGE)+log(Slope_pct)+log(Aspect_deg)+
           log(Elev_m), data=S.WFB)
summary(fit.an)
ResidPlots(fit.an)
(exp(var(residuals(fit.an)))-1)^.5 #calculate SE-prediction

#check out FE with dummies
fitFE<-lm(log(min7day)~factor(site_no)+log(totprecip)+log(avgtmax.T)+
            log(L1totprecip)+log(L2totprecip)+log(L3totprecip)+log(L4totprecip)+log(L1avgtmax.T)+log(L2avgtmax.T)+log(L3avgtmax.T)+log(L4avgtmax.T)+
            log(DRAIN_SQMI)+log(LAT_GAGE)+ log(LNG_GAGE.T)+log(Slope_pct)+log(Aspect_deg)+
            log(Elev_m), data=S.WFB)
summary(fitFE)
(exp(var(residuals(fitFE)))-1)^.5 #calculate SE-prediction

#try adding average BFI super significant, TOPWET isn't. Only increased adjR2 a little
fitBFI<-lm(log(min7day)~log(totprecip)+
             log(L1totprecip)+log(L2totprecip)+log(L3totprecip)+log(L4totprecip)+log(L1avgtmax.T)+log(L2avgtmax.T)+log(L3avgtmax.T)+log(L4avgtmax.T)+
             log(DRAIN_SQMI)+log(LAT_GAGE)+log(Slope_pct)+log(Aspect_deg)+
             log(Elev_m)+log(BFI_AVE), data=S.WFB)
summary(fitBFI)
(exp(var(residuals(fitBFI)))-1)^.5 #calculate SE-prediction
ResidPlots(fitBFI)

####ME
fit2<-lmer(log(min7day)~(1+log(totprecip)|season)+log(totprecip)+log(avgtmax.T)+
             log(L1totprecip)+log(L2totprecip)+log(L3totprecip)+log(L4totprecip)+log(L1avgtmax.T)+log(L2avgtmax.T)+log(L3avgtmax.T)+log(L4avgtmax.T)+
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

fit4<-lmer(log(min7day)~(1|season)+(0+log(avgtmax.T)+log(totprecip)|season)+log(totprecip)+log(avgtmax.T)+log(DRAIN_SQMI)+log(LAT_GAGE)+
             log(LNG_GAGE.T)+log(SLOPE_PCT)+log(ASPECT_DEGREES)+log(ELEV_SITE_M),data=S.WFB,REML=F)
summary(fit4)
(exp(var(residuals(fit4)))-1)^.5 #calculate SE-prediction

####final Model - ME ####
fitF<-lmer(log(min7day)~(1+log(totprecip)|season)+log(totprecip)+log(avgtmax.T)+
             log(L1totprecip)+log(L2totprecip)+log(L3totprecip)+log(L4totprecip)+log(L1avgtmax.T)+log(L2avgtmax.T)+log(L3avgtmax.T)+log(L4avgtmax.T)+
             log(DRAIN_SQMI)+log(LAT_GAGE)+ log(LNG_GAGE.T)+log(Slope_pct)+log(Aspect_deg)+
             log(Elev_m), data=S.WFB)
summary(fitF)

#### GOF and Residual plots ##
ResidPlots(fitF)

#calculate SE-prediction
(exp(var(residuals(fitF)))-1)^.5 #.85

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

NSE(S.WFB$preds,S.WFB$min7day) #.67
NSE(log(S.WFB$preds),log(S.WFB$min7day)) #.8

#resids - just a few sites? Yes! only 5 sites for the 22 weird residuals
S.WFB$resids<-residuals(fitF)
length(S.WFB$site_no[S.WFB$resids< -2.4])
length(unique(S.WFB$site_no[S.WFB$resids< -2.4]))
badsites<-unique(S.WFB$site_no[S.WFB$resids< -2.4])
USGS_BC[USGS_BC$site_no==badsites[5],] #two small sites, two average, one big

#remove these 5 sites?? ex: d<-d[!(d$A=="B" & d$E==0),]

#### LOO-CV for the gaged sites ####
S.WFB$sNDX<-as.numeric(as.factor(S.WFB$site_no)) #site index
site_no<-unique(S.WFB$site_no) #list of sites

# S.WFB$seasonfac[S.WFB$season=="winter"]<-1
# S.WFB$seasonfac[S.WFB$season=="spring"]<-2
# S.WFB$seasonfac[S.WFB$season=="summer"]<-3
# S.WFB$seasonfac[S.WFB$season=="fall"]<-4

meNSE<-rep(NA,length(site_no)) #initalize vars
meLNSE<-rep(NA,length(site_no))
sNSE<-matrix(data=NA,nrow=length(site_no),4)
sLNSE<-matrix(data=NA,nrow=length(site_no),4)
for (i in 1:length(site_no)){
  #1 Subset data sets
  sitedata<-S.WFB[which(S.WFB$sNDX==i),]
  data_CV <- S.WFB[-which(S.WFB$sNDX==i),] #obtain data set with SITE i omitted

  #2 Estimate model
  CV_ME<- lmer(log(min7day)~(1+log(totprecip)|season)+log(totprecip)+log(avgtmax.T)+
                 log(L1totprecip)+log(L2totprecip)+log(L3totprecip)+log(L4totprecip)+log(L1avgtmax.T)+log(L2avgtmax.T)+log(L3avgtmax.T)+log(L4avgtmax.T)+
                 log(DRAIN_SQMI)+log(LAT_GAGE)+ log(LNG_GAGE.T)+log(Slope_pct)+log(Aspect_deg)+
                 log(Elev_m), data=data_CV)
  
  #3 - Predict annual 7day min flows based on the 3 models and save
  sitedata$ME.pred<-exp(predict(CV_ME,sitedata))
  
  #4 - GOF
  meNSE[i]<-NSE(sitedata$ME.pred,sitedata$min7day)
  meLNSE[i]<-NSE(log(sitedata$ME.pred),log(sitedata$min7day))
  #5 GOF by season
  for (j in 1:4){
    seasondata<- sitedata[which(sitedata$season.f==j),]
    sNSE[i,j]<-NSE(seasondata$ME.pred,seasondata$min7day)
    sLNSE[i,j]<-NSE(log(seasondata$ME.pred),log(seasondata$min7day))
  }
}
boxplot(meLNSE,meNSE)
boxplot(meLNSE,meNSE,ylim=c(-1,1),names=c("LNSE","NSE"),col=c("lightblue","grey"))
abline(0,0)

sum(meNSE< -1)
sum(meLNSE< -1)

#are low flow predictions better at certain times of year??
boxplot(S.LNSE[,1],S.LNSE[,2],S.LNSE[,3],S.LNSE[,4],ylim=c(-1,1))

##
USGS_BC1$meLNSE<-meLNSE
USGS_BC1$meNSE<-meNSE

plot(log(USGS_BC1$DRAIN_SQMI),USGS_BC1$meLNSE,ylab="Log-space NSE",xlab="log(DA in mi2)",main="DA vs LNSE",col="blue")

for (i in 1:length(fishSC1$DRAIN_SQMI)){
  abline(v=log(fishSC1$DRAIN_SQMI[i]),col="darkgrey")
}

USGS_BC1[USGS_BC1$meLNSE< -1,]
#### Predict at fish sites ####
FishPredRR<-exp(predict(fitF,S.WFC,allow.new.levels = T)) #get correct fish data
names(S.WFC)
S.WFC$RRpreds<-FishPredRR

#collapse to annual level and merge in fish sample data
RRA.WFC <- dcast(S.WFC, site_no + year.f ~ season,value.var = "RRpreds") #need to get wide format
head(RRA.WFC)
names(RRA.WFC)<-c("site_no","year.f","RRpredsfall", "RRpredsspring", "RRpredssummer", "RRpredswinter")

#merge in with main data set
class(A.FWC$site_no);class(RRA.WFC$site_no)
class(A.FWC$year.f);class(RRA.WFC$year.f)
names(A.FWC)
A.FWC$site_no<-paste("F_",A.FWC$site_no, sep = "")

A.FWC_RR<-merge(A.FWC,RRA.WFC,by=c("site_no","year.f"))



