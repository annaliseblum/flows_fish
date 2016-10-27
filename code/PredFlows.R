##Flow Predictions: 4 types of metrics x 4 seasons = 16 types of flows to predict
#Build and assess models of flow
###Impact of Extreme Streamflows on Brook Trout Young-of-Year Abundance
### Annalise G Blum
##Created: Oct 5,2016; Last modified: Oct 18, 2016
##Data sets created in this file: 
#To dos:
#FIX GLM PREDS section 5
# start with Section 6 write regs
#RENAME glm.DuSum TOO SIMILAR TO DURATION

library(lme4); library(MASS);library(hydroGOF);library(stargazer)

#### 1 - Load Data  ####
load("output/S.Flow.rdata") #Seasonal flow data
load("output/S.Fish.rdata") #Seasonal fish data

#names(S.Flow) #see what variables we have to work with
#summary(S.Flow)
#View(S.Flow[S.Flow$type=="UVA",])

#UVA sites because they are missing REACH_CODE, Slope_pct, Aspect_deg, BFI_AVE, TOPWET
#Otherwise only vars with NAs are totprecipL1,totprecipL2,avgtmaxL1.T,avgtmaxL2.T and Pjune-Pdec
S.Flow$REACH_CODE<-NULL
S.Flow$Slope_pct<-NULL
S.Flow$Aspect_deg<-NULL
S.Flow$BFI_AVE<-NULL
S.Flow$TOPWET<-NULL
S.Flow$HUC02<-NULL
summary(S.Flow)
#now will just lose 60 observations because of 2 lags in weather
S.Flow<-S.Flow[complete.cases(S.Flow),]

#do the same for fish data
S.Fish$REACH_CODE<-NULL
S.Fish$Slope_pct<-NULL
S.Fish$Aspect_deg<-NULL
S.Fish$BFI_AVE<-NULL
S.Fish$TOPWET<-NULL
S.Fish$HUC02<-NULL

##Make months into Month1-3 for each season to make stargazer results look prettier
S.Flow$PMonth1<-"NA"; S.Flow$PMonth2<-"NA"; S.Flow$PMonth3<-"NA"
S.Flow$PMonth1<-ifelse(S.Flow$Nseason==1,S.Flow$Pjune,ifelse(S.Flow$Nseason==2,S.Flow$Psept,ifelse(S.Flow$Nseason==3,S.Flow$Pdec,S.Flow$Pmar)))
S.Flow$PMonth2<-ifelse(S.Flow$Nseason==1,S.Flow$Pjuly,ifelse(S.Flow$Nseason==2,S.Flow$Poct,ifelse(S.Flow$Nseason==3,S.Flow$Pjan,S.Flow$Papril)))
S.Flow$PMonth3<-ifelse(S.Flow$Nseason==1,S.Flow$Paug,ifelse(S.Flow$Nseason==2,S.Flow$Pnov,ifelse(S.Flow$Nseason==3,S.Flow$Pfeb,S.Flow$Pmay)))
#### 2 - Skip this section - Once have Preds, do Plots and NSE diagnostics that will be used (paste in throughout) ####
#45 USGS and 5 UVA sites
Preds<-S.Flow$HFPreds #DumPreds #AvgPreds
Obs<-S.Flow$max3dayflow # Drought # and Flood #avgSflow
#Metric<-"Avg Seasonal Flow"

#real space
NSE(Preds,Obs) #NSE
plot(Obs,Preds,main=bquote("NSE="~ .(round(NSE(log(Preds),log(Obs)),3)))) #plots
abline(0,1)
#bquote("S-W p=" ~.(round(shapiro.test(resid)$p.value,digits=4))),adj=c(-.2, 1.5))

#LOG space
NSE(log(Preds),log(Obs)) #LNSE
plot(log(Obs),log(Preds),main=paste(Metric,(bquote("LNSE="~.(round(NSE(log(Preds),log(Obs)),3))))))
abline(0,1)

##All seasonal data
##Just UVA sites
NSE(log(Preds[S.Flow$type=="UVA"]),log(Obs[S.Flow$type=="UVA"])) #LNSE
plot(log(Obs[S.Flow$type=="UVA"]),log(Preds[S.Flow$type=="UVA"]),
     main=bquote("UVA only LNSE="~ .(round(NSE(log(Preds),log(Obs)),3)))) #plots
abline(0,1)

#NSE
NSE(S.Flow$AvgPreds,S.Flow$avgSflow)
NSE(log(S.Flow$AvgPreds),log(S.Flow$avgSflow))
pdf(file="plots/ObsPredavgS.pdf")
plot(log(S.Flow$avgSflow),log(S.Flow$AvgPreds),main="Observed vs Predicted (LOG) Seasonal Flow LNSE=0.91 (n=50)")
abline(0,1)
dev.off()

#how well does the model do for the UVA sites?
NSE(log(S.Flow$AvgPreds[S.Flow$type=="UVA"]),log(S.Flow$avgSflow[S.Flow$type=="UVA"])) #still 0.63, not bad!
#### 3 - Metric 1: Average seasonal flows  ####
S.Flow$AvgPreds<-NA #create prediction variables

#linear power law regional regression models predicting each season's flow seperately
lm.AvgSum<-lm(log(avgSflow)~log(DA_SQKM)+log(Elev_m)+log(LAT_GAGE)+log(LNG_GAGE.T)
            +log(PMonth1)+log(PMonth2)+log(PMonth3)+log(totprecipL1)+log(totprecipL2)
            #+log(avgtmax.T)+log(avgtmaxL1.T)+log(avgtmaxL2.T)
            ,data=S.Flow[S.Flow$season=="summer"&S.Flow$type=="USGS",])
summary(lm.AvgSum); ResidPlots(lm.AvgSum)
S.Flow$AvgPreds[S.Flow$season=="summer"]<-exp(predict(lm.AvgSum,S.Flow[S.Flow$season=="summer",],allow.new.levels = T)) #predict

lm.AvgF<-lm(log(avgSflow)~log(DA_SQKM)+log(Elev_m)+log(LAT_GAGE)+log(LNG_GAGE.T)
            +log(PMonth1)+log(PMonth2)+log(PMonth3)+log(totprecipL1)+log(totprecipL2)
              #+log(avgtmax.T)+log(avgtmaxL1.T)+log(avgtmaxL2.T)
            ,data=S.Flow[S.Flow$season=="fall"&S.Flow$type=="USGS",])
summary(lm.AvgF); ResidPlots(lm.AvgF)
S.Flow$AvgPreds[S.Flow$season=="fall"]<-exp(predict(lm.AvgF,S.Flow[S.Flow$season=="fall",],allow.new.levels = T)) #predict

lm.AvgW<-lm(log(avgSflow)~log(DA_SQKM)+log(Elev_m)+log(LAT_GAGE)+log(LNG_GAGE.T)
            +log(PMonth1)+log(PMonth2)+log(PMonth3)+log(totprecipL1)+log(totprecipL2)
              #+log(avgtmax.T)+log(avgtmaxL1.T)+log(avgtmaxL2.T)
            ,data=S.Flow[S.Flow$season=="winter"&S.Flow$type=="USGS",])
summary(lm.AvgW); ResidPlots(lm.AvgW)
S.Flow$AvgPreds[S.Flow$season=="winter"]<-exp(predict(lm.AvgW,S.Flow[S.Flow$season=="winter",],allow.new.levels = T)) #predict

lm.AvgSp<-lm(log(avgSflow)~log(DA_SQKM)+log(Elev_m)+log(LAT_GAGE)+log(LNG_GAGE.T)
             +log(PMonth1)+log(PMonth2)+log(PMonth3)+log(totprecipL1)+log(totprecipL2)
              #+log(avgtmax.T)+log(avgtmaxL1.T)+log(avgtmaxL2.T)
            ,data=S.Flow[S.Flow$season=="spring"&S.Flow$type=="USGS",])
summary(lm.AvgSp); ResidPlots(lm.AvgSp)
S.Flow$AvgPreds[S.Flow$season=="spring"]<-exp(predict(lm.AvgSp,S.Flow[S.Flow$season=="spring",],allow.new.levels = T)) #predict at USGS and UVA sites

# #predict at fish sites: - vars in different order - does that matter?? Seems not to matter!
# S.Fish$AvgPreds[S.Fish$season=="summer"]<-exp(predict(lm.AvgSum,S.Fish[S.Fish$season=="summer",],allow.new.levels = T))
# S.Fish$AvgPreds[S.Fish$season=="fall"]<-exp(predict(lm.AvgF,S.Fish[S.Fish$season=="fall",],allow.new.levels = T))
# S.Fish$AvgPreds[S.Fish$season=="winter"]<-exp(predict(lm.AvgW,S.Fish[S.Fish$season=="winter",],allow.new.levels = T))
# S.Fish$AvgPreds[S.Fish$season=="spring"]<-exp(predict(lm.AvgSp,S.Fish[S.Fish$season=="spring",],allow.new.levels = T))

#Export regression results
stargazer(lm.AvgSum,lm.AvgF,lm.AvgW,lm.AvgSp,
          title="Average seasonal flows - USGS sites only",
          align=T,star.char = c("", "", ""),omit.table.layout = "n",
          dep.var.labels = "Seasonal Average Flow",
          column.labels = c("Summer","Fall","Winter","Spring"),
          no.space=T,dep.var.caption = "", report = "vct*", digits=2,
          model.numbers = F,type="text",out="output/regTXT/AvgSeason.txt")

#plots
plot(S.Flow$avgSflow[S.Flow$season=="spring"],S.Flow$AvgPreds[S.Flow$season=="spring"])
abline(0,1)

plot(log(S.Flow$avgSflow[S.Flow$type=="USGS"]),log(S.Flow$AvgPreds[S.Flow$type=="USGS"]),
     main="45 USGS sites: Obs vs Pred Seasonal Flow LNSE=0.89")
abline(0,1)
NSE(log(S.Flow$AvgPreds[S.Flow$type=="USGS"]),log(S.Flow$avgSflow[S.Flow$type=="USGS"]))

plot(log(S.Flow$avgSflow[S.Flow$type=="UVA"]),log(S.Flow$AvgPreds[S.Flow$type=="UVA"]),
     main="CV for 5 UVA sites: Obs vs Pred Seasonal Flow LNSE=0.63")
abline(0,1)
NSE(log(S.Flow$AvgPreds[S.Flow$type=="UVA"]),log(S.Flow$avgSflow[S.Flow$type=="UVA"]))
#### 4 - Metric 2: Magnitude (Intensity/Severity) LF and HFs  ####
S.Flow$LF7Preds<-NA #create prediction variables
S.Flow$HF3Preds<-NA #create prediction variables

#linear power law regional regression models predicting each season's flow seperately

#LFs: summer, fall
lm.LF7.Sum<-lm(log(min7day)~log(DA_SQKM)+log(Elev_m)+log(LAT_GAGE)+log(LNG_GAGE.T)
               +log(PMonth1)+log(PMonth2)+log(PMonth3)+log(totprecipL1)+log(totprecipL2)
                 #+log(avgtmax.T)+log(avgtmaxL1.T)+log(avgtmaxL2.T)
               ,data=S.Flow[S.Flow$season=="summer"&S.Flow$type=="USGS",])
summary(lm.LF7.Sum); ResidPlots(lm.LF7.Sum)
S.Flow$LF7Preds[S.Flow$season=="summer"]<-exp(predict(lm.LF7.Sum,S.Flow[S.Flow$season=="summer",],allow.new.levels = T)) #predict

lm.LF7.F<-lm(log(min7day)~log(DA_SQKM)+log(Elev_m)+log(LAT_GAGE)+log(LNG_GAGE.T)
             +log(PMonth1)+log(PMonth2)+log(PMonth3)+log(totprecipL1)+log(totprecipL2)
               #+log(avgtmax.T)+log(avgtmaxL1.T)+log(avgtmaxL2.T)
             ,data=S.Flow[S.Flow$season=="fall"&S.Flow$type=="USGS",])
summary(lm.LF7.F); ResidPlots(lm.LF7.F)
S.Flow$LF7Preds[S.Flow$season=="fall"]<-exp(predict(lm.LF7.F,S.Flow[S.Flow$season=="fall",],allow.new.levels = T)) #predict

#HFs: winter, spring
lm.HF3.W<-lm(log(max3dayflow)~log(DA_SQKM)+log(Elev_m)+log(LAT_GAGE)+log(LNG_GAGE.T)
             +log(PMonth1)+log(PMonth2)+log(PMonth3)+log(totprecipL1)+log(totprecipL2)+log(maxP3day)
               #+log(avgtmax.T)+log(avgtmaxL1.T)+log(avgtmaxL2.T)
             ,data=S.Flow[S.Flow$season=="winter"&S.Flow$type=="USGS",])
summary(lm.HF3.W); ResidPlots(lm.HF3.W)
S.Flow$HF3Preds[S.Flow$season=="winter"]<-exp(predict(lm.HF3.W,S.Flow[S.Flow$season=="winter",],allow.new.levels = T)) #predict

lm.HF3.Sp<-lm(log(max3dayflow)~log(DA_SQKM)+log(Elev_m)+log(LAT_GAGE)+log(LNG_GAGE.T)
              +log(PMonth1)+log(PMonth2)+log(PMonth3)+log(totprecipL1)+log(totprecipL2)+log(maxP3day)
                #+log(avgtmax.T)+log(avgtmaxL1.T)+log(avgtmaxL2.T)
              ,data=S.Flow[S.Flow$season=="spring"&S.Flow$type=="USGS",])
summary(lm.HF3.Sp); ResidPlots2(lm.HF3.Sp)
S.Flow$HF3Preds[S.Flow$season=="spring"]<-exp(predict(lm.HF3.Sp,S.Flow[S.Flow$season=="spring",],allow.new.levels = T)) #predict

#Export regression results
stargazer(lm.LF7.Sum,lm.LF7.F,lm.HF3.W,lm.HF3.Sp,
          title="Magnitude of Low Flows (7daymin) or High flows (3daymax)- USGS sites only",
          align=T,star.char = c("", "", ""),omit.table.layout = "n",
          #dep.var.labels = "", #model.names = FALSE,
          column.labels = c("Summer","Fall","Winter","Spring"),
          no.space=T,dep.var.caption = "", report = "vct*", digits=2,
          model.numbers = F,type="text",out="output/regTXT/MagLF7HF3.txt")

#Obs vs Preds plots
plot(S.Flow$max3dayflow[S.Flow$season=="spring"|S.Flow$season=="winter"],S.Flow$HF3Preds[S.Flow$season=="spring"|S.Flow$season=="winter"])
abline(0,1)

plot(log(S.Flow$max3dayflow[S.Flow$type=="USGS" & (S.Flow$season=="spring"|S.Flow$season=="winter")]),
     log(S.Flow$HF3Preds[S.Flow$type=="USGS"& (S.Flow$season=="spring"|S.Flow$season=="winter")]),
     main="45 USGS sites: Winter and Spring 3day high flow LNSE=0.89",xlab="log Observed high flows",ylab="log Predicted high flows")
abline(0,1)
NSE(log(S.Flow$HF3Preds[S.Flow$type=="USGS"& (S.Flow$season=="spring"|S.Flow$season=="winter")]),
    log(S.Flow$max3dayflow[S.Flow$type=="USGS"& (S.Flow$season=="spring"|S.Flow$season=="winter")]))

plot(log(S.Flow$max3dayflow[S.Flow$type=="UVA"& (S.Flow$season=="spring"|S.Flow$season=="winter")]),
     log(S.Flow$HF3Preds[S.Flow$type=="UVA"& (S.Flow$season=="spring"|S.Flow$season=="winter")]),
     main="CV for 5 UVA sites:Winter and Spring 3day high flow LNSE=0.60",
     xlab="log Observed high flows",ylab="log Predicted high flows")
abline(0,1)
NSE(log(S.Flow$HF3Preds[S.Flow$type=="UVA"& (S.Flow$season=="spring"|S.Flow$season=="winter")]),
    log(S.Flow$max3dayflow[S.Flow$type=="UVA"& (S.Flow$season=="spring"|S.Flow$season=="winter")]))

#plot residuals by season
par(mfrow = c(4, 2),mar=c(4,4,3,4)) # c(bottom, left, top, right) 
#summer
qqnorm(lm.LF7.Sum$resid,xlab="Residual",ylab="Percent",main="Normal Probability Plot")
qqline(lm.LF7.Sum$resid) 
grid()
plot(predict(lm.LF7.Sum),lm.LF7.Sum$resid,xlab="Fitted Value",ylab="Residual",main="Versus Fits")
abline(0,0)
#fall
qqnorm(lm.LF7.F$resid,xlab="Residual",ylab="Percent",main="")
qqline(lm.LF7.F$resid) 
grid()
plot(predict(lm.LF7.Sum),lm.LF7.Sum$resid,xlab="Fitted Value",ylab="Residual")
abline(0,0)
#winter
qqnorm(lm.HF3.W$resid,xlab="Residual",ylab="Percent",main="")
qqline(lm.HF3.W$resid) 
grid()
plot(predict(lm.HF3.W),lm.HF3.W$resid,xlab="Fitted Value",ylab="Residual")
abline(0,0)
#spring
qqnorm(lm.HF3.Sp$resid,xlab="Residual",ylab="Percent",main="")
qqline(lm.HF3.Sp$resid) 
grid()
plot(predict(lm.HF3.Sp),lm.HF3.Sp$resid,xlab="Fitted Value",ylab="Residual")
abline(0,0)
#### 5 - Metric 3: Dummy variable for drought or flood: LOGISTIC  ####
S.Flow$DroughtPreds<-NA #create prediction variables
S.Flow$FloodPreds<-NA

#Drought: summer, fall - RENAME glm.DuSum TOO SIMILAR TO DURATION
glm.DuSum<-glm(Drought~log(DA_SQKM)+log(Elev_m)+log(LAT_GAGE)+log(LNG_GAGE.T)
              +log(PMonth1)+log(PMonth2)+log(PMonth3)+log(totprecipL1)+log(totprecipL2)+
                +log(avgtmax.T)+log(avgtmaxL1.T)+log(avgtmaxL2.T)
              ,data=S.Flow[S.Flow$season=="summer"&S.Flow$type=="USGS",],family=binomial(logit))
summary(glm.DuSum)
S.Flow$DroughtPreds[S.Flow$season=="summer"]<-predict.glm(glm.DuSum,S.Flow[S.Flow$season=="summer",],allow.new.levels = T,type="response") #predict

glm.DuF<-glm(Drought~log(DA_SQKM)+log(Elev_m)+log(LAT_GAGE)+log(LNG_GAGE.T)
            +log(PMonth1)+log(PMonth2)+log(PMonth3)+log(totprecipL1)+log(totprecipL2)+
              +log(avgtmax.T)+log(avgtmaxL1.T)+log(avgtmaxL2.T)
            ,data=S.Flow[S.Flow$season=="fall"&S.Flow$type=="USGS",],family="binomial")
summary(glm.DuF)
S.Flow$DroughtPreds[S.Flow$season=="fall"]<-exp(predict(glm.DuF,S.Flow[S.Flow$season=="fall",],allow.new.levels = T)) #predict

#Flood: winter, spring
glm.DuW<-glm(Flood~log(DA_SQKM)+log(Elev_m)+log(LAT_GAGE)+log(LNG_GAGE.T)
            +log(PMonth1)+log(PMonth2)+log(PMonth3)+log(totprecipL1)+log(totprecipL2)+
              +log(avgtmax.T)+log(avgtmaxL1.T)+log(avgtmaxL2.T)
            ,data=S.Flow[S.Flow$season=="winter"&S.Flow$type=="USGS",],family="binomial")
summary(glm.DuW)
S.Flow$FloodPreds[S.Flow$season=="winter"]<-exp(predict(glm.DuW,S.Flow[S.Flow$season=="winter",],allow.new.levels = T)) #predict

glm.DuSp<-glm(Flood~log(DA_SQKM)+log(Elev_m)+log(LAT_GAGE)+log(LNG_GAGE.T)
             +log(PMonth1)+log(PMonth2)+log(PMonth3)+log(totprecipL1)+log(totprecipL2)+
               +log(avgtmax.T)+log(avgtmaxL1.T)+log(avgtmaxL2.T)
             ,data=S.Flow[S.Flow$season=="spring"&S.Flow$type=="USGS",],family="binomial")
summary(glm.DuSp)
S.Flow$FloodPreds[S.Flow$season=="spring"]<-exp(predict(glm.DuSp,S.Flow[S.Flow$season=="spring",],allow.new.levels = T)) #predict

#Export regression results
stargazer(glm.DuSum,glm.DuF,glm.DuW,glm.DuSp,
          title="Drought (flow<.01p MA-FDC) or Flood (flow>.99p MA-FDC)- USGS sites only",
          align=T,star.char = c("", "", ""),omit.table.layout = "n",
          #dep.var.labels = "", #model.names = FALSE,
          column.labels = c("Summer","Fall","Winter","Spring"),
          no.space=T,dep.var.caption = "", report = "vct*", digits=1,
          model.numbers = F,type="text",out="output/regTXT/FloodDrought.txt")

#How to look at residuals for logit?
#ResidPlots(glm.DuSum) #not like this!
#### 6 - Metric 4: Duration: days below .05 percentile MA-FDC or above .95 MA-FDC: POISSON?  ####
S.Flow$DurLFPreds<-NA #create prediction variables
S.Flow$DurHFPreds<-NA

#####FIX THESE!! - Esp the glm predictions
#Drought: summer, fall
lm.DurSum<-lm(LF_days~log(DA_SQKM)+log(Elev_m)+log(LAT_GAGE)+log(LNG_GAGE.T)
               +log(PMonth1)+log(PMonth2)+log(PMonth3)+log(totprecipL1)+log(totprecipL2)+
                 +log(avgtmax.T)+log(avgtmaxL1.T)+log(avgtmaxL2.T)
               ,data=S.Flow[S.Flow$season=="summer"&S.Flow$type=="USGS",])
summary(glm.DurSum)
S.Flow$DurLFPreds[S.Flow$season=="summer"]<-predict(lm.DurSum,S.Flow[S.Flow$season=="summer",],allow.new.levels = T,type="response") #predict

lm.DurF<-lm(LF_days~log(DA_SQKM)+log(Elev_m)+log(LAT_GAGE)+log(LNG_GAGE.T)
             +log(PMonth1)+log(PMonth2)+log(PMonth3)+log(totprecipL1) #+log(totprecipL2)+
               #+log(avgtmax.T)+log(avgtmaxL1.T)+log(avgtmaxL2.T)
             ,data=S.Flow[S.Flow$season=="fall"&S.Flow$type=="USGS",])
summary(glm.DurF)
S.Flow$DurLFPreds[S.Flow$season=="fall"]<-exp(predict(lm.DurF,S.Flow[S.Flow$season=="fall",],allow.new.levels = T,type="response")) #predict

#Flood: winter, spring
lm.DurW<-lm(HF_days~log(DA_SQKM)+log(Elev_m)+log(LAT_GAGE)+log(LNG_GAGE.T)
             +log(PMonth1)+log(PMonth2)+log(PMonth3)+log(totprecipL1)+log(totprecipL2)
               #+log(avgtmax.T)+log(avgtmaxL1.T)+log(avgtmaxL2.T)
             ,data=S.Flow[S.Flow$season=="winter"&S.Flow$type=="USGS",])
summary(glm.DurW)
S.Flow$DurHFPreds[S.Flow$season=="winter"]<-exp(predict(lm.DurW,S.Flow[S.Flow$season=="winter",],allow.new.levels = T,type="response")) #predict

lm.DurSp<-lm(HF_days~log(DA_SQKM)+log(Elev_m)+log(LAT_GAGE)+log(LNG_GAGE.T)
              +log(PMonth1)+log(PMonth2)+log(PMonth3)+log(totprecipL1)+log(totprecipL2)
                #+log(avgtmax.T)+log(avgtmaxL1.T)+log(avgtmaxL2.T)
              ,data=S.Flow[S.Flow$season=="spring"&S.Flow$type=="USGS",])
summary(glm.DurSp)
S.Flow$DurHFPreds[S.Flow$season=="spring"]<-exp(predict(lm.DurSp,S.Flow[S.Flow$season=="spring",],allow.new.levels = T,type="response")) #predict

#Export regression results
stargazer(lm.DurSum,lm.DurF,lm.DurW,lm.DurSp,
          title="days with flow<.05p MA-FDC or flow>.95p MA-FDC)- USGS sites only",
          align=T,
          #dep.var.labels = "", #model.names = FALSE,
          column.labels = c("Summer","Fall","Winter","Spring"),
          no.space=T,dep.var.caption = "", report = "vct*", digits=1,
          model.numbers = F,type="text",out="output/regTXT/DurationRegs.txt")

#### 7 - Merge Metrics Predictions datasets ####
str(S.Flow)

#### 8 - CV Seasonal####
SiteList<-unique(S.Flow$site_no)
USGSsites<-SiteList[1:45]
S.Flow$SiteIndex<-as.numeric(as.factor(S.Flow$site_no))
NSE_sum<-rep(NA,length(USGSsites))
NSE_fall<-rep(NA,length(USGSsites))
NSE_win<-rep(NA,length(USGSsites))
NSE_spr<-rep(NA,length(USGSsites))
LNSE_sum<-rep(NA,length(USGSsites))
LNSE_fall<-rep(NA,length(USGSsites))
LNSE_win<-rep(NA,length(USGSsites))
LNSE_spr<-rep(NA,length(USGSsites))

NSE<-rep(NA,length(USGSsites))
LNSE<-rep(NA,length(USGSsites))
Avgbias<-rep(NA,length(USGSsites))

for (i in 1:length(USGSsites)) {
  #set test and training data sets
  train<-S.Flow[-which(i==S.Flow$SiteIndex),]
  test<-S.Flow[which(i==S.Flow$SiteIndex),]
  test$Preds<-NA
  
  #linear power law regional regression models predicting each season's flow seperately
  #Summer
  test.AvgSum<-lm(log(avgSflow)~log(DA_SQKM)+log(Elev_m)+log(LAT_GAGE)+log(LNG_GAGE.T)
                +log(PMonth1)+log(PMonth2)+log(PMonth3)+log(totprecipL1)+log(totprecipL2)
                ,data=train[train$season=="summer"&train$type=="USGS",])
  #predictions                         
  test$Preds[test$season=="summer"]<-exp(predict(test.AvgSum,test[test$season=="summer",],allow.new.levels = T))
  NSE_sum[i]<-NSE(test$Preds[test$season=="summer"],test$avgSflow[test$season=="summer"])  
  LNSE_sum[i]<-NSE(log(test$Preds[test$season=="summer"]),log(test$avgSflow[test$season=="summer"]))  
  
  #Fall
  test.AvgF<-lm(log(avgSflow)~log(DA_SQKM)+log(Elev_m)+log(LAT_GAGE)+log(LNG_GAGE.T)
              +log(PMonth1)+log(PMonth2)+log(PMonth3)+log(totprecipL1)+log(totprecipL2)
              ,data=train[train$season=="fall"&train$type=="USGS",])
  #predictions
  test$Preds[test$season=="fall"]<-exp(predict(test.AvgF,test[test$season=="fall",],allow.new.levels = T))
  NSE_fall[i]<-NSE(test$Preds[test$season=="fall"],test$avgSflow[test$season=="fall"])  
  LNSE_fall[i]<-NSE(log(test$Preds[test$season=="fall"]),log(test$avgSflow[test$season=="fall"]))  
  
  #Winter
  test.AvgW<-lm(log(avgSflow)~log(DA_SQKM)+log(Elev_m)+log(LAT_GAGE)+log(LNG_GAGE.T)
              +log(PMonth1)+log(PMonth2)+log(PMonth3)+log(totprecipL1)+log(totprecipL2)
              ,data=train[train$season=="winter"&train$type=="USGS",])
  #predictions                         
  test$Preds[test$season=="winter"]<-exp(predict(test.AvgW,test[test$season=="winter",],allow.new.levels = T))
  NSE_win[i]<-NSE(test$Preds[test$season=="winter"],test$avgSflow[test$season=="winter"])  
  LNSE_win[i]<-NSE(log(test$Preds[test$season=="winter"]),log(test$avgSflow[test$season=="winter"]))  
  
  #Spring
  test.AvgSp<-lm(log(avgSflow)~log(DA_SQKM)+log(Elev_m)+log(LAT_GAGE)+log(LNG_GAGE.T)
               +log(PMonth1)+log(PMonth2)+log(PMonth3)+log(totprecipL1)+log(totprecipL2)
               ,data=train[train$season=="spring"&train$type=="USGS",])
  #predictions                         
  test$Preds[test$season=="spring"]<-exp(predict(test.AvgSp,test[test$season=="spring",],allow.new.levels = T))
  NSE_spr[i]<-NSE(test$Preds[test$season=="spring"],test$avgSflow[test$season=="spring"])  
  LNSE_spr[i]<-NSE(log(test$Preds[test$season=="spring"]),log(test$avgSflow[test$season=="spring"]))  
  
  #overall NSE for each site:
  NSE[i]<-NSE(test$Preds,test$avgSflow)  
  LNSE[i]<-NSE(log(test$Preds),log(test$avgSflow))
  Avgbias[i]<-sum(test$Preds-test$avgSflow)/test$avgSflow
}
par(mfrow=c(1,1),mar=rep(4,4))  

boxplot(NSE_sum,NSE_fall,NSE_win,NSE_spr,ylim=c(-1,1),names=c("summer","fall","winter","spring"),
        ylab="NSE",main="NSE by USGS site (n=45)",col=c("yellow","orange","lightblue","lightgreen"))
abline(0,0,lty=2)
sum(NSE_sum< -1); sum(NSE_fall< -1);sum(NSE_win< -1); sum(NSE_spr< -1)

boxplot(LNSE_sum,LNSE_fall,LNSE_win,LNSE_spr,ylim=c(-1,1),names=c("summer","fall","winter","spring"),
        ylab="LNSE",main="LNSE by USGS site (n=45)",col=c("yellow","orange","lightblue","lightgreen"))
abline(0,0,lty=2)
sum(LNSE_sum< -1); sum(LNSE_fall< -1);sum(LNSE_win< -1); sum(LNSE_spr< -1)
boxplot(Avgbias*100)
abline(0,0,lty=2)
#### 9 - CV Magnitude ####
SiteList<-unique(S.Flow$site_no)
USGSsites<-SiteList[1:45]
S.Flow$SiteIndex<-as.numeric(as.factor(S.Flow$site_no))
NSE_sum<-rep(NA,length(USGSsites))
NSE_fall<-rep(NA,length(USGSsites))
NSE_win<-rep(NA,length(USGSsites))
NSE_spr<-rep(NA,length(USGSsites))
LNSE_sum<-rep(NA,length(USGSsites))
LNSE_fall<-rep(NA,length(USGSsites))
LNSE_win<-rep(NA,length(USGSsites))
LNSE_spr<-rep(NA,length(USGSsites))

NSE<-rep(NA,length(USGSsites))
LNSE<-rep(NA,length(USGSsites))
Avgbias<-rep(NA,length(USGSsites))


#LFs: summer, fall
lm.LF7.Sum<-lm(log(min7day)~log(DA_SQKM)+log(Elev_m)+log(LAT_GAGE)+log(LNG_GAGE.T)
               +log(PMonth1)+log(PMonth2)+log(PMonth3)+log(totprecipL1)+log(totprecipL2)
               ,data=S.Flow[S.Flow$season=="summer"&S.Flow$type=="USGS",])
summary(lm.LF7.Sum); ResidPlots(lm.LF7.Sum)
S.Flow$LF7Preds[S.Flow$season=="summer"]<-exp(predict(lm.LF7.Sum,S.Flow[S.Flow$season=="summer",],allow.new.levels = T)) #predict

lm.LF7.F<-lm(log(min7day)~log(DA_SQKM)+log(Elev_m)+log(LAT_GAGE)+log(LNG_GAGE.T)
             +log(PMonth1)+log(PMonth2)+log(PMonth3)+log(totprecipL1)+log(totprecipL2)
             #+log(avgtmax.T)+log(avgtmaxL1.T)+log(avgtmaxL2.T)
             ,data=S.Flow[S.Flow$season=="fall"&S.Flow$type=="USGS",])
summary(lm.LF7.F); ResidPlots(lm.LF7.F)
S.Flow$LF7Preds[S.Flow$season=="fall"]<-exp(predict(lm.LF7.F,S.Flow[S.Flow$season=="fall",],allow.new.levels = T)) #predict

#HFs: winter, spring
lm.HF3.W<-lm(log(max3dayflow)~log(DA_SQKM)+log(Elev_m)+log(LAT_GAGE)+log(LNG_GAGE.T)
             +log(PMonth1)+log(PMonth2)+log(PMonth3)+log(totprecipL1)+log(totprecipL2)+log(maxP3day)
             #+log(avgtmax.T)+log(avgtmaxL1.T)+log(avgtmaxL2.T)
             ,data=S.Flow[S.Flow$season=="winter"&S.Flow$type=="USGS",])
summary(lm.HF3.W); ResidPlots(lm.HF3.W)
S.Flow$HF3Preds[S.Flow$season=="winter"]<-exp(predict(lm.HF3.W,S.Flow[S.Flow$season=="winter",],allow.new.levels = T)) #predict

lm.HF3.Sp<-lm(log(max3dayflow)~log(DA_SQKM)+log(Elev_m)+log(LAT_GAGE)+log(LNG_GAGE.T)
              +log(PMonth1)+log(PMonth2)+log(PMonth3)+log(totprecipL1)+log(totprecipL2)+log(maxP3day)
              #+log(avgtmax.T)+log(avgtmaxL1.T)+log(avgtmaxL2.T)
              ,data=S.Flow[S.Flow$season=="spring"&S.Flow$type=="USGS",])
summary(lm.HF3.Sp); ResidPlots2(lm.HF3.Sp)
S.Flow$HF3Preds[S.Flow$season=="spring"]<-exp(predict(lm.HF3.Sp,S.Flow[S.Flow$season=="spring",],allow.new.levels = T)) #predict


for (i in 1:length(USGSsites)) {
  #set test and training data sets
  train<-S.Flow[-which(i==S.Flow$SiteIndex),]
  test<-S.Flow[which(i==S.Flow$SiteIndex),]
  test$Preds<-NA
  
  #linear power law regional regression models predicting each season's flow seperately
  #Summer
  test.SMagLF<-lm(log(min7day)~log(DA_SQKM)+log(Elev_m)+log(LAT_GAGE)+log(LNG_GAGE.T)
                  +log(PMonth1)+log(PMonth2)+log(PMonth3)+log(totprecipL1)+log(totprecipL2)
                  ,data=train[train$season=="summer"&train$type=="USGS",])
  #predictions                         
  test$Preds[test$season=="summer"]<-exp(predict(test.SMagLF,test[test$season=="summer",],allow.new.levels = T))
  NSE_sum[i]<-NSE(test$Preds[test$season=="summer"],test$min7day[test$season=="summer"])  
  LNSE_sum[i]<-NSE(log(test$Preds[test$season=="summer"]),log(test$min7day[test$season=="summer"]))  
  
  #Fall
  test.FMagLF<-lm(log(min7day)~log(DA_SQKM)+log(Elev_m)+log(LAT_GAGE)+log(LNG_GAGE.T)
                +log(PMonth1)+log(PMonth2)+log(PMonth3)+log(totprecipL1)+log(totprecipL2)
                ,data=train[train$season=="fall"&train$type=="USGS",])
  #predictions
  test$Preds[test$season=="fall"]<-exp(predict(test.FMagLF,test[test$season=="fall",],allow.new.levels = T))
  NSE_fall[i]<-NSE(test$Preds[test$season=="fall"],test$min7day[test$season=="fall"])  
  LNSE_fall[i]<-NSE(log(test$Preds[test$season=="fall"]),log(test$min7day[test$season=="fall"]))  
  
  #Winter
  test.WMagHF<-lm(log(max3dayflow)~log(DA_SQKM)+log(Elev_m)+log(LAT_GAGE)+log(LNG_GAGE.T)
                +log(PMonth1)+log(PMonth2)+log(PMonth3)+log(totprecipL1)+log(totprecipL2)+log(maxP3day)
                ,data=train[train$season=="winter"&train$type=="USGS",])
  #predictions                         
  test$Preds[test$season=="winter"]<-exp(predict(test.WMagHF,test[test$season=="winter",],allow.new.levels = T))
  NSE_win[i]<-NSE(test$Preds[test$season=="winter"],test$max3dayflow[test$season=="winter"])  
  LNSE_win[i]<-NSE(log(test$Preds[test$season=="winter"]),log(test$max3dayflow[test$season=="winter"]))  
  
  #Spring
  test.SMagHF<-lm(log(max3dayflow)~log(DA_SQKM)+log(Elev_m)+log(LAT_GAGE)+log(LNG_GAGE.T)
                 +log(PMonth1)+log(PMonth2)+log(PMonth3)+log(totprecipL1)+log(totprecipL2)+log(maxP3day)
                 ,data=train[train$season=="spring"&train$type=="USGS",])
  #predictions                         
  test$Preds[test$season=="spring"]<-exp(predict(test.SMagHF,test[test$season=="spring",],allow.new.levels = T))
  NSE_spr[i]<-NSE(test$Preds[test$season=="spring"],test$max3dayflow[test$season=="spring"])  
  LNSE_spr[i]<-NSE(log(test$Preds[test$season=="spring"]),log(test$max3dayflow[test$season=="spring"]))  
  
  #overall NSE for each site:
  NSE[i]<-NSE(test$Preds,test$max3dayflow)  
  LNSE[i]<-NSE(log(test$Preds),log(test$max3dayflow))
  Avgbias[i]<-mean(test$Preds-test$max3dayflow)/test$max3dayflow
}

par(mfrow=c(1,1),mar=rep(4,4))  

boxplot(NSE_sum,NSE_fall,NSE_win,NSE_spr,ylim=c(-1,1),names=c("summer LF","fall LF","winter HF","spring HF"),
        ylab="NSE",main="Magnitude - NSE by USGS site (n=45)",col=c("yellow","orange","lightblue","lightgreen"))
abline(0,0,lty=2)
abline(v=2.5,lwd=2)
sum(NSE_sum< -1); sum(NSE_fall< -1);sum(NSE_win< -1); sum(NSE_spr< -1)

boxplot(LNSE_sum,LNSE_fall,LNSE_win,LNSE_spr,ylim=c(-1,1),names=c("summer LF","fall LF","winter HF","spring HF"),
        ylab="LNSE",main="Magnitude - LNSE by USGS site (n=45)",col=c("yellow","orange","lightblue","lightgreen"))
abline(0,0,lty=2)
abline(v=2.5,lwd=2)
sum(LNSE_sum< -1); sum(LNSE_fall< -1);sum(LNSE_win< -1); sum(LNSE_spr< -1)
boxplot(Avgbias*100,ylab="Mean bias (%)",main="Magnitude - mean % bias by USGS site (n=45)")
abline(0,0,lty=2)
