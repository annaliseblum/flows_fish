##Flow Predictions: 4 types of metrics x 4 seasons = 16 types of flows to predict
###Impact of Extreme Streamflows on Brook Trout Young-of-Year Abundance
### Annalise G Blum
##Created: Oct 5,2016; Last modified: Oct 14, 2016
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

#### 2 - Skip this section - Once have Preds, do Plots and NSE diagnostics that will be used (paste in throughout) ####
#45 USGS and 5 UVA sites
Preds<-S.Flow$HFPreds #DumPreds #AvgPreds
Obs<-S.Flow$# Drought # and Flood #avgSflow
  Metric<-"Avg Seasonal Flow"

#real space
NSE(Preds,Obs) #NSE
plot(Obs,Preds,main=bquote("NSE="~ .(round(NSE(log(Preds),log(Obs)),3)))) #plots
abline(0,1)

#LOG space
NSE(log(Preds),log(Obs)) #LNSE
plot(log(Obs),log(Preds),main=paste(Metric,(bquote("LNSE="~.(round(NSE(log(Preds),log(Obs)),3))))))
abline(0,1)

##Just UVA sites
NSE(log(Preds[S.Flow$type=="UVA"]),log(Obs[S.Flow$type=="UVA"])) #LNSE
plot(log(Obs[S.Flow$type=="UVA"]),log(Preds[S.Flow$type=="UVA"]),
     main=bquote("UVA only LNSE="~ .(round(NSE(log(Preds),log(Obs)),3)))) #plots
abline(0,1)

#### 3 - Metric 1: Average seasonal flows  ####
S.Flow$AvgPreds<-NA #create prediction variables

#linear power law regional regression models predicting each season's flow seperately
lm.AvgSum<-lm(log(avgSflow)~log(DA_SQKM)+log(Elev_m)+log(LAT_GAGE)+log(LNG_GAGE.T)
              +log(Pjune)+log(Pjuly)+log(Paug)+log(totprecipL1)+log(totprecipL2)
              #+log(avgtmax.T)+log(avgtmaxL1.T)+log(avgtmaxL2.T)
              ,data=S.Flow[S.Flow$season=="summer"&S.Flow$type=="USGS",])
summary(lm.AvgSum); ResidPlots(lm.AvgSum)
S.Flow$AvgPreds[S.Flow$season=="summer"]<-exp(predict(lm.AvgSum,S.Flow[S.Flow$season=="summer",],allow.new.levels = T)) #predict

lm.AvgF<-lm(log(avgSflow)~log(DA_SQKM)+log(Elev_m)+log(LAT_GAGE)+log(LNG_GAGE.T)
            +log(Psept)+log(Poct)+log(Pnov)+log(totprecipL1)+log(totprecipL2)
            #+log(avgtmax.T)+log(avgtmaxL1.T)+log(avgtmaxL2.T)
            ,data=S.Flow[S.Flow$season=="fall"&S.Flow$type=="USGS",])
summary(lm.AvgF); ResidPlots(lm.AvgF)
S.Flow$AvgPreds[S.Flow$season=="fall"]<-exp(predict(lm.AvgF,S.Flow[S.Flow$season=="fall",],allow.new.levels = T)) #predict

lm.AvgW<-lm(log(avgSflow)~log(DA_SQKM)+log(Elev_m)+log(LAT_GAGE)+log(LNG_GAGE.T)
            +log(Pdec)+log(Pjan)+log(Pfeb)+log(totprecipL1)+log(totprecipL2)
            #+log(avgtmax.T)+log(avgtmaxL1.T)+log(avgtmaxL2.T)
            ,data=S.Flow[S.Flow$season=="winter"&S.Flow$type=="USGS",])
summary(lm.AvgW); ResidPlots(lm.AvgW)
S.Flow$AvgPreds[S.Flow$season=="winter"]<-exp(predict(lm.AvgW,S.Flow[S.Flow$season=="winter",],allow.new.levels = T)) #predict

lm.AvgSp<-lm(log(avgSflow)~log(DA_SQKM)+log(Elev_m)+log(LAT_GAGE)+log(LNG_GAGE.T)
             +log(Pmar)+log(Papril)+log(Pmay)+log(totprecipL1)+log(totprecipL2)+
               +log(avgtmax.T)+log(avgtmaxL1.T)+log(avgtmaxL2.T)
             ,data=S.Flow[S.Flow$season=="spring"&S.Flow$type=="USGS",])
summary(lm.AvgSp); ResidPlots(lm.AvgSp)
S.Flow$AvgPreds[S.Flow$season=="spring"]<-exp(predict(lm.AvgSp,S.Flow[S.Flow$season=="spring",],allow.new.levels = T)) #predict at USGS and UVA sites

# #predict at fish sites: - vars in different order - does that matter?? Seems not to matter!
# S.Fish$AvgPreds[S.Fish$season=="summer"]<-exp(predict(lm.AvgSum,S.Fish[S.Fish$season=="summer",],allow.new.levels = T))
# S.Fish$AvgPreds[S.Fish$season=="fall"]<-exp(predict(lm.AvgF,S.Fish[S.Fish$season=="fall",],allow.new.levels = T))
# S.Fish$AvgPreds[S.Fish$season=="winter"]<-exp(predict(lm.AvgW,S.Fish[S.Fish$season=="winter",],allow.new.levels = T))
# S.Fish$AvgPreds[S.Fish$season=="spring"]<-exp(predict(lm.AvgSp,S.Fish[S.Fish$season=="spring",],allow.new.levels = T))

#### 4 - Metric 2: Magnitude (Intensity/Severity) LF and HFs  ####
S.Flow$LF7Preds<-NA #create prediction variables
S.Flow$HF3Preds<-NA #create prediction variables

#linear power law regional regression models predicting each season's flow seperately

#LFs: summer, fall
lm.LF7.Sum<-lm(log(min7day)~log(DA_SQKM)+log(Elev_m)+log(LAT_GAGE)+log(LNG_GAGE.T)
               +log(Pjune)+log(Pjuly)+log(Paug)+log(totprecipL1)+log(totprecipL2)+
                 +log(avgtmax.T)+log(avgtmaxL1.T)+log(avgtmaxL2.T)
               ,data=S.Flow[S.Flow$season=="summer"&S.Flow$type=="USGS",])
summary(lm.LF7.Sum); ResidPlots(lm.LF7.Sum)
S.Flow$LF7Preds[S.Flow$season=="summer"]<-exp(predict(lm.LF7.Sum,S.Flow[S.Flow$season=="summer",],allow.new.levels = T)) #predict

lm.LF7.F<-lm(log(min7day)~log(DA_SQKM)+log(Elev_m)+log(LAT_GAGE)+log(LNG_GAGE.T)
             +log(Psept)+log(Poct)+log(Pnov)+log(totprecipL1)+log(totprecipL2)+
               +log(avgtmax.T)+log(avgtmaxL1.T)+log(avgtmaxL2.T)
             ,data=S.Flow[S.Flow$season=="fall"&S.Flow$type=="USGS",])
summary(lm.LF7.F); ResidPlots(lm.LF7.F)
S.Flow$LF7Preds[S.Flow$season=="fall"]<-exp(predict(lm.LF7.F,S.Flow[S.Flow$season=="fall",],allow.new.levels = T)) #predict

#HFs: winter, spring
lm.HF3.W<-lm(log(max3dayflow)~log(DA_SQKM)+log(Elev_m)+log(LAT_GAGE)+log(LNG_GAGE.T)
             +log(Pdec)+log(Pjan)+log(Pfeb)+log(totprecipL1)+log(totprecipL2)+
               +log(avgtmax.T)+log(avgtmaxL1.T)+log(avgtmaxL2.T)
             ,data=S.Flow[S.Flow$season=="winter"&S.Flow$type=="USGS",])
summary(lm.HF3.W); ResidPlots(lm.HF3.W)
S.Flow$HF3Preds[S.Flow$season=="winter"]<-exp(predict(lm.HF3.W,S.Flow[S.Flow$season=="winter",],allow.new.levels = T)) #predict

lm.HF3.Sp<-lm(log(max3dayflow)~log(DA_SQKM)+log(Elev_m)+log(LAT_GAGE)+log(LNG_GAGE.T)
              +log(Pmar)+log(Papril)+log(Pmay)+log(totprecipL1)+log(totprecipL2)+
                +log(avgtmax.T)+log(avgtmaxL1.T)+log(avgtmaxL2.T)
              ,data=S.Flow[S.Flow$season=="spring"&S.Flow$type=="USGS",])
summary(lm.HF3.Sp); ResidPlots(lm.HF3.Sp)
S.Flow$HF3Preds[S.Flow$season=="spring"]<-exp(predict(lm.HF3.Sp,S.Flow[S.Flow$season=="spring",],allow.new.levels = T)) #predict

##Predict at fish sites
S.Fish$LF7Preds[S.Fish$season=="summer"]<-exp(predict(lm.LF7.Sum,S.Fish[S.Fish$season=="summer",],allow.new.levels = T))
S.Fish$LF7Preds[S.Fish$season=="fall"]<-exp(predict(lm.LF7.F,S.Fish[S.Fish$season=="fall",],allow.new.levels = T))
S.Fish$HF3Preds[S.Fish$season=="winter"]<-exp(predict(lm.HF3.W,S.Fish[S.Fish$season=="winter",],allow.new.levels = T))
S.Fish$HF3Preds[S.Fish$season=="spring"]<-exp(predict(lm.HF3.Sp,S.Fish[S.Fish$season=="spring",],allow.new.levels = T))



#### 5 - Metric 3: Dummy variable for drought or flood: LOGISTIC  ####
S.Flow$DroughtPreds<-NA #create prediction variables
S.Flow$FloodPreds<-NA

#Drought: summer, fall - RENAME glm.DuSum TOO SIMILAR TO DURATION
glm.DuSum<-glm(Drought~log(DA_SQKM)+log(Elev_m)+log(LAT_GAGE)+log(LNG_GAGE.T)
               +log(Pjune)+log(Pjuly)+log(Paug)+log(totprecipL1)+log(totprecipL2)+
                 +log(avgtmax.T)+log(avgtmaxL1.T)+log(avgtmaxL2.T)
               ,data=S.Flow[S.Flow$season=="summer"&S.Flow$type=="USGS",],family=binomial(logit))
summary(glm.DuSum)
S.Flow$DroughtPreds[S.Flow$season=="summer"]<-predict.glm(glm.DuSum,S.Flow[S.Flow$season=="summer",],allow.new.levels = T,type="response") #predict

glm.DuF<-glm(Drought~log(DA_SQKM)+log(Elev_m)+log(LAT_GAGE)+log(LNG_GAGE.T)
             +log(Psept)+log(Poct)+log(Pnov)+log(totprecipL1)+log(totprecipL2)+
               +log(avgtmax.T)+log(avgtmaxL1.T)+log(avgtmaxL2.T)
             ,data=S.Flow[S.Flow$season=="fall"&S.Flow$type=="USGS",],family="binomial")
summary(glm.DuF)
S.Flow$DroughtPreds[S.Flow$season=="fall"]<-exp(predict(glm.DuF,S.Flow[S.Flow$season=="fall",],allow.new.levels = T)) #predict

#Flood: winter, spring
glm.DuW<-glm(Flood~log(DA_SQKM)+log(Elev_m)+log(LAT_GAGE)+log(LNG_GAGE.T)
             +log(Pdec)+log(Pjan)+log(Pfeb)+log(totprecipL1)+log(totprecipL2)+
               +log(avgtmax.T)+log(avgtmaxL1.T)+log(avgtmaxL2.T)
             ,data=S.Flow[S.Flow$season=="winter"&S.Flow$type=="USGS",],family="binomial")
summary(glm.DuW)
S.Flow$FloodPreds[S.Flow$season=="winter"]<-exp(predict(glm.DuW,S.Flow[S.Flow$season=="winter",],allow.new.levels = T)) #predict

glm.DuSp<-glm(Flood~log(DA_SQKM)+log(Elev_m)+log(LAT_GAGE)+log(LNG_GAGE.T)
              +log(Pmar)+log(Papril)+log(Pmay)+log(totprecipL1)+log(totprecipL2)+
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
glm.DurSum<-lm(LF_days~log(DA_SQKM)+log(Elev_m)+log(LAT_GAGE)+log(LNG_GAGE.T)
               +log(Pjune)+log(Pjuly)+log(Paug)+log(totprecipL1)+log(totprecipL2)+
                 +log(avgtmax.T)+log(avgtmaxL1.T)+log(avgtmaxL2.T)
               ,data=S.Flow[S.Flow$season=="summer"&S.Flow$type=="USGS",],family="poisson")
summary(glm.DurSum)
S.Flow$DurLFPreds[S.Flow$season=="summer"]<-predict(glm.DurSum,S.Flow[S.Flow$season=="summer",],allow.new.levels = T,type="response") #predict

glm.DurF<-lm(LF_days~log(DA_SQKM)+log(Elev_m)+log(LAT_GAGE)+log(LNG_GAGE.T)
             +log(Psept)+log(Poct)+log(Pnov)+log(totprecipL1) #+log(totprecipL2)+
             #+log(avgtmax.T)+log(avgtmaxL1.T)+log(avgtmaxL2.T)
             ,data=S.Flow[S.Flow$season=="fall"&S.Flow$type=="USGS",])
summary(glm.DurF)
S.Flow$DurLFPreds[S.Flow$season=="fall"]<-exp(predict(glm.DurF,S.Flow[S.Flow$season=="fall",],allow.new.levels = T,type="response")) #predict

#Flood: winter, spring
glm.DurW<-lm(HF_days~log(DA_SQKM)+log(Elev_m)+log(LAT_GAGE)+log(LNG_GAGE.T)
             +log(Pdec)+log(Pjan)+log(Pfeb)+log(totprecipL1)+log(totprecipL2)
             #+log(avgtmax.T)+log(avgtmaxL1.T)+log(avgtmaxL2.T)
             ,data=S.Flow[S.Flow$season=="winter"&S.Flow$type=="USGS",])
summary(glm.DurW)
S.Flow$DurHFPreds[S.Flow$season=="winter"]<-exp(predict(glm.DurW,S.Flow[S.Flow$season=="winter",],allow.new.levels = T,type="response")) #predict

glm.DurSp<-lm(HF_days~log(DA_SQKM)+log(Elev_m)+log(LAT_GAGE)+log(LNG_GAGE.T)
              +log(Pmar)+log(Papril)+log(Pmay)+log(totprecipL1)+log(totprecipL2)
              #+log(avgtmax.T)+log(avgtmaxL1.T)+log(avgtmaxL2.T)
              ,data=S.Flow[S.Flow$season=="spring"&S.Flow$type=="USGS",])
summary(glm.DurSp)
S.Flow$DurHFPreds[S.Flow$season=="spring"]<-exp(predict(glm.DurSp,S.Flow[S.Flow$season=="spring",],allow.new.levels = T,type="response")) #predict

#Export regression results
stargazer(glm.DurSum,glm.DurF,glm.DurW,glm.DurSp,
          title="days with flow<.05p MA-FDC or flow>.95p MA-FDC)- USGS sites only",
          align=T,
          #dep.var.labels = "", #model.names = FALSE,
          column.labels = c("Summer","Fall","Winter","Spring"),
          no.space=T,dep.var.caption = "", report = "vct*", digits=1,
          model.numbers = F,type="text",out="output/regTXT/DurationRegs.txt")
