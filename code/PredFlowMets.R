#New - Predict Flow metrics
###Annalise Blum
#Created: Dec 5,2016 Updated: Dec 5,2016
#Data sets created: "output/A.Fishmerge.rdata"

#### 1 - Load datasets
#### 2 - Clean and merge datasets; predict flows
#### 3 - Collapse to Annual level

#rm(list=ls())

#NN matches
load("output/fish_UVA4gaged_list.rdata")
load("output/fish_USGSgaged_list.rdata") 

#estimated metrics at gaged sites
load("output/UVA_metrics.rdata")
load("output/USGS_metrics.rdata")

#### 2 - Clean and merge datasets; predict flows ####
#rename site_no as gaged_site_no
names(UVA_metrics)[1]<-"gaged_site_no"
names(USGS_metrics)[1]<-"gaged_site_no"

#add type variable
UVA_metrics$type<-"UVA"
USGS_metrics$type<-"USGS"

#merge datasets
Pred_UVA<-merge(fish_UVA4gaged_list,UVA_metrics,by="gaged_site_no")
Pred_USGS<-merge(fish_USGSgaged_list,USGS_metrics,by="gaged_site_no")

#combine USGS and UVA data
Preds<-rbind(Pred_USGS,Pred_UVA)

#Predict metrics at fish sites by DAR and gaged site (NN): avgSNNflow,p5,p95,LF05_days,HF95_days
Preds$predAvgCFS<-(Preds$avgSNNflow/Preds$gaged_DA_SQKM)*Preds$DA_SQKM
Preds$predP5<-(Preds$p5/Preds$gaged_DA_SQKM)*Preds$DA_SQKM
Preds$predP95<-(Preds$p95/Preds$gaged_DA_SQKM)*Preds$DA_SQKM
Preds$predDurLF<-(Preds$LF25_days/Preds$gaged_DA_SQKM)*Preds$DA_SQKM
Preds$predDurHF<-(Preds$HF75_days/Preds$gaged_DA_SQKM)*Preds$DA_SQKM
Preds_metrics<-Preds

Preds_metrics$Nyear<-ifelse(Preds_metrics$Nseason>2,Preds_metrics$year-1981,Preds_metrics$year-1980) #to get years 1-29
Preds_metrics<-Preds_metrics[Preds_metrics$Nyear>0,]

save(Preds_metrics,file="output/Preds_metrics.rdata")

#### 3 - Collapse to Annual level ####

#load("output/Preds_metrics.rdata") #direct prediction
names(Preds_metrics)

#seasonal avg flows
Seasonalcast <- dcast(Preds_metrics, site_no + Nyear ~ Nseason,value.var = "predAvgCFS") 
#head(Seasonalcast) #summer = 1; fall=2, etc
names(Seasonalcast)<-c("site_no","Nyear","AvgQsummer","AvgQfall", "AvgQwinter","AvgQspring")
A.Fishmerge<-Seasonalcast

#Magnitude: p5 and p95
Magcast5 <- dcast(Preds_metrics, site_no + Nyear ~ Nseason,value.var = "predP5") 
#head(Magcast5) #summer = 1; fall=2, etc
names(Magcast5)<-c("site_no","Nyear","p5summer","p5fall", "p5winter","p5spring")
A.Fishmerge<-merge(A.Fishmerge,Magcast5,by=c("site_no","Nyear"))

Magcast95 <- dcast(Preds_metrics, site_no + Nyear ~ Nseason,value.var = "predP95") 
#head(Magcast95) #summer = 1; fall=2, etc
names(Magcast95)<-c("site_no","Nyear","p95summer","p95fall", "p95winter","p95spring")
A.Fishmerge<-merge(A.Fishmerge,Magcast95,by=c("site_no","Nyear"))

#Duration
DurLFcast <- dcast(Preds_metrics, site_no + Nyear ~ Nseason,value.var = "predDurLF") 
#head(DurLFcast) #summer = 1; fall=2, etc
names(DurLFcast)<-c("site_no","Nyear","DurLFsummer","DurLFfall", "DurLFwinter","DurLFspring")
A.Fishmerge<-merge(A.Fishmerge,DurLFcast,by=c("site_no","Nyear"))

DurHFcast <- dcast(Preds_metrics, site_no + Nyear ~ Nseason,value.var = "predDurHF") 
#head(DurHFcast) #summer = 1; fall=2, etc
names(DurHFcast)<-c("site_no","Nyear","DurHFsummer","DurHFfall", "DurHFwinter","DurHFspring")
A.Fishmerge<-merge(A.Fishmerge,DurHFcast,by=c("site_no","Nyear"))

save(A.Fishmerge,file="output/A.Fishmerge.rdata")

##### 4 - estimate daily flows ####
load("output/UVA_daily.rdata")
load("output/USGSdaily.rdata")
load("output/fish_USGSgaged_list.rdata") 

#add type variable
UVA_daily$type<-"UVA"
USGSdaily$type<-"USGS"

UVA_daily93<-UVA_daily[UVA_daily$Date>"1992-12-31",] #subset the years

#find the USGS sites of interest
USGS_NNsites<-as.data.frame(unique(fish_USGSgaged_list$gaged_site_no)) #USGS sites "01632900" "01634500" "01665500" "02028500"
names(USGS_NNsites)<-"site_no"

USGSdaily82<-USGSdaily[USGSdaily$Date<"1993-01-01",]
USGSdaily82<-merge(USGSdaily82,USGS_NNsites,by="site_no")

#rename site_no as gaged_site_no
names(UVA_daily93)[1]<-"gaged_site_no"
names(USGSdaily82)[1]<-"gaged_site_no"

#merge datasets
df_Pred_USGS<-merge(fish_USGSgaged_list,USGSdaily82,by="gaged_site_no")
df_Pred_UVA<-merge(fish_UVA4gaged_list,UVA_daily93,by="gaged_site_no")

#combine USGS and UVA data
DailyFlows<-rbind(df_Pred_USGS,df_Pred_UVA)

DailyFlows$pred_cfs<-(DailyFlows$cfs/DailyFlows$gaged_DA_SQKM)*DailyFlows$DA_SQKM

Pred_df<-DailyFlows

save(Pred_df,file="output/Pred_df.rdata")

#### 5 - As comparison, now predict metrics from the daily flows - go back and run flow mets ####
#pull site_no, Date, cfs, day, month, year
df_preds<-Pred_df[c("site_no","Date","pred_cfs","day","month","year")]
names(df_preds)<-c("site_no","Date","cfs","day","month","year")
save(df_preds,file="output/df_preds.rdata")

#run FlowMets
## load("output/Pred_fromDF.rdata") #this one has metrics predicted from daily time series
#directly predicted: Preds_metrics - missing 2 seasons in 1981 and 2 in 2010
length(Preds_metrics$p5) 
length(Pred_fromDF$p5)
Preds_metrics<-Preds_metrics[Preds_metrics$Nyear<30,]

Preds_Directly<-Preds_metrics[c("site_no","year","Nseason","predAvgCFS","predP5","predP95","predDurLF",
                                "predDurHF")]

Pred_comp<-merge(Pred_fromDF,Preds_Directly,by=c("site_no","year","Nseason"))

plot(Pred_comp$avgSNNflow,Pred_comp$predAvgCFS,xlab="DF time series pred",ylab="direct pred")
plot(Pred_comp$LF25_days,Pred_comp$predDurLF,xlab="DF time series pred",ylab="direct pred")

#Compare A.Fishmerge (UVA and USGS) to A.FishNNPreds (all USGS)

load("output/A.Fishmerge.rdata")
colnames(A.Fishmerge)[3:22] <- paste("B", colnames(A.Fishmerge)[3:22], sep = "_")

load("output/A.FishNNPreds.rdata")

str(A.Fishmerge); str(A.FishNNPreds)
Comparison<-merge(A.Fishmerge,A.FishNNPreds,by=c("site_no","Nyear"))
plot(Comparison$B_AvgQfall[Comparison$Nyear<11],Comparison$AvgQfall[Comparison$Nyear<11])
abline(0,1)

save(All_fish,file="output/All_fish.rdata") #with weather also

A.FishNNPredsCC<-A.FishNNPreds[complete.cases(A.FishNNPreds),] #remove observations with ANY missing values
All_fishCC<-All_fish[complete.cases(All_fish),] #remove observations with ANY missing values

#just pre-1993
A.FishNNPredsCC<-A.FishNNPredsCC[A.FishNNPredsCC$Nyear<11,] #remove observations with ANY missing values
All_fishCC<-All_fishCC[All_fishCC$Nyear<11,] #remove observations with ANY missing values

Both<-lmer(log(EstYOYAbu)~(1|site_no)+log(AvgQsummer)+log(AvgQfall)+log(AvgQwinter)+log(AvgQspring) 
         ,data=All_fishCC, REML=F)
summary(Both)

All_fishCC$Abu_predsBoth<-exp(predict(Both,All_fishCC)) #for whole model, no omitted data

RMSEBoth<-(mean((All_fishCC$EstYOYAbu- All_fishCC$Abu_predsBoth)^2))^.5; RMSEBoth

#just USGS
JustUSGS<-lmer(log(EstYOYAbu)~(1|site_no)+log(AvgQsummer)+log(AvgQfall)+log(AvgQwinter)+log(AvgQspring) 
           ,data=A.FishNNPredsCC, REML=F)
summary(JustUSGS)

A.FishNNPredsCC$Abu_predsUSGS<-exp(predict(JustUSGS)) #for whole model, no omitted data

RMSEUSGS<-(mean((A.FishNNPredsCC$EstYOYAbu- A.FishNNPredsCC$Abu_predsUSGS)^2))^.5; RMSEUSGS


##### 6 - Collapse to Annual level Metrics from Daily predictions both UVA and USGS ####
load("output/Pred_fromDF.rdata") #from daily time series prediction
names(Pred_fromDF) #what file was this created in??

Pred_fromDF$Nyear<-ifelse(Pred_fromDF$Nseason>2,Pred_fromDF$year-1981,Pred_fromDF$year-1980)
Pred_fromDF<-Pred_fromDF[Pred_fromDF$Nyear<30&Pred_fromDF$Nyear>0,]

#seasonal avg flows
Seasonalcast <- dcast(Pred_fromDF, site_no + Nyear ~ Nseason,value.var = "avgSNNflow") 
#head(Seasonalcast) #summer = 1; fall=2, etc
names(Seasonalcast)<-c("site_no","Nyear","AvgQsummer","AvgQfall", "AvgQwinter","AvgQspring")
A.Fishmerge2<-Seasonalcast

#Magnitude: p5 and p95
Magcast5 <- dcast(Pred_fromDF, site_no + Nyear ~ Nseason,value.var = "p5") 
#head(Magcast5) #summer = 1; fall=2, etc
names(Magcast5)<-c("site_no","Nyear","p5summer","p5fall", "p5winter","p5spring")
A.Fishmerge2<-merge(A.Fishmerge2,Magcast5,by=c("site_no","Nyear"))

Magcast95 <- dcast(Pred_fromDF, site_no + Nyear ~ Nseason,value.var = "p95") 
#head(Magcast95) #summer = 1; fall=2, etc
names(Magcast95)<-c("site_no","Nyear","p95summer","p95fall", "p95winter","p95spring")
A.Fishmerge2<-merge(A.Fishmerge2,Magcast95,by=c("site_no","Nyear"))

#Duration
DurLFcast <- dcast(Pred_fromDF, site_no + Nyear ~ Nseason,value.var = "LF05_days") #LF25_days
#head(DurLFcast) #summer = 1; fall=2, etc
names(DurLFcast)<-c("site_no","Nyear","DurLFsummer","DurLFfall", "DurLFwinter","DurLFspring")
A.Fishmerge2<-merge(A.Fishmerge2,DurLFcast,by=c("site_no","Nyear"))

DurHFcast <- dcast(Pred_fromDF, site_no + Nyear ~ Nseason,value.var = "HF95_days") #HF75_days
#head(DurHFcast) #summer = 1; fall=2, etc
names(DurHFcast)<-c("site_no","Nyear","DurHFsummer","DurHFfall", "DurHFwinter","DurHFspring")
A.Fishmerge2<-merge(A.Fishmerge2,DurHFcast,by=c("site_no","Nyear"))

save(A.Fishmerge2,file="output/A.Fishmerge2.rdata")

load("output/A.Fish.rdata")
All_fish2<-merge(A.Fish,A.Fishmerge2,by=c("site_no","Nyear"))
save(All_fish2,file="output/All_fish2.rdata")

#### Now do more comparisons ####
load("output/All_fish.rdata")
load("output/A.FishNNPreds.rdata") #just USGS sites used in NN

All_fish2CC<-All_fish2[complete.cases(All_fish2),] #remove observations with ANY missing values
All_fishCC<-All_fish[complete.cases(All_fish),] #remove observations with ANY missing values
A.FishNNPredsCC<-A.FishNNPreds[complete.cases(A.FishNNPreds),] #remove observations with ANY missing values

#just pre-1993
A.FishNNPredsCC<-A.FishNNPredsCC[A.FishNNPredsCC$Nyear<11,] #remove observations with ANY missing values
All_fish2CC<-All_fish2CC[All_fish2CC$Nyear<11,] #remove observations with ANY missing values


#Direct preds
lm<-lmer(log(EstYOYAbu)~(1|site_no)+log(p5summer+.05)+log(p5fall+.05)+
           log(p95winter)+log(p95spring) 
          ,data=A.FishNNPredsCC, REML=F)
summary(lm)

A.FishNNPredsCC$Abu_preds<-exp(predict(lm)) #for durations predicted from daily time series

RMSE<-(mean((A.FishNNPredsCC$EstYOYAbu- A.FishNNPredsCC$Abu_preds)^2))^.5; RMSE
#104.401
#96.34857
#2
lm2<-lmer(log(EstYOYAbu)~(1|site_no)+log(p5summer+.05)+log(p5fall+.05)+
            log(p95winter)+log(p95spring) 
               ,data=All_fish2CC, REML=F)
summary(lm2)

All_fish2CC$Abu_preds2<-exp(predict(lm2)) #for durations predicted from daily time series

RMSE2<-(mean((All_fish2CC$EstYOYAbu- All_fish2CC$Abu_preds2)^2))^.5; RMSE2

