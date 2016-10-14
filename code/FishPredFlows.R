##Predict extreme high and low flows at 115 fish sites AND use predicted flows with temp to predict fish abundance
###Annalise Blum
#Created: Oct 14,2016  Updated: Oct 14,2016
##Data sets need to run: 
##Data sets created in this file: "output/A.FishPreds.rdata"

##TO DOs:
#all missing for 115 values !!! FIX THIS - all the first year for each of the sites, probabaly??
## NEED WEATHER DATA FROM KYLE FOR 1981
#Use different LF and HF variables or combine into one variable??
#FIX THESE GLM PREDS section 2

#### 1 - Run all 16 models: 4 metrics x 4 season
#### 2 -  Predict 16 flows at 115 fish sites
#### 3 - Standardize variables
#### 4 - Predict fish outcomes

load("output/S.Flow.rdata") #Seasonal flow data
load("output/S.Fish.rdata") #Seasonal fish data
load("output/A.Fish.rdata") #Annual fish data

#### 1 - Run all 16 models: 4 metrics x 4 season ####

#### 2 -  Predict 16 flows at 115 fish sites ####
#predict at fish sites: - vars in different order - does that matter?? Seems not to matter!

#Seasonal average flows
S.Fish$AvgPreds[S.Fish$season=="summer"]<-exp(predict(lm.AvgSum,S.Fish[S.Fish$season=="summer",],allow.new.levels = T))
S.Fish$AvgPreds[S.Fish$season=="fall"]<-exp(predict(lm.AvgF,S.Fish[S.Fish$season=="fall",],allow.new.levels = T))
S.Fish$AvgPreds[S.Fish$season=="winter"]<-exp(predict(lm.AvgW,S.Fish[S.Fish$season=="winter",],allow.new.levels = T))
S.Fish$AvgPreds[S.Fish$season=="spring"]<-exp(predict(lm.AvgSp,S.Fish[S.Fish$season=="spring",],allow.new.levels = T))

#collapse to annual level and merge in fish sample data
castS.Fish <- dcast(S.Fish, site_no + Nyear ~ season,value.var = "AvgPreds") #need to get wide format #all missing for 1 site!!!FIX THIS
names(castS.Fish)<-c("site_no","Nyear","Spredsfall", "Spredsspring", "Spredssummer", "Spredswinter")
A.FishPreds<-merge(A.Fish,castS.Fish,by=c("site_no","Nyear"))

#Magnitude metrics
S.Fish$LF7Preds[S.Fish$season=="summer"]<-exp(predict(lm.LF7.Sum,S.Fish[S.Fish$season=="summer",],allow.new.levels = T))
S.Fish$LF7Preds[S.Fish$season=="fall"]<-exp(predict(lm.LF7.F,S.Fish[S.Fish$season=="fall",],allow.new.levels = T))
S.Fish$HF3Preds[S.Fish$season=="winter"]<-exp(predict(lm.HF3.W,S.Fish[S.Fish$season=="winter",],allow.new.levels = T))
S.Fish$HF3Preds[S.Fish$season=="spring"]<-exp(predict(lm.HF3.Sp,S.Fish[S.Fish$season=="spring",],allow.new.levels = T))

#for now, create one variable for both magnitude measures: S.Fish$MagPreds
S.Fish$MagPreds<-ifelse(is.na(S.Fish$HF3Preds)==T,S.Fish$LF7Preds,S.Fish$HF3Preds)

#collapse to annual level and merge in fish sample data
castS.Fish2 <- dcast(S.Fish, site_no + Nyear ~ season,value.var = "MagPreds") #need to get wide format #all missing for 1 site!!!FIX THIS
names(castS.Fish2)<-c("site_no","Nyear","Mpredsfall", "Mpredsspring", "Mpredssummer", "Mpredswinter")
A.FishPreds<-merge(A.FishPreds,castS.Fish2,by=c("site_no","Nyear"))

#Binary flood/drought metrics - FIX THESE GLM PREDS
S.Fish$DroughtPreds[S.Fish$season=="summer"]<-predict(glm.DuSum,S.Fish[S.Fish$season=="summer",],allow.new.levels = T)
S.Fish$DroughtPreds[S.Fish$season=="fall"]<-exp(predict(glm.DuF,S.Fish[S.Fish$season=="fall",],allow.new.levels = T))
S.Fish$FloodPreds[S.Fish$season=="winter"]<-exp(predict(glm.DuW,S.Fish[S.Fish$season=="winter",],allow.new.levels = T))
S.Fish$FloodPreds[S.Fish$season=="spring"]<-exp(predict(glm.DuSp,S.Fish[S.Fish$season=="spring",],allow.new.levels = T))

#for now, create one variable for both magnitude measures: S.Fish$DumPreds
S.Fish$DumPreds<-ifelse(is.na(S.Fish$DroughtPreds)==T,S.Fish$FloodPreds,S.Fish$DroughtPreds)

#collapse to annual level and merge in fish sample data
castS.Fish2 <- dcast(S.Fish, site_no + Nyear ~ season,value.var = "MagPreds") #need to get wide format #all missing for 1 site!!!FIX THIS
names(castS.Fish2)<-c("site_no","Nyear","Mpredsfall", "Mpredsspring", "Mpredssummer", "Mpredswinter")
A.FishPreds<-merge(A.FishPreds,castS.Fish2,by=c("site_no","Nyear"))

# #merge in with main data set
# class(A.Fish$site_no);class(castS.Fish$site_no)
# class(A.Fish$Nyear);class(castS.Fish$Nyear)
# names(A.Fish)
# names(castS.Fish)

save(A.FishPreds,file="output/A.FishPreds.rdata")

names(A.FishPreds)
#### 3 - Standardize variables ####

#### 4 - Predict fish outcomes ####
#### OLS linear regression
fit<-lm(EstYOYAbu~Spredsfall+Spredsspring+Spredssummer+Spredswinter+
          Pfall + Pspring + Psummer + Pwinter+
          MaxTfall + MaxTspring + MaxTsummer + MaxTwinter
        ,data=A.FishPreds)
summary(fit)

fitP<-lm(EstYOYAbu ~ Spredsfall+Spredsspring+Spredssummer+Spredswinter #just my seasonal flow estimate vars
           ,data=A.FishPreds)
summary(fitP)

fitK<-lm(EstYOYAbu ~ Pfall + Pspring + Psummer + Pwinter+
           MaxTfall + MaxTspring + MaxTsummer + MaxTwinter #just Kanno P and T
         ,data=A.FishPreds)
summary(fitK)

fitPT<-lm(EstYOYAbu ~ Spredsfall+Spredsspring+Spredssummer+Spredswinter+
           MaxTfall + MaxTspring + MaxTsummer + MaxTwinter #just my Seasonal flow vars + Temp
         ,data=A.FishPreds)
summary(fitPT)

fitMT<-lm(EstYOYAbu ~ Mpredssummer+Mpredsfall+Mpredswinter+Mpredsspring+ #just my magnitude flow vars + Temp
            MaxTfall + MaxTspring + MaxTsummer + MaxTwinter 
          ,data=A.FishPreds)
summary(fitMT)

####FIX THESE
fitMT<-lm(EstYOYAbu ~ Mpredssummer+Mpredsfall+Mpredswinter+Mpredsspring+ #My dummy flow vars + Temp
            MaxTfall + MaxTspring + MaxTsummer + MaxTwinter 
          ,data=A.FishPreds)
summary(fitMT)

fitMT<-lm(EstYOYAbu ~ Mpredssummer+Mpredsfall+Mpredswinter+Mpredsspring+ #My duration flow vars + Temp
            MaxTfall + MaxTspring + MaxTsummer + MaxTwinter 
          ,data=A.FishPreds)
summary(fitMT)


#### 5 - from old file ####
#with standardized vars
fitPs<-lm(EstYOYAbu ~ fallPmin7day + springPmin7day + summerPmin7day + winterPmin7day + 
            winterTempStd,data=AbuPFlows)
summary(fitPs)

step <- stepAIC(fit, direction="both")
step$anova

##Stepwise selects a model with my flow predicitons for both fall and winter!
fitPs<-lm(EstYOYAbu ~ fallPmin7day +  winterPmin7day+ springPmin7day + summerPmin7day + 
            fallPmin7day+winterTempStd+springTempStd+summerTempStd,data=AbuPFlows)
summary(fitPs)

step <- stepAIC(fitPs, direction="both")
step$anova

fitStepW<-lm(EstYOYAbu ~ fallPmin7day + winterPmin7day + summerPmin7day + 
               springTempStd,data=AbuPFlows)
summary(fitStepW)

#### poisson regression
fit2<-glm(RYOYAbu ~ fallPmin7day +  winterPmin7day+ springPmin7day + summerPmin7day + 
            fallPmin7day+winterTempStd+springTempStd+summerTempStd,data=AbuPFlows,family=poisson)
summary(fit2)

#### mixed effects - RIpoisson regression
fit3<-glmer(RYOYAbu~(1|site_no)+fallPmin7day +  summerPmin7day + 
              fallPmin7day+winterTempStd+springTempStd+summerTempStd,data=AbuPFlows,family=poisson)
summary(fit3)
#since this is low flows, just do LFs hypotheses: need higher low flows in fall and summer, so

#first preliminary model to use CV:
model5<-glmer(RYOYAbu ~(1|site_no)+fallPmin7day+ #fallTempStd+ #corr is .69 between fall temp and fall flow - that doesn't make sense...
                winterTempStd+springTempStd
              ,data=AbuPFlows,family=poisson)
summary(model5) 

AbuPFlows$preds<-predict(model5)