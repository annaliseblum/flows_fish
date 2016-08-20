##Preliminary frequentist regressions
##July 19,2016; updated Aug 19, 2016
##Annalise Blum

#### OLS linear regression ####
fit<-lm(EstYOYAbu~RRpredsfall+RRpredsspring+RRpredssummer+RRpredswinter+
          Pfall + Pspring + Psummer + Pwinter+
          MaxTfall + MaxTspring + MaxTsummer + MaxTwinter
        ,data=A.FWC_RR)
summary(fit)

fitP<-lm(EstYOYAbu ~ RRpredsfall + RRpredsspring + RRpredssummer + RRpredswinter + 
  MaxTwinter,data=A.FWC_RR)
summary(fitP)

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

#### poisson regression ####
fit2<-glm(RYOYAbu ~ fallPmin7day +  winterPmin7day+ springPmin7day + summerPmin7day + 
            fallPmin7day+winterTempStd+springTempStd+summerTempStd,data=AbuPFlows,family=poisson)
summary(fit2)

#### mixed effects - RIpoisson regression ####
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

#### Average across sites - annual-level TS regression ####
A.FWC$tally<-1
avgSites <- ddply(A.FWC, .(year.f), summarize, 
                 sitesperyear=sum(tally),
                 YOY_P1 = mean(YOY_P1, na.rm = T),
                 Pfall = mean(Pfall, na.rm = T),
                 Pspring = mean(Pspring, na.rm = T),
                 Psummer = mean(Psummer, na.rm = T),
                 Pwinter = mean(Pwinter, na.rm = T),
                 MaxTfall = mean(MaxTfall, na.rm = T),
                 MaxTspring = mean(MaxTspring, na.rm = T),
                 MaxTsummer = mean(MaxTsummer, na.rm = T),
                 MaxTwinter = mean(MaxTwinter, na.rm = T),
                 Aspect_deg = mean(Aspect_deg, na.rm = T),
                 Slope_deg = mean(Slope_deg, na.rm = T),
                 Elev_m = mean(Elev_m, na.rm = T),
                 Lat_n83 = mean(Aspect_deg, na.rm = T),
                 Lon_n83 = mean(Aspect_deg, na.rm = T)
                 )

#drop 1994 because we don't have weather data for summer or fall before fish sample
avgSites<-avgSites[avgSites$year>1994,] #so now only 16 years of data...not ideal

plot(as.factor(avgSites$year.f),avgSites$YOY_P1)
lines(as.factor(avgSites$year.f),avgSites$YOY_P1,type = "l")

#Pfall + Pspring + Psummer + Pwinter + MaxTfall + MaxTspring + MaxTsummer + MaxTwinter
fit<-lm(YOY_P1~Pwinter+MaxTfall+MaxTwinter,data=avgSites)
summary(fit)

fit2<-lm(YOY_P1~scale(Pfall)+scale(Pwinter)+scale(MaxTfall),data=avgSites)
summary(fit2)

#### Try using % of gaged sites with floods or droughts that season as predictors ####

#merge Extremes with fish data
avgSitesEx<-merge(avgSites,Extremes,by="year.f")

#replacing winter precip with fraction of sites with a winter flood increases adjR2 from .59 to .72
fit2<-lm(YOY_P1~Flwinter+MaxTfall+MaxTwinter,data=avgSitesEx)
summary(fit2)

#fall drought not helpful at all Drfall
#try stepwise
fit2<-lm(YOY_P1~Drfall+Drspring+Drsummer+Drwinter+Flfall+Flspring+Flsummer+Flwinter,
         data=avgSitesEx)
summary(fit2)

step <- stepAIC(fit2, direction="both")
step$anova

#final anova model
fit3<-lm(YOY_P1 ~ Drspring + Flsummer + Flwinter+MaxTfall+MaxTwinter+Pfall + Psummer + Pwinter + 
           MaxTspring,data=avgSitesEx)
summary(fit3)
step <- stepAIC(fit3, direction="both")
step$anova

