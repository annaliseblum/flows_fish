##combine and prelim frequentist models

library(reshape2)
#merge fish long with predicted flows
names(fish_long)
names(NNpreds)

names(fish_long)<-c("fish_site","year","YOYP1_C")
class(NNpreds$fish_site)

#need to reshape NNpreds to be at annual level with seasonal mins as variables
NNpreds2<-NNpreds[c("fish_site","year","season","NN_pred")]
NNpredsM <- dcast(NNpreds2, fish_site + year ~ season)

dataAll<-merge(fish_long,NNpredsM,by=c("fish_site","year"))

fit1<-lm(YOYP1_C~scale(fall)+scale(spring)+scale(summer)+scale(winter),dataAll)
summary(fit1)

fit2<-glm(YOYP1_C~scale(fall)+scale(spring)+scale(summer),dataAll,family=poisson)
summary(fit2)

library(lme4)

fit3<-glmer(YOYP1_C~scale(fall)+scale(spring)+scale(summer)+(1|fish_site),dataAll,family=poisson)
summary(fit3)

##Try adding weather
table(SDAYMET$site_no)

dataAll$site_no<-dataAll$fish_site
head(dataAll)

dataAll2<-merge(dataAll,annual.DAYMET,by=c("site_no","year"))

#doesn't match Kanno at all!
fit4<-glm(YOYP1_C~scale(fall)+scale(spring)+scale(summer)+
            scale(Pfall)+scale(Pspring)+scale(Psummer)+ scale(Pwinter)+scale(MaxTfall)+scale(MaxTspring)+scale(MaxTsummer)+
            scale(MaxTwinter)+scale(MinTfall)+ 
            scale(MinTspring)+scale(MinTsummer)+ scale(MinTwinter),dataAll2,family=poisson)
summary(fit4)

#not scaled the same basically
fit5<-glm(YOYP1_C~winter+fall+spring+summer+
            Pfall+Pspring+Psummer+ Pwinter+MaxTfall+MaxTspring+MaxTsummer+
            MaxTwinter+MinTfall+ 
            MinTspring+MinTsummer+ MinTwinter,dataAll2,family=poisson)
summary(fit5)

#remove flow - STILL TOTALLY NOT CONSISTENT WITH KANNO
fit6<-glm(YOY_P1~Pspring+ Pwinter+MaxTspring+
            MaxTwinter,A.FWC,family=poisson)
summary(fit6)

cor(c(dataAll2$spring,dataAll2$summer, dataAll2$winter, dataAll2$fall,
      dataAll2$Pfall,dataAll2$Pspring,dataAll2$Psummer, dataAll2$Pwinter,dataAll2$MaxTfall,dataAll2$MaxTspring,dataAll2$MaxTsummer,
      dataAll2$MaxTwinter,dataAll2$MinTfall, dataAll2$MinTspring,dataAll2$MinTsummer, dataAll2$MinTwinter))

dim(dataAll2)
str(dataAll2)
View(cor(as.matrix(dataAll2[,5:20])))

