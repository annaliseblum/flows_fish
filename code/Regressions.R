##Regressions: to predict abundance
###Annalise Blum
#Created: Nov 24,2016  Updated: Dec 6,2016 
##Data NEEDED for this file: "output/UVAA.FishNNPreds.rdata"; "output/A.FishNNPreds.rdata"
##Data sets created in this file:

rm(list=ls())

load("output/All_fish2.rdata") #Use this version where they are predicted from daily values
#duration works better
All_fishCC<-All_fish2[complete.cases(All_fish2),] #remove observations with ANY missing values

#regression to pred RE
load("output/fishSC.rdata") #all the site characteristics available

R7mag<-lmer(log(EstYOYAbu)~(1|site_no)+log(p95spring) +log(p95winter)
            +log(MaxTfall)+log(MaxTspring)+log(MaxTwinter)
            +log(maxP1fall)+log(maxP1spring)+log(maxP1winter)
            ,data=All_fishCC, REML=F); #summary(R7mag)
length(ranef(R7mag)$site_no)

REdf<-as.data.frame(ranef(R7mag)$site_no)
REdf$site_no<-rownames(REdf)
names(REdf)<-c("RE","site_no")
fishSC_RE<-merge(fishSC,REdf,by="site_no")

lm_RE<-lm(RE~DA_SQKM+LAT_GAGE+LNG_GAGE+Slope_pct+Aspect_deg+Elev_m,fishSC_RE)
summary(lm_RE)
plot(fishSC_RE$Elev_m,fishSC_RE$RE)

#try dummies for non-linearities
dummy <- as.numeric(year == 1957)


#OLDER FILES
# load("output/UVAA.FishNNPreds.rdata") #with UVA sites used in NN
# load("output/A.FishNNPreds.rdata") #just USGS sites used in NN
#load("output/All_fish.rdata") #USGS preds for 1982-1992, UVA 1993-2010

# summary(All_fish); names(All_fish) #missing 117 for first year weather
# All_fishCC<-All_fish[!is.na(All_fish$EstYOYAbu),] #remove observations with missing abundance values
# All_fishCC<-All_fish[complete.cases(All_fish),] #remove observations with ANY missing values

#start with my version of Kanno's model
R1<-lmer(log(EstYOYAbu)~(1|site_no)+log(Psummer)+log(Pfall)+log(Pwinter)+log(Pspring) 
         +log(MaxTsummer) +log(MaxTfall)+log(MaxTwinter)+log(MaxTspring)
            ,data=All_fishCC, REML=F)
summary(R1)
#fixef(R1) #just fe
#points(fixef(R1)[2:8], col="blue")

##plot comparison with Kanno et al 2016####
load("~/flows_fish/output/outpaperFull_df.rdata")
pdf("plots/My_Fig2vsLMERpoints.pdf") #
labels=paste(c("Summer Precip","Fall Precip","Winter Precip","Spring Precip","Summer Temp","Fall Temp",
               "Winter Temp","Spring Temp"))
par(mar = c(7, 4, 4, 2) + 0.1)
plot(fixef(R1)[2:9], col="blue",pch=15,xlab="",ylab="",xaxt="n")
boxplot(outpaperFull_df$`g.0[1,1]`,outpaperFull_df$`g.0[2,1]`,outpaperFull_df$`g.0[3,1]`,outpaperFull_df$`g.0[4,1]`,
        outpaperFull_df$`g.0[5,1]`,outpaperFull_df$`g.0[6,1]`,outpaperFull_df$`g.0[7,1]`,outpaperFull_df$`g.0[8,1]`,
        xaxt="n", xlab="",ylab="effect size",add=TRUE,
        main="Figure 2 from Kanno et al (2016) - All weather covariates")
points(fixef(R1)[2:9], col="blue",pch=15)
axis(1, at=1:8,labels = FALSE)
## Plot x axis labels at default tick marks
text(1:8, par("usr")[3] - 0.03, srt = 45, adj = 1,
     labels = labels, xpd = TRUE)
abline(0,0)
dev.off()

## my models####

#seasonal mean flows instead of Precips
R2<-lmer(log(EstYOYAbu)~(1|site_no)+log(AvgQsummer)+log(AvgQfall)+log(AvgQwinter)+log(AvgQspring) 
         +log(MaxTsummer) +log(MaxTfall)+log(MaxTwinter)+log(MaxTspring)
         ,data=All_fishCC, REML=F)
summary(R2)

anova(R1,R2)

# ##maxT winter, maxT summer and AvgQspring have t-values<2 so omit
# R3<-lmer(log(EstYOYAbu)~(1|site_no)+log(AvgQsummer)+log(AvgQfall)+log(AvgQwinter)+ 
#          log(MaxTfall)+log(MaxTspring)
#          ,data=All_fishCC, REML=F)
# summary(R3)
# 
# anova(R2,R3) #not statistically different

#magnitude - maxTsummer, maxTwinter, p95spring not significant again
R4<-lmer(log(EstYOYAbu)~(1|site_no)+log(p5summer+.001)+log(p5fall+.001)+log(p95winter)+log(p95spring)
          +log(MaxTsummer) +log(MaxTfall)+log(MaxTwinter)+log(MaxTspring)
        ,data=All_fishCC, REML=F)
summary(R4)
anova(R2,R4) #not statistically different

#duration DurHFfall - how to do as power law - gets weird with too many zeros!
R5<-lmer(log(EstYOYAbu)~(1|site_no)+log(DurLFfall+.05)+log(DurHFwinter+.05)+
        log(DurHFspring+.05) +
        #log(DurHFfall+.05)+
          log(DurLFsummer+.05)+
        +log(MaxTsummer) +log(MaxTfall)+log(MaxTwinter)+log(MaxTspring)
        ,data=All_fishCC, REML=F)
summary(R4)
anova(R4,R5)
anova(R2,R4)

# #dummies
# R6<-lmer(log(EstYOYAbu)~(1|site_no)+DroPredsummer+DroPredfall+FloodPredwinter+FloodPredspring
#         +log(MaxTsummer) +log(MaxTfall)+log(MaxTwinter)+log(MaxTspring)
#         ,data=A.FishNNPredsCC, REML=F)
# summary(R6) #6 is wierd
# anova(R1,R2,R3,R4,R5,R6) # 5 is the best, then 2
# anova(R5,R6) #5 is statistically significantly better than 2

##add other variables: log(maxP1winter)+log(maxP1spring)
#START WITH duration DurHFfall
# 1 - Remove insignificant variables MaxTwinter and DurHFspring
#2 - add max daily precip in winter and spring and remove insignicant ones

All_fishCCUVA<-All_fishCC[All_fishCC$Nyear>11,]

R1<-lmer(log(EstYOYAbu)~(1|site_no)+log(Pfall)+log(Pwinter)+log(Pspring) 
         +log(MaxTfall)+log(MaxTwinter)+log(MaxTspring)
         +log(maxP1fall)
         ,data=All_fishCC, REML=F)
summary(R1)
All_fishCC$Abu_preds<-exp(predict(R1)) #for durations predicted from daily time series
RMSEweather<-(mean((All_fishCC$EstYOYAbu- All_fishCC$Abu_preds)^2))^.5

R7dur<-lmer(log(EstYOYAbu)~(1|site_no)+log(DurLFfall+.05)+log(DurHFwinter+.05)+log(DurHFspring+.05) 
         +log(MaxTfall)+log(MaxTspring)+log(MaxTwinter)#+log(MaxTfall)*log(DurLFfall+.05)
        +log(maxP1fall)
         ,data=All_fishCC, REML=F)
summary(R7dur)
All_fishCC$Abu_preds<-exp(predict(R7dur)) #for durations predicted from daily time series
RMSEdur<-(mean((All_fishCC$EstYOYAbu- All_fishCC$Abu_preds)^2))^.5

R7mag<-lmer(log(EstYOYAbu)~(1|site_no)+log(p5fall+.05)+log(p95spring) +log(p95winter)
          +log(MaxTfall)+log(MaxTspring)+log(MaxTwinter)
         +log(maxP1fall)
         ,data=All_fishCC, REML=F)
summary(R7mag)
All_fishCC$Abu_preds<-exp(predict(R7mag)) #for durations predicted from daily time series
RMSEmag<-(mean((All_fishCC$EstYOYAbu- All_fishCC$Abu_preds)^2))^.5

R7avg<-lmer(log(EstYOYAbu)~(1|site_no)+log(AvgQfall)+log(AvgQwinter)+log(AvgQspring) 
          +log(MaxTfall)+log(MaxTspring)+log(MaxTwinter)
         +log(maxP1fall)
         ,data=All_fishCC, REML=F)
summary(R7avg)

All_fishCC$Abu_preds<-exp(predict(R7avg)) #for durations predicted from daily time series
RMSEavg<-(mean((All_fishCC$EstYOYAbu- All_fishCC$Abu_preds)^2))^.5

#comparison
anova(R1,R7dur,R7mag,R7avg)
anova(R1,R7dur)

RMSEweather; RMSEdur;RMSEmag; RMSEavg

#check correlation, maxTsummer and intercept are correlated 0.8 so remove maxT summer
R8<-lmer(log(EstYOYAbu)~(1|site_no)+log(DurLFfall+.05)+log(DurHFwinter+.05)+log(DurHFfall+.05) 
          +log(MaxTfall)+log(MaxTspring)
         +log(maxP1fall)+log(maxP1winter)
         ,data=All_fishCC, REML=F)
summary(R8)
anova(R7,R8)

#add back other flow variables? +log(AvgQsummer)+log(AvgQfall)+log(AvgQwinter)+ 
R9<-lmer(log(EstYOYAbu)~(1|site_no)+log(DurLFfall+.05)+log(DurHFfall+.05) 
         +log(MaxTfall)+log(MaxTspring)
         +log(AvgQfall)+log(AvgQwinter)+ log(AvgQspring)+ #log(AvgQsummer)+
         +log(maxP1fall)
         ,data=All_fishCC, REML=F)
summary(R9)
anova(R7,R9)

##add interactions: Hyp: LF and temp fall?
R10<-lmer(log(EstYOYAbu)~(1|site_no)+log(DurLFfall+.05)+log(DurHFwinter+.05)+log(DurHFfall+.05) 
         +log(MaxTfall)*log(DurLFfall+.05)+log(MaxTspring)
         +log(maxP1fall)+log(maxP1winter)
         ,data=A.FishNNPredsCC, REML=F)
summary(R10)
anova(R9,R10)

##compare best to standardization one

R11<-lmer(log(EstYOYAbu)~(1|site_no)+log(DurLFfall+.05)+log(DurHFwinter+.05)+log(DurHFfall+.05) 
          +log(MaxTfall)*log(DurLFfall+.05)+log(MaxTspring)
          #+log(maxP1fall)+log(maxP1winter)
          ,data=A.FishNNPredsS, REML=F)
summary(R11)

R12<-lmer(log(EstYOYAbu)~(1|site_no)+StdDurLFfall+StdDurHFwinter+StdDurHFfall
          +StdMaxTfall*StdDurLFfall+StdMaxTspring
          #+StdmaxP1fall+StdmaxP1winter
          ,data=A.FishNNPredsS, REML=F)
summary(R12)
anova(R11,R12)


#Final method
plot(predict(R10),resid(R10)); abline(0,0)
qqnorm(resid(R10),xlab="Residual",ylab="Percent",main="Normal Probability Plot")
qqline(resid(R10)); grid()
shapiro.test(resid(R10)) #p-value = 0.001618; R10 not normal

#bias
sum(log(A.FishNNPredsCC$EstYOYAbu)>predict(R10))/length(predict(R10))

#log space
qplot(log(A.FishNNPredsCC$EstYOYAbu), predict(R10),
      xlab="log Empirical",
      ylab="log Predicted",main="Brook Trout YOY Abundance: Power law Model") +geom_abline(intercept = 0, slope = 1)

#real space
qplot(A.FishNNPredsCC$EstYOYAbu, exp(predict(R10)),
      xlab="Empirical",
      ylab="Predicted",main="Brook Trout YOY Abundance: Power law Model") +geom_abline(intercept = 0, slope = 1)

##### figures for selected model ##### 
#boxplots and correlation of explanatory vars - Duration
pdf("plots/Corr_DurationMod.pdf")
pairs(~DurLFfall+DurHFwinter+DurHFfall+MaxTfall+MaxTspring+maxP1fall+maxP1winter, 
      data=A.FishNNPredsCC, lower.panel = panel.smooth,upper.panel= NULL, 
      main="Covariate correlation - Selected model")
dev.off()

#boxplots
p2<-ggplot(A.FishNNPredsCC, aes(x=as.factor(Nyear), y=EstYOYAbu)) + theme_bw() + #, color=as.factor(fish_site))
  geom_boxplot(fill="grey")+ geom_smooth(se=F, aes(group=1),color="darkgreen")+coord_cartesian(ylim=c(0, 500)) +
  labs(x = "",y="Estimated YOY Abundance")
#+ scale_x_discrete(breaks=NULL) #+ annotate("text", x = 28, y = 400, label = "Some text") 
#+theme(axis.text.x = element_blank(), axis.text.y = element_blank(), axis.ticks = element_blank())

p3<-ggplot(A.FishNNPredsCC, aes(x=as.factor(Nyear), y=DurLFfall)) + #, color=as.factor(fish_site))
  geom_boxplot()+ geom_smooth(se=F, aes(group=1)) + labs(x = "", y="Fall low flow duration")

p4<-ggplot(A.FishNNPredsCC, aes(x=as.factor(Nyear), y=DurHFwinter)) + #, color=as.factor(fish_site))
  geom_boxplot()+ geom_smooth(se=F, aes(group=1))+ labs(x = "", y="Winter high flow duration")

p5<-ggplot(A.FishNNPredsCC, aes(x=as.factor(Nyear), y=DurHFfall)) + #, color=as.factor(fish_site))
  geom_boxplot()+ geom_smooth(se=F, aes(group=1))+ labs(x = "", y="Fall high flow duration")

p6<-ggplot(A.FishNNPredsCC, aes(x=as.factor(Nyear), y=MaxTfall)) + #, color=as.factor(fish_site))
  geom_boxplot()+ geom_smooth(se=F, aes(group=1))+ labs(x = "", y="Fall  max temperature")

p7<-ggplot(A.FishNNPredsCC, aes(x=as.factor(Nyear), y=MaxTspring)) + #, color=as.factor(fish_site))
  geom_boxplot()+ geom_smooth(se=F, aes(group=1))+ labs(y="Spring max temperature")

pdf("plots/fish_Weather.pdf",width=8,height=12) #
multiplot(p2,p3,p4,p6,p7, cols=1)
dev.off()


plot(log(A.FishNNPredsCC$EstYOYAbu), predict(R9),
     xlab="Empirical",
     ylab="Predicted",main="Brook Trout YOY Abundance"); abline(0,1) #Empirical vs Predicted

##other functional forms: standardization? RHS variables non-linear?

#### more plots ####
plot(predict(R5),resid(R5)); abline(0,0)
qqnorm(resid(R5),xlab="Residual",ylab="Percent",main="Normal Probability Plot")
qqline(resid(R5)); grid()
shapiro.test(resid(R5)) #p-value = 0.001618

qplot(log(A.FishNNPredsCC$EstYOYAbu), predict(R9),
      xlab="Empirical",
      ylab="Predicted",main="Brook Trout YOY Abundance: Linear Model")

plot(log(A.FishNNPredsCC$EstYOYAbu), predict(R9),
     xlab="Empirical",
     ylab="Predicted",main="Brook Trout YOY Abundance"); abline(0,1) #Empirical vs Predicted
points(log(A.FishNNPredsCC$EstYOYAbu), predict(R5), col="blue")
points(log(A.FishNNPredsCC$EstYOYAbu), predict(fR11), col="red")

sum(log(A.FishNNPredsCC$EstYOYAbu)>predict(R5))/length(predict(R9)) # 0.5085911 NO BIAS!!


##FINAL MODEL R5 WITH JUST DURATION AND FALL AND SPRING TEMPS DOES WELL
fR5<-lmer(log(EstYOYAbu)~(1|site_no)+log(DurLFfall+.05)+log(DurHFwinter+.05)+ log(DurHFfall+.05)+
            +log(MaxTfall)+log(MaxTspring)
         ,data=A.FishNNPredsCC, REML=F)
summary(fR5)

##FINAL MODEL R5 WITH JUST DURATION AND FALL AND SPRING TEMPS DOES WELL
fR6<-lmer(log(EstYOYAbu)~(1|site_no)+log(DurLFfall+.05)+log(DurHFwinter+.05)+ log(DurHFfall+.05)+
           +log(MaxTfall)+log(MaxTspring)
         +log(AvgQfall)+log(AvgQwinter)+ log(AvgQspring) #log(AvgQsummer)+
           ,data=A.FishNNPredsCC, REML=F)
summary(fR6)
anova(fR5,fR6)

#model R9 BETTER BUT HAS A LOT MORE VARIABLES
fR9<-lmer(log(EstYOYAbu)~(1|site_no)+log(DurLFfall+.05)+log(DurHFwinter+.05)+log(DurHFfall+.05) 
         +log(MaxTfall)+log(MaxTspring)
         +log(maxP1fall)+log(maxP1winter)
         ,data=A.FishNNPredsCC, REML=F)
summary(fR9)
anova(fR5,fR9)

fR11<-lmer(log(EstYOYAbu)~(1|site_no)+log(DurLFfall+.05)+log(DurHFwinter+.05)+log(DurHFfall+.05)
          +log(maxP1fall)+log(maxP1winter)
          ,data=A.FishNNPredsCC, REML=F)
summary(fR11)
anova(fR9,fR11)

#Predicted vs Observed
A.FishNNPredsCC$Preds.lm<-predict(lm1)
p1=ggplot( A.FishNNPredsCC,aes(x=log(EstYOYAbu), y=Preds.lm))+
  labs(x="Log(Empirical)", y="Log(Predicted)", title="Power Law Model")+
  geom_point()+geom_abline(intercept = 0,slope=1)

A.FishNNPredsCC$Preds.lmRS<-exp(predict(lm1))
p2=ggplot( A.FishNNPredsCC,aes(x=EstYOYAbu, y=Preds.lmRS))+
  labs(x="Empirical", y="Predicted")+
  geom_point()+geom_abline(intercept = 0,slope=1)

NSE(A.FishNNPredsCC$Preds.lm,log(A.FishNNPredsCC$EstYOYAbu))
NSE(A.FishNNPredsCC$Preds.lm,A.FishNNPredsCC$EstYOYAbu)

NSE(A.FishNNPredsCC$Preds.lmer,log(A.FishNNPredsCC$EstYOYAbu))
NSE(A.FishNNPredsCC$Preds.lmer,A.FishNNPredsCC$EstYOYAbu)

A.FishNNPredsCC$Preds.lmer<-predict(lmer1)
p3=ggplot( A.FishNNPredsCC,aes(x=log(EstYOYAbu), y=Preds.lmer))+
  labs(x="Log(Empirical)", y="Log(Predicted)", title="ME Model")+
  geom_point()+geom_abline(intercept = 0,slope=1)

A.FishNNPredsCC$Preds.lmerRS<-exp(predict(lmer1))
p4=ggplot( A.FishNNPredsCC,aes(x=EstYOYAbu, y=Preds.lmerRS))+
  labs(x="Empirical", y="Predicted", title="")+
  geom_point()+geom_abline(intercept = 0,slope=1)

#multi-plot
multiplot(p1, p2, p3, p4, cols=2)

plot(predict(lm1),resid(lm1),xlab="Predicted abundance",ylab="Residuals",
     main="Homoskedastic?"); abline(0,0) #Heterskedascity?

##add residuals to data set to plot
A.FishNNPredsCC$resid1<-resid(lm1)

A.FishNNPredsCC$resid1
ggplot(A.FishNNPredsCC, aes(x=as.factor(Nyear), y=resid1)) + #, color=as.factor(fish_site))
  geom_boxplot()+ geom_smooth( se=TRUE, aes(group=1)) #+scale_y_continuous(limits = c(0,400))

#residuals
qqnorm(resid(lm1),xlab="Residual",ylab="Percent",main="Normal Probability Plot")
qqline(resid(lm1)); grid()

plot(predict(lm1),resid(lm1),xlab="Predicted abundance",ylab="Residuals",
     main="Homoskedastic?"); abline(0,0) #Heterskedascity?

A.FishNNPredsCC$resid1<-resid(lm1)
ggplot(A.FishNNPredsCC, aes(x=as.factor(Nyear), y=resid1)) + #, color=as.factor(fish_site))
  geom_boxplot()+ geom_smooth( se=TRUE, aes(group=1))
