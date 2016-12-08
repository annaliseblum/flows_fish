##AGU 2016 plots

load("output/All_fish2.rdata") #Use this version where they are predicted from daily values
All_fishCC<-All_fish2[complete.cases(All_fish2),] #remove observations with ANY missing values

# Figure 1 box plots and trend of estimated abundance
pdf("plots/P1_AbuTS.pdf",width=8,height=5) #
ggplot(All_fishCC, aes(x=as.factor(Nyear), y=EstYOYAbu)) + theme_bw() + #, color=as.factor(fish_site))
  geom_boxplot(fill="grey")+ geom_smooth(se=F, aes(group=1),color="darkgreen")+coord_cartesian(ylim=c(0, 500)) +
  labs(x = "",y="Estimated YOY Abundance")
dev.off()

# Table 1 - comparison of models assess - just AIC
R1<-lmer(log(EstYOYAbu)~(1|site_no)+log(Pfall)+log(Pwinter)+log(Pspring) 
         +log(MaxTfall)+log(MaxTwinter)+log(MaxTspring)
         +log(maxP1fall)+log(maxP1spring)+log(maxP1winter)
                  ,data=All_fishCC, REML=F)

R7durNEW<-lmer(log(EstYOYAbu)~(1|site_no)+log(DurLFfall+.05)+log(DurHFwinter+.05)+log(DurHFspring+.05) 
            +log(MaxTfall)+log(MaxTspring)+log(MaxTwinter)
            +log(maxP1fall)+log(maxP1spring)+log(maxP1winter)
            ,data=All_fishCC, REML=F); summary(R7durNEW)

summary(R7dur)
anova(R1,R7durNEW)
anova(R1,R7avg,R7mag,R7durNEW)

R7mag<-lmer(log(EstYOYAbu)~(1|site_no)+log(p95spring) +log(p95winter)
            +log(MaxTfall)+log(MaxTspring)+log(MaxTwinter)
            +log(maxP1fall)+log(maxP1spring)+log(maxP1winter)
            ,data=All_fishCC, REML=F); #summary(R7mag)

R7avg<-lmer(log(EstYOYAbu)~(1|site_no)+log(AvgQfall)+log(AvgQwinter)+log(AvgQspring) 
            +log(MaxTfall)+log(MaxTspring)+log(MaxTwinter)
            +log(maxP1fall)+log(maxP1spring)+log(maxP1winter)
                        ,data=All_fishCC, REML=F)
summary(R7avg)
anova(R1,R7dur,R7mag,R7avg)
anova(R7avg,R7mag)

# Figure 2 - side by side 3-D plots showing extreme precip more useful than total precip (and my flows even more useful??)
# # - use 1 color getting darker not rainbow
# - cover the part that is “unfair” with white
# - research smoothing for these plots to remove misleading bumps (maybe I can do this by rounding my data!!)

summary(All_fishCC)

#round variables to smooth plot
All_fishCC$rAvgQfall<-round(All_fishCC$AvgQfall)
All_fishCC$rp95winter<-round(All_fishCC$p95winter)
All_fishCC$rEstYOYAbu<-round(All_fishCC$EstYOYAbu)

install.packages("viridis")
library(viridis)

require(akima); library(rworldmap)

#1 Pfall vs Pwinter
d3plot1 <- with(All_fishCC, interp(x= Pfall , y= Pwinter, z=EstYOYAbu, duplicate="mean"))
filled.contour(d3plot1,
               xlab="Fall precipitation (mm)",
               ylab="Winter precipitation (mm)",
               main="Estimated Abundance", color.palette=plasma,nlevels=6, 
               #nlevels = 10,col = YIOrRd(10),
               #plot.axes={points(All_fishCC$Pfall, All_fishCC$Pwinter)}
               # plot.axes = {
               #   axis(1)
               #   axis(2)
               #   contour(d3plot1, add=T)
               # }
)

#AvgQfall p95winter
d3plot1 <- with(All_fishCC, interp(x= Pfall , y= Pwinter, z=EstYOYAbu, duplicate="mean"))
filled.contour(d3plot1,
               xlab="Fall precipitation (mm)",
               ylab="95%ile flow winter (cfs)",
               main="Estimated Abundance", color.palette=plasma,nlevels=6, 
              #nlevels = 10,col = YIOrRd(10),
               plot.axes={points(All_fishCC$Pfall, All_fishCC$Pwinter)}
               # plot.axes = {
               #   axis(1)
               #   axis(2)
               #   contour(d3plot1, add=T)
               # }
               )

d3plot1 <- with(All_fishCC, interp(x= maxP3fall , y= p95winter, z=EstYOYAbu, duplicate="mean"))
filled.contour(d3plot1,  
               xlab="fall max 3day precip",
               ylab="95%ile flow in winter (cfs)",
               main="Estimated Abundance",color.palette=plasma,nlevels=6, 
               #plot.axes={points(All_fishCC$maxP3fall, All_fishCC$p95winter)}
               # plot.axes = {
               #   axis(1)
               #   axis(2)
               #   contour(d3plot1, add=T)
               # }
)

plot(All_fishCC$maxP3fall,All_fishCC$EstYOYAbu)
summary(lm(EstYOYAbu~maxP1fall+maxP1winter+maxP1spring,data=All_fishCC))


# Figure 4 - 4 subplots of coefficients from JAGS
# - do ppcheck  to make sure my output is ok, part of JAGSUI

#make sure to get the variables right:
# b[1,i,j]*SummerFlow[i,t] + b[2,i,j]*FallFlow[i,t] +  #remove is to make shared Bs
#   b[3,i,j]*WinterFlow[i,t] + b[4,i,j]*SpringFlow[i,t] +
#   b[5,i,j]*summer.temp[i,t] + b[6,i,j]*fall.temp[i,t] +
#   b[7,i,j]*winter.temp[i,t] + b[8,i,j]*spring.temp[i,t] 

# summer.temp=MaxP1fallStd, fall.temp=fallTempAryStd, #summerTempAryStd
# winter.temp=winterTempAryStd, spring.temp=springTempAryStd,
# SummerFlow=MaxP1winStd, FallFlow=MaxP1springStd,
# WinterFlow=AvgQwinterStd, SpringFlow=AvgQspringStd,

pdf("plots/maxPAvgTestflows.pdf") #
#labels
labels=paste(c("MaxP1winStd","MaxP1springStd","AvgQwinterStd","AvgQspringStd","MaxP1fallStd","Fall Temp","Winter Temp","Spring Temp"))
par(mar = c(7, 4, 4, 2) + 0.1)
## Create plot with no x axis and no x axis label
boxplot(outJUI_Dec8$sims.list$g.0[,1,1],outJUI_Dec8$sims.list$g.0[,2,1],outJUI_Dec8$sims.list$g.0[,3,1],outJUI_Dec8$sims.list$g.0[,4,1],
        outJUI_Dec8$sims.list$g.0[,5,1],outJUI_Dec8$sims.list$g.0[,6,1],outJUI_Dec8$sims.list$g.0[,7,1],outJUI_Dec8$sims.list$g.0[,8,1],
        col = c("gold","orange","lightblue","lightgreen"),  xaxt="n", xlab="",ylab="effect size",
        main="Magnitude of low (5%ile) and high (95%) flows")
axis(1, at=1:8,labels = FALSE)
## Plot x axis labels at default tick marks
text(1:8, par("usr")[3] - 0.03, srt = 45, adj = 1,
     labels = labels, xpd = TRUE)
abline(0,0)
dev.off()

#compare to
R7mag<-lmer(log(EstYOYAbu)~(1|site_no)+log(maxP1winter)+log(maxP1spring)+log(AvgQwinter) +log(AvgQspring)+log(maxP1fall)+
          log(MaxTfall)+log(MaxTspring)+log(MaxTwinter)
            ,data=All_fishCC, REML=F); #summary(R7mag)
summary(R7mag)


# Figure 5 - explaining plot of RE vs DA? or other variables?

#### Figure 3 selected model obs vs predicted with % bias and fraction of obs>preds ####
R7mag<-lmer(log(EstYOYAbu)~(1|site_no)+log(p95spring) +log(p95winter)
            +log(MaxTfall)+log(MaxTspring)+log(MaxTwinter)
            +log(maxP1fall)+log(maxP1spring)+log(maxP1winter)
            ,data=All_fishCC, REML=F); #summary(R7mag)
length(ranef(R7mag)$site_no)

All_fishCC$Preds.lm<-predict(R7mag)
p1=ggplot(R7mag,aes(x=log(EstYOYAbu), y=Preds.lm))+
  labs(x="Log(Empirical)", y="Log(Predicted)", title="Power Law Model")+
  geom_point()+geom_abline(intercept = 0,slope=1)

All_fishCC$Preds.lmRS<-exp(predict(R7mag))
p2=ggplot( All_fishCC,aes(x=EstYOYAbu, y=Preds.lmRS))+
  labs(x="Empirical", y="Predicted")+
  geom_point()+geom_abline(intercept = 0,slope=1)

#Final method
plot(predict(R7mag),resid(R7mag)); abline(0,0)
qqnorm(resid(R7mag),xlab="Residual",ylab="Percent",main="Normal Probability Plot")
qqline(resid(R7mag)); grid()
shapiro.test(resid(R7mag)) #p-value = 0.001618; R10 not normal

#bias
sum(log(All_fishCC$EstYOYAbu)>predict(R7mag))/length(predict(R7mag)) #p used in ecology = 0.54
100*mean((exp(predict(R7mag))-All_fishCC$EstYOYAbu)/All_fishCC$EstYOYAbu) #%mean bias mean(pred-observed)/observed =47.85

#log space
qplot(log(A.FishNNPredsCC$EstYOYAbu), predict(R10),
      xlab="log Empirical",
      ylab="log Predicted",main="Brook Trout YOY Abundance: Power law Model") +geom_abline(intercept = 0, slope = 1)

#real space
qplot(A.FishNNPredsCC$EstYOYAbu, exp(predict(R10)),
      xlab="Empirical",
      ylab="Predicted",main="Brook Trout YOY Abundance: Power law Model") +geom_abline(intercept = 0, slope = 1)
