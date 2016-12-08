##NEW Explore: make Plots
###Annalise Blum
#Created Oct 31,2016; Last Modified: Oct 31,2016

load("output/A.FishNNPreds.rdata")

load("~/flows_fish/YK/Site_by_year seasonal climate var standardized 115.rdata")

####Reshape standardized variable matrices into one data set ####
# summer.temp=summerTempAryStd, fall.temp=fallTempAryStd,
# winter.temp=winterTempAryStd, spring.temp=springTempAryStd,

# SummerFlow=summerPrcpAryStd, FallFlow=fallPrcpAryStd,
# WinterFlow=winterPrcpAryStd, SpringFlow=springPrcpAryStd,

# SummerFlow=AvgQsummerStd, FallFlow=AvgQfallStd,
# WinterFlow=AvgQwinterStd, SpringFlow=AvgQspringStd,

# SummerFlow=MagLFsumStd, FallFlow=MagLFfallStd,
# WinterFlow=MagHFwinStd, SpringFlow=MagHFspringStd, 

# SummerFlow=DurLFsumStd, FallFlow=DurLFfallStd,
# WinterFlow=DurHFwinStd, SpringFlow=DurHFsprStd, 


library(reshape2)

#Melt all the variables to long

#Temps
# summer.temp=summerTempAryStd, fall.temp=fallTempAryStd,
# winter.temp=winterTempAryStd, spring.temp=springTempAryStd,

msummerTempAryStd <- reshape2::melt(summerTempAryStd)
names(msummerTempAryStd)<-c("site_no","year","summerTempAryStd")
mfallTempAryStd <- reshape2::melt(fallTempAryStd)
names(mfallTempAryStd)<-c("site_no","year","fallTempAryStd")
mwinterTempAryStd <- reshape2::melt(winterTempAryStd)
names(mwinterTempAryStd)<-c("site_no","year","winterTempAryStd")
mspringTempAryStd <- reshape2::melt(springTempAryStd)
names(mspringTempAryStd)<-c("site_no","year","springTempAryStd")

Temps<-merge(msummerTempAryStd,mfallTempAryStd, by=c("site_no","year"))
Temps<-merge(Temps,mwinterTempAryStd, by=c("site_no","year"))
Temps<-merge(Temps,mspringTempAryStd, by=c("site_no","year"))
#cor(Temps[,3:6])

#Precip
# SummerFlow=summerPrcpAryStd, FallFlow=fallPrcpAryStd,
# WinterFlow=winterPrcpAryStd, SpringFlow=springPrcpAryStd,
msummerPrcpAryStd <- reshape2::melt(summerPrcpAryStd)
names(msummerPrcpAryStd)<-c("site_no","year","summerPrcpAryStd")
mfallPrcpAryStd <- reshape2::melt(fallPrcpAryStd)
names(mfallPrcpAryStd)<-c("site_no","year","fallPrcpAryStd")
mwinterPrcpAryStd <- reshape2::melt(winterPrcpAryStd)
names(mwinterPrcpAryStd)<-c("site_no","year","winterPrcpAryStd")
mspringPrcpAryStd <- reshape2::melt(springPrcpAryStd)
names(mspringPrcpAryStd)<-c("site_no","year","springPrcpAryStd")

Precip<-merge(msummerPrcpAryStd,mfallPrcpAryStd, by=c("site_no","year"))
Precip<-merge(Precip,mwinterPrcpAryStd, by=c("site_no","year"))
Precip<-merge(Precip,mspringPrcpAryStd, by=c("site_no","year"))

#Avg flow
# SummerFlow=AvgQsummerStd, FallFlow=AvgQfallStd,
# WinterFlow=AvgQwinterStd, SpringFlow=AvgQspringStd,
mAvgQsummerStd <- reshape2::melt(AvgQsummerStd)
names(mAvgQsummerStd)<-c("site_no","year","AvgQsummerStd")
mAvgQfallStd <- reshape2::melt(AvgQfallStd)
names(mAvgQfallStd)<-c("site_no","year","AvgQfallStd")
mAvgQwinterStd <- reshape2::melt(AvgQwinterStd)
names(mAvgQwinterStd)<-c("site_no","year","AvgQwinterStd")
mAvgQspringStd <- reshape2::melt(AvgQspringStd)
names(mAvgQspringStd)<-c("site_no","year","AvgQspringStd")

SAvg<-merge(mAvgQsummerStd,mAvgQfallStd, by=c("site_no","year"))
SAvg<-merge(SAvg,mAvgQwinterStd, by=c("site_no","year"))
SAvg<-merge(SAvg,mAvgQspringStd, by=c("site_no","year"))

#Magnitude
# SummerFlow=MagLFsumStd, FallFlow=MagLFfallStd,
# WinterFlow=MagHFwinStd, SpringFlow=MagHFspringStd, 
mMagLFsumStd <- reshape2::melt(MagLFsumStd)
names(mMagLFsumStd)<-c("site_no","year","MagLFsumStd")
mMagLFfallStd <- reshape2::melt(MagLFfallStd)
names(mMagLFfallStd)<-c("site_no","year","MagLFfallStd")
mMagHFwinStd <- reshape2::melt(MagHFwinStd)
names(mMagHFwinStd)<-c("site_no","year","MagHFwinStd")
mMagHFspringStd <- reshape2::melt(MagHFspringStd)
names(mMagHFspringStd)<-c("site_no","year","MagHFspringStd")

Magnitude<-merge(mMagLFsumStd,mMagLFfallStd, by=c("site_no","year"))
Magnitude<-merge(Magnitude,mMagHFwinStd, by=c("site_no","year"))
Magnitude<-merge(Magnitude,mMagHFspringStd, by=c("site_no","year"))

#Duration
# SummerFlow=DurLFsumStd, FallFlow=DurLFfallStd,
# WinterFlow=DurHFwinStd, SpringFlow=DurHFsprStd, 
mDurLFsumStd <- reshape2::melt(DurLFsumStd)
names(mDurLFsumStd)<-c("site_no","year","DurLFsumStd")
mDurLFfallStd <- reshape2::melt(DurLFfallStd)
names(mDurLFfallStd)<-c("site_no","year","DurLFfallStd")
mDurHFwinStd <- reshape2::melt(DurHFwinStd)
names(mDurHFwinStd)<-c("site_no","year","DurHFwinStd")
mDurHFsprStd <- reshape2::melt(DurHFsprStd)
names(mDurHFsprStd)<-c("site_no","year","DurHFsprStd")

Duration<-merge(mDurLFsumStd,mDurLFfallStd, by=c("site_no","year"))
Duration<-merge(Duration,mDurHFwinStd, by=c("site_no","year"))
Duration<-merge(Duration,mDurHFsprStd, by=c("site_no","year"))

#Then merge them all together into one data set that I can make correlation plots
TempPre<-merge(Temps,Precip, by=c("site_no","year"))
TempPreSA<-merge(TempPre,SAvg, by=c("site_no","year"))
TempPreSAMag<-merge(TempPreSA,Magnitude, by=c("site_no","year"))
TempPreSAMagDur<-merge(TempPreSAMag,Duration, by=c("site_no","year")) ##FIX DURATION

save(TempPreSAMagDur,file="output/TempPreSAMagDur.rdata")

#### Covariate correlation plots ####
load("output/TempPreSAMag.rdata")
names(TempPreSAMag)

pdf("plots/Corr_Summer.pdf")
pairs(~summerTempAryStd + summerPrcpAryStd + AvgQsummerStd+MagLFsumStd+DurLFsumStd,
      data=TempPreSAMagDur, lower.panel = panel.smooth,
      upper.panel= panel.cor, main="Covariate correlation - Summer")
dev.off()

pdf("plots/Corr_Fall.pdf") #
pairs(~fallTempAryStd + fallPrcpAryStd + AvgQfallStd+MagLFfallStd+DurLFfallStd,
      data=TempPreSAMagDur, lower.panel = panel.smooth,
      upper.panel= panel.cor, main="Covariate correlation - Fall")
dev.off()

pdf("plots/Corr_Winter.pdf") #
pairs(~winterTempAryStd + winterPrcpAryStd + AvgQwinterStd+MagHFwinStd+DurHFwinStd,
      data=TempPreSAMagDur, lower.panel = panel.smooth,
      upper.panel= panel.cor, main="Covariate correlation - Winter")
dev.off()

pdf("plots/Corr_Spring.pdf") #
pairs(~springTempAryStd + springPrcpAryStd + AvgQspringStd+MagHFspringStd+DurHFsprStd,
      data=TempPreSAMagDur, lower.panel = panel.smooth,
      upper.panel= panel.cor, main="Covariate correlation - Spring")
dev.off()

pairs(~AvgQfall +AvgQwinter + AvgQspring+ maxP1winter + maxP1spring,
      data=A.FishNNPredsCC, lower.panel = panel.smooth,
      upper.panel= panel.cor, main="Covariate correlation - Model 1")

pairs(~p5fall +p95winter + p95spring+ maxP1winter + maxP1spring,
      data=A.FishNNPredsCC, lower.panel = panel.smooth,
      upper.panel= panel.cor, main="Covariate correlation - Model 2")

pairs(~DurLFfall +DurHFwinter + DurHFspring+ maxP1winter + maxP1spring,
      data=A.FishNNPredsCC, lower.panel = panel.smooth,
      upper.panel= panel.cor, main="Covariate correlation - Model 3")

#max 1 day precips
log()+log()

###for flow metrics merMetrics
load("output/merUVAMetrics.rdata")
pairs(~avgSNNflow + Pmin7day + min1day+maxflow3+maxflow1,
      data=merUVAMetrics, lower.panel = panel.smooth,
      upper.panel= panel.cor, main="Flow metrics correlation")

##by season
SummerUVAMetrics<-merUVAMetrics[merUVAMetrics$Nseason==1,]
FallmerUVAMetrics<-merUVAMetrics[merUVAMetrics$Nseason==2,]
WinmerUVAMetrics<-merUVAMetrics[merUVAMetrics$Nseason==3,]
SprimerUVAMetrics<-merUVAMetrics[merUVAMetrics$Nseason==4,]

pairs(~avgSNNflow + Pmin7day + min1day+maxflow3+maxflow1,
      data=SummerUVAMetrics, lower.panel = panel.smooth,
      upper.panel= panel.cor, main="Summer - Flow metrics correlation")
pairs(~avgSNNflow + Pmin7day + min1day+maxflow3+maxflow1,
      data=FallmerUVAMetrics, lower.panel = panel.smooth,
      upper.panel= panel.cor, main="Fall-Flow metrics correlation")
pairs(~avgSNNflow + Pmin7day + min1day+maxflow3+maxflow1,
      data=WinmerUVAMetrics, lower.panel = panel.smooth,
      upper.panel= panel.cor, main="Winter-Flow metrics correlation")
pairs(~avgSNNflow + Pmin7day + min1day+maxflow3+maxflow1,
      data=SprimerUVAMetrics, lower.panel = panel.smooth,
      upper.panel= panel.cor, main="Spring-Flow metrics correlation")

##### Flows correlation plots #####
load("output/USGSdaily.rdata") 
load("output/UVA_daily.rdata")

#pull only the 4 sites that I'm using
USGS_NNfish_Sites<-as.data.frame(USGS_NNfish_Sites)
names(USGS_NNfish_Sites)<-"gaged_site_no"
USGS_NNfish_Sites$gaged_site_no<-as.character(USGS_NNfish_Sites$gaged_site_no)
USGSdaily4NNfish<-merge(USGS_NNfish_Sites,USGSdaily,by="gaged_site_no")

#cast USGSdaily4NNfish by site_no
USGS4NNfishcast <- dcast(USGSdaily4NNfish, Date ~ gaged_site_no,value.var = "cfs") 
#head(USGS4NNfishcast) 
names(USGS4NNfishcast)<-c("Date","USGS1","USGS2","USGS3", "USGS4")

#cast UVA_daily by site_no
UVA_dailycast <- dcast(UVA_daily, Date ~ gaged_site_no,value.var = "cfs") 
#head(UVA_dailycast) 
UVA_dailycastCC<-UVA_dailycast[complete.cases(UVA_dailycast),]

##Combine USGS and UVA into one data set
gagedFlows<-merge(USGS4NNfishcast,UVA_dailycastCC,by="Date")

pdf("plots/Corr_USGS_UVAflows.pdf")
pairs(~USGS1+USGS2+USGS3+USGS4+UVA_NFDR+UVA_PAIN+UVA_PINE+UVA_STAN+UVA_WOR1,
      data=gagedFlows, lower.panel = panel.smooth,
      upper.panel= panel.cor, main="USGS and UVA sites flow correlation (1992-2010)")
dev.off()

pdf("plots/Corr_USGSflows.pdf")
pairs(~USGS1+USGS2+USGS3+USGS4,
      data=USGS4NNfishcast, lower.panel = panel.smooth,
      upper.panel= panel.cor, main="USGS sites flow correlation (1981-2010)")
dev.off()

pdf("plots/Corr_UVAflows.pdf")
pairs(~UVA_NFDR+UVA_PAIN+UVA_PINE+UVA_STAN+UVA_WOR1,
      data=UVA_dailycastCC, lower.panel = panel.smooth,
      upper.panel= panel.cor, main="UVA sites flow correlation (1992-2010)")
dev.off()

#### OLD ####
##Fall
sumTQCorrSite<-sapply(seq.int(115), function(i) cor(summerPrcpAryStd[i,], AvgQsummerStd[i,]))
summary(sumTQCorrSite) 


#to get correlations - SUMMER AVERAGE FLOWS
sumTQCorrSite<-sapply(seq.int(dim(summerTempAryStd)[1]), function(i) cor(summerTempAryStd[i,], AvgQsummerStd[i,]))
summary(sumTQCorrSite) 

sumTQCorrSite<-sapply(seq.int(115), function(i) cor(summerPrcpAryStd[i,], AvgQsummerStd[i,]))
summary(sumTQCorrSite) 

sumTDurCorrSite<-sapply(seq.int(115), function(i) cor(summerPrcpAryStd[i,], DurLFsumStd[i,]))
summary(sumTDurCorrSite) 

sumFallPrcpCorrSite<-sapply(seq.int(dim(summerTempAryStd)[1]), function(i) cor(summerTempAryStd[i,], fallPrcpAryStd[i,]))
summary(sumFallTempCorrSite)

sumFallPrcpCorrSite<-sapply(seq.int(dim(summerTempAryStd)[1]), function(i) cor(summerTempAryStd[i,], fallPrcpAryStd[i,]))
summary(sumFallTempCorrSite)

#correlation of variables
#from http://www.inside-r.org/r-doc/graphics/pairs
panel.cor <- function(x, y, digits = 2, prefix = "corr=", cex.cor, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- cor(x, y)
  txt <- format(c(r, 0.123456789), digits = 2)[1]
  txt <- paste0(prefix, txt)
  if(missing(cex.cor)) cex.cor <- 1
  text(0.5, 0.5, txt, cex = cex.cor)
}

pdf("plots/Corr_Weather173.pdf",width=6,height=10) #
pairs(~Pfall + Pspring + Psummer + Pwinter + MaxTfall + MaxTspring + MaxTsummer + MaxTwinter,
      data=A.FishNNPreds, lower.panel = panel.smooth,
      upper.panel= panel.cor, main="Correlation of weather variables for all sites(n=173)")
dev.off()

pairs(~Pfall +MaxTfall+AvgQfall+p5fall,
      data=A.FishNNPreds, lower.panel = panel.smooth,
      upper.panel= panel.cor, main="Correlation of weather variables for all sites(n=173)")

pairs(~Pfall +MaxTfall+AvgQfall+p5fall+DurHFfall+DurLFfall,
      data=StdDataPredsCC, lower.panel = panel.smooth,
      upper.panel= panel.cor, main="Correlation of weather variables for all sites(n=173)")

cor(StdDataPreds$Pfall,StdDataPreds$DurLFfall)

#### Fish plots ####

#are there trends in the Fish Abundance - plot by site over time
names(A.FishNNPredsCC)

#year-effects
p1<-qplot(as.factor(c(1,as.numeric(rownames(ranef(lmer3)$Nyear)))),
          c(NA,ranef(lmer3)$Nyear[,]))+geom_line(aes(group=1))

p2<-ggplot(A.FishNNPredsCC, aes(x=as.factor(Nyear), y=EstYOYAbu)) + #, color=as.factor(fish_site))
  geom_boxplot()+ geom_smooth(se=TRUE, aes(group=1)) +scale_y_continuous(limits = c(0,300))+ labs(x = "")

p3<-ggplot(A.FishNNPredsCC, aes(x=as.factor(Nyear), y=AvgQfall)) + #, color=as.factor(fish_site))
  geom_boxplot()+ geom_smooth(se=TRUE, aes(group=1)) + labs(x = "")

p4<-ggplot(A.FishNNPredsCC, aes(x=as.factor(Nyear), y=AvgQwinter)) + #, color=as.factor(fish_site))
  geom_boxplot()+ geom_smooth(se=TRUE, aes(group=1))+ labs(x = "")

p5<-ggplot(A.FishNNPredsCC, aes(x=as.factor(Nyear), y=AvgQspring)) + #, color=as.factor(fish_site))
  geom_boxplot()+ geom_smooth(se=TRUE, aes(group=1))

multiplot(p2,p3,p4, cols=1)

ggplot(aes(x=rownames(ranef(lmer3)$Nyear), y=ranef(lmer3)$Nyear[,])) + geom_line()

##correlation of daymet and flow predc
p2<-ggplot(A.FishNNPredsCC, aes(x=as.factor(Nyear), y=EstYOYAbu)) + #, color=as.factor(fish_site))
  geom_boxplot()+ geom_smooth(se=TRUE, aes(group=1)) +scale_y_continuous(limits = c(0,300))

p3<-ggplot(A.FishNNPredsCC, aes(x=as.factor(Nyear), y=AvgQfall)) + #, color=as.factor(fish_site))
  geom_boxplot()+ geom_smooth(se=TRUE, aes(group=1)) 

p4<-ggplot(A.FishNNPredsCC, aes(x=as.factor(Nyear), y=AvgQwinter)) + #, color=as.factor(fish_site))
  geom_boxplot()+ geom_smooth(se=TRUE, aes(group=1))

p5<-ggplot(A.FishNNPredsCC, aes(x=as.factor(Nyear), y=AvgQspring)) + #, color=as.factor(fish_site))
  geom_boxplot()+ geom_smooth(se=TRUE, aes(group=1))

p6<-ggplot(A.FishNNPredsCC, aes(x=as.factor(Nyear), y=resid1)) + #, color=as.factor(fish_site))
  geom_boxplot()+ geom_smooth(se=TRUE, aes(group=1))

multiplot(p2,p6,p3,p4, cols=1)

#daymet and flows
p1<-ggplot(A.FishNNPredsCC, aes(x=as.factor(Nyear), y=AvgQfall)) + #, color=as.factor(fish_site))
  geom_boxplot()+ geom_smooth(se=TRUE, aes(group=1)) 

p2<-ggplot(A.FishNNPredsCC, aes(x=as.factor(Nyear), y=Pfall)) + #, color=as.factor(fish_site))
  geom_boxplot()+ geom_smooth(se=TRUE, aes(group=1))

p3<-ggplot(A.FishNNPredsCC, aes(x=as.factor(Nyear), y=maxP1fall)) + #, color=as.factor(fish_site))
  geom_boxplot()+ geom_smooth(se=TRUE, aes(group=1))

p4<-ggplot(A.FishNNPredsCC, aes(x=as.factor(Nyear), y=MaxTfall)) + #, color=as.factor(fish_site))
  geom_boxplot()+ geom_smooth(se=TRUE, aes(group=1))

multiplot(p1,p2,p3,p4, cols=1)



#assumes method=loess 
p1=ggplot(A.FishNNPredsCC, aes(x=as.factor(Nyear), y=EstYOYAbu)) + #, color=as.factor(fish_site))
  geom_boxplot()+ geom_smooth(se=TRUE, aes(group=1))+scale_y_continuous(limits = c(0,300))

p2=ggplot(A.Fish, aes(x=as.factor(Nyear), y=Pwinter)) + #, color=as.factor(fish_site))
  geom_boxplot()+ geom_smooth( se=TRUE, aes(group=1))

p3=ggplot(A.Fish, aes(x=as.factor(Nyear), y=MaxTwinter)) + #, color=as.factor(fish_site))
  geom_boxplot()+ geom_smooth( se=TRUE, aes(group=1))

p4=ggplot(A.Fish, aes(x=as.factor(Nyear), y=MaxTspring)) + #, color=as.factor(fish_site))
  geom_boxplot()+ geom_smooth( se=TRUE, aes(group=1))

pdf("plots/fish_Weather.pdf",width=6,height=10) #
multiplot(p1, p2, p3, p4, cols=1)
dev.off()

#### 3-D plots ####
load("output/All_fish2.rdata") #Use this version where they are predicted from daily values
All_fishCC<-All_fish2[complete.cases(All_fish2),] #remove observations with ANY missing values

require(akima); library(rworldmap)
d3plot1 <- with(All_fishCC, interp(x= maxP1fall , y= p95winter, z=EstYOYAbu, duplicate="mean"))
filled.contour(d3plot1, color=rainbow,
               xlab="maximum 1 day fall precip ",
               ylab="95%ile flow in winter",
               main="Estimated YOY Abundance",
               plot.axes={points(All_fishCC$maxP1fall, All_fishCC$p95winter)}
               # plot.axes = {
               #   axis(1)
               #   axis(2)
               #   contour(d3plot1, add=T)
               # }
               )
points(All_fishCC$maxP1fall, All_fishCC$p95winter,add=T) #can't add points easily to filled contour because of the legend


#how to get the points in the right place?
#http://stackoverflow.com/questions/19429350/adding-points-to-filled-contour-in-r-at-the-right-place

#AvgQwinter p95winter
d3plot1 <- with(All_fishCC, interp(x= AvgQwinter , y= p95winter, z=EstYOYAbu, duplicate="mean"))
filled.contour(d3plot1, color=rainbow,
               xlab="Avg flow winter ",
               ylab="95%ile flow in winter",
               main="Estimated YOY Abundance",
               plot.axes = {
                 axis(1)
                 axis(2)
                 contour(d3plot1, add=T)             
               })

#AvgQfall p95winter
d3plot1 <- with(All_fishCC, interp(x= AvgQfall , y= p95winter, z=EstYOYAbu, duplicate="mean"))
filled.contour(d3plot1, color=rainbow,
               xlab="Avg flow fall ",
               ylab="95%ile flow winter",
               main="Estimated YOY Abundance",
               plot.axes = {
                 axis(1)
                 axis(2)
                 contour(d3plot1, add=T)             
               })

contour(d3plot1)
points(fall.WFB$avgtmax, fall.WFB$totprecip,add=T) #can't add points easily to filled contour because of the legend

d3plot1 <- with(All_fishCC, interp(x= maxP1fall , y= p95winter, z=EstYOYAbu, duplicate="mean"))
filled.contour(d3plot1, color=rainbow,
               xlab="maximum 1 day fall precip ",
               ylab="95%ile flow in winter",
               main="Estimated YOY Abundance",
               plot.axes = {
                 axis(1)
                 axis(2)
                 contour(d3plot1, add=T)             
               })

#Mean seasonal flow avgSflow
d3plot3 <- with(fall.WFB, interp(x= totprecip, y= avgSflow, z=min7day, duplicate="mean"))
filled.contour(d3plot3, color=rainbow,xlab="total fall precip",ylab="average fall flow",
               plot.axes = {
                 axis(1)
                 axis(2)
                 contour(d3plot3, add=T)
                 points(fall.WFB$totprecip, fall.WFB$avgSflow, add=T)
               })

d3plot2 <- with(fall.WFB, interp(x= Slope_pct , y=log(DRAIN_SQMI), z=min7day, duplicate="mean")) ##DA
filled.contour(d3plot2, color=rainbow, xlab="Slope",ylab="log Drainage area",
               plot.axes = {
                 axis(1)
                 axis(2)
                 contour(d3plot2, add=T)             
               })


points(fall.WFB$Slope_pct, fall.WFB$Elev_m,add=T)


