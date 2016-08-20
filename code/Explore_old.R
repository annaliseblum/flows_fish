####Building a non-stationary stochastic streamflow model (SSM)
##Annalise Blum
##April 13, 2016
## part 3 - Explore data##

library(ggplot2)
library(trend)

#1st Run DataPrep.R file
source("code/DataPrep.R") #check that this works

#### check for trends and change points in min7day, null is no trend ####

data<-Aflow
Sites<-unique(data$site_no)
data$siteNDX<-as.numeric(as.factor(data$site_no))

m7d.mkp<-rep(NA,length=length(Sites))
mT.mkp<-rep(NA,length=length(Sites))
sP.mkp<-rep(NA,length=length(Sites))
m7d.Pp<- rep(NA,length=length(Sites))
mT.Pp<- rep(NA,length=length(Sites))
sP.Pp<- rep(NA,length=length(Sites))

for (i in 1:length(Sites)){
  #Mendall-Kau test for trend
  m7d.mkp[i]<- mk.test(as.ts(data$min7day[data$siteNDX==i]))$pvalue
  #Petitt test for change point
  m7d.Pp[i]<- pettitt.test(as.ts(data$min7day[data$siteNDX==i]))$p.value
}

#to get diretion of slope
sens.slope(as.ts(data$min7day[data$siteNDX==29])) #23,44,5,29 are statistically significant

sum(mk.pval<.05) #10 of 45 sites reject null of no trend at 5% significance 
#would expect 5% to randomly be less than .05, so more than the expected 2.25 sites (.05*45)

sum(mT.mkp<.05) 
sum(sP.mkp<.05) 

trend_d<-cbind.data.frame(Sites,m7d.mkp,mT.mkp,sP.mkp,m7d.Pp,mT.Pp,sP.Pp)

trend_d$m7d.trend<- ifelse(trend_d$m7d.mkp <.05, 1, 0) 
trend_d$mT_trend<- ifelse(trend_d$mT.mkp <.05, 1, 0) 
trend_d$sPd.trend<- ifelse(trend_d$sP.mkp <.05, 1, 0) 
trend_d$m7d.CP<- ifelse(trend_d$m7d.Pp <.05, 1, 0) #2
trend_d$mT_CP<- ifelse(trend_d$mT.Pp <.05, 1, 0) #2
trend_d$sPd.CP<- ifelse(trend_d$sP.Pp <.05, 1, 0) #3




#find out in which months the 7day min flows are falling
##Collapse data set to site-year level with annual minimum 7day rolling average flow
ALLyrdata <- ddply(Sdailydata, .(site_no, year), summarize, min7day = min(RA7day, na.rm = TRUE))
timing<-merge(ALLyrdata,dailydata, by=c("site_no", "year"))
table(timing$month[timing$min7day==timing$RA7day])

# 1   2   3   4   5   6   7   8   9  10  11  12 #from Sdailydata
# 79 119  46   5  25  90  91 140 231 111  32  23 

# 1   2   3   4   5   6   7   8   9  10  11  12 #this is totally different... why??
# 13  51  12   1   3  17 218 593 727 168  17  15 
#fraction of A7d min flows occuring in june-sept?
JJAScount<-sum(timing$month[timing$min7day==timing$RA7day]>5& timing$month[timing$min7day==timing$RA7day]<10,na.rm=T) #1555
ALLcount<-sum(timing$month[timing$min7day==timing$RA7day]<13,na.rm=T) #1835
JJAScount/ALLcount #85% of annual mininum low flows occur in june-september, use that as summer

# pdf(file="plots/timing7daymin.pdf")
# hist(timing$month[timing$min7day==timing$RA7day],breaks=12, xlab="month",main="Counts of 7day min flows falling in each month")
# dev.off()


#### 1 - PLOTs showing the challenge - Annual level relationships ####
data$siteNDX<-as.numeric(as.factor(data$site_no))
Site_d$siteNDX<-as.numeric(as.factor(Site_d$site_no))

ggplot(data=data, aes(x=as.factor(year), y=min7day, color=as.factor(siteNDX))) +geom_point()+ geom_smooth(method=lm, aes(group=as.factor(siteNDX)),se=F) + theme(axis.text.x = element_text(angle = 90, hjust = 1))
#one site is a huge outlier... 0143400680 remove??
#set site
ex<-4
p1=ggplot(data=data[data$siteNDX==ex,], aes(x=year, y=min7day)) +geom_point()+ geom_smooth(method=lm) +geom_hline(aes(yintercept=Site_d$MED7day[Site_d$siteNDX==ex]))+labs(x="", y="annual minimum 7day flow (m3/s)", title="Summer low streamflow") + 
  annotate("text", x = 1957, y = Site_d$MED7day[Site_d$siteNDX==ex]-.01, label = "median min7day (7Q2)")

p2=ggplot(data=data[data$siteNDX==ex,], aes(x=year, y=sum_precip)) +geom_point()+ geom_smooth(method=lm) +geom_hline(aes(yintercept=Site_d$Avgsum_prec[Site_d$siteNDX==ex]),linetype = 2)+labs(x="", y="total precipitation (mm)", title="Total April-Sept precipitation ") 

p3=ggplot(data=data[data$siteNDX==ex,], aes(x=year, y=avg_pet)) +geom_point()+ geom_smooth(method=lm) +geom_hline(aes(yintercept=Site_d$avgPET[Site_d$siteNDX==ex]),linetype = 2)+labs(x="", y="average PET (m)", title="average April-Sept PET") 

pdf("plots/Ex4SiteTS.pdf",width=6,height=9) #
multiplot(p1, p2, p3, cols=1)
dev.off()
sens.slope(as.ts(data$min7day[data$siteNDX==4])) #23,44,5,29 are statistically significant

#### BOXPLOTS ####

p4=ggplot(data=data[data$year>1959,], aes(x=as.factor(year), y=min7day)) +
  labs(x="", y="minimum 7day flow (m3/s)", title="Summer minimum 7 day flow") + #
  geom_boxplot() +theme_bw() + theme(legend.position="none",axis.text=element_text(size=10))+ geom_smooth(method=loess, aes(group=1))+ 
  scale_x_discrete(breaks=labels, labels=as.character(labels))

pdf("plots/7Qovertime.pdf",width=9,height=6)
p4
dev.off()

p5=ggplot(data=data[data$year>1959,], aes(x=as.factor(year), y=sum_precip)) +
  labs(x="", y="Precipitation (mm)", title="Sum of April-Sept precip") + #
  geom_boxplot() +theme_bw() + theme(legend.position="none",axis.text=element_text(size=10),axis.title.x = element_blank())+ geom_smooth(method=loess, aes(group=1))+ scale_x_discrete(breaks=labels, labels=as.character(labels))

p6=ggplot(data=data[data$year>1959,], aes(x=as.factor(year), y=avg_pet)) +
  labs(x="", y="PET (m)", title="Average PET April-Sept") + #
  geom_boxplot() +theme_bw() + theme(legend.position="none",axis.text=element_text(size=10))+ geom_smooth(method=loess, aes(group=1))+ scale_x_discrete(breaks=labels, labels=as.character(labels))

pdf("plots/Climateovertime.pdf",width=8,height=11)
multiplot(p4, p5, p6, cols=1)
dev.off()

#+ theme(axis.text.x = element_text(angle = 90, hjust = 1))

recs.yr <- aggregate(data$year,by=list(data$year),FUN=length)

#this is good but how many observations at each year? also deal with axes

ggplot(data=data, aes(x=as.factor(year), y=sum_precip)) +
  labs(x="", y="summer precipitation", title="Summer precipitation at sites 1950-2010") + 
  geom_boxplot() +theme_bw() + theme(legend.position="none")+ geom_smooth(method=loess, aes(group=1))

ggplot(data=data, aes(x=as.factor(year), y=years.rec)) +
  labs(x="", y="summer maximum Temp (C)", title="Summer max temperature at sites 1950-2010") + 
  geom_boxplot() +theme_bw() + theme(legend.position="none")+ geom_smooth(method=loess, aes(group=1))


#plot timeseries of relationship with X vars by site
ggplot(data=data, aes(x=may_precip, y=min7day, color=as.factor(siteNDX))) +geom_point()+ geom_smooth(method=lm, aes(group=as.factor(siteNDX)),se=F)
ggplot(data=data, aes(x=june_precip, y=min7day, color=as.factor(siteNDX))) +geom_point()+ geom_smooth(method=lm, aes(group=as.factor(siteNDX)),se=F)
ggplot(data=data, aes(x=july_precip, y=min7day, color=as.factor(siteNDX))) +geom_point()+ geom_smooth(method=lm, aes(group=as.factor(siteNDX)),se=F) #this seems to be the most important predictor
ggplot(data=data, aes(x=aug_precip, y=min7day, color=as.factor(siteNDX))) +geom_point()+ geom_smooth(method=lm, aes(group=as.factor(siteNDX)),se=F)
ggplot(data=data, aes(x=sept_precip, y=min7day, color=as.factor(siteNDX))) +geom_point()+ geom_smooth(method=lm, aes(group=as.factor(siteNDX)),se=F)

ggplot(data=data, aes(x=may_temp, y=min7day, color=as.factor(siteNDX))) +geom_point()+ geom_smooth(method=lm, aes(group=as.factor(siteNDX)),se=F)
ggplot(data=data, aes(x=june_temp, y=min7day, color=as.factor(siteNDX))) +geom_point()+ geom_smooth(method=lm, aes(group=as.factor(siteNDX)),se=F)
ggplot(data=data, aes(x=july_temp, y=min7day, color=as.factor(siteNDX))) +geom_point()+ geom_smooth(method=lm, aes(group=as.factor(siteNDX)),se=F)
ggplot(data=data, aes(x=aug_temp, y=min7day, color=as.factor(siteNDX))) +geom_point()+ geom_smooth(method=lm, aes(group=as.factor(siteNDX)),se=F)
ggplot(data=data, aes(x=sept_temp, y=min7day, color=as.factor(siteNDX))) +geom_point()+ geom_smooth(method=lm, aes(group=as.factor(siteNDX)),se=F)

#maxT more clear relationship for most sites than individual month temps
ggplot(data=data, aes(x=maxT, y=min7day, color=as.factor(siteNDX))) +geom_point()+ geom_smooth(method=lm, aes(group=as.factor(siteNDX)),se=F)

#boxplots by site
p5=ggplot(data=data[data$year>1994,], aes(x=as.factor(siteNDX), y=min7day)) +
  labs(x="", y="min 7 day", title="min 7 day by site") + 
  geom_boxplot() +theme_bw() + theme(legend.position="none")

p6=ggplot(data=data[data$water.yr>1994,], aes(x=as.factor(siteNDX), y=Precipmm)) +
  labs(x="", y="Precipmm", title="Precipmm by site") + 
  geom_boxplot() +theme_bw() + theme(legend.position="none")

p7=ggplot(data=data[data$water.yr>1994,], aes(x=as.factor(siteNDX), y=Tmax)) +
  labs(x="", y="Tmax", title="Tmax by site") + 
  geom_boxplot() +theme_bw() + theme(legend.position="none")

p8=ggplot(data=data[data$water.yr>1994,],  aes(x=as.factor(siteNDX), y=PET)) +
  labs(x="", y="mean PET", title="mean PET by site") + 
  geom_boxplot() +theme_bw() + theme(legend.position="none")

pdf('plots/BoxPlotsbysite.pdf')
multiplot(p5, p6, p7, p8, cols=2)
dev.off()

#get boxplot of values for a given year
class(data$water.yr) #it's numeric, need to change it to a factor

p9=ggplot(data=data[data$year>1994,], aes(x=as.factor(year), y=min7day)) +
  labs(x="", y="min 7 day", title="min 7 day by year") + 
  geom_boxplot() +theme_bw() + theme(legend.position="none")+ geom_smooth(method=loess, aes(group=1))

p10=ggplot(data=data[data$water.yr>1994,], aes(x=as.factor(water.yr), y=Precipmm)) +
  labs(x="", y="Precipmm", title="Precipmm by year") + 
  geom_boxplot() +theme_bw() + theme(legend.position="none")+ geom_smooth(method=loess, aes(group=1))

p11=ggplot(data=data[data$water.yr>1994,], aes(x=as.factor(water.yr), y=Tmax)) +
  labs(x="", y="Tmax", title="Tmax by year") + 
  geom_boxplot() +theme_bw() + theme(legend.position="none")+ geom_smooth(method=loess, aes(group=1))
##error message suggested adding , aes(group=1)) but I'm not sure what it does; i guess best fit across all

p12=ggplot(data=data[data$water.yr>1994,], aes(x=as.factor(water.yr), y=PET)) +
  labs(x="", y="mean PET", title="mean PET by year") + 
  geom_boxplot() +theme_bw() + theme(legend.position="none")+ geom_smooth(method=loess, aes(group=1))

pdf('plots/BoxPlotsbyYR.pdf')
multiplot(p9, p10, p11, p12, cols=2)
dev.off()


##Try to illustrate the additive comparison of FE
ggplot(data=data[data$site_no=="01100568",],aes(x=Precipmm, y=min7day))+geom_point()+theme_bw()+
  geom_abline(intercept = lm2$coeff[1]+lm2$coeff[2], slope = 0)

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

pdf('plots/7dayminFlowPairs.pdf') #width = 480, height = 480,
pairs(~min7day+Precipmm+PET+Tmax+water.yr,data=data, lower.panel = panel.smooth,
      upper.panel= panel.cor,
      labels=c("min7dflow (ft3/s)","Precip (mm)","PET (in)","Tavg (C)","water year"))
dev.off()

#### 2 - Site level relationships ####
#### 1 - Annual level relationships ####
siteNDX<-as.numeric(as.factor(Site_d$site_no))
Site_d<-cbind(siteNDX,Site_d)

#plot timeseries by site
ggplot(data=Site_d, aes(x=may_precip, y=emp7Q10)) +geom_point() + geom_smooth(method=lm)
ggplot(data=Site_d, aes(x=june_precip, y=emp7Q10)) +geom_point() + geom_smooth(method=lm)
ggplot(data=Site_d, aes(x=july_precip, y=emp7Q10)) +geom_point() + geom_smooth(method=lm)
ggplot(data=Site_d, aes(x=aug_precip, y=emp7Q10)) +geom_point() + geom_smooth(method=lm)
ggplot(data=Site_d, aes(x=sept_precip, y=emp7Q10)) +geom_point() + geom_smooth(method=lm)

# ggplot(data=Site_d, aes(x=may_temp, y=emp7Q10)) +geom_point() + geom_smooth(method=lm) #these aren't in the data set yet
# ggplot(data=Site_d, aes(x=june_temp, y=emp7Q10)) +geom_point() + geom_smooth(method=lm)
# ggplot(data=Site_d, aes(x=july_temp, y=emp7Q10)) +geom_point() + geom_smooth(method=lm)
# ggplot(data=Site_d, aes(x=aug_temp, y=emp7Q10)) +geom_point() + geom_smooth(method=lm)
# ggplot(data=Site_d, aes(x=sept_temp, y=emp7Q10)) +geom_point() + geom_smooth(method=lm)
ggplot(data=Site_d, aes(x=avgmaxT, y=emp7Q10)) +geom_point() + geom_smooth(method=lm)

#####Monthly data ####
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

pdf('plots/monthlyflowsPairs.pdf') #width = 480, height = 480,
pairs(~flow.mean +precip.total+pet+tavg+tmax+tmin,data=d.monthly1, lower.panel = panel.smooth,
      upper.panel= panel.cor,
      labels=c("flow (ft3/s)","Precip (mm)","PET (in)","Tavg (C)","Tmax (C)","Tmin (C)"))
dev.off()

#plot of time series correlation for a given site:
plot(dataBCEx1$year, dataBCEx1$tot_precip_in, type="l")
par(mfrow=c(4,1), mar= c(2, 2, 2, 2)) 

startyr<-min(VSite2$year)
ts_data<-ts(dataBCEx1$tot_precip_in, start=startyr, frequency=1)

png('plots/ExTS.png',res=90) #width = 480, height = 480,
VS2pred <- ts(VS2pred, start = startyr, freq = 1)
min7day <- ts(VSite2$min7day, start = startyr, freq = 1)
Precip <- ts(VSite2$Precipmm, start = startyr, freq = 1)
maxTemp <- ts(VSite2$Tmax*FE$coeff[12], start = startyr, freq = 1)
plot(cbind(VS2pred,min7day, Precip, maxTemp), cex.lab=.7, main="") 
dev.off()

##trying to show additve nature of FE
VSite2n<-VSite2
VSite2n$contPrecip<-VSite2n$Precipmm*FE$coeff[11]
VSite2n$contT<-VSite2n$Tmax*FE$coeff[12]
VSite2n$FE<-FE$coeff[10]+FE$coeff[1]
VSite2n$VS2pred<-VS2pred

ggplot(data=VSite2n, aes(x=year, y=FE))+labs(x="", y="min7day", title="min7day") +geom_line(color="black") +
  theme_bw() + theme(legend.position="none")+geom_line(aes(y=FE+contPrecip),color="blue") +
  geom_line(data=VSite2n,aes(x=year, y=FE+contT+contPrecip),color="red") +geom_point(aes(y=VS2pred))+
  geom_line(aes(y=min7day),color="purple")

#plot timeseries by site - only with 10 sites or less 
p1=ggplot(data=data, aes(x=water.yr, y=min7day, color=as.factor(siteNDX))) +
  labs(x="", y="min 7 day", title="Annual min 7day flows by site") + 
  facet_grid(siteNDX~.) +geom_line() +theme_bw() + theme(legend.position="none",axis.text.y=element_blank())

p2=ggplot(data=data, aes(x=water.yr, y=Precipmm,color=as.factor(siteNDX))) +
  labs(x="", y="total precip (mm)", title="Precip") + 
  facet_grid(siteNDX~.) +geom_line() +theme_bw() + theme(legend.position="none",axis.text.y=element_blank())

p3=ggplot(data=data, aes(x=water.yr, y=Tmax, color=as.factor(siteNDX))) +
  labs(x="", y="Tmax", title="Tmax") + 
  facet_grid(siteNDX~.) +geom_line() +theme_bw() + theme(legend.position="none",axis.text.y=element_blank())

p4=ggplot(data=data, aes(x=water.yr, y=PET, color=as.factor(siteNDX))) +
  labs(x="", y="mean PET", title="mean PET") + 
  facet_grid(siteNDX~.) +geom_line() +theme_bw() + theme(legend.position="none",axis.text.y=element_blank())

pdf('plots/TSbysite.pdf')
multiplot(p1, p2, p3, p4, cols=2)
dev.off()