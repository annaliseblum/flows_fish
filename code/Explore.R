##Explore Plots

#load merged data sets for plots
load("output/S.WFB.rdata") #Seasonal flow data
load("output/A.FWC.rdata") #Annual fish data

#### Flows plots ####
data<-S.WFB
data$siteNDX<-as.numeric(as.factor(data$site_no))

#are there trends in the low flows - plot by site over time
ggplot(data, aes(x=as.factor(year), y=min7day, color=as.factor(siteNDX))) +geom_point() + geom_smooth(method=lm, 
       aes(group=as.factor(siteNDX)),se=F) + theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  scale_y_continuous(limits = c(0,100))

#annual LFs occur during which seasons? Aflow works but why doesn't data??
pdf("plots/TimingLFs.pdf",width=6,height=10) #
ggplot(data=Aflow, aes(x=season, y=min7day))+stat_summary(fun.y=length, geom="bar")+
  labs(x="", y="number of min7day flows in season", title="Timing of Annual 7day minimum flows") 
dev.off()

#low flows vs elevation
siteElevflow <- ddply(S.WFB, .(site_no), summarize, 
                  medmin7day=median(min7day),
                  ELEV_SITE_M=mean(ELEV_SITE_M)
 )

ggplot(data=siteElevflow, aes(x=ELEV_SITE_M, y=medmin7day))+geom_point() 
#all the highest median7day are at lower elevation sites
summary(lm(medmin7day~ELEV_SITE_M,siteElevflow)) #no relationship found

##flows
p1=ggplot(S.WFB, aes(x=as.factor(year), y=min7day)) + #, color=as.factor(fish_site))
  geom_boxplot()+ geom_smooth( se=TRUE, aes(group=1))+scale_y_continuous(limits = c(0,400))

p2=ggplot(S.WFB[S.WFB$season=="winter",], aes(x=as.factor(year), y=totprecip)) + #, color=as.factor(fish_site))
  geom_boxplot()+ geom_smooth( se=TRUE, aes(group=1))

p3=ggplot(S.WFB[S.WFB$season=="winter",], aes(x=as.factor(year), y=avgtmax)) + #, color=as.factor(fish_site))
  geom_boxplot()+ geom_smooth( se=TRUE, aes(group=1))

p4=ggplot(S.WFB[S.WFB$season=="spring",], aes(x=as.factor(year), y=avgtmax)) + #, color=as.factor(fish_site))
  geom_boxplot()+ geom_smooth( se=TRUE, aes(group=1))

pdf("plots/flow_Weather.pdf",width=6,height=10) #
multiplot(p1, p2, p3, p4, cols=1)
dev.off()

#### Fish plots ####

#are there trends in the Fish Abundance - plot by site over time
names(A.FWC)

#assumes method=loess 
p1=ggplot(A.FWC, aes(x=as.factor(year), y=YOY_P1)) + #, color=as.factor(fish_site))
  geom_boxplot()+ geom_smooth(se=TRUE, aes(group=1))+scale_y_continuous(limits = c(0,300))

p2=ggplot(A.FWC, aes(x=as.factor(year), y=Pwinter)) + #, color=as.factor(fish_site))
  geom_boxplot()+ geom_smooth( se=TRUE, aes(group=1))

p3=ggplot(A.FWC, aes(x=as.factor(year), y=MaxTwinter)) + #, color=as.factor(fish_site))
  geom_boxplot()+ geom_smooth( se=TRUE, aes(group=1))

p4=ggplot(A.FWC, aes(x=as.factor(year), y=MaxTspring)) + #, color=as.factor(fish_site))
  geom_boxplot()+ geom_smooth( se=TRUE, aes(group=1))

pdf("plots/fish_Weather.pdf",width=6,height=10) #
multiplot(p1, p2, p3, p4, cols=1)
dev.off()

#### Weather plots ####

##Correlation of weather variables
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

pdf("plots/Corr_Weather.pdf",width=6,height=10) #
pairs(~Pfall + Pspring + Psummer + Pwinter + MaxTfall + MaxTspring + MaxTsummer + MaxTwinter,
      data=avgSites, lower.panel = panel.smooth,
      upper.panel= panel.cor)
dev.off()

#### Maps ####

