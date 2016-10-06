##Explore: make Plots
###Annalise Blum
#July 2016  Updated: Oct 5,2016

#load data sets for plots
load("output/S.Flow.rdata") #Seasonal flow data
load("output/A.FlowW.rdata") #Weather for 50 Gaged sites
load("output/A.Fish.rdata") #Annual fish data
load("output/sflow.rdata") #Seasonal flow data
load("output/aDAYMET.rdata") #weather - Annual
load("output/gagedsites_BC.data")# gages Basin Chars
load("output/fishSC.rdata")

#### Flows plots ####

##Correlation in flow metrics
library(corrgram)

dim(sflow)
length(unique(sflow$site_no)) #5 UVA + 45 USGS

pdf("plots/Flow_correlation.pdf")
corrgram(sflow[,8:15], order=TRUE, lower.panel=panel.cor,
         upper.panel=panel.pts, text.panel=panel.txt,
         diag.panel=panel.minmax,
         main="Correlation of seaonal flow metrics, 45 USGS sites (n=4367)")
dev.off()

data<-S.Flow
data$siteNDX<-as.numeric(as.factor(data$site_no))

# #are there trends in the low flows - plot by site over time
# ggplot(data, aes(x=as.factor(year), y=min7day, color=as.factor(siteNDX))) +geom_point() + geom_smooth(method=lm, 
#        aes(group=as.factor(siteNDX)),se=F) + theme(axis.text.x = element_text(angle = 90, hjust = 1))+
#   scale_y_continuous(limits = c(0,100))

#annual LFs occur during which seasons? 
##Collapse to ANNUAL level to get which seasonal min7day is the minimum among the seasons
aflow <- aggregate(sflow$min7day,by=list(sflow$site_no, sflow$Nyear),FUN=min)
names(aflow)<-c("site_no","Nyear","min7day")
Aflow<-merge(aflow,sflow,by=c("site_no","Nyear","min7day"))
Aflow$seasonnames<-"NA"
Aflow$seasonnames[Aflow$Nseason==1]<-"summer"
Aflow$seasonnames[Aflow$Nseason==2]<-"fall"
Aflow$seasonnames[Aflow$Nseason==3]<-"winter"
Aflow$seasonnames[Aflow$Nseason==4]<-"spring"

Aflow$seasonnames<-as.factor(Aflow$seasonnames)
pdf("plots/TimingLFs.pdf") #
ggplot(data=Aflow, aes(x=seasonnames, y=min7day))+stat_summary(fun.y=length, geom="bar")+
  labs(x="", y="number of min7day flows in season", title="Timing of Annual 7day minimum flows") 
dev.off()

#Is there a relationship between low flows and elevation?
siteElevflow <- ddply(S.Flow, .(site_no), summarize, 
                  medmin7day=median(min7day),
                  meanElev_m=mean(Elev_m)
 )

ggplot(data=siteElevflow, aes(x=meanElev_m, y=medmin7day))+geom_point() 
#all the highest median7day are at lower elevation sites
summary(lm(medmin7day~meanElev_m,siteElevflow)) #no relationship found

##flows A.Flows
p1=ggplot(S.Flow, aes(x=as.factor(year), y=min7day)) + #, color=as.factor(fish_site))
  geom_boxplot()+ geom_smooth( se=TRUE, aes(group=1))+scale_y_continuous(limits = c(0,400))

p2=ggplot(S.Flow[S.Flow$season=="winter",], aes(x=as.factor(year), y=totprecip)) + #, color=as.factor(fish_site))
  geom_boxplot()+ geom_smooth( se=TRUE, aes(group=1))

p3=ggplot(S.Flow[S.Flow$season=="winter",], aes(x=as.factor(year), y=avgtmax)) + #, color=as.factor(fish_site))
  geom_boxplot()+ geom_smooth( se=TRUE, aes(group=1))

p4=ggplot(S.Flow[S.Flow$season=="spring",], aes(x=as.factor(year), y=avgtmax)) + #, color=as.factor(fish_site))
  geom_boxplot()+ geom_smooth( se=TRUE, aes(group=1))

pdf("plots/flow_Weather.pdf",width=6,height=10) #
multiplot(p1, p2, p3, p4, cols=1)
dev.off()

#### Fish plots ####

#are there trends in the Fish Abundance - plot by site over time
names(A.Fish)

#assumes method=loess 
p1=ggplot(A.Fish, aes(x=as.factor(Nyear), y=EstYOYAbu)) + #, color=as.factor(fish_site))
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

pdf("plots/Corr_Weather173.pdf",width=6,height=10) #
pairs(~Pfall + Pspring + Psummer + Pwinter + MaxTfall + MaxTspring + MaxTsummer + MaxTwinter,
      data=aDAYMET, lower.panel = panel.smooth,
      upper.panel= panel.cor, main="Correlation of weather variables for all sites(n=173)")
dev.off()

#### Paired weather Comparison ####
A.Flows<-A.FlowW
length(unique(A.Flows$site_no)); length(unique(A.Fish$site_no))
names(A.Flows)
names(A.Fish)

Sites_U.w<-A.Flows[c("site_no","Nyear","DA_SQKM","Slope_pct","Aspect_deg","Elev_m","Pfall","Pspring","Psummer","Pwinter","MaxTfall",
                     "MaxTspring","MaxTsummer","MaxTwinter","MinTfall","MinTspring","MinTsummer",
                     "MinTwinter","type")] #already have a type variable

Sites_F.w<-A.Fish[c("site_no","Nyear","DA_SQKM","Slope_pct","Aspect_deg","Elev_m","Pfall","Pspring","Psummer","Pwinter","MaxTfall",
                     "MaxTspring","MaxTsummer","MaxTwinter","MinTfall","MinTspring","MinTsummer",
                     "MinTwinter")]
Sites_F.w$type<-"fish" #have to add type var

Site_Comp<-rbind(Sites_U.w,Sites_F.w)
Site_Comp$type<-as.factor(Site_Comp2$type)

#parallel time series
ggplot(Site_Comp, aes(x=as.factor(Nyear), y=Pwinter)) + #, color=as.factor(fish_site))
  geom_boxplot(aes(fill = factor(type))) + geom_smooth(se=F,aes(group = factor(type),color=factor(type)))

#simple boxplots
p1=ggplot(Site_Comp, aes(factor(type), Psummer)) + geom_boxplot()+ labs(title = "Summer Precip",x="",y="")
p2=ggplot(Site_Comp, aes(factor(type), Pfall)) + geom_boxplot()+ labs(title = "Fall Precip",x="",y="")
p3=ggplot(Site_Comp, aes(factor(type), Pwinter)) + geom_boxplot()+ labs(title = "Winter Precip",x="",y="")
p4=ggplot(Site_Comp, aes(factor(type), Pspring)) + geom_boxplot()+ labs(title = "Spring Precip",x="",y="")

pdf("plots/Site_comparison.pdf") #
multiplot(p1, p2, p3, p4, cols=2)
dev.off()

summary(Site_Comp[Site_Comp$type=="fish",])
summary(Site_Comp[Site_Comp$type=="USGS",])
summary(Site_Comp[Site_Comp$type=="UVA",])

#### Basin Characteristics Comparison ####

p1=ggplot(Site_Comp, aes(factor(type), DA_SQKM)) + geom_boxplot()+ labs(title = "Drainage area (km2)",x="",y="km2")+
  scale_y_continuous(limits = c(0,500))
p2=ggplot(Site_Comp, aes(factor(type), Slope_pct)) + geom_boxplot()+ labs(title = "Slope Percent",x="",y="") #no data for UVA
p3=ggplot(Site_Comp, aes(factor(type), Aspect_deg)) + geom_boxplot()+ labs(title = "Aspect degree",x="",y="") #no data for UVA
p4=ggplot(Site_Comp, aes(factor(type), Elev_m)) + geom_boxplot()+ labs(title = "Elevation (m)",x="",y="")

pdf("plots/Site_comparison.pdf") #
multiplot(p1, p2, p3, p4, cols=2)
dev.off()

#### Maps ####
library("ggmap")
library("ggplot2")

summary(gagedsites_BC$LAT_GAGE)
summary(gagedsites_BC$LNG_GAGE)

summary(fishSC$LAT_GAGE)
summary(fishSC$LNG_GAGE)

# lat <- c(37, 41.5) #define our map's ylim
# lon <- c(-81, -74.1) #define our map's xlim
# center = c(mean(lat), mean(lon))  #tell what point to center on
# myLocation<-c(mean(lat), mean(lon))
myLocation<-c(-79,37.95,-78.1,39.05)
myLocation<-"Elkton, Virginia" #"Shenandoah, Virginia"

##All the sites
myMap<- get_map(location=myLocation, source="google", maptype="terrain", crop=FALSE)
#zoom = 7 captures all points, zoom=8 loses 11 USGS sites, zoom=9 loses 34 USGS sites
pdf(file="plots/site_map.pdf")
ggmap(myMap)+geom_point(aes(x = LNG_GAGE, y = LAT_GAGE), data = gagedsites_BC, color="darkred",size = 3)+
  geom_point(aes(x = LNG_GAGE, y = LAT_GAGE), data = fishSC, color="black",size = 2,pch=1)
dev.off()

#### With regression results ####
##Map LNSE of gaged sites
pdf(file="plots/LNSE_map.pdf")
ggmap(myMap)+geom_point(aes(x = LNG_GAGE, y = LAT_GAGE,col=meLNSE), data = USGS_BC1,size = 3)+
  scale_colour_gradientn(colours =rainbow(4))
dev.off()


#### All LF covariates ####
pairs(~log(min7day) + log(totprecip) + log(L1totprecip) + 
        log(L3totprecip) + log(L4totprecip) + log(L1avgtmax.T),
      data=fall.WFB, lower.panel = panel.smooth,
      upper.panel= panel.cor)
cor(fall.WFB$L1avgtmax.T,fall.WFB$L4avgtmax.T)

#site characteristicx + log(DRAIN_SQMI) + log(LAT_GAGE) + log(LNG_GAGE.T) + 


#### 3-D plots ###
require(akima); library(rworldmap)
d3plot1 <- with(fall.WFB, interp(x= avgtmax , y=totprecip, z=min7day, duplicate="mean"))
filled.contour(d3plot1, color=rainbow,
               plot.axes = {
                 axis(1)
                 axis(2)
                 contour(d3plot1, add=T)             
               })

contour(d3plot1)
points(fall.WFB$avgtmax, fall.WFB$totprecip,add=T) #can't add points easily to filled contour because of the legend

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
