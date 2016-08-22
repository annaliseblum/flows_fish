##Explore Plots

#load merged data sets for plots
load("output/S.WFB.rdata") #Seasonal flow data
load("output/A.FWC.rdata") #Annual fish data
load("output/USGS_BC.rdata")#USGS gages Basin Chars
load("output/fishSC.rdata") #all the site characteristics available

#### Flows plots ####
data<-S.WFB #A.FW
data$siteNDX<-as.numeric(as.factor(data$site_no))

#are there trends in the low flows - plot by site over time
ggplot(data, aes(x=as.factor(year), y=min7day, color=as.factor(siteNDX))) +geom_point() + geom_smooth(method=lm, 
       aes(group=as.factor(siteNDX)),se=F) + theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  scale_y_continuous(limits = c(0,100))

#annual LFs occur during which seasons? Aflow works but why doesn't data??
pdf("plots/TimingLFs.pdf") #
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

##flows A.Flows
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

pdf("plots/Corr_WeatherF.pdf",width=6,height=10) #
pairs(~Pfall + Pspring + Psummer + Pwinter + MaxTfall + MaxTspring + MaxTsummer + MaxTwinter,
      data=avgSites, lower.panel = panel.smooth,
      upper.panel= panel.cor)
dev.off()

#### Paired weather Comparison ####

length(unique(A.Flows$site_no)); length(unique(A.FWC$site_no))
names(A.Flows)
names(A.FWC)

Sites_U.w<-A.Flows[c("site_no","year.f","Pfall","Pspring","Psummer","Pwinter","MaxTfall",
                     "MaxTspring","MaxTsummer","MaxTwinter","MinTfall","MinTspring","MinTsummer",
                     "MinTwinter")]

Sites_F.w<-A.FWC[c("site_no","year.f","Pfall","Pspring","Psummer","Pwinter","MaxTfall",
                     "MaxTspring","MaxTsummer","MaxTwinter","MinTfall","MinTspring","MinTsummer",
                     "MinTwinter")]

Sites_U.w$type<-"USGS"
Sites_F.w$type<-"fish"

Site_Comp2<-rbind(Sites_U.w,Sites_F.w)
Site_Comp2$type<-as.factor(Site_Comp2$type)

p1=ggplot(Site_Comp2, aes(factor(type), Pwinter)) + geom_boxplot()+ labs(title = "Winter Precipitation",x="",y="")

ggplot(Site_Comp2, aes(x=as.factor(year.f), y=Pwinter)) + #, color=as.factor(fish_site))
  geom_boxplot(aes(fill = factor(type))) #+ stat_smooth( aes(fill = factor(type)))

stat_smooth(method=lm, aes(fill = factor(cyl)))

p <- ggplot(mtcars, aes(factor(cyl), mpg)) + geom_boxplot(aes(fill = factor(am)))

p + geom_boxplot()
p 

p2=ggplot(Site_Comp, aes(factor(type), Slope_pct)) + geom_boxplot()+ labs(title = "Slope Percent",x="",y="")
p3=ggplot(Site_Comp, aes(factor(type), Aspect_deg)) + geom_boxplot()+ labs(title = "Aspect degree",x="",y="")
p4=ggplot(Site_Comp, aes(factor(type), Elev_m)) + geom_boxplot()+ labs(title = "Elevation (m)",x="",y="")

pdf("plots/Site_comparison.pdf") #
multiplot(p1, p2, p3, p4, cols=2)
dev.off()

summary(Site_Comp[Site_Comp$type=="fish",])
summary(Site_Comp[Site_Comp$type=="USGS",])

#### Basin Characteristics Comparison ####

#PULL sites I'm actually using!

#combine fish and USGS site characteristics to make boxplot comparisons
names(USGS_BC1)
names(fishSC1)

Sites_U<-USGS_BC1[c("site_no","LAT_GAGE","LNG_GAGE","DRAIN_SQMI","Slope_pct","Aspect_deg","Elev_m")]
Sites_F<-fishSC1[c("site_no","LAT_GAGE","LNG_GAGE","DRAIN_SQMI","Slope_pct","Aspect_deg","Elev_m")]

Sites_U$type<-"USGS"
Sites_F$type<-"fish"

Site_Comp<-rbind(Sites_U,Sites_F)
Site_Comp$type<-as.factor(Site_Comp$type)

p1=ggplot(Site_Comp, aes(factor(type), DRAIN_SQMI)) + geom_boxplot()+ labs(title = "Drainage area (mi2)",x="",y="")
p2=ggplot(Site_Comp, aes(factor(type), Slope_pct)) + geom_boxplot()+ labs(title = "Slope Percent",x="",y="")
p3=ggplot(Site_Comp, aes(factor(type), Aspect_deg)) + geom_boxplot()+ labs(title = "Aspect degree",x="",y="")
p4=ggplot(Site_Comp, aes(factor(type), Elev_m)) + geom_boxplot()+ labs(title = "Elevation (m)",x="",y="")

pdf("plots/Site_comparison.pdf") #
multiplot(p1, p2, p3, p4, cols=2)
dev.off()

summary(Site_Comp[Site_Comp$type=="fish",])
summary(Site_Comp[Site_Comp$type=="USGS",])

#### Maps ####
library("ggmap")
library("ggplot2")

summary(SitesHUC2$LAT_GAGE)
summary(SitesHUC2$LNG_GAGE)

summary(fishsiteDf$Lat_n83)
summary(fishsiteDf$Lon_n83)

# lat <- c(37, 41.5) #define our map's ylim
# lon <- c(-81, -74.1) #define our map's xlim
# center = c(mean(lat), mean(lon))  #tell what point to center on
# myLocation<-c(mean(lat), mean(lon))
# myLocation<-c(40, -82, 49,-67.1)
myLocation<-"Shenandoah, Virginia"

##All the sites
myMap<- get_map(location=myLocation, source="google", maptype="terrain", crop=FALSE,zoom=7)
pdf(file="plots/site_map.pdf")
ggmap(myMap)+geom_point(aes(x = LNG_GAGE, y = LAT_GAGE), data = SitesHUC2, color="darkred",size = 3)+
  geom_point(aes(x = Lon_n83, y = Lat_n83), data = fishsiteDf, color="black",size = 2)
dev.off()

##Just the 29/54 I'm using
LL1<-Sites_U["LAT_GAGE","LNG_GAGE"]
USGS_BC1$type<-"USGS"
fishSC1$type<-"fish"
justSites<-rbind(USGS_BC1,fishSC1)

myMap<- get_map(location=myLocation, source="google", maptype="terrain", crop=FALSE,zoom=7)
pdf(file="plots/site_map2.pdf")
ggmap(myMap)+geom_point(aes(x = LNG_GAGE, y = LAT_GAGE), data = Sites_U, color="darkred",size = 3)+
  geom_point(aes(x = LNG_GAGE, y = LAT_GAGE), data = Sites_F, color="black",size = 2)
dev.off()

##Map LNSE of gaged sites
pdf(file="plots/LNSE_map.pdf")
ggmap(myMap)+geom_point(aes(x = LNG_GAGE, y = LAT_GAGE,col=meLNSE), data = USGS_BC1,size = 3)+
  scale_colour_gradientn(colours =rainbow(4))
dev.off()

USGS_BC1

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
