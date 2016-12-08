##Explain site-specific intercepts

Reg_Dur<-lmer(log(EstYOYAbu)~(1|site_no)+log(DurLFfall+.05)+log(DurHFwinter+.05)+ log(DurHFfall+.05)+
                +log(MaxTfall)+log(MaxTspring)
            +log(maxP1fall)+log(maxP1winter)
          ,data=A.FishNNPredsCC, REML=F)
summary(Reg_Dur)

length(unlist(ranef(Reg_Dur))) #115 values, one for each site

#load fish site info:
load("output/fishSC.rdata")
summary(fishSC)

fishSC$ranef_RegDur<-unlist(ranef(Reg_Dur))

regSS1<-lm(ranef_RegDur~Elev_m,data=fishSC) # explains random site effects!!
#"DA_SQKM"      "HUC8"         "LAT_GAGE"     "LNG_GAGE"     "REACH_CODE"   "Slope_pct" "Aspect_deg"   "Elev_m"  
summary(regSS1)


#map random effects:
myMap<- get_map(location=myLocation, source="google", maptype="terrain", crop=FALSE) #,zoom = 7
#zoom = 7 captures all points, zoom=8 loses 11 USGS sites, zoom=9 loses 34 USGS sites
pdf(file="plots/site_mapzoomed.pdf")
ggmap(myMap)+geom_point(aes(x = LNG_GAGE, y = LAT_GAGE, color=ranef_RegDur), data = fishSC, 
                        size = 3)
dev.off()