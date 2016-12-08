##Regional regression model to predict seasonal High Flows (winter and spring)
##Annalise Blum
##Created: July 18,2016
##Modified last: Oct 4,2016

####REMEMBER NOW YEAR IS FISH YEAR - NEED TO ADJUST REGRESSIONS ACCORDINGLY ###
#STARTS IN JUNE/SUMMER - Nyear

S.WFB<-S.WFB[complete.cases(S.WFB),]

####Model Selection####
#Just focusing on HFs in winter and spring
WinSp.WFB<-S.WFB[S.WFB$season=="winter"|S.WFB$season=="spring",]
summary(WinSp.WFB)

#possible variables:
# > names(S.WFB)
#"site_no"           "year"              "Pfall"             "Pspring"           "Psummer"           "Pwinter"      "MaxTfall"         
# "MaxTspring"        "MaxTsummer"        "MaxTwinter"        "MinTfall"          "MinTspring"        "MinTsummer" "MinTwinter"       
# "Pjan"              "Pfeb"              "Pmar"              "Papril"            "Pmay"              "Pjune"       "Pjuly"            
# "Paug"              "Psept"             "Poct"              "Pnov"              "Pdec"              "maxP3fall"  "maxP3spring"      
# "maxP3summer"       "maxP3winter"       "maxP1fall"         "maxP1spring"       "maxP1summer"       "maxP1winter"  "season"           
# "avgSflow"          "days.01p"          "min7day"           "min3day"           "days.98p"          "maxdayflow"  "max3dayflow"      
# "year.f"            "DRAIN_SQMI"        "HUC02"             "LAT_GAGE"          "LNG_GAGE"          "REACH_CODE"   "Slope_pct"        
# "Aspect_deg"        "Elev_m"            "BFI_AVE"           "TOPWET"            "ELEV_MEAN_M_BASIN" "LNG_GAGE.T"     

#### 3day high flows ####
#combined summer and fall, which vars are useful? start with seasonal P and T and lags
TDHF3w<-lm(log(max3dayflow)~ log(Pdec+Pjan+Pfeb)+log(avgtmax.T)+ log(DA_SQKM)+log(LAT_GAGE)+ log(LNG_GAGE.T)+log(Slope_pct)+
            log(Aspect_deg)+log(Elev_m)
          , data=WinSp.WFB[WinSp.WFB$season=="winter",])
summary(TDHF3w)
ResidPlots(TDHF3w) #Actually pretty normal, homoskedastic and little autocorrelation!

TDHF3s<-lm(log(max3dayflow)~ log(Pwinter)+log(MaxTwinter+3)+log(Pspring)+log(MaxTspring) + log(DRAIN_SQMI)+log(LAT_GAGE)+ 
             log(LNG_GAGE.T)+log(Slope_pct)+
            log(Aspect_deg)+log(Elev_m)
          , data=WinSp.WFB[WinSp.WFB$season=="spring",])
summary(TDHF3s)
ResidPlots(TDHF3s) #Actually pretty normal, homoskedastic and little autocorrelation!

#remove non-sig vars
TDHF3w<-lm(log(max3dayflow)~ log(Pwinter)+log(MaxTwinter+3)+ 
             log(DRAIN_SQMI)+log(Slope_pct) +log(LAT_GAGE)+ log(LNG_GAGE.T) #+log(Aspect_deg)+log(Elev_m)
             , data=WinSp.WFB[WinSp.WFB$season=="winter",])
summary(TDHF3w)
ResidPlots(TDHF3w) #Actually pretty normal, homoskedastic and little autocorrelation!
#title(main="TDHF - winter")

#lat and long significant for spring high flows but not for winter ones...
TDHF3s<-lm(log(max3dayflow)~ log(Pwinter)+log(MaxTwinter+3)+log(Pspring) + 
             log(DRAIN_SQMI)+log(Slope_pct)+log(LAT_GAGE)+ log(LNG_GAGE.T) #log(Aspect_deg)+log(Elev_m)
                        , data=WinSp.WFB[WinSp.WFB$season=="spring",])
summary(TDHF3s)
ResidPlots(TDHF3s) #Actually pretty normal, homoskedastic and little autocorrelation!

##ME with site-specific intercepts
TDHF3s<-lmer(log(max3dayflow)~ (1|site_no)+log(Pwinter)+log(MaxTwinter+3)+log(Pspring) + 
             log(DRAIN_SQMI)+log(Slope_pct)+log(LAT_GAGE)+ log(LNG_GAGE.T) #log(Aspect_deg)+log(Elev_m)
           , data=WinSp.WFB[WinSp.WFB$season=="spring",])
summary(TDHF3s)
ResidPlots(TDHF3s)

#### 1day high flows ####

#similar to 3day high flows
TDHF1w<-lm(log(maxdayflow)~ log(Pwinter)+log(MaxTwinter+3)+ 
             log(DRAIN_SQMI)+log(Slope_pct)+log(LAT_GAGE)+ log(LNG_GAGE.T)
           , data=WinSp.WFB[WinSp.WFB$season=="winter",])
summary(TDHF1w)
ResidPlots(TDHF1w) #Actually really normal, homoskedastic and little autocorrelation!

TDHF1s<-lm(log(maxdayflow)~ log(Pwinter)+log(MaxTwinter+3)+log(Pspring)+ 
             log(DRAIN_SQMI)+log(Slope_pct)+log(LAT_GAGE)+ log(LNG_GAGE.T)
           , data=WinSp.WFB[WinSp.WFB$season=="spring",])
summary(TDHF1s)
ResidPlots(TDHF1s)
