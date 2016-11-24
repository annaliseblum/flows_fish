##Predict fish abundances
###Annalise Blum
#Created: Oct 24,2016  Updated: Oct 26,2016
#Data sets needed: "output/A.FishNNPreds.rdata";"output/fishSC.rdata"
##Data sets created in this file: 

#maybe when try to standardize by site go to NAN if all values are 0? DroPredwin DroPredspring DurLFwinter DurLFspring 
#compare Kanno and my models with lmer using p-values
#run in a bayesian framework...

#### 1 - load and prep data
#### 2 - Predict fish outcomes


#### 1 - load and prep data #### 
load("output/fishSC.rdata")

#load flows

#If variables are all 0, when I try to standardize they go to NAN, replace with 0
StdData<-as.matrix(StdData)
StdData[is.na(StdData)] <- 0
StdData<-as.data.frame(StdData)

##Add site-specific characteristics back in:
StdData<-merge(StdData, fishSC,by="site_no")

#Flow predictions
# "AvgQsummer"        "AvgQfall"          "AvgQwinter"       
# [51] "AvgQspring"        "Droughtpredsummer" "Droughtpredsfall"  "Floodpredsummer"   "Floodpredfall"    
# [56] "Floodpredwinter"   "Floodpredspring"   "p5summer"          "p5fall"            "p5winter"         
# [61] "p5spring"          "p95summer"         "p95fall"           "p95winter"         "p95spring" 

# #other stuff:
# "Psummer"           "Pfall"             "Pwinter"          
# [6] "Pspring"           "MaxTsummer"        "MaxTfall"          "MaxTwinter"        "MaxTspring"       
# [11] "MinTsummer"        "MinTfall"          "MinTwinter"        "MinTspring"        "Pjune"            
# [16] "Pjuly"             "Paug"              "Psept"             "Poct"              "Pnov"             
# [21] "Pdec"              "Pjan"              "Pfeb"              "Pmar"              "Papril"           
# [26] "Pmay"              "maxP3summer"       "maxP3fall"         "maxP3winter"       "maxP3spring"      
# [31] "maxP1summer"       "maxP1fall"         "maxP1winter"       "maxP1spring"       "totAnnPrecip"     
# [36] "L1AnnPrec"  
names(StdData)
#### 2 - Predict lm fish outcomes ####
#benchmark: can I do better than Kanno et al 2016 with just weather??
fitK<-lm(EstYOYAbu ~ Pfall + Pspring + Psummer + Pwinter+
           MaxTfall + MaxTspring + MaxTsummer + MaxTwinter
           #+DA_SQKM+LAT_GAGE+LNG_GAGE+Slope_pct+Aspect_deg+Elev_m #site-specific characteristics not std
         ,data=StdData)
summary(fitK)

fitP<-lm(EstYOYAbu ~ AvgQsummer+AvgQfall+AvgQwinter+AvgQspring #just my seasonal flow estimate vars
         ,data=StdData)
summary(fitP)

fitS<-lm(EstYOYAbu ~ AvgQsummer+AvgQfall+
           #p5summer + #+p5fall+p95winter+p95spring+
           AvgQwinter+AvgQspring+
           #Floodpredwinter+#Floodpredspring+#Floodpredsummer+Floodpredfall+
           #Droughtpredsummer+Droughtpredsfall+
           MaxTfall + MaxTspring + MaxTsummer + MaxTwinter+
           Pfall + Psummer + Pwinter #+ # Pspring +
           #DA_SQKM+LAT_GAGE +LNG_GAGE +Slope_pct+Aspect_deg+Elev_m #just my Seasonal flow vars + Temp
         ,data=StdData)
summary(fitS)

fitS<-lm(EstYOYAbu ~ #AvgQsummer+AvgQfall+
           AvgQwinter+#AvgQspring+ 
           Floodpredwinter+#Floodpredspring+#Floodpredsummer+Floodpredfall+
           #Droughtpredsummer+Droughtpredsfall+
           #MaxTfall + MaxTspring + MaxTsummer + MaxTwinter+
           Pfall + Psummer + Pwinter+ # Pspring +
           DA_SQKM+LAT_GAGE #+LNG_GAGE #+Slope_pct+Aspect_deg+Elev_m #just my Seasonal flow vars + Temp
         ,data=StdData)
summary(fitS)

##Corr
AvgQsummer+AvgQfall+AvgQwinter+AvgQspring+ 
  Floodpredwinter+Floodpredspring+#Floodpredsummer+Floodpredfall+
  #Droughtpredsummer+Droughtpredsfall+
  MaxTfall + MaxTspring + MaxTsummer + MaxTwinter+
  Pfall + Pspring + Psummer + Pwinter+
  
  library(corrgram)

ForCorrGr<-A.FishPreds[c("Pfall","Pspring","Psummer","Pwinter","AvgQfall","AvgQsummer","AvgQwinter","AvgQspring")]

#pdf("plots/Flow_correlationNew.pdf")
corrgram(ForCorrGr,
         lower.panel=panel.conf,
         upper.panel=panel.pts, text.panel=panel.txt,
         diag.panel=panel.minmax,
         main="Correlation of Covariates")
#dev.off()
#order=TRUE, ##

#stepwise
library(MASS)

fitSW<-lm(EstYOYAbu ~ AvgQsummer+AvgQfall+AvgQwinter+AvgQspring+ #average flows
           p5summer + p5fall+p95winter+p95spring+ #LFs in sum/fall; HFs in Win/spring
           #Floodpredwinter+Floodpredspring+Floodpredsummer+Floodpredfall+ #floods
           #Droughtpredsummer+Droughtpredsfall+ #droughts
           MaxTfall + MaxTspring + MaxTsummer + MaxTwinter+ #temp
           Pfall + Psummer + Pwinter+ Pspring #+ #precip
          # DA_SQKM+LAT_GAGE +LNG_GAGE +Slope_pct+Aspect_deg+Elev_m
         ,data=)

step <- stepAIC(fitSW, direction="both")
step$anova # display results

fitSWfinal<-lm(EstYOYAbu ~ AvgQsummer + AvgQwinter + p5fall + p95spring + MaxTfall + 
                 MaxTsummer + Pfall + Psummer + Pwinter
          ,data=StdData)
summary(fitSWfinal)

#### lmer - With site-specific intercepts: #####

lmerK<-lmer(EstYOYAbu ~(1|site_no)+ Pfall + Pspring + Psummer + Pwinter+
           MaxTfall + MaxTspring + MaxTsummer + MaxTwinter
         ,data=StdData, REML=F)
summary(lmerK)

lmer1<-lmer(EstYOYAbu ~ (1|site_no) + DurLFspring +AvgQwinter + AvgQsummer #+ p95spring  + +AvgQsummer +
              + Pfall + Pspring + Psummer + Pwinter+
              MaxTfall + MaxTspring + MaxTsummer + MaxTwinter
               ,data=StdData, REML=F)
summary(lmer1)

anova(lmerK,lmer1)
#Does adding each of my variables improve on the Kanno model?
#Average winter flows yes!

##Display model results
stargazer(fitMK,fitPT,fitMT,
          title="Fish Abundance",
          align=T,star.char = c("", "", ""),omit.table.layout = "n",
          #dep.var.labels = "", #model.names = FALSE,
          column.labels = c("Just weather","Seasonal preds","Magnitude preds"),
          no.space=T,dep.var.caption = "", report = "vct*", digits=2,
          model.numbers = F,type="text",out="output/regTXT/FishAbuComp.txt")


###Try more refined weather metrics - stepwise
fitweather<-lm(EstYOYAbu ~ 
            MaxTfall + MaxTspring + MaxTsummer + MaxTwinter+ #temp
            Pfall + Psummer + Pwinter+ Pspring + #precip
            DA_SQKM+LAT_GAGE +LNG_GAGE +Slope_pct+Aspect_deg+Elev_m+
          ##NEW 
          maxP3summer + maxP3fall+maxP3winter+maxP3spring+maxP1summer+maxP1fall+maxP1winter+maxP1spring
          ,data=A.FishPreds)

step <- stepAIC(fitweather, direction="both")
step$anova # display results

fitweatherfinal<-lm( EstYOYAbu ~ Pfall + Psummer + Pwinter + Pspring + DA_SQKM + LAT_GAGE + 
                 LNG_GAGE + maxP3summer + maxP3fall + 
                 maxP3spring + maxP1winter #+ Slope_pct + Aspect_deg 
               ,data=A.FishPreds)
summary(fitweatherfinal) #well this doesn't make much sense... because such high correlation!

lmerweather<-lmer( EstYOYAbu ~ (1|site_no)+Pfall + Psummer + Pwinter + Pspring + DA_SQKM + LAT_GAGE + 
                       LNG_GAGE + maxP3summer + maxP3fall + 
                       maxP3spring + maxP1winter #+ Slope_pct + Aspect_deg 
                     ,data=A.FishPreds)
summary(lmerweather)

#"Psummer"           "Pfall"             "Pwinter"          
# [6] "Pspring"           "MaxTsummer"        "MaxTfall"          "MaxTwinter"        "MaxTspring"       
# [11] "MinTsummer"        "MinTfall"          "MinTwinter"        "MinTspring"        "Pjune"            
# [16] "Pjuly"             "Paug"              "Psept"             "Poct"              "Pnov"             
# [21] "Pdec"              "Pjan"              "Pfeb"              "Pmar"              "Papril"           
# [26] "Pmay"              "maxP3summer"       "maxP3fall"         "maxP3winter"       "maxP3spring"      
# [31] "maxP1summer"       "maxP1fall"         "maxP1winter"       "maxP1spring"       "totAnnPrecip"     
# [36] "L1AnnPrec
