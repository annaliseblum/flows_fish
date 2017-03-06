#Standardize variables to feed into the JAGS models
###Impact of Extreme Streamflows on Brook Trout Young-of-Year Abundance
### Annalise G Blum
##Created:Dec 7, 2016, last modified: Feb 23,2017
##Data NEEDED for this file:
load("output/aDAYMET.rdata") #from WeatherPrep.R
load("output/A.Fishmerge.rdata") #from PredFlowMets
##Data sets created in this file: 
#load("output/max7Precip.rdata") #has: max7prcpwinterStd, max7prcpspringStd, max7prcpfallStd
#load("output/predExFlows.rdata") #has: save(p5fallStd, p95winterStd, p95springStd

#Selected model
R11<-lmer(log(EstNyoy)~(1+Stdp5fall+Stdp95winter|site_no)+
            Stdp5fall+Stdp95winter+Stdp95spring+ #flows
            StdMaxTfall+StdMaxTwinter+StdMaxTspring+ #temps
            Stdmax7prcpfall+Stdmax7prcpwinter+Stdmax7prcpspring+ #max precip
            Stdmax7prcpfall2+ Stdmax7prcpspring2+StdMaxTfall2+StdMaxTspring2+ #quadratic terms
            Stdmax7prcpfall3+ Stdmax7prcpspring3+StdMaxTfall3+StdMaxTspring3 #cubic terms
          ,data=Fish_WeaFlStd);#summary(R11)

##Example of Kanno's arrays
# #matrix of 115 sites by 29 years
# load("~/flows_fish/YK/Site_by_year seasonal climate var standardized 115.rdata") #examples
# rownames(winterPrcpAryStd)
# colnames(winterPrcpAryStd)

#### Make arrays for JAGS ####
#use aDAYMET for precip
aDAYMET<-aDAYMET[aDAYMET$Nyear!=1,]

Flows<- A.Fishmerge[c("site_no","Nyear","p5fall","p95winter","p95spring")]

PrecipExt<-aDAYMET[c("site_no","Nyear","max7prcpfall","max7prcpwinter","max7prcpspring")]

PrecipExt<-merge(PrecipExt,Flows,by=c("site_no","Nyear"))

standard<-function(x) { #but need to do this by site
  stand.var<-(x-mean(x,na.rm = T))/sd(x,na.rm = T)
  return(stand.var)
}
# dim(PrecipExt)
# names(PrecipExt)

L_PrecipExt<-split(PrecipExt[,2:ncol(PrecipExt)],PrecipExt[1]) #make into list of sites - now just 106
#L_PrecipExt[[1]] 

# EXample of first site
#for all sites:
#To avoid standardizing Nyear, pull it out: L_A.FishPreds[[1]][2:ncol(L_A.FishPreds[[1]])]
# (summer and fall weather variables are all missing because of year 1 NA, but ok for now)
#initalize with first site
StdData<-as.data.frame(sapply(L_PrecipExt[[1]][2:(ncol(L_PrecipExt[[1]]))],standard)) #this exludes first column which is Nyear
StdData<-as.data.frame(c(L_PrecipExt[[1]][1],StdData)) #add year back in - same order? seems like it - hopefully
StdData$site_no<-names(L_PrecipExt[1])

#loop through the other sites
for (i in 2: length(L_PrecipExt)){
  iStdData<-as.data.frame(sapply(L_PrecipExt[[i]][2:(ncol(L_PrecipExt[[i]]))],standard)) #standardize all vars except Nyear
  iStdData<-as.data.frame(c(L_PrecipExt[[i]][1],iStdData)) #first column is Nyear
  iStdData$site_no<-names(L_PrecipExt[i])
  StdData<-rbind(StdData,iStdData)
}
#summary(StdData) 

#cast new precip metrics (temp is already done)
#max 7 day precip
max7prcpfallStd <- dcast(StdData, site_no ~ Nyear,value.var = "max7prcpfall") 
rownames(max7prcpfallStd)
rownames(max7prcpfallStd)<-paste("Site",rownames(max7prcpfallStd),sep="")
max7prcpfallStd$site_no<-NULL;colnames(max7prcpfallStd)
colnames(max7prcpfallStd)<-paste("Year",colnames(max7prcpfallStd),sep="")
max7prcpfallStd<-as.matrix(max7prcpfallStd); str(max7prcpfallStd)

max7prcpwinterStd <- dcast(StdData, site_no ~ Nyear,value.var = "max7prcpwinter") 
rownames(max7prcpwinterStd)
rownames(max7prcpwinterStd)<-paste("Site",rownames(max7prcpwinterStd),sep="")
max7prcpwinterStd$site_no<-NULL;colnames(max7prcpwinterStd)
colnames(max7prcpwinterStd)<-paste("Year",colnames(max7prcpwinterStd),sep="")
max7prcpwinterStd<-as.matrix(max7prcpwinterStd); str(max7prcpwinterStd)

max7prcpspringStd <- dcast(StdData, site_no ~ Nyear,value.var = "max7prcpspring") 
rownames(max7prcpspringStd)
rownames(max7prcpspringStd)<-paste("Site",rownames(max7prcpspringStd),sep="")
max7prcpspringStd$site_no<-NULL;colnames(max7prcpspringStd)
colnames(max7prcpspringStd)<-paste("Year",colnames(max7prcpspringStd),sep="")
max7prcpspringStd<-as.matrix(max7prcpspringStd); str(max7prcpspringStd)

save(max7prcpwinterStd, max7prcpspringStd, max7prcpfallStd, file="output/max7Precip.rdata")

##Flows: low fall, high winter and spring
#fall
p5fallStd <- dcast(StdData, site_no ~ Nyear,value.var = "p5fall")
rownames(p5fallStd)<-paste("Site",rownames(p5fallStd),sep="")
p5fallStd$site_no<-NULL
colnames(p5fallStd)<-paste("Year",colnames(p5fallStd),sep="")
p5fallStd<-as.matrix(p5fallStd)
str(p5fallStd)

#winter
p95winterStd <- dcast(StdData, site_no ~ Nyear,value.var = "p95winter")
rownames(p95winterStd)<-paste("Site",rownames(p95winterStd),sep="")
p95winterStd$site_no<-NULL
colnames(p95winterStd)<-paste("Year",colnames(p95winterStd),sep="")
p95winterStd<-as.matrix(p95winterStd)
str(p95winterStd)

#spring
p95springStd <- dcast(StdData, site_no ~ Nyear,value.var = "p95spring")
rownames(p95springStd)<-paste("Site",rownames(p95springStd),sep="")
p95springStd$site_no<-NULL
colnames(p95springStd)<-paste("Year",colnames(p95springStd),sep="")
p95springStd<-as.matrix(p95springStd)
str(p95springStd)

save(p5fallStd, p95winterStd, p95springStd, file="output/predExFlows.rdata")



#### OLD ####
#consecutive dry days
conU1fallStd <- dcast(StdDataPreds, site_no ~ Nyear,value.var = "conU1fall") 
rownames(conU1fallStd)
rownames(conU1fallStd)<-paste("Site",rownames(conU1fallStd),sep="")
conU1fallStd$site_no<-NULL;colnames(conU1fallStd)
colnames(conU1fallStd)<-paste("Year",colnames(conU1fallStd),sep="")
conU1fallStd<-as.matrix(conU1fallStd); str(conU1fallStd)
conU1fallStd[is.na(conU1fallStd)]<-0 #replace NAs in year 1 with 0s

conU1winterStd <- dcast(StdDataPreds, site_no ~ Nyear,value.var = "conU1winter") 
rownames(conU1winterStd)
rownames(conU1winterStd)<-paste("Site",rownames(conU1winterStd),sep="")
conU1winterStd$site_no<-NULL;colnames(conU1winterStd)
colnames(conU1winterStd)<-paste("Year",colnames(conU1winterStd),sep="")
conU1winterStd<-as.matrix(conU1winterStd); str(conU1winterStd)

conU1springStd <- dcast(StdDataPreds, site_no ~ Nyear,value.var = "conU1spring") 
rownames(conU1springStd)
rownames(conU1springStd)<-paste("Site",rownames(conU1springStd),sep="")
conU1springStd$site_no<-NULL;colnames(conU1springStd)
colnames(conU1springStd)<-paste("Year",colnames(conU1springStd),sep="")
conU1springStd<-as.matrix(conU1springStd); str(conU1springStd)


####OLDISH####
#need to cast by year - Average flows AvgQsummerStd AvgQfallStd AvgQwinterStd AvgQspringStd
#summer
AvgQsummerStd <- dcast(StdDataPreds, site_no ~ Nyear,value.var = "AvgQsummer") 
rownames(AvgQsummerStd)
rownames(AvgQsummerStd)<-paste("Site",rownames(AvgQsummerStd),sep="")
AvgQsummerStd$site_no<-NULL;colnames(AvgQsummerStd)
colnames(AvgQsummerStd)<-paste("Year",colnames(AvgQsummerStd),sep="")
AvgQsummerStd<-as.matrix(AvgQsummerStd); str(AvgQsummerStd)

#fall
AvgQfallStd <- dcast(StdDataPreds, site_no ~ Nyear,value.var = "AvgQfall") 
rownames(AvgQfallStd)
rownames(AvgQfallStd)<-paste("Site",rownames(AvgQfallStd),sep="")
AvgQfallStd$site_no<-NULL
colnames(AvgQfallStd)
colnames(AvgQfallStd)<-paste("Year",colnames(AvgQfallStd),sep="")
AvgQfallStd<-as.matrix(AvgQfallStd)
str(AvgQfallStd)

#winter
AvgQwinterStd <- dcast(StdDataPreds, site_no ~ Nyear,value.var = "AvgQwinter") 
rownames(AvgQwinterStd)
rownames(AvgQwinterStd)<-paste("Site",rownames(AvgQwinterStd),sep="")
AvgQwinterStd$site_no<-NULL
colnames(AvgQwinterStd)
colnames(AvgQwinterStd)<-paste("Year",colnames(AvgQwinterStd),sep="")
AvgQwinterStd<-as.matrix(AvgQwinterStd)
str(AvgQwinterStd)
str(winterPrcpAryStd) #matches!! 

#spring
AvgQspringStd <- dcast(StdDataPreds, site_no ~ Nyear,value.var = "AvgQspring") 
rownames(AvgQspringStd)
rownames(AvgQspringStd)<-paste("Site",rownames(AvgQspringStd),sep="")
AvgQspringStd$site_no<-NULL
colnames(AvgQspringStd)
colnames(AvgQspringStd)<-paste("Year",colnames(AvgQspringStd),sep="")
AvgQspringStd<-as.matrix(AvgQspringStd)
str(AvgQspringStd)

##Duration
#summer
DurLFsumStd <- dcast(StdDataPreds, site_no ~ Nyear,value.var = "DurLFsummer")
rownames(DurLFsumStd)<-paste("Site",rownames(DurLFsumStd),sep="")
DurLFsumStd$site_no<-NULL
colnames(DurLFsumStd)<-paste("Year",colnames(DurLFsumStd),sep="")
DurLFsumStd<-as.matrix(DurLFsumStd)
str(DurLFsumStd)

#fall
DurLFfallStd <- dcast(StdDataPreds, site_no ~ Nyear,value.var = "DurLFfall")
rownames(DurLFfallStd)<-paste("Site",rownames(DurLFfallStd),sep="")
DurLFfallStd$site_no<-NULL
colnames(DurLFfallStd)<-paste("Year",colnames(DurLFfallStd),sep="")
DurLFfallStd<-as.matrix(DurLFfallStd)
str(DurLFfallStd)

#replace NAs with 0
DurLFfallStd[is.na(DurLFfallStd)]<-0 #some sites with all 0s, messed up still

#winter
DurHFwinStd <- dcast(StdDataPreds, site_no ~ Nyear,value.var = "DurHFwinter")
rownames(DurHFwinStd)<-paste("Site",rownames(DurHFwinStd),sep="")
DurHFwinStd$site_no<-NULL
colnames(DurHFwinStd)<-paste("Year",colnames(DurHFwinStd),sep="")
DurHFwinStd<-as.matrix(DurHFwinStd)
str(DurHFwinStd)

#spring
DurHFsprStd <- dcast(StdDataPreds, site_no ~ Nyear,value.var = "DurHFspring")
rownames(DurHFsprStd)<-paste("Site",rownames(DurHFsprStd),sep="")
DurHFsprStd$site_no<-NULL
colnames(DurHFsprStd)<-paste("Year",colnames(DurHFsprStd),sep="")
DurHFsprStd<-as.matrix(DurHFsprStd)
str(DurHFsprStd)

#### Magnitude: MagLFsumStd MagLFfallStd MagHFwinStd MagHFspringStd ####
#summer
MagLFsumStd <- dcast(StdDataPreds, site_no ~ Nyear,value.var = "p5summer")
rownames(MagLFsumStd)<-paste("Site",rownames(MagLFsumStd),sep="")
MagLFsumStd$site_no<-NULL
colnames(MagLFsumStd)<-paste("Year",colnames(MagLFsumStd),sep="")
MagLFsumStd<-as.matrix(MagLFsumStd)
str(MagLFsumStd)
#summary(t(MagLFsumStd)) #to check that means are all zero, summarize a transpose of matrix

#fall
MagLFfallStd <- dcast(StdDataPreds, site_no ~ Nyear,value.var = "p5fall")
rownames(MagLFfallStd)<-paste("Site",rownames(MagLFfallStd),sep="")
MagLFfallStd$site_no<-NULL
colnames(MagLFfallStd)<-paste("Year",colnames(MagLFfallStd),sep="")
MagLFfallStd<-as.matrix(MagLFfallStd)
str(MagLFfallStd)

#winter
MagHFwinStd <- dcast(StdDataPreds, site_no ~ Nyear,value.var = "p95winter")
rownames(MagHFwinStd)<-paste("Site",rownames(MagHFwinStd),sep="")
MagHFwinStd$site_no<-NULL
colnames(MagHFwinStd)<-paste("Year",colnames(MagHFwinStd),sep="")
MagHFwinStd<-as.matrix(MagHFwinStd)
str(MagHFwinStd)

#spring
MagHFspringStd <- dcast(StdDataPreds, site_no ~ Nyear,value.var = "p95spring")
rownames(MagHFspringStd)<-paste("Site",rownames(MagHFspringStd),sep="")
MagHFspringStd$site_no<-NULL
colnames(MagHFspringStd)<-paste("Year",colnames(MagHFspringStd),sep="")
MagHFspringStd<-as.matrix(MagHFspringStd)
str(MagHFspringStd)

#### Max daily precip #### maxP1fall (115 missing bc need 1981) maxP1winter maxP1spring
#replace NA with 0, that is the mean anyway
StdDataPreds<-StdDataPreds2
#fall
MaxP1fallStd <- dcast(StdDataPreds, site_no ~ Nyear,value.var = "maxP1fall")
rownames(MaxP1fallStd)<-paste("Site",rownames(MaxP1fallStd),sep="")
MaxP1fallStd$site_no<-NULL
colnames(MaxP1fallStd)<-paste("Year",colnames(MaxP1fallStd),sep="")
MaxP1fallStd<-as.matrix(MaxP1fallStd)
str(MaxP1fallStd)
MaxP1fallStd[is.na(MaxP1fallStd)]<- 0 #REPLACED FIRST YEAR NAS WITH 0

#winter
MaxP1winStd <- dcast(StdDataPreds, site_no ~ Nyear,value.var = "maxP1winter")
rownames(MaxP1winStd)<-paste("Site",rownames(MaxP1winStd),sep="")
MaxP1winStd$site_no<-NULL
colnames(MaxP1winStd)<-paste("Year",colnames(MaxP1winStd),sep="")
MaxP1winStd<-as.matrix(MaxP1winStd)
str(MaxP1winStd)

#spring
MaxP1springStd <- dcast(StdDataPreds, site_no ~ Nyear,value.var = "maxP1spring")
rownames(MaxP1springStd)<-paste("Site",rownames(MaxP1springStd),sep="")
MaxP1springStd$site_no<-NULL
colnames(MaxP1springStd)<-paste("Year",colnames(MaxP1springStd),sep="")
MaxP1springStd<-as.matrix(MaxP1springStd)
str(MaxP1springStd)

####OLD ####

##standardizing my variables
#write dcast function for this data set StdDataPreds
library("data.table")
test <- dcast(setDT(StdDataPreds), site_no ~ Nyear,value.var = c("AvgQfall","AvgQsummer"))

#PRECIP
#summer
PsummerStd <- dcast(StdData, site_no ~ Nyear,value.var = "Psummer")
rownames(PsummerStd)<-paste("Site",rownames(PsummerStd),sep="")
PsummerStd$site_no<-NULL
colnames(PsummerStd)<-paste("Year",colnames(PsummerStd),sep="")
PsummerStd<-as.matrix(PsummerStd)
str(PsummerStd)
View(PsummerStd)

#fall
PfallStd <- dcast(StdDataPreds, site_no ~ Nyear,value.var = "Pfall") 
rownames(PfallStd)<-paste("Site",rownames(PfallStd),sep="")
PfallStd$site_no<-NULL
colnames(PfallStd)<-paste("Year",colnames(PfallStd),sep="")
PfallStd<-as.matrix(PfallStd)
str(PfallStd)

#winter
PwinterStd <- dcast(StdData, site_no ~ Nyear,value.var = "Pwinter") 
rownames(PwinterStd)<-paste("Site",rownames(PwinterStd),sep="")
PwinterStd$site_no<-NULL
colnames(PwinterStd)<-paste("Year",colnames(PwinterStd),sep="")
PwinterStd<-as.matrix(PwinterStd)
str(PwinterStd)

PwinterStd-winterPrcpAryStd


#spring
PspringStd <- dcast(StdDataPreds, site_no ~ Nyear,value.var = "Pspring") 
rownames(PspringStd)<-paste("Site",rownames(PspringStd),sep="")
PspringStd$site_no<-NULL
colnames(PspringStd)<-paste("Year",colnames(PspringStd),sep="")
PspringStd<-as.matrix(PspringStd)
str(PspringStd)

#load("output/A.FishNNPreds.rdata") #UVAA.FishNNPreds
# dim(A.FishNNPreds)
# summary(A.FishNNPreds) #1401 obs NA for abundance, so only 712 years - now 691 why??
#A.FishPredsCC<-A.FishNNPreds[complete.cases(A.FishNNPreds),]
#dim(A.FishPredsCC)

# #need at least 3 years of observations at a given site: #this is for when using UVA sites or something
# A.FishPredsCCrl<-aggregate(A.FishPredsCC$site_no,by=list(A.FishPredsCC$site_no),length)
# names(A.FishPredsCCrl)<-c("site_no","yearsRec")
# A.FishPredsCC1<-merge(A.FishPredsCC,A.FishPredsCCrl,by="site_no")
# A.FishPredsN<-A.FishPredsCC1[A.FishPredsCC1$yearsRec>2,]
# length(unique(A.FishPredsN$site_no)) 
# A.FishPredsN$yearsRec<-NULL #now remove this extra variable
#A.FishNNPreds<-UVAA.FishNNPreds
#standardize all of the covariates

#make a list of sites: 
#dim(A.FishNNPreds) #712 x 66 variables; just pull the ones i'm using
#now 71 vars but last 4 are flood/drought ones... #UVAA.FishNNPreds
#names(A.FishNNPreds)

A.FishNNPreds1<-A.FishNNPreds[,c(1:14,38, 48:(ncol(A.FishNNPreds)))] #keep site-characteristics seperate...
# names(A.FishNNPreds1)
# dim(A.FishNNPreds1)
L_A.FishPreds<-split(A.FishNNPreds1[,2:ncol(A.FishNNPreds1)],A.FishNNPreds1[1]) #make into list of sites - now just 106
#L_A.FishPreds[[1]]  

#for all sites:
#how to deal with the Nyear var getting messed up?? L_A.FishPreds[[1]][2:ncol(L_A.FishPreds[[1]])]
#initalize with first site
StdData<-as.data.frame(sapply(L_A.FishPreds[[1]][2:(ncol(L_A.FishPreds[[1]])-4)],standard)) #this exludes first column which is Nyear
StdData<-as.data.frame(c(L_A.FishPreds[[1]][1],StdData)) #add year back in - same order? seems like it - hopefully
StdData$site_no<-names(L_A.FishPreds[1])
#iDroughtFlood<-as.data.frame(L_A.FishPreds[1])[,23:26]
# names(iDroughtFlood)<-c("DroPredsummer","DroPredfall","FloodPredwinter","FloodPredspring")
# StdData<-cbind(StdData,iDroughtFlood)

#loop through the other sites
for (i in 2: length(L_A.FishPreds)){
  iStdData<-as.data.frame(sapply(L_A.FishPreds[[i]][2:(ncol(L_A.FishPreds[[i]])-4)],standard)) #standardize all vars except Nyear
  iStdData<-as.data.frame(c(L_A.FishPreds[[i]][1],iStdData)) #first column is Nyear
  iStdData$site_no<-names(L_A.FishPreds[i])
  # iDroughtFlood<-as.data.frame(L_A.FishPreds[i])[,23:26]
  # names(iDroughtFlood)<-c("DroPredsummer","DroPredfall","FloodPredwinter","FloodPredspring")
  # iFDStdData<-cbind(iStdData,iDroughtFlood)
  StdData<-rbind(StdData,iStdData)
}
#summary(StdData)
UVAStdDataPreds <-StdData #StdDataPreds

#save(StdDataPreds,file="output/StdDataPreds.rdata")
save(UVAStdDataPreds,file="output/UVAStdDataPreds.rdata")

#merge into main observation data frame:
load("output/UVAStdDataPreds.rdata"); head(UVAStdDataPreds)
load("output/A.FishNNPreds.rdata"); head(A.FishNNPreds)

#need to add"Std" to begining of standardized variables
colnames(UVAStdDataPreds) <- paste("Std", colnames(UVAStdDataPreds), sep = "")
Nyear<-UVAStdDataPreds$StdNyear; UVAStdDataPreds<-cbind(Nyear,UVAStdDataPreds)
site_no<-UVAStdDataPreds$Stdsite_no; UVAStdDataPreds<-cbind(site_no,UVAStdDataPreds)

UVAStdDataPreds$StdNyear<-NULL;UVAStdDataPreds$StdEstYOYAbu<-NULL;
UVAStdDataPreds$Stdsite_no<-NULL

#get rid of duplicates:
UVAStdDataPreds$StdEstYOYAbu<-NULL
UVAStdDataPreds$StdDroPredfall<-NULL;UVAStdDataPreds$StdDroPredsummer<-NULL
UVAStdDataPreds$StdFloodPredwinter<-NULL;UVAStdDataPreds$StdFloodPredspring<-NULL

#merge
A.FishNNPredsS<-merge(A.FishNNPreds,UVAStdDataPreds,by=c("site_no","Nyear"))
A.FishNNPredsS$site_no<-as.factor(A.FishNNPredsS$site_no)

#Dummies - THE WAY I'M DEFINING, THEY ARE CONSTANT BY YEAR ACROSS ALL THE SITES
#"DroPredsummer"   "DroPredfall"  "FloodPredwinter" "FloodPredspring" 

# #summer
# DrosumStd <- dcast(StdDataPreds, site_no ~ Nyear,value.var = "DroPredsummer")
# rownames(DrosumStd)<-paste("Site",rownames(DrosumStd),sep="")
# DrosumStd$site_no<-NULL
# colnames(DrosumStd)<-paste("Year",colnames(DrosumStd),sep="")
# DrosumStd<-as.matrix(DrosumStd)
# str(DrosumStd)
# 
# #fall
# MagLFfallStd <- dcast(StdDataPreds, site_no ~ Nyear,value.var = "p5fall")
# rownames(MagLFfallStd)<-paste("Site",rownames(MagLFfallStd),sep="")
# MagLFfallStd$site_no<-NULL
# colnames(MagLFfallStd)<-paste("Year",colnames(MagLFfallStd),sep="")
# MagLFfallStd<-as.matrix(MagLFfallStd)
# str(MagLFfallStd)
# 
# #winter
# MagHFwinStd <- dcast(StdDataPreds, site_no ~ Nyear,value.var = "p95winter")
# rownames(MagHFwinStd)<-paste("Site",rownames(MagHFwinStd),sep="")
# MagHFwinStd$site_no<-NULL
# colnames(MagHFwinStd)<-paste("Year",colnames(MagHFwinStd),sep="")
# MagHFwinStd<-as.matrix(MagHFwinStd)
# str(MagHFwinStd)
# 
# #spring
# MagHFspringStd <- dcast(StdDataPreds, site_no ~ Nyear,value.var = "p95spring")
# rownames(MagHFspringStd)<-paste("Site",rownames(MagHFspringStd),sep="")
# MagHFspringStd$site_no<-NULL
# colnames(MagHFspringStd)<-paste("Year",colnames(MagHFspringStd),sep="")
# MagHFspringStd<-as.matrix(MagHFspringStd)
# str(MagHFspringStd)