###Impact of Extreme Streamflows on Brook Trout Young-of-Year Abundance
### Annalise G Blum, Ben H Letcher, Yoichiro Kanno, Richard M Vogel

#rm(list = setdiff(ls(), lsf.str())) #clears all variables except functions

#load packages:
library(geoR);library(fossil); library(plyr); library(rgeos);library(sp); library("ggmap")
library(DataCombine); library(ggplot2)

#set plot area
par(mfrow=c(1,1))

##Run functions file
source(file="code/Functions.R")
 
#Import data or load raw data sets:
#source(file="code/DataImport.R") #(previously SitesImport.R)

#load:
load("data/rawDailyData.rdata") #flows for 47 USGS sites in HUC2 and GAGESII EasternMts
load("output/rawUSGS_BC.rdata") #basin characteristics from GAGESII
load("output/fishsiteDf.rdata") #fish site characteristics from Kanno et al (2016)
load("output/countAr.rdata") #fish DATA array from Kanno et al (2016)
load("output/rawUVA_BC.rdata") #UVA sites characteristics
load("output/UVA_Discharge.rdata") #UVA flow data

## Data cleaning ###
source(file="code/FlowPrep.R") #with NN only need section 1 of this data set...
source(file="code/FishPrep.R")
source(file="code/WeatherPrep.R")

## NN - DAR flow predictions ##
source(file="code/New_NN.R")
source(file="code/FlowMets.R")
source(file="code/PredFlowMets.R")

## Combine data sets ###
source(file="code/Combine.R")

#Explore - make comparison plots #
#source(file="code/Explore.R")

## Compare GOF of fish prediction models ##
#CV

##Run models in a Baysian framework
#MyKanno

#### OLD ####
#Attempting to install climdiv to get PSDI Palmer drought
#install.packages("climdiv", repos = "http://ulmo.ucmerced.edu/data/R/climdiv/", type="source")

## Develop Flow prediction models##
#source(file="code/PredFlows.R")
## Predict seasonal HFs and LFs at ungaged fish sites##
#source(file="code/FishPredFlows.R")
