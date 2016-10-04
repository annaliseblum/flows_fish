###Impact of Extreme Streamflows on Brook Trout Young-of-Year Abundance
### Annalise G Blum, Ben H Letcher, Richard M Vogel

#rm(list = setdiff(ls(), lsf.str())) #clears all variables except functions

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
source(file="code/FlowPrep.R")
source(file="code/FishPrep.R")
source(file="code/WeatherPrep.R")

## Combine data sets ###
source(file="code/Combine.R")

## Develop Flow prediction models##
#source(file="code/SRegRegLow.R")
#source(file="code/RRegHigh.R")

## Predict seasonal HFs and LFs at ungaged fish sites##


## Compare GOF of fish prediction models ##
#CV

##Run models in a Baysian framework
#MyKanno
