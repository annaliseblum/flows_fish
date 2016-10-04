###Impact of Extreme Streamflows on Brook Trout Young-of-Year Abundance
### Annalise G Blum, Ben H Letcher, Richard M Vogel 

## Code Set up ###
#Import data or load raw data sets:
#source(file="code/DataImport.R") #(previously SitesImport.R)

#load:
load("data/rawDailyData.rdata") #flows for 47 USGS sites in HUC2 and GAGESII EasternMts
load("output/USGS_BC.rdata") #basin characteristics from GAGESII
load("output/fishsiteDf.rdata") #fish site characteristics from Kanno et al (2016)
load("output/countAr.rdata") #fish DATA array from Kanno et al (2016)
load("output/UVAstreamsSC.rdata") #UVA sites characteristics
load("output/UVA_Discharge.rdata") #UVA flow data

## Data cleaning ###
source(file="code/FlowPrep.R")
source(file="code/FishPrep.R")
source(file="code/WeatherPrep.R")

## Combine data sets ###
source(file="code/Combine.R")
