##AB call she yoy model 3.5
##June 6, 2016

##SLOPE STANDARDIZATION DOESN'T HAVE SD=1; FIX THIS

getwd()
library(reshape2);library(rjags);library(plyr);library(ggplot2);library(knitr);library(arm);library(boot)
load.module("glm")

## Read in data

## fish count data
load("~/flows_fish/YK/countArray 115 sites.rdata")
dim(countAr) #list of 4 (115  29   2   3)
countAr[1:115, ,,] #to look at first part

## seasonal climate data
load("~/flows_fish/YK/Site_by_year seasonal climate var standardized 115.rdata")

## site covariate data
load("~/flows_fish/YK/standardized site cov 115 sites.rdata")

## watershed area data
load("~/flows_fish/YK/watershed area standardized 115 sites.rdata")

## detection covariate data
load("~/flows_fish/YK/julian_prcpTot.rdata")

## replace NA in julian dates and prcpTot with 0
julian.std.ar[is.na(julian.std.ar)] <- 0
prcpTot.std.ar[is.na(prcpTot.std.ar)] <- 0

##subset data for 34 sites and 17 years
mycountAr<-countAr[site.pos,13:29,1,] #only 34 sites and only 1996-2010 (lost 1994 and 1995 because of lagged flows for prediction and fish years?)
dim(mycountAr); dimnames(mycountAr)

#matrices of sites by year
str(fallTempAryStd)
fallTempAryStd34<-fallTempAryStd[site.pos,13:29] #only 34 sites and only 1996-2010 (lost 1994 and 1995 because of lagged flows for prediction and fish years?)
winterTempAryStd34<-winterTempAryStd[site.pos,13:29]
springTempAryStd34<-springTempAryStd[site.pos,13:29]
summerTempAryStd34<-summerTempAryStd[site.pos,13:29]

str(julian.std.ar); dim(julian.std.ar)
julian.std.ar34<-julian.std.ar[site.pos,13:29]
prcpTot.std.ar34<-prcpTot.std.ar[site.pos,13:29]

#vectors of 1:115 sites, just pull sites
elev.std34=elev.std[site.pos]
lat.std34=lat.std[site.pos]
area.std34=area.std[site.pos]

## Setting up for the JAGs model
# data structure
nSites34=34; nYears34=17; nPasses34=3 ##I changed nSites and nYears #removed nAges=2;
nCovs=8

#remove adults from data array
dimnames(mycountAr)

# bundle data
dat <- list(nSites=nSites34, nYears=nYears34, nCovs=nCovs, #nAges=nAges,
            y=mycountAr,  # three-pass of both adults & YOY
            summer.temp=summerTempAryStd34, fall.temp=fallTempAryStd34,
            winter.temp=winterTempAryStd34, spring.temp=springTempAryStd34,
            summer.prcp=summerPrcpAryStd34, fall.prcp=fallPrcpAryStd34,
            winter.prcp=winterPrcpAryStd34, spring.prcp=springPrcpAryStd34,
            elev=elev.std34, lat=lat.std34,
            julian=julian.std.ar34, prcpTot=prcpTot.std.ar34, area=area.std34)

## set initial values
init <- function() list( mu=runif(1,0,5),
                         eps=array(runif(nSites*nYears,-1,1), c(nSites,nYears)),
                         b=array(rnorm(nCovs*nSites,0,2), c(nCovs,nSites)),
                         sigmaN=runif(1,0,3), 
                         sigma.b=array(runif(nCovs,0,3), c(nCovs)),
                         g.0=array(rnorm(nCovs,0), c(nCovs)), 
                         g.1=array(rnorm(nCovs,0), c(nCovs)),
                         g.2=array(rnorm(nCovs,0), c(nCovs)),
                         g.3=array(rnorm(nCovs,0), c(nCovs)),
                         p.mean=0.5, p.b=array(rnorm(3,0,0.5), c(3)), 
                         N=array(500, dim=c(nSites, nYears)),
                         b.day=runif(1,-2,2),
                         b.site=array(rnorm(3,0,0.5), c(3)))

## Running JAGS
## sequential
set.seed(234)
StageBurnin <- jags.model(paste("code/My shen yoy model 3.5.r", sep=""),
                          dat, init, n.chains=3, n.adapt=10000) #0

## concise summary
Niter=50000
Nthin=20
pars <- c("mu","sigmaN2","sigma2.b","g.0","g.1","g.2","g.3",
          "p.mean","p.b","b.day","b.site") #is 
out2 <- coda.samples(StageBurnin, pars, n.iter=Niter, n.thin=Nthin)
summary(out2)
plot(out1)


out1_df<-as.data.frame(as.matrix(out1))
save(out1_df,file="output/out1_df.rdata")

## Gelman
library(coda)
gelman.diag(out1)

## detailed summary and for graphing
pars2 <- c("mu","sigmaN2","sigma2.b","g.0","g.1","g.2","g.3",
           "p.mean","p.b","b.day","b.site","b","N","p") 
out2 <- jags.samples(StageBurnin, pars2, n.iter=Niter, thin=Nthin)


