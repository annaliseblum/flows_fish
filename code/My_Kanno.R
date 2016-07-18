##AB call she yoy model 3.5
##June 6, 2016

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

## Setting up for the JAGs model
# data structure
nSites=115; nYears=29; nAges=2; nPasses=3
nCovs=8

# bundle data
dat <- list(nSites=nSites, nYears=nYears, nCovs=nCovs, nAges=nAges,
            y=countAr,  # three-pass of both adults & YOY
            summer.temp=summerTempAryStd, fall.temp=fallTempAryStd,
            winter.temp=winterTempAryStd, spring.temp=springTempAryStd,
            summer.prcp=summerPrcpAryStd, fall.prcp=fallPrcpAryStd,
            winter.prcp=winterPrcpAryStd, spring.prcp=springPrcpAryStd,
            elev=elev.std, lat=lat.std,
            julian=julian.std.ar, prcpTot=prcpTot.std.ar, area=area.std)

## set initial values
init <- function() list( mu=runif(nAges,0,5),
                         eps=array(runif(nSites*nYears*nAges,-1,1), c(nSites,nYears,nAges)),
                         b=array(rnorm(nCovs*nSites*nAges,0,2), c(nCovs,nSites,nAges)),
                         sigmaN=runif(2,0,3), 
                         sigma.b=array(runif(nCovs*nAges,0,3), c(nCovs,nAges)),
                         g.0=array(rnorm(nCovs*nAges,0), c(nCovs,nAges)), 
                         g.1=array(rnorm(nCovs*nAges,0), c(nCovs,nAges)),
                         g.2=array(rnorm(nCovs*nAges,0), c(nCovs,nAges)),
                         g.3=array(rnorm(nCovs*nAges,0), c(nCovs,nAges)),
                         p.mean=rep(0.5,2), p.b=array(rnorm(3*nAges,0,0.5), c(3,nAges)), 
                         N=array(500, dim=c(nSites, nYears, nAges)),
                         b.day=runif(nAges,-2,2),
                         b.site=array(rnorm(3*nAges,0,0.5), c(3,nAges)))

## Running JAGS
## sequential
set.seed(234)
StageBurnin <- jags.model(paste("YK/shen yoy model 3.5.r", sep=""),
                          dat, init, n.chains=3, n.adapt=100000)

## concise summary
Niter=50000
Nthin=20
pars <- c("mu","sigmaN2","sigma2.b","g.0","g.1","g.2","g.3",
          "p.mean","p.b","b.day","b.site") 
out1 <- coda.samples(StageBurnin, pars, n.iter=Niter, n.thin=Nthin)
summary(out1)
plot(out1)

## Gelman
library(coda)
gelman.diag(out1)

## detailed summary and for graphing
pars2 <- c("mu","sigmaN2","sigma2.b","g.0","g.1","g.2","g.3",
           "p.mean","p.b","b.day","b.site","b","N","p") 
out2 <- jags.samples(StageBurnin, pars2, n.iter=Niter, thin=Nthin) ##STUCK HERE - WANTS ME TO RECOMPILE

N.est <- p.est <- array(NA, dim=c(nSites,nYears,nAges))
for(i in 1:nSites){
  for(t in 1:nYears){
    for(j in 1:nAges){
      N.est[i,t,j] <- median(out2$N[i,t,j,1:(Niter/Nthin),1:3])
      p.est[i,t,j] <- median(out2$p[i,t,j,1:(Niter/Nthin),1:3])
    }
  }
}

y.est <- array(NA, dim=c(nSites,nYears,nAges,nPasses))
y.est[,,,1] <- N.est*p.est
y.est[,,,2] <- N.est*(1-p.est)*p.est
y.est[,,,3] <- N.est*(1-p.est)*(1-p.est)*p.est



