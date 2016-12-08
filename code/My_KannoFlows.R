##AB call she yoy model 3.5
##June 6, 2016; Modified Oct 27,2016
#rm(list = setdiff(ls(), lsf.str())) #clears all variables except functions
##SLOPE STANDARDIZATION DOESN'T HAVE SD=1; FIX THIS

getwd()
library(reshape2);library(rjags);library(plyr);library(ggplot2);library(knitr);library(arm);library(boot)
load.module("glm")

## Read in data

## fish count data
load("~/flows_fish/YK/countArray 115 sites.rdata")
dim(countAr) #list of 4 (115  29   2   3)
#countAr[1:115, ,,] #to look at first part

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
nSites=115; nYears=29; nAges=1; nPasses=3 ## Just run for nAge = YOY

nCovs=8

# bundle data
dat <- list(nSites=nSites, nYears=nYears, nCovs=nCovs, nAges=nAges,
            y=countAr,  # three-pass of both adults & YOY
            summer.temp=MaxP1fallStd, fall.temp=fallTempAryStd, #summerTempAryStd
            winter.temp=winterTempAryStd, spring.temp=springTempAryStd,
            SummerFlow=MaxP1winStd, FallFlow=MaxP1springStd,
            WinterFlow=MagHFwinStd, SpringFlow=MagHFspringStd,
            elev=elev.std, lat=lat.std,
            julian=julian.std.ar, prcpTot=prcpTot.std.ar, area=area.std)

# #New variables
# summer.temp=MaxP1fallStd, fall.temp=fallTempAryStd,
# winter.temp=winterTempAryStd, spring.temp=springTempAryStd,
# SummerFlow=MaxP1winStd, FallFlow=MaxP1springStd,
# WinterFlow=AvgQwinterStd, SpringFlow=AvgQspringStd,
# elev=elev.std, lat=lat.std,
# julian=julian.std.ar, prcpTot=prcpTot.std.ar, area=area.std)

# SummerFlow=AvgQsummerStd, FallFlow=AvgQfallStd,
# WinterFlow=AvgQwinterStd, SpringFlow=AvgQspringStd,

# SummerFlow=summerPrcpAryStd, FallFlow=fallPrcpAryStd,
# WinterFlow=winterPrcpAryStd, SpringFlow=springPrcpAryStd,

# SummerFlow=DurLFsumStd, FallFlow=DurLFfallStd,
# WinterFlow=DurHFwinStd, SpringFlow=DurHFsprStd, #try replacing  precip with avg flows (outJUI3), outJUI4 is duration
# 
# SummerFlow=MagLFsumStd, FallFlow=MagLFfallStd,
# WinterFlow=MagHFwinStd, SpringFlow=MagHFspringStd, 

## set initial values
init <- function() list( mu=runif(nAges,0,5),
                         eps=array(runif(nSites*nYears*nAges,-1,1), c(nSites,nYears,nAges)),
                         b=array(rnorm(nCovs*nSites*nAges,0,2), c(nCovs,nSites,nAges)),
                         sigmaN=runif(nAges,0,3), #replaced 2 with nAges
                         sigma.b=array(runif(nCovs*nAges,0,3), c(nCovs,nAges)),
                         g.0=array(rnorm(nCovs*nAges,0), c(nCovs,nAges)), #trying loop without site-specific vars predicting Bs
                         g.1=array(rnorm(nCovs*nAges,0), c(nCovs,nAges)),
                         g.2=array(rnorm(nCovs*nAges,0), c(nCovs,nAges)),
                         g.3=array(rnorm(nCovs*nAges,0), c(nCovs,nAges)),
                         p.mean=rep(0.5,nAges), p.b=array(rnorm(3*nAges,0,0.5), c(3,nAges)), #p.mean=rep(0.5,nAges) from 2
                         N=array(500, dim=c(nSites, nYears, nAges)),
                         b.day=runif(nAges,-2,2),
                         b.site=array(rnorm(3*nAges,0,0.5), c(3,nAges)))

# ## Running JAGS
# ## sequential
# set.seed(234)
# StageBurnin <- jags.model(paste("YK/shen yoy model 3.5.r", sep=""),
#                           dat, init, n.chains=3, n.adapt=100000)
# 
# ## concise summary
# Niter=50000
# Nthin=20
# pars <- c("mu","sigmaN2","sigma2.b","g.0",
#           "p.mean","p.b","b.day","b.site","N") 
# outpaperFull <- coda.samples(StageBurnin, pars, n.iter=Niter, n.thin=Nthin) #same as paper: 115 sites and 29 years
# 
# summary(out1)
# plot(out1)
# 
# outpaperFull_df<-as.data.frame(as.matrix(outpaperFull))
# save(outpaperFull_df,file="output/outpaperFull_df.rdata")


##with JAAGUI - started at 11: am pm - ended 1 pm! with 3 cores
#started at 2:15pm; done at 3:43 pm
nc=3;ni=50000; nt=20; nb=10000
pars <- c("mu","sigmaN2","sigma2.b","b",
          "p.mean","p.b","b.day","b.site","g.0") 
# pars <- c("mu","sigmaN2","sigma2.b","g.0","g.1","g.2","g.3",
#           "p.mean","p.b","b.day","b.site","eps") 
library(jagsUI)
outJUI_Dec8 <- jags( #started 9:35, done at 11:10
  data=dat,
  inits=init,
  model = paste("code/New my yoy model 1.r", sep=""),
  parameters.to.save = pars,
  n.chains=nc,
  n.iter = ni,
  n.thin = nt,
  n.burnin=nb,
  parallel=T)

summary(outJUI_Dec8)
save(outJUI_Dec8,file="output/outJUI_Dec8.rdata") #with max P1


save(outJUI2,file="output/outJUI2_avgW.rdata") #Kanno weather only
save(outJUI3,file="output/outJUI3_avgS.rdata") #Seasonal averages
save(outJUI4,file="output/outJUI4_dur.rdata") #Duration
save(outJUI5,file="output/outJUI4_mag.rdata") #Magnitude

save(outJUI3_uva,file="output/outJUI3_uva_avgS.rdata") #Seasonal averages



#can check out commands whiskerplot and traceplot
## Gelman
library(coda)
gelman.diag(out1)

## detailed summary and for graphing
pars2 <- c("mu","sigmaN2","sigma2.b","g.0","g.1","g.2","g.3",
           "p.mean","p.b","b.day","b.site","b","N","p") 
out2 <- jags.samples(StageBurnin, pars2, n.iter=Niter, thin=Nthin) 

#pull out mean of bs
outpaper2_df<-as.data.frame(as.matrix(outpaper2))
save(outpaper2_df,file="output/outpaper2_df.rdata")

sumoutpaper2 <-summary(outpaper2)
head(summary(outpaper2)[2]$quantiles)
colMeans(summary(outpaper2)[2]$quantiles)

#  g.0[h,j] ~ dnorm(0, 0.01) where h is covariates (1=8) and j is ages 1 =YOY
#want g.0[h,1]

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



