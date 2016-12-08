model{
  
  ## varying-intercept model - my modifications
  for(i in 1:nSites){
    for(t in 1:nYears){
      N[i,t] ~ dpois(min(lamb[i,t],1000))
      log(lamb[i,t]) <-  alpha[i] + #mu[j] +
        eps[i,t] +
        b1*SummerFlow[i,t] + b2*FallFlow[i,t] +  #remove is to make shared Bs
        b3*WinterFlow[i,t] + b4*SpringFlow[i,t] +
        b5*summer.temp[i,t] + b6*fall.temp[i,t] +
        b7*winter.temp[i,t] + b8*spring.temp[i,t] +
        b.day[j]*julian[i,t] 
    }
  }
}

## priors
### intercept
for(j in 1:nAges){
  mu[j] ~ dnorm(0, 0.001)T(0,5)
}
### site*year random effect
for(i in 1:nSites){
  for(t in 1:nYears){
    for(j in 1:nAges){
      eps[i,t,j] ~ dnorm(0, tauN[j])
    }
  }
}

for(j in 1:nAges){ #this is to get tau for the error for the site*year random effect (or model error?)
  tauN[j] <- pow(sigmaN[j], -2)
  sigmaN[j] ~ dunif(0, 3)
  sigmaN2[j] <- pow(sigmaN[j], 2)
}

### intercept
for(h in 1:nCovs){
  for(j in 1:nAges){
    for(i in 1:nSites){
      alpha[i,j] ~ dnorm(mu.a[i,j], tau.b[j])
      mu.a[i,j] ~ dnorm(0, 0.01) #g.0[h,j] + g.1[h,j]*elev[i] + g.2[h,j]*lat[i] + g.3[h,j]*area[i]
    }  
    # g.0[h,j] ~ dnorm(0, 0.01)
    # g.1[h,j] ~ dnorm(0, 0.01)
    # g.2[h,j] ~ dnorm(0, 0.01)
    # g.3[h,j] ~ dnorm(0, 0.01)
    
    tau.b[j] <- pow(sigma.b[j], -2)
    sigma.b[j] ~ dunif(0, 3)
    sigma2.b[j] <- pow(sigma.b[j], 2) ##what is this for? to see variance
  }
}

### sampling day effect on abundance (Grossman comment) & spatial covariates 
for(j in 1:nAges){
  b.day[j] ~ dnorm(0, 0.01)
  b.site[1,j] ~ dnorm(0, 0.01)
  b.site[2,j] ~ dnorm(0, 0.01)
  b.site[3,j] ~ dnorm(0, 0.01)
}


###fixed slopes
b1~ dnorm(0, 0.01)
b2~ dnorm(0, 0.01)
b3~ dnorm(0, 0.01)
b4~ dnorm(0, 0.01)
b5~ dnorm(0, 0.01)
b6~ dnorm(0, 0.01)
b7~ dnorm(0, 0.01)
b8~ dnorm(0, 0.01)

#### Detection
for(i in 1:nSites) {
  for(t in 1:nYears){
    for(j in 1:nAges){
      y[i,t,j,1] ~ dbin(p[i,t,j], N[i,t,j])
      y[i,t,j,2] ~ dbin(p[i,t,j]*(1-p[i,t,j]), N[i,t,j])
      y[i,t,j,3] ~ dbin(p[i,t,j]*(1-p[i,t,j])*(1-p[i,t,j]), N[i,t,j])  
      
      p[i,t,j] <- 1/(1 + exp(-lp.lim[i,t,j]))
      lp.lim[i,t,j] <- min(999, max(-999, lp[i,t,j]))
      lp[i,t,j] <- p.mu[j] + p.b[1,j]*julian[i,t] + p.b[2,j]*prcpTot[i,t] + p.b[3,j]*area[i]
    }
  }  
}

#### Detection prior
for(j in 1:nAges){
  p.mean[j] ~ dunif(0.1,0.9)
  p.mu[j] <- log(p.mean[j]/(1-p.mean[j]))
  
  p.b[1,j] ~ dnorm(0,0.37)T(-3,3)
  p.b[2,j] ~ dnorm(0,0.37)T(-3,3)
  p.b[3,j] ~ dnorm(0,0.37)T(-3,3)
}
}