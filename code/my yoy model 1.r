model{
  
  ## varying-slope model
  for(i in 1:nSites){
    for(t in 1:nYears){
      for(j in 1:nAges){
        N[i,t,j] ~ dpois(min(lamb[i,t,j],1000))
        log(lamb[i,t,j]) <-  #alpha[i,j] +
                             mu[j] + eps[i,t,j] +
                             b[1,i,j]*SummerFlow[i,t] + b[2,i,j]*FallFlow[i,t] + 
                             b[3,i,j]*WinterFlow[i,t] + b[4,i,j]*SpringFlow[i,t] +
                             b[5,i,j]*summer.temp[i,t] + b[6,i,j]*fall.temp[i,t] +
                             b[7,i,j]*winter.temp[i,t] + b[8,i,j]*spring.temp[i,t] +
                             b.day[j]*julian[i,t]  + # to respond to Grossman's comment
                             b.site[1,j]*elev[i] + b.site[2,j]*lat[i] + b.site[3,j]*area[i]
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
  
  ### slope
  for(h in 1:nCovs){
    for(j in 1:nAges){
      for(i in 1:nSites){
        b[h,i,j] ~ dnorm(mu.b[h,i,j], tau.b[h,j])
        mu.b[h,i,j] <- g.0[h,j] + g.1[h,j]*elev[i] + g.2[h,j]*lat[i] + g.3[h,j]*area[i]
      }  
    g.0[h,j] ~ dnorm(0, 0.01)
    g.1[h,j] ~ dnorm(0, 0.01)
    g.2[h,j] ~ dnorm(0, 0.01)
    g.3[h,j] ~ dnorm(0, 0.01)

    tau.b[h,j] <- pow(sigma.b[h,j], -2)
    sigma.b[h,j] ~ dunif(0, 3)
    sigma2.b[h,j] <- pow(sigma.b[h,j], 2)
    }
  }
  
  ### sampling day effect on abundance (Grossman comment) & spatial covariates 
  for(j in 1:nAges){
    b.day[j] ~ dnorm(0, 0.01)
    b.site[1,j] ~ dnorm(0, 0.01)
    b.site[2,j] ~ dnorm(0, 0.01)
    b.site[3,j] ~ dnorm(0, 0.01)
  }
  
  
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