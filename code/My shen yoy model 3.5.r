model{
  
  ## varying-slope model
  for(i in 1:nSites){
    for(t in 1:nYears){
        N[i,t] ~ dpois(min(lamb[i,t],1000))
        log(lamb[i,t]) <- mu + eps[i,t] +
                             b[1,i]*summer.prcp[i,t] + b[2,i]*fall.prcp[i,t] + 
                             b[3,i]*winter.prcp[i,t] + b[4,i]*spring.prcp[i,t] +
                             b[5,i]*summer.temp[i,t] + b[6,i]*fall.temp[i,t] +
                             b[7,i]*winter.temp[i,t] + b[8,i]*spring.temp[i,t] +
                             b.day*julian[i,t]  + # to respond to Grossman's comment
                             b.site[1]*elev[i] + b.site[2]*lat[i] + b.site[3]*area[i]
      }
  }
  
  ## priors
  ### intercept
  mu ~ dnorm(0, 0.001)T(0,5) #T means truncate, here between 0 and 5
  ### site*year random effect
  for(i in 1:nSites){
    for(t in 1:nYears){
        eps[i,t] ~ dnorm(0, tauN)
    }
  }
  
    tauN <- pow(sigmaN, -2)
    sigmaN~ dunif(0, 3)
    sigmaN2 <- pow(sigmaN, 2)
  
  ### slope
  for(h in 1:nCovs){
      for(i in 1:nSites){
        b[h,i] ~ dnorm(mu.b[h,i], tau.b[h])
        mu.b[h,i] <- g.0[h] + g.1[h]*elev[i] + g.2[h]*lat[i] + g.3[h]*area[i]
      }  
    g.0[h] ~ dnorm(0, 0.01)
    g.1[h] ~ dnorm(0, 0.01)
    g.2[h] ~ dnorm(0, 0.01)
    g.3[h] ~ dnorm(0, 0.01)

    tau.b[h] <- pow(sigma.b[h], -2)
    sigma.b[h] ~ dunif(0, 3)
    sigma2.b[h] <- pow(sigma.b[h], 2)
  }
  
  ### sampling day effect on abundance (Grossman comment) & spatial covariates 
    b.day~ dnorm(0, 0.01)
    b.site[1] ~ dnorm(0, 0.01)
    b.site[2] ~ dnorm(0, 0.01)
    b.site[3] ~ dnorm(0, 0.01)
  
  #### Detection
  for(i in 1:nSites) {
    for(t in 1:nYears){
        y[i,t,1] ~ dbin(p[i,t], N[i,t])
        y[i,t,2] ~ dbin(p[i,t]*(1-p[i,t]), N[i,t])
        y[i,t,3] ~ dbin(p[i,t]*(1-p[i,t])*(1-p[i,t]), N[i,t])  
      
        p[i,t] <- 1/(1 + exp(-lp.lim[i,t]))
        lp.lim[i,t] <- min(999, max(-999, lp[i,t]))
        lp[i,t] <- p.mu + p.b[1]*julian[i,t] + p.b[2]*prcpTot[i,t] + p.b[3]*area[i]
    }  
  }
  
  #### Detection prior
    p.mean~ dunif(0.1,0.9)
    p.mu <- log(p.mean/(1-p.mean))
     
    p.b[1] ~ dnorm(0,0.37)T(-3,3)
    p.b[2] ~ dnorm(0,0.37)T(-3,3)
    p.b[3] ~ dnorm(0,0.37)T(-3,3)
    
} 