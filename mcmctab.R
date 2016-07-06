# R function for summarizing MCMC output in a regression-style table
# Johannes Karreth

# I use this function mainly for teaching.

# The function produces a table with means, SDs, credible intervals, and
# the % of posterior draws below/above 0 from MCMC output from 
# R2jags, rjags, R2WinBUGS, R2OpenBUGS, and MCMCpack

# Depends on packages: coda, rstan (if working with rstan objects)

# Arguments: 
# sims: output from R2jags, rjags, R2WinBUGS, R2OpenBUGS, MCMCpack, rstan
# ci: desired credible interval, default: 0.95
# digits: desired number of digits in the table, default: 2

mcmctab <- function(sims, ci = 0.95, digits = 2){
  
  require(coda)	
  
  if(class(sims) == "jags" | class(sims) == "rjags"){
    sims <- as.matrix(as.mcmc(sims))
  }
  if(class(sims) == "bugs"){
    sims <- sims$sims.matrix
  }  
  if(class(sims) == "mcmc"){
    sims <- as.matrix(sims)
  }    
  if(class(sims) == "mcmc.list"){
    sims <- as.matrix(sims)
  }      
  if(class(sims) == "stanfit"){
    stan_sims <- rstan::As.mcmc.list(sims)
    sims <- as.matrix(stan_sims)
  }      
  
  dat <- t(sims)
  mcmctab <- apply(dat, 1, 
                   function(x) c(Mean = round(mean(x), digits = digits), # Posterior mean
                                 SD = round(sd(x), digits = 3), # Posterior SD
                                 Lower = as.numeric(round(quantile(x, probs = c((1 - ci) / 2)), digits = digits)), # Lower CI of posterior
                                 Upper = as.numeric(round(quantile(x, probs = c((1 + ci) / 2)), digits = digits)), # Upper CI of posterior
                                 Pr = round(ifelse(mean(x) > 0, length(x[x > 0]) / length(x), length(x[x < 0]) / length(x)), digits = digits) # Probability of posterior >/< 0
                   ))
  return(t(mcmctab))
}