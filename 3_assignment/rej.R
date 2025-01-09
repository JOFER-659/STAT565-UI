rej <- function(n, k, stats){
# @ description
# Performs the acceptance step for the rejection method.
# Rejection sampling of the joint posterior distribution of parameters (theta, sigma)
# given the jointly sufficient summary statistics stats.
# Uses the (prior) uniform distribution on theta and sigma as proposal
# Assumes prior independence of parameters
#----------------------------------------------------------------  
# @args in
# n, k, stats
#----------------------------------------------------------------   
# @args out 
# List of two, each vector of length sample size, posterior
# samples of (theta, sigma).
#---------------------------------------------------------------- 
# @calls
# sim.par  
#----------------------------------------------------------------
# @lastChange 2024-09-10
#-------------------------------------------------      
# begin rej  
#-------------------------------------------------    
posterior.sample = list(theta = numeric(n), sigma = numeric(n))  
#
i = 1
while(i < n+1){ # sample size from posterior is n
pars = sim.par()
c = constants(pars[1], pars[2])
log.lik = -pars[2]*stats[1] + (pars[1] -1)*stats[2] - c
#
# This is not good coding practice. We need to recall which element
# of pars was thetastar and which sigmastar for args in to c. 
# Further, we need to recall which element
# of stats was theta and which sigma. A good coding practice is to return a list
# for all functions and name the list elements meaningfully as in the model 
# check out the "names" function to assign names to the list elements 
#
if(log(runif(1)) < log.lik - sup.lik){
  posterior.sample[[1]][i] = pars[1] # assign names to output of sim.par in the list
  posterior.sample[[2]][i] = pars[2]
  i = i+1
}
}
#
return(posterior.sample)
}
#---------------------------------------------------------------
# end rej