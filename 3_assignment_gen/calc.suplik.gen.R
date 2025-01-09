calc.suplik.gen <- function(stats){
#--------------------------------------------------
# @description
# Calculates the supremum of the loglikelihood function on
# a grid of parameter space.
#--------------------------------------------------
# @args in 
# Summary statistics h(x),g(x) by stats
# k, number of alleles
#  
# mintheta, maxtheta, minsigma, maxsigma, step.size are 
# defined as global variables in the main
# and avaiable for use   
#--------------------------------------------------  
# @args out 
# Scalar. Supremum of the loglikelihood function.
#--------------------------------------------------  
# @calls
# None 
#-------------------------------------------------    
# @lastChange 2024-09-10
#-------------------------------------------------
# begin calc.suplik  
#-------------------------------------------------  
  theta.vec  = seq(mintheta, maxtheta, step.size)
  sigma.vec  = seq(minsigma, maxsigma, step.size)
  lt = length(theta.vec)
  ls = length(sigma.vec)

  log.lik = matrix(NA, nrow = lt, ncol = ls)
  
  # Loop over all combinations of theta and sigma
  for (i in 1:lt) {
    for (j in 1:ls) {
      # Initialize total log-likelihood for this combination of theta and sigma
      total_log_lik = 0
      
      # Loop over all observations
      for (obs in 1:nrow(stats)) {
        h.x = stats[obs, 1]  # h(x) for this observation
        g.x = stats[obs, 2]  # g(x) for this observation
        
        # Calculate the log-likelihood for this observation and sum it up
        log.lik_obs = -sigma.vec[j] * h.x + (theta.vec[i] - 1) * g.x - constants(theta.vec[i], sigma.vec[j])
        total_log_lik = total_log_lik + log.lik_obs
      }
      
      # Store the total log-likelihood
      log.lik[i, j] = total_log_lik
    }
  }
  
  # Calculate the supremum of the log-likelihood
  sup.lik = max(log.lik)
  
  return(sup.lik)
}
#-------------------------------------------------------------
# end calc.suplik