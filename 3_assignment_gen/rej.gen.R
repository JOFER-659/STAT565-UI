rej.gen <- function(n, stats){
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
  while (i < n + 1) { 
    pars = sim.par()  
    theta_star = pars[1]
    sigma_star = pars[2]
    
    total_log_lik = 0
    for (obs in 1:nrow(stats)) {
      h.x = stats[obs, 1]  # Extract h(x) for this observation
      g.x = stats[obs, 2]  # Extract g(x) for this observation
      
      # Calculate log-likelihood for this observation
      log.lik = -sigma_star * h.x + (theta_star - 1) * g.x - constants(theta_star, sigma_star)
      total_log_lik = total_log_lik + log.lik  # Sum across all observations
    }
    
    # Compare the total log-likelihood to the supremum
    if (log(runif(1)) < total_log_lik - sup.lik) {
      posterior.sample$theta[i] = theta_star  
      posterior.sample$sigma[i] = sigma_star  
      i = i + 1
    }
  }
  
  return(posterior.sample)
}
#---------------------------------------------------------------
# end rej