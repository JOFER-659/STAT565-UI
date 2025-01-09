calc.suplik <- function(stats){
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
#  
log.lik = matrix(NA, nrow = lt, ncol = ls)
#
for(i in 1:lt){
    for(j in 1:ls){
      log.lik[i,j] = -sigma.vec[j]*stats[1] + (theta.vec[i] - 1)*stats[2] 
      - constants(theta.vec[i], sigma.vec[j])
# theta in rows, sigma in cols
    }
  }
#   
sup.lik = max(log.lik)
#
return(sup.lik)
}
#-------------------------------------------------------------
# end calc.suplik