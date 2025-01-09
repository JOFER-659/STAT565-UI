sim.data <- function(k, theta, sigma){
#----------------------------------------------------------
# @ description
# Simulates population relative frequencies
# by rejection using Dirichlet distribution with same theta 
# as the proposal distribution
#----------------------------------------------------------  
# @args in 
# k, number of alleles
# (theta, sigma) under which frequencies are desired 
# Uses global variable constants loaded by the test.R 
# 
#---------------------------------------------------------- 
# @args out
# Vector of population relative frequencies x
# ---------------------------------------------- 
# @calls  
# sim.dirichlet  
#----------------------------------------------------------
# @ return 
# Vector of length k of population frequencies
#----------------------------------------------------------
# @lastChange 2024-09-10
#----------------------------------------------------------  
# @NB---------------------------------------------
# Check that the acceptance condition for the rejection is correct
# especially sup.lik part
# and check for the B(theta) constant whether
# the acceptance condition needs to be multiplied by that.
#----------------------------------------------------------
# begin sim.data
#----------------------------------------------------------  
x = rep(NA, k)
theta.vec = rep(theta,k)
i = 0
while(i!=1){
ystar = sim.dirichlet(1, theta.vec) 
# to simulate from Dirichlet or it uses Gamma distribution to simulate from Dirichlet.
if(log(runif(1)) < (-sigma)*sum(ystar^2)+(theta - 1)*sum(log(ystar)) - constants(theta, sigma)){
  if(sum(ystar) == 1){
   x = ystar
  i = 1
  }
}
}
#
return(x)
}
#--------------------------------------------------------
# end sim.data