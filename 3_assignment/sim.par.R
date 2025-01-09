sim.par <- function(){
# @description
# Simulates parameters from prior distributions as
# proposals for rejection algorithm.  
# Simulates parameters theta, sigma from uniform distribution independently
# theta in [mintheta, maxtheta], sigma in [minsigma, maxsigma]
# takes in mintheta, maxtheta, minsigma, maxsigma, res.round as globals
# from the main
#-----------------------------------------------------------
# @args in 
# None.
#-----------------------------------------------------------
# @args out 
# Vector of length two: thetas, sigma.
#-----------------------------------------------------------  
# @calls 
# None.
#-----------------------------------------------------------  
# @lastChange 2024-09-10
#-------------------------------------------------      
# begin sim.par
#-----------------------------------------------------------  
theta.star = runif(1, mintheta, maxtheta)
sigma.star = runif(1, minsigma, maxsigma)
epsilon = 0.001
pars = round(c(theta.star, sigma.star), res.round)
pars[pars == 0] <- epsilon #added this to avoid zeros when indexing constant matrix
# it is okay that some values are 1, as they can still be indexed properly
# runif returns from continuous
# uniform, but constant is built on a grid of stepsize. 
# Thus the rounding to lookup the relevant constant.
#
return(pars)
}
#-----------------------------------------------------------
# end sim.par