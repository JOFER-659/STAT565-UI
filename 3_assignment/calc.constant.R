source("/Users/jacobhofer/Desktop/UI_Grad_School/STAT_565/Homework/3_assignment/src/sim.dirichlet.R")
calc.constant <- function(N, k.max){
#---------------------------------------------------------------------
# @description
# Calculate and save log of integration constants
#---------------------------------------------------------------------
# @args in
# calc.constant is run outside of main.R
# So, needs the global variables defined in main.R as arguments in.
# N := Number of Monte Carlo simulations 
# (i.e., number of Dirichlet random variables to be generated)
# k.max := maximum number of alleles for which the constants will be calculated
# and saved
# mintheta := minimum value of theta parameter
# maxtheta := maximum value of theta parameter
# minsigma := minimum value of sigma parameter
# maxsigma := maximum value of sigma parameter
# step.size := grid resolution
#---------------------------------------------------------------------
# @ args out
# None. Saves the log constants as a list to a file.
#---------------------------------------------------------------------
# @ Calls
# sim.dirichlet
#---------------------------------------------------------------------
# begin calc.constant
#---------------------------------------------------------------------
N = 1e4 #tuning parameter
k.max = 6 #max k value
mintheta = 0.001 #smallest theta 
maxtheta = 1 #largest theta
minsigma = 0.001 #smallest sigma
maxsigma = 1 #largest sigma
step.size = 0.001 #steps between thetas and sigmas
#--------------------------------
k.vec = c(1:k.max)
lk = length(k.vec)
log.constant = vector("list", lk)
#--------------------------------
theta = seq(from = mintheta, to = maxtheta, by = step.size)
sigma = seq(from = minsigma, to = maxsigma, by = step.size)
lt = length(theta)
ls = length(sigma)
#
i = 3 #loop through list
j = 1 #loops through thetas
s = 1 #loops through sigmas
begin.time = Sys.time()
for(i in 3:k.max){
  log.constant[[i]]=matrix(nrow=lt,ncol=ls) #create matrices for different k's
  for(j in 1:lt){
    theta.vec = rep(theta[j],k.vec[i])
    y = sim.dirichlet(N, theta.vec)
    sum.y = rowSums(y^2)
    log.beta.theta = i*log(gamma(theta[j])) - log(gamma(i*theta[j])) 
    for(s in 1:ls){
      log.constant[[i]][j,s] = log.beta.theta + log(mean(exp(-sigma[s]*sum.y)))
    }
  }
}
 end.time = Sys.time()
 end.time-begin.time
 #save object to data folder in directory
 saveRDS(log.constant, file = "/Users/jacobhofer/Desktop/UI_Grad_School/STAT_565/Homework/3_assignment/data/constant.RDS")
 return(log.constant)
}
#---------------------------------------
# end calc.constant
