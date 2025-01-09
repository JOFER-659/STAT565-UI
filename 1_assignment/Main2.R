#############################################################################
# Question 6.2
# Main2.R
# BY: Jacob Hofer
#
#This function gives a nice interface to check the estimated variance
#and expected value of the genie buffon problem using dirichlet distribution
#to generate pi
#############################################################################

source("../1_assignment/GenieBuffon.Estimated.ExpectedandVariance2.R")

Combine_Buffon2 <- function(N, n, k, a){
  sim<-Sim_BuffonExpNVar2(N, n, k, a)
  cat("Based on parameters n =", n, ", k =", k, ", alpha =", a, ", N =", N, "\n",
      "---------------------------------- \n",
      "The estimated expected number of drops is", sim$Expectation, "\n",
      "The estimated variance of number of drops is", sim$Variance)
}

Combine_Buffon2(N = 100000, n = 34, k = 3, a = c(0.1,0.2,0.7))
