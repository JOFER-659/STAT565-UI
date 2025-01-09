#############################################################################
# Question 6
# Main.R
# BY: Jacob Hofer
#
#This function sources and gives a nice interface for comparing the theoretical
#and estimates values of the expectation and variance of the genie buffon 
#############################################################################

source("../1_assignment/GenieBuffon.ExpectedandVariance.R")
source("../1_assignment/GenieBuffon.Extimated.ExpectedandVariance.R")

Combine_Buffon <- function(N, n, k, a){
  sim<-Sim_BuffonExpNVar(N, n, k, a)
  theory<-BuffonExpNVar(n, k, a)
  cat("Based on parameters n =", n, ", k =", k, ", alpha =", a, ", N =", N, "\n",
      "---------------------------------- \n",
      "The expected number of drops is", theory$Expectation, "\n",
      "The estimated expected number of drops is", sim$Expectation, "\n",
      "The variance of number of drops is", theory$Variance, "\n",
      "The estimated variance of number of drops is", sim$Variance)
}

Combine_Buffon(10000, 10, 6, 2)
