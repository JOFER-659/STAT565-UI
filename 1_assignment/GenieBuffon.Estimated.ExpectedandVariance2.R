#############################################################################
# Question 5.2
# GenieBuffon.Estimated.ExpectedandVariance2.R
# BY: Jacob Hofer

#This function takes in arguements of N (number of simulations),
#n which is the number of pins dropped, k which is the number
# of imps conducting the experiment, and a which is a vector of 
# parameters for the dirichlet distribution to generate random
# numbers from.
#############################################################################

library(gtools)

#Creating Function
Sim_BuffonExpNVar2<-function(N, n, k, a){
  if (length(a) != k) {
    stop("Length of 'alpha' must be equal to 'k'")
  }
  if (sum(a) != 1) {
    stop("Sum of parameters in a must equal 1")
  }
  if (n <= 0) {
      stop("n must be greater than 0")
    }
  if (k <= 0) {
      stop("k must be greater than 0")
    }
  sim_result <- numeric(N)
  for (i in 1:N){
    p<-rdirichlet(1, alpha = a)
    sum_crosses <- 0
    for (j in 1:k){
      crosses<-rbinom(1, size = n, prob = p[j])
      sum_crosses = sum_crosses + crosses
    }
    sim_result[i] <- sum_crosses
  }
  Expectation <- mean(sim_result)
  Variation <- var(sim_result)
  
  meh<-list(Expectation = Expectation,
            Variance = Variation)
  return(meh)
}

#Example of Use
Sim_BuffonExpNVar2(10000,3,3,c(0.1,0.5,0.4))

