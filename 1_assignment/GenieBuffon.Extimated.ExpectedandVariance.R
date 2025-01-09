#############################################################################
# Question 5
# GenieBuffon.Estimated.ExpectedandVariance.R
# BY: Jacob Hofer
#
#This function estimates the expectation and variance of the genie buffon 
#problem using N (number of sims), n (number of drops per imp), k (number of
# imps), and a (the beta parameter).
#############################################################################

#Creating Function
Sim_BuffonExpNVar<-function(N, n, k, a){
  if (a <= 0) {
    stop("a must be greater than 0")
  }
  if (n <= 0) {
    stop("n must be greater than 0")
  }
  if (k <= 0) {
    stop("k must be greater than 0")
  }
  if (N <= 0) {
    stop("N must be greater than 0")
  }
  sim_result <- numeric(N)
  for (i in 1:N){
  p<-rbeta(k, shape1 = a, shape2 = a)
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
Sim_BuffonExpNVar(10000,4,28,2)


