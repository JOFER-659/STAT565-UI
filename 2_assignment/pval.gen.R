######################################################
# pval.gen.R
# BY: Jacob Hofer
# This function produces p-values associated with the
# test of hypothesis H0: MU <= MU_0 & HA: MU > MU_0
######################################################



pval.gen<- function(muknot, sigma){
  N <- 10000 #number of hypothesis tests
  NR <- 9991 #number of sample sizes
  mu <- muknot
  p_values <- matrix(NA, nrow = NR, ncol = N) # Initialize matrix to store p-values
  
  for (n in 10:10000) { 
    xbars <- my.rnorm.box_muller(NR, mu, sigma / sqrt(n)) #generate xbars
    
    test_R <- (xbars - muknot) / (sigma / sqrt(n))  # Calculating z-scores
    p_values[, n - 9] <- 1 - pnorm(test_R)   # Generating p-values from z-stats
  }
  return(p_values)
}