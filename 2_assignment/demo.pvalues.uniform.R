######################################################
# demo.pvalues.uniform.R
# BY: Jacob Hofer
# This is the single function r script that is being
# used to show that p-values from the test of 
# the hypothesis H0: MU <= MU_0 & HA: MU > MU_0
# are uniformly distributed. 
#
# Within the function unif_pvalues 
# we have to specify muknot, sigma
# muknot can be any real number
# sigma can be any positive real number
######################################################


unif_pvalues <- function(muknot, sigma) {
  print("This should take around 30 seconds")
  # Record start time
  start_time <- Sys.time()
  
  # Box-Muller Method
  my.rnorm.box_muller <- function(n, mu, sd) {
    U1 <- runif(n)                 # Generating uniform 
    U2 <- runif(n)                 # Values to transform
    
    Z1 <- sqrt(-2 * log(U1)) * cos(2 * pi * U2)   # Generating normal variates
    Z2 <- sqrt(-2 * log(U1)) * sin(2 * pi * U2)   # From uniforms
    
    N1 <- mu + sd * Z1
    N2 <- mu + sd * Z2
    
    return(c(N1, N2)[1:n])
  }
  
  N <- 10000 #number of hypothesis tests
  NR <- 9991 #number of sample sizes
  mu <- muknot
  p_values <- matrix(NA, nrow = NR, ncol = N) # Initialize matrix to store p-values
  
  for (n in 10:10000) { 
    xbars <- my.rnorm.box_muller(NR, mu, sigma / sqrt(n)) #generate xbars
    
    test_R <- (xbars - muknot) / (sigma / sqrt(n))  # Calculating z-scores
    p_values[, n - 9] <- 1 - pnorm(test_R)   # Generating p-values from z-stats
  } 
  
  ## Generate MAD statistics
  MAD_stat <- numeric()      # Initialize vector to store MAD for each n
  expect <- 100 # Expected count per bin
  for (i in 1:9991) {
    obs <- hist(p_values[, i], breaks = seq(0, 1, length.out = 101), plot = FALSE)$counts
    MAD_stat[i] <- mean(abs(obs - expect)) / 100
  }
  
  plot(10:10000, MAD_stat, type = "p", ylab = 'MAD Statistic', xlab = 'Sample Size') # Generate the plot
  
  
  end_time <- Sys.time()
  
  # Calculate the total duration
  duration <- end_time - start_time
  print(paste("Execution time:", duration)) 
  
  # Return p-values 
  # return(p_values)  
}
