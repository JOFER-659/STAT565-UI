######################################################
# my.rnorm.box_muller.R
# BY: Jacob Hofer
# This function produces rnorm values using box-muller
# method
#
# In the function you must specify n, mu, and sd
# n is the number of RV to produce
# mu is the mean of normal dist the RV is being generated from
# sd is the standard dev of the normal dist for generation
######################################################


my.rnorm.box_muller <- function(n, mu, sd) {
  U1 <- runif(n)                 # Generating uniform 
  U2 <- runif(n)                 # Values to transform
  
  Z1 <- sqrt(-2 * log(U1)) * cos(2 * pi * U2)   # Generating normal variates
  Z2 <- sqrt(-2 * log(U1)) * sin(2 * pi * U2)   # From uniforms
  
  N1 <- mu + sd * Z1
  N2 <- mu + sd * Z2
  
  return(c(N1, N2)[1:n])
}