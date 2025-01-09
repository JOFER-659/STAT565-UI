#############################################################################
# Question 3
# GenieBuffon.ExpectedandVariance.R
# BY: Jacob Hofer
#
#This function generates the theoretical expectation and variance for the genie
#buffon problem
#############################################################################

#Creating function
BuffonExpNVar <- function(n, k, a){
  if (a <= 0){
    stop("a must be greater than 0")
  }
  if (n <= 0){
    stop("n must be greater than 0")
  }
  if (k <= 0){
    stop("k must be greater than 0")
  }
  exp <- (n*k)/2
  var <- k*((2*n*a+n^2)/(8*a + 4))
  final <- list(Expectation = exp, Variance = var)
  return(final)
}

# Example of use
BuffonExpNVar(n = 10, k = 30, a = 2)




