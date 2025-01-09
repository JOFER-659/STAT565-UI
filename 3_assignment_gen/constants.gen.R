constants <- function(theta, sigma) {
 
  
  # Calculate the index for theta and sigma
  # Since each step is 0.001, we can multiply by 1000 to get the correct index
  theta_index <- round(theta * 1000)
  sigma_index <- round(sigma * 1000)

  return(constant[theta_index, sigma_index])
}
