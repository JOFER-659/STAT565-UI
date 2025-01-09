test.gen <- function(k, nk, theta,sigma, n){
#---------------------------------------------------------
# @description 
# Tests whether the inference works reasonably well
#---------------------------------------------------------
# @args in 
# k, number of alleles  
# true parameter values (theta, sigma) on the allowed space 
# (else return error) 
# n, sample size desired from the posterior of (theta, sigma) 
#
# Uses global variable constants loaded by main
#---------------------------------------------------------
# @ arg out 
# Vector of simulated population frequencies 
# A sample of size n from the posterior of (theta, sigma)
#---------------------------------------------------------
# @calls 
# sim.data (simulate data), main
#---------------------------------------------------------  
# begin test  
  script_dir <<- dirname(rstudioapi::getActiveDocumentContext()$path)
  if (substr(script_dir, nchar(script_dir) - 2, nchar(script_dir)) != "src"){
    script_dir <<- paste0(script_dir,"/src")
  }
  constants_path <- file.path(script_dir, "..", "data", "constant.RDS")
  constant <- readRDS(constants_path)
  scripts_to_source <- c("sim.dirichlet.gen.R", "sim.data.gen.R", "main.gen.R", "constants.gen.R")
  
  for (script in scripts_to_source) {
    script_path <- file.path(script_dir, script)
    if (file.exists(script_path)) {
      source(script_path)
    } else {
      stop("Required script ", script, " not found at ", script_path)
    }
  }  
  
 # number of alleles
  kth_element <- constant[[k]]
  constant <<- kth_element
m <- sim.data.gen(k, nk, theta = theta, sigma = sigma)
meh <- main.gen(m, n)  
est_theta <- mean(meh$theta)
est_sigma <- mean(meh$sigma)

# Output the results
cat("True theta is", theta, "\n",
    "True Sigma is", sigma, "\n",
    "Estimated theta is", est_theta, "\n",
    "Estimated sigma is", est_sigma, "\n")
}
#---------------------------------------------------------  
# end test

