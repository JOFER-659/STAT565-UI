main.gen <- function(x,n){
  #--------------------------------------------------
  #--------------------------------------------------
  # @description 
  # Main (called by the user) for a suit of programs to sample the 
  # posterior distribution of the parameters
  # of the stationary distribution of allele frequencies 
  # under heterozygote advantage with selection-mutation- genetic drift
  # balance
  #-------------------------------------------------- 
  # @args in 
  # Vector of population frequencies (data), sample size
  # required from the posterior of (theta, sigma).
  #--------------------------------------------------  
  # @args out 
  # List of two. Posterior sample of theta and sigma. 
  # Errors for sanity checks: 
  # Population frequencies should sum to 1, sample size should
  #  be an integer (with some bounds on it).
  #--------------------------------------------------  
  # @calls
  # Not sure. Possibly all, except test.R (test the program).
  #  Examples: calc.sumstats.R (summary statistics), calc.suplik.R
  #  (supremum of the likelihood), rej.R (rejection).  
  #--------------------------------------------------
  # @current version
  # draft
  # inconsistencies to be cleared:
  # 
  # bad coding practices to be polished: 
  # 
  # improve efficiency: 
  #
  #--------------------------------------------------  
  # @typical usage
  # main(x = c(0.2, 0.3, 0.5), n = 100) 
  #
  #--------------------------------------------------
  # @note 
  # 1. The integration constants c(theta,sigma)
  # are loaded as a look up table to make the 
  # program run faster. 
  # Needs to be computed for each k.
  # 2. sim.dirichlet.R is not yet available.
  # Write this function. Either uses beta distributed marginals
  # to simulate from Dirichlet or it uses Gamma distribution to 
  # simulate from Dirichlet.
  #--------------------------------------------------
  # @PlannedChanges
  # None.
  #-------------------------------------------------
  # @lastChange 2024-09-10
  #--------------------------------------------------
  # @Changes 
  # None
  #--------------------------------------------------
  #--------------------------------------------------  
  #--------------------------------------------------  
  # begin main
  #--------------------------------------------------
  ###########################
  ## SANITY CHECKS (FIRST)
  ###########################
  if(ncol(x)<3){stop("k in k-allele model must be at least 3")}
  if(any(x < 0)){stop("population relative frequencies cannot be negative")}
  if(any(rowSums(x) != 1)){stop("population relative frequencies should sum to 1")}
  if(n%%1 != 0){stop("sample size should be integer valued")}
  # n%%1==0 tests whether a number is integer
  if(n<1){stop("sample size should be positive integer")}
  if(n>1e3){warning("desired sample size is large, it may take a while for the program 
                  to obtain that sample size")}
  # more possible
  #--------------------------------------------------
  #
  #############
  #PACKAGES
  #############
  #library(MCMCpack)
  # or write your own sim.dirichlet.R instead
  #
  #############
  ## PATHS
  #############
  # Edit full path to the base directory of the project
  #
  script_dir <<- dirname(rstudioapi::getActiveDocumentContext()$path)
  if (substr(script_dir, nchar(script_dir) - 2, nchar(script_dir)) != "src"){
    script_dir <<- paste0(script_dir,"/src")
  }
  constants_path <- file.path(script_dir, "..", "data", "constant.RDS")
  constant <- readRDS(constants_path)
  scripts_to_source <- c("sim.dirichlet.gen.R", "sim.data.gen.R", "calc.sumstats.gen.R", "calc.suplik.gen.R", "sim.par.gen.R", "rej.gen.R", "constants.gen.R")
  
  for (script in scripts_to_source) {
    script_path <- file.path(script_dir, script)
    if (file.exists(script_path)) {
      source(script_path)
    } else {
      stop("Required script ", script, " not found at ", script_path)
    }
  }
  
  #############
  ## CONSTANTS
  #############
  #
  # Load the c(theta,sigma) constants needed to calculate the loglikelihood
  # after they are calculated once by calc.const.R
  #
  # NB.--------------------------------------------- 
  # Constants need to be calculated for each k separately.
  # Check whether the size of the resulting output is feasible to
  # package with the program. Otherwise constants need to be calculated
  # on user's end. Check computing time it takes.
  # calc.const.R SHOULD NOT OUTPUT the constants
  # in a matrix named x. Because it overwrites
  # the population frequencies x. Give it a unique name.
  #-------------------------------------------------
  k = length(x[1,]) # number of alleles
  kth_element <- constant[[k]]
  constant <<- kth_element
  # note the use of global variable assignment <<-
  # this makes const available to all functions globally
  #
  ###################
  ## INPUT PARAMETERS
  ###################
  # Global variables
  # theta support
  mintheta <<- 0.001 # minimum theta, global
  maxtheta <<- 1 # maximum theta, global
  minsigma <<- 0.001 # minimum sigma, global
  maxsigma <<- 1 # maximum sigma, global
  step.size <<- 0.001 # the grid resolution on which the constants are computed
  res.round <<- 3 # number of digits to be rounded for uniforms. 
  # calculate this from step.size (better) 
  #
  #--------------------------------------------------
  ###############################
  ## EXTRACT FEATURES FROM INPUTS
  ###############################
  
  #--------------------------------------------------
  ####################
  ## FUNCTION CALLS
  ####################
  #
  # NB.-------------------------------------------------------
  # If we will calculate constants for user defined k
  # then we need a function call here to calculate the constants
  # loading constants will become moot
  # just do not forget to make the constants global so that
  # all functions can access it
  #----------------------------------------------------------
  # Calculate summary statistics h(x) and sum of g(x)
  stats = calc.sumstats.gen(x)
  
  # Calculate supremum of the loglikelihood
  sup.lik <<- calc.suplik.gen(stats) 
  # global variable
  
  # Sample the posterior distribution
  post.sample = rej.gen(n, stats) 
  # takes mintheta, maxtheta, minsigma, maxsigma, res.round 
  # and constants as global arguments in
  
  theta = post.sample[[1]] # NB. Not good coding practice. Define the output list of rej.R 
  sigma = post.sample[[2]] # with proper names theta and sigma using "names" function
  
  return(post.sample)
  # Add: Save files. Save the posterior sample and relevant parameters into a file.
}
#--------------------------------------------------------
# end main
