######################################################
# demo.pvalues.uniform.multi.R
# BY: Jacob Hofer
# This is the modular function r script that is being
# used to show that p-values from the test of 
# the hypothesis H0: MU <= MU_0 & HA: MU > MU_0
# are uniformly distributed. 
#
# Within the function unif_pvalues 
# we have to specify muknot, sigma
# muknot can be any real number
# sigma can be any positive real number
######################################################
source("../2_assignment/my.rnorm.box_muller.R") #box muller function
source("../2_assignment/pval.gen.R") #pval generation
source("../2_assignment/MAD.gen.R")# MAD stat generation
unif_pvalues.mod<-function(muknot, sigma){
  print('This should take around 30 seconds')
  start_time <- Sys.time()
  
  pval<-pval.gen(muknot = muknot, sigma = sigma)
  MAD_stat<-MAD.gen(pval)
  plot(10:10000, MAD_stat, type = "p", ylab = "MAD Statistic", xlab = 'Sample Size')
  
  
  # Record end time
  end_time <- Sys.time()
  
  # Calculate the total duration
  duration <- end_time - start_time
  print(paste("Execution time:", duration))
}

