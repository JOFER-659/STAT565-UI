calc.sumstats.gen <- function(x){
#--------------------------------------------------
# @description
# Calculates the summary statistics 
# h(x) = sum(x^2) and g(x) = sum(log(x)).
# h(x) and g(x) are jointly sufficient for (theta, sigma).
#--------------------------------------------------  
# @args in 
# Vector of population frequencies
#-------------------------------------------------- 
# @args out 
# Vector [h(x), g(x)]
#-------------------------------------------------- 
# @calls
# None
#-------------------------------------------------
# @lastChange 2024-09-10
#-------------------------------------------------
# begin calc.sumstats  
#-------------------------------------------------
  stats_matrix <- matrix(NA, nrow = nrow(x), ncol = 2)
  

  for (i in 1:nrow(x)) {
    h.x = sum(x[i, ]^2)  # sum of squares of allele frequencies
    g.x = sum(log(x[i, ]))  # sum of log of allele frequencies
    
    # Store h.x and g.x in the corresponding row of the stats matrix
    stats_matrix[i, ] = c(h.x, g.x)
  }

  return(stats_matrix)
}
#-------------------------------------------------------------
# end calc.sumstats