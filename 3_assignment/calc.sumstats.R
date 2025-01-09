calc.sumstats <- function(x){
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
h.x = sum(x^2)
g.x = sum(log(x))
stats = c(h.x,g.x)
#
return(stats)
}
#-------------------------------------------------------------
# end calc.sumstats