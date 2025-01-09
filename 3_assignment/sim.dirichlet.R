sim.dirichlet <- function(n, theta){
#------------------------------------------------------
# @description
# Simulates from Dirichlet distribution with parameter vector
# theta = (theta_1, theta_2,...,theta_k)
# using Gamma distibuted rvs. (rgamma)
# Theory:
# Let Y_i ~ Gamma(theta_i, 1), where theta_i is the shape parameter
# of the gamma distribution.
# Let X_i = Y_i/sum(Y_i) 
# Then X = (X_1,X_2,...,X_k) ~ Dirichlet(theta_1, theta_2,...,theta_k)
#-------------------------------------------------------
# @args in
# n, the sample size from the Dirichlet distribution
# theta, vector valued parameter of the Dirichlet distribution
#-------------------------------------------------------
# @args out
# A matrix of size n by k where each row is a draw from the 
# Dirichlet distribution
#-----------------------------------------
# begin sim.dirichlet 
#-----------------------------------------
k = length(theta)
y = matrix(NA, nrow = n, ncol = k)
epsilon = 1e-10
for(i in 1:k){
y[,i] = rgamma(n, theta[i], 1)
}
row_sums = rowSums(y)
row_sums[row_sums < 1e-15] = row_sums[row_sums < 1e-15] + epsilon #included to 
#not let NA's happen

x = y / row_sums

return(x)
}
#--------------------------------------------
# end sim.dirichlet