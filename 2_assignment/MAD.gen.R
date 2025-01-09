######################################################
# MAD.gen.R
# BY: Jacob Hofer
# This function checks how uniform the p-values generated
# by the function pval.gen are
######################################################

MAD.gen<- function(p_values){
MAD_stat <- numeric()
expect<-100
for (i in 1:9991){
  obs<- hist(p_values[,i], breaks = seq(0,1,length.out = 101), plot = F)$counts
  MAD_stat[i] <- mean(abs(obs-expect))/100
}
return(MAD_stat)
}