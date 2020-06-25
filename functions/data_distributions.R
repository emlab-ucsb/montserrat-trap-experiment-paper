#function to check data distribution: https://ase.tufts.edu/gsc/gradresources/guidetomixedmodelsinr/mixed%20model%20guide.html

#Note: these plots should not be used as the sole criteria for determining which model family to use: https://stats.stackexchange.com/questions/259810/use-of-gamma-distribution-for-count-data
data_distributions <- function(data, column){
  par(mfrow=c(2,2)) 
  
 norm_plot <- car::qqp(data[[column]], "norm", ylab = column, main = "Normal distribution")
 lnorm_plot <- car::qqp(data[[column]], "lnorm", ylab = column, main = "Log-normal distribution")
 
 gamma <- MASS::fitdistr(data[[column]], "gamma")
 gamma_plot <- car::qqp(data[[column]], "gamma", shape = gamma$estimate[[1]], rate = gamma$estimate[[2]],  ylab = column, main = "Gamma distribution")
 
 #these only work for integer values 
 if(all(data[[column]] == floor(data[[column]]))) {
   #can't get negative binomial and poisson distribution plots to work
   nbinom <- MASS::fitdistr(data[[column]], "negative binomial")
   car::qqp(data[[column]], "nbinom", size = nbinom$estimate[[1]], mu = nbinom$estimate[[2]])
   
   poiss <- MASS::fitdistr(data[[column]], "poisson")
   car::qqp(data[[column]], "pois", lambda = poiss$estimate)
 }
}