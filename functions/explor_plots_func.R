  #function for creating exploratory plots of response vs explanatory variables
explor_plots_func <- function(data, x_var,y_var){
  
#function for placement of "n =" labels above plots
n_fun <- function(x){
  return(data.frame(y =median(x)+IQR(x)+2, label = paste0("n = ", length(x))))
}

data %>% 
  ggplot(aes_string(x = x_var, y= y_var)) +
  geom_boxplot() +
  stat_summary(fun.data = n_fun, geom = "text", fun = median) +
  theme(axis.text.x = element_text(angle=45, hjust = 0.5))
}