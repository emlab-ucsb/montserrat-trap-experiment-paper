#function for creating exploratory plots of response vs explanatory variables - for continous explanatory variables
explor_plots_func_continuous <- function(data, x_var,y_var){
  
  data %>% 
    ggplot(aes_string(x = x_var, y= y_var)) +
    geom_point() +
    geom_smooth(method = 'auto') +
    theme(axis.text.x = element_text(angle=45, hjust = 0.5))
}