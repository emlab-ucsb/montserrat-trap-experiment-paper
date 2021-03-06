#Kolmogorov-smirnov test and cumulative distribution plotting function - generic function, different to specific one used for species level testing

ks_testing_generic <- function(data){
  control_length <- data %>% filter(Exp_or_Cont == "Control") %>% pull(Length_cm)
  exp_length <- data %>% filter(Exp_or_Cont == "Experimental") %>% pull(Length_cm)
  
  test_result<- ks.test(control_length,exp_length)
  
  # create ECDF of data
  cdf1 <- ecdf(control_length) 
  cdf2 <- ecdf(exp_length) 
  # find min and max statistics to draw line between points of greatest distance
  minMax <- seq(min(control_length, exp_length), max(control_length, exp_length), length.out=length(exp_length)) 
  x0 <- minMax[which( abs(cdf1(minMax) - cdf2(minMax)) == max(abs(cdf1(minMax) - cdf2(minMax))) )] 
  y0 <- cdf1(x0) 
  y1 <- cdf2(x0) 
  
  ecdf_plot<- ggplot(data, aes(x = Length_cm, group = Exp_or_Cont, color = Exp_or_Cont))+
    stat_ecdf(size=1) +
    xlab("Sample") +
    ylab("ECDF") +
    theme_bw() +
    scale_fill_manual(values = plot_colours) +
    geom_segment(aes(x = x0[1], y = y0[1], xend = x0[1], yend = y1[1]),
                 linetype = "dashed", color = "red") +
    geom_point(aes(x = x0[1] , y= y0[1]), color="red", size=1) +
    geom_point(aes(x = x0[1] , y= y1[1]), color="red", size=1) +
    theme(legend.title=element_blank()) + 
    annotate("text", label = paste0("Kolmogorov-Smirnov Test results", "\n", "p-value = ",sprintf("%0.2f",test_result$p.value), "\n", "D = ",sprintf("%0.2f" , test_result$statistic)), x=Inf, y = -Inf, hjust = 1.0, vjust = -0.4)
  
  hist_plot <- ggplot(data, aes(x=Length_cm, fill= Exp_or_Cont)) +
    geom_histogram(binwidth = 1) +    
    theme_bw() +
    scale_fill_manual(values = plot_colours) +
    guides(fill = FALSE) +
    theme(axis.text = element_text(size = 14), axis.title = element_blank(), strip.text.x = element_text(size = 14)) +
    facet_wrap(~Exp_or_Cont, nrow = 2)
  
  ifelse(test_result$p.value>0.05,print("Not significant"), print(paste0(unique(data$common), " p-value = ", test_result$p.value)))

  return(plot_grid(ecdf_plot, hist_plot, nrow = 2))
  
}
