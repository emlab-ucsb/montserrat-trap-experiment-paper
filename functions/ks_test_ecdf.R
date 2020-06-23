#Kolmogrov-smirnov test and cumulative distribution plotting function

ks_testing <- function(data){
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
    geom_segment(aes(x = x0[1], y = y0[1], xend = x0[1], yend = y1[1]),
                 linetype = "dashed", color = "red") +
    geom_point(aes(x = x0[1] , y= y0[1]), color="red", size=1) +
    geom_point(aes(x = x0[1] , y= y1[1]), color="red", size=1) +
    ggtitle(unique(data$common)) +
    theme(legend.title=element_blank()) + 
    annotate("text", label = paste0("Kolmogorov-Smirnov Test results", "\n", "p-value = ",sprintf("%0.2f",test_result$p.value), "\n", "D = ",sprintf("%0.2f" , test_result$statistic)), x=Inf, y = -Inf, hjust = 1.0, vjust = -0.4)
 
 hist_plot <- ggplot(data, aes(x=Length_cm, fill= Exp_or_Cont)) +
   geom_histogram() +    
   theme_bw() +
   guides(fill = FALSE) +
   theme(axis.text = element_text(size = 14), axis.title = element_blank(), strip.text.x = element_text(size = 14)) +
   facet_wrap(~Exp_or_Cont, nrow = 2)
 
 ifelse(test_result$p.value>0.05,print("Not significant"), print(paste0(unique(data$common), " p-value = ", test_result$p.value)))
 
 summary_stats <- data %>%
   group_by(Exp_or_Cont) %>%
   summarise(mean = mean(Length_cm), stdDev = sd(Length_cm), n = n())
 
 if (test_result$p.value<0.05) {
   #print(ggplot_build(ecdf_plot)$data)
   print(summary_stats)
   print(data %>% 
     group_by(Length_cm, Exp_or_Cont) %>% 
     summarise(no_fish = n()) %>% 
     ungroup() %>% 
       spread(key = Exp_or_Cont, value = no_fish, fill = 0) %>%
     mutate(prop_fish_Control = 100*Control/sum(Control), cum_sum_Control = 100*cumsum(Control)/sum(Control), prop_fish_Exp = 100*Experimental/sum(Experimental), cum_sum_Exp = 100*cumsum(Experimental)/sum(Experimental)))
   
   ggplot(data, aes(x= Length_cm, fill = Exp_or_Cont)) +
     geom_histogram(binwidth = 1) +
     geom_vline(data = summary_stats, aes(xintercept = mean), colour = "black", linetype = "dashed", show.legend = FALSE) +
     theme_bw() +
     labs(x = "Length of fish (cm)", y="Number of fish") +
     guides(fill = FALSE) +
     theme(axis.text = element_text(size = 16), axis.title = element_text(size = 16), strip.text.x = element_text(size = 16)) +
     facet_wrap(~Exp_or_Cont, nrow = 2)

   ggsave(paste0("../outputs/figures/species_ecdf_hist/p_less_than_0.05_histograms/", unique(data$common), ".png"), width = 16, height = 8)
 }
 
return(plot_grid(ecdf_plot, hist_plot, nrow = 2))
  
}
