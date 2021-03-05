#standardised function for frequency plots
hist_plot_func_lmat <- function(data, x, group, bin_widths){
  
  summary_stats <- data %>%
    group_by(!!ensym(group)) %>%
    summarise("Common name" = unique(common), mean = mean(!!ensym(x)), stdDev = sd(!!ensym(x)), n = n())
  print(summary_stats)
  
  ggplot(data, aes(x= !!ensym(x), fill = !!ensym(group))) +
    geom_histogram(binwidth = bin_widths) +
    geom_vline(data = data, aes(xintercept = Lmat), linetype = "solid", show.legend = FALSE ) +
    geom_vline(data = summary_stats, aes(xintercept = mean), colour = "black", linetype = "dashed", show.legend = FALSE) +
    theme_bw() +
    guides(fill = FALSE) +
    labs(title = unique(data$common)) +
    theme(axis.text = element_text(size = 14), axis.title = element_blank(), strip.text.x = element_text(size = 14)) +
    facet_wrap(reformulate(group), nrow = 2)
}