#function to do normality tests and run t-test
testing_2sample_sig_diff <-function(data, variable, group, paired_Tor_F){

#diagnostics - histogram, qq plot and variance test
print(ggplot(data, aes_string(x = variable)) +
    geom_histogram() +
    facet_wrap(reformulate(group), scale = "free"))
  
print(ggplot(data, aes_string(sample = variable)) +
    geom_qq() +
    facet_wrap(reformulate(group), scale = "free"))

print(var.test(data = data, as.formula(paste0(variable,"~", group))))

#paired t.test
print(t.test(data = data, as.formula(paste0(variable,"~", group)), paired = paired_Tor_F))

}