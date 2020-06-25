#function to get all models, plot all model coefficient estimatea and get model evaluation summaries and diagnostics. Everything is output to single pdf

all_models <- function(dataset, full_model, response_var, response_var_name_plot, filename) {
  #open pdf file for writing outputs to
  pdf(
    str_glue("../outputs/lmm_models/", filename, ".pdf"),
    width = 12,
    height = 8
  )
  #histogram of response variable
  print(ggplot(data = dataset) +
    geom_histogram(aes(!!ensym(response_var))) +
    labs(x= str_glue(response_var_name_plot, " in trap haul"), title = str_glue("Histogram of ", response_var_name_plot)))
  
  #check data distribution
  print(data_distributions(dataset, response_var))
  
  par(mfrow=c(1,1)) #reset to 1 row 1 column output after data_distribution function which has 2 rows 2 columns
  
  #generate full set of potential models (all combinations of fixed effects) - output the selection table
  dredge_output <- MuMIn::dredge(full_model)
  gplots::textplot(capture.output(dredge_output), show.rownames = FALSE, valign = "top", cex = 0.7)
  #get the fitted models
  all_models <- MuMIn::get.models(dredge_output, subset = TRUE)

  #get the formulas and name each model in the list using the formula, otherwise models are just numbered in plots and tables
  model_names <- all_models %>%
    map(formula) %>%
    map_dfr(as.character) %>%
    pivot_longer(everything(), names_to = "model_no", values_to = "formula") %>%
    #extract only the fixed coefficients from the model formula
    mutate(
      #extract everything after "~" and before a "("
      formula = str_extract(formula, "(?<=~)[^(]+"),
      #get rid of "+" sign at end of string
      formula = str_sub(formula, end = -3),
      formula = str_trim(formula),
      #if model name is NULL, dwplot doesn't use the model names given to it, so add following code to make sure the model with no fixed coefficients has a name longer than zero length
      formula = case_when(str_length(formula) < 1 ~ "none",
                          TRUE ~ as.character(formula))
    ) %>%
    pull(formula)
  
  names(all_models) <- model_names
  
  #get tidy version of all model evaluation parameters, e.g. AIC, residuals
  gplots::textplot(
    all_models %>%
      map_dfr(broom::glance, .id = "model") %>%
      mutate_if(is.double, sprintf, fmt = "%0.2f"),
    show.rownames = FALSE,
    valign = "top"
  )
  
  #dotwhisker plots of all model fixed coefficients estimates
  print(
    dwplot(all_models, effects = "fixed") +
      geom_vline(xintercept = 0, lty = 2) +
      ggtitle(str_glue(
        "Predicting ",  response_var_name_plot, " in trap haul"
      )) +
      xlab("Coefficient estimate") +
      guides(
        colour = guide_legend(
          "Fixed coefficients in model \n (highest to lowest AIC)",
          reverse = TRUE, 
          ncol = 1
        )
      )
  )
  
  #find optimum model using backward reduction method from the lmerTest package, keeping all random effects
  step_res <- step(full_model, reduce.random = FALSE)
 # par(mfrow= c(2,1))
  #use cex 0.8 to scale output to fit on pdf page
  gplots::textplot(capture.output(step_res), valign = "top", cex = 0.8)
  opt_model <- get_model(step_res)
  gplots::textplot(capture.output(summary(opt_model)), valign = "top", cex = 0.8)
  title("Optimal model found using 'step' function which performs backward elimination of fixed-effect terms")
  
  par(mfrow = c(1,1))
  #model diagnostics plots
  print(sjPlot::plot_model(opt_model, type = "diag"))
  
  #get models with delta AIC < 2
  best_models <- MuMIn::get.models(dredge_output, subset = delta < 2)

  #get the formulas and name each model in the list using the formula, otherwise models are just numbered in plots and tables
  model_names_best <- best_models %>%
    map(formula) %>%
    map_dfr(as.character) %>%
    pivot_longer(everything(), names_to = "model_no", values_to = "formula") %>%
    #extract only the fixed coefficients from the model formula
    mutate(
      #extract everything after "~" and before a "("
      formula = str_extract(formula, "(?<=~)[^(]+"),
      #get rid of "+" sign at end of string
      formula = str_sub(formula, end = -3),
      formula = str_trim(formula),
      #if model name is NULL, dwplot doesn't use the model names given to it, so add following code to make sure the model with no fixed coefficients has a name longer than zero length
      formula = case_when(str_length(formula) < 1 ~ "none",
                          TRUE ~ as.character(formula))
    ) %>%
    pull(formula)
  
  names(best_models) <- model_names_best
  
  #print model summaries for all models with delta AIC <2 so we can compare - if statement used because variable number of model summaries to print and more than 3 doesn't fit on one page
  if(length(best_models) <= 3){
          gplots::textplot(capture.output(map(best_models[1:length(best_models)], summary)), valign = "top", cex = 0.3)
          title("Model summaries for all models with delta AIC < 2")
  }
  else{
    par(mfrow=c(1,2))
    gplots::textplot(capture.output(map(best_models[1:3], summary)), valign = "top", cex = 0.3)
    title("Model summaries for all models with delta AIC < 2")
    gplots::textplot(capture.output(map(best_models[4:length(best_models)], summary)), valign = "top", cex = 0.3)
  }

#output full model results  
  gplots::textplot(capture.output(summary(full_model)), valign = "top", cex = 0.3)
  title("Full model summary")
  dev.off()
}