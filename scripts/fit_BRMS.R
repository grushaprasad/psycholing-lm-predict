library(argparse)
library(brms)
library(tidyverse)
library(tidybayes)
library(tidyr)

get_args <- function(){
  parser <- ArgumentParser()
  parser$add_argument("--data", nargs=1, help="Path to data")
  parser$add_argument("--dv", nargs=1, help="Dependent variable")
  parser$add_argument("--model_fpath", nargs=1, help="Directory to store BRMS model")
  parser$add_argument("--posterior_fpath", nargs=1, help="Directory to store predicted RTs")
  
  args <- parser$parse_args()
  return(args)
}


get_posterior <- function(fit, rand_name){
  
  samples <- as_draws_df(fit)  %>%
    gather_variables() 
  
  fixef_samples <- samples %>%
    filter(str_detect(.variable,'b_')) %>%
    spread(.variable, .value)
  
  rand_regex <- paste('r_', rand_name, '\\[', sep='')
  
  post_samples <- samples %>%
    filter(str_detect(.variable, rand_regex))  %>%
    mutate(.variable = str_replace(.variable, rand_regex, ''))  %>%
    separate(.variable, c('item', 'coef'), sep= ',') %>%
    mutate(coef = str_replace(coef, '\\]', ''),
           coef = paste('r_', coef, sep='')) %>%
    spread(coef, .value) %>%
    merge(fixef_samples, by = c('.chain', '.draw', '.iteration'))
  
  return(post_samples)
}


fit_model <- function(args, prior){
  fixef = "condition"
  ranef = "(1 + condition | partid) + (1 + condition | item)"
  model_formula = paste0(args$dv, "~", fixef, "+", ranef)
  
  dat = read.csv(args$data)
  print(unique(dat$condition))
  
  fit = brm(as.formula(model_formula),
            data = dat,
            prior = prior,
            cores = 4,
            iter = 12000,
            seed = 7,
            warmup = 6000,
            control = list(adapt_delta = 0.8))
  print('Fit the model!')
  
  return(fit)
}


args = get_args()

prior = c(prior("normal(300,1000)", class = "Intercept"),
         prior("normal(0,150)", class = "b"),  
         prior("normal(0,200)", class = "sd"),    
         prior("normal(0,500)", class = "sigma"))

fit = fit_model(args, prior)
saveRDS(fit, args$model_fpath)

posterior = get_posterior(fit, "item")
saveRDS(posterior, args$posterior_fpath)




