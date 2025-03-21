library(tidyverse)
library(lme4)
library(argparse)
library(readr)
## Bind surps
## Need a way of getting frequencies.. 
## Get filler model formula
## Maybe use argparse so this can run for any one model
## Generate predicted RTs -- 
##  or should filler model and predicted RTs be different files? 

## PredictingRT should take filler model as a parameter.. Currently it doesn't

bind_surps <- function(emp_dat, surp_dat){
  merged = merge(x = surp_dat, y = emp_dat,
                 by = c("sentence", "sentid", "word_pos", "word"), all.x=TRUE)
  
  print("succesfully merged")
  ## Z-score. Note, need to modify if you want to z-score across datasets
  merged = merged %>%
    mutate(surprisal_s = scale(surp)[,1], 
           length_s = scale(length)[,1],
           logfreq_s = scale(logfreq)[,1]
           )
  
  
  with_lags <- merged %>% group_by_at(vars(sentid, partid)) %>%
    mutate(RT_p1 = lag(RT), 
           RT_p2 = lag(RT_p1), 
           RT_p3 = lag(RT_p2),
           length_p1_s = lag(length_s), 
           length_p2_s = lag(length_p1_s),
           length_p3_s = lag(length_p2_s),
           logfreq_p1_s = lag(logfreq_s), 
           logfreq_p2_s = lag(logfreq_p1_s),
           logfreq_p3_s = lag(logfreq_p2_s),
           surprisal_p1_s = lag(surprisal_s),
           surprisal_p2_s = lag(surprisal_p1_s),
           surprisal_p3_s = lag(surprisal_p2_s)
    )
  
  with_lags$sent_length <- lapply(str_split(with_lags$sentence, " "), length)
  
  # Exclude NAs and also last words of sentences. Separate printing.
  ## Things can be NA because lag will result in NA for initial words..
  dropped <- subset(with_lags, !is.na(surprisal_s) &
                      !is.na(surprisal_p1_s) &
                      !is.na(surprisal_p2_s) &
                      !is.na(surprisal_p3_s) &
                      !is.na(logfreq_s) & !is.na(logfreq_p1_s) &
                      !is.na(logfreq_p2_s) & !is.na(logfreq_p3_s) &
                      (with_lags$sent_length != with_lags$word_pos))

  print(paste0("dropped: ", nrow(with_lags) - nrow(dropped)))
  return(dropped)
}

get_args <- function(){
  parser <- ArgumentParser()
  parser$add_argument("--filler_surp", nargs=1, help="Path to surprisal data")
  parser$add_argument("--critical_surp", nargs=1, help="Path to surprisal data")
  parser$add_argument("--emp_dat", nargs=1, help="Path to empirical data")
  parser$add_argument("--model_fdir", nargs=1, help="Directory to store filler model")
  parser$add_argument("--pred_fdir", nargs=1, help="Directory to store predicted RTs")
  
  args <- parser$parse_args()
  return(args)
}

fit_fillermodel <- function(dat, args, modelname){
  
  fixef = "surprisal_s + surprisal_p1_s + surprisal_p2_s + surprisal_p3_s +
         scale(word_pos) + logfreq_s*length_s + logfreq_p1_s*length_p1_s + 
         logfreq_p2_s*length_p2_s + logfreq_p3_s*length_p3_s"
  
  ranef = "(1 + surprisal_s + surprisal_p1_s + surprisal_p2_s + surprisal_p3_s || partid) +
         (1 | item)"
  
  model_formula = paste0("RT~", fixef, "+", ranef)
  
  fit = lmer(as.formula(model_formula),
             data=dat,
             control=lmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5))
  )
  
  fpath = paste0(args$model_fdir, "/", modelname, "_fillermodel.rds")

  print(paste("Saving", fpath))
  saveRDS(fit, fpath)
  
  return(fit)
  
}

generate_predictions <- function(dat, args, filler_model){
  dat$predicted <- predict(filler_model, newdata=dat, allow.new.levels = TRUE)
  
}

args = get_args()

emp_dat = read.csv(args$emp_dat, sep = '\t') 

# emp_dat = read.csv("./analysis_tutorial/data/empirical/toy_data.tsv", sep = '\t')

filler_surp_dat = read.csv(args$filler_surp, sep = '\t')

critical_surp_dat = read.csv(args$critical_surp, sep = '\t')
# surp_dat = read.csv('./analysis_tutorial/data/surprisal/filler_byword.tsv', sep = '\t')
# critical_surp_dat = read.csv('./analysis_tutorial/data/surprisal/critical_byword.tsv', sep = '\t')


for(m in unique(filler_surp_dat$model)){
  print(paste("Processing", m))
  modelname = ifelse(m=='distilbert/distilgpt2', 'distilgpt2', m)
  
  print("Fitting filler model")
  curr_filler_surp = subset(filler_surp_dat, model == m)
  merged_filler = bind_surps(subset(emp_dat, condition=='filler'),
                             curr_filler_surp)
  curr_fit = fit_fillermodel(merged_filler, args, modelname)
  
  print("Generating predictions")
  curr_critical_surp = subset(critical_surp_dat, model == m)
  
  merged_critical = bind_surps(subset(emp_dat, condition!='filler'),
                               curr_critical_surp)
  
  merged_critical$predicted = predict(curr_fit,
                                      newdata=merged_critical,
                                      allow.new.levels = TRUE)
  
  merged_critical = merged_critical %>%
    select(sentid, item, sentence, condition, word_pos, word, partid, RT, predicted)
  fname = paste0(args$pred_fdir, "/", modelname, "_predictions.csv")
  write_csv(merged_critical, fname)
}





