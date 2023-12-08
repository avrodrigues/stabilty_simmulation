
library(tidyverse)
library(mobsim)
library(tictoc)
library(furrr)

source("function/helper_functions.R")

## simmulation the community

## The simulation will use:
param_sad <- data.frame(
  n_species = c(8,16,32,64,128),
  n_ind = c(120, 240, 320, 640, 640)
  )

freq_amp <- data.frame(
  freq = rep(c(0.5, 1, 2), each = 2),
  amp = c(4, 8, 8, 16, 16, 32) 
)

## interaction between freq and amp, freq high - amp low (two values)

sd_freq = c(0, 0.01, 0.1, 1)
cv_amp = c(0.5, 1, 2)

## Each combination will be repeated 100 times

param_df <- expand_grid(param_sad, freq_amp, sd_freq, cv_amp)

set.seed(5604215)
smp_param <- param_df  %>%  
  slice_sample(n = 5, by = n_species)


plan(multisession, workers = 35)

nrep = 100

tic("simmulations_100_rep")

future_map(1:nrow(param_df), function(i){
  
  param <- param_df[i,] %>% unlist()
  param_chr <- paste0(param, collapse = "_") %>% 
    str_replace_all("\\.", "")
  
  ## implement this loop in parallel
  future_map(1:nrep,  function(k){
    comm_dyn <- simm_one_comm(
      n_species = param["n_species"], 
      n_ind = param["n_ind"],
      mean_freq = param["freq"], 
      sd_freq = param["sd_freq"],
      mean_amp = param["amp"],
      cv_amp = param["cv_amp"], 
      total_years = 40, 
      burn_in_years = 10
    )
    
    file_name <- paste0("output/comm_sim/param_", param_chr, "_", "rep_", k, ".rds")
    
    saveRDS(comm_dyn, file_name)
  })
    
}) 
toc()

#simmulations_100_rep: 174963.55 sec elapsed
# 174963.55/60/60 = 48.60099

## Runned at the REC remote desktop
## using 35 parallel computation


## Explicitly close multisession workers by switching plan
plan(sequential)
