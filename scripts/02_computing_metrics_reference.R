library(codyn)
library(tictoc)
library(furrr)

source("function/helper_functions.R")

l_comm_pattern <- list.files("output/comm_sim/") %>% 
  word(1, 7, "_") %>% 
  unique()


pattern_test <- c(
  "param_8_120_2_32_001_05",
  "param_64_640_2_16_01_2",
  "param_32_320_05_4_1_2",
  "param_128_120_05_8_001_1",
  "param_16_240_05_8_01_05")


plan(multisession, workers = 35)

tic("calculating comm stats")
future_map(l_comm_pattern, function(pttrn){
  
  comm_file <- list.files("output/comm_sim/",  pattern = pttrn, full.names = T)
  
  stat_df <- future_map_dfr(comm_file, function(file){
    
    comm_description <- word(file, 3,sep = "/")
    
    comm <- readRDS(file)
    stats <- stat_comm_dyn(comm) %>% unlist()
    c(comm_description = comm_description, stats)
    
  })
  
  stat_df <- stat_df %>% 
    mutate(across(2:4, as.numeric))
  
  file_name <- paste0("output/comm_stats/stats_", pttrn, ".rds")
  
  saveRDS(stat_df, file_name)
  
})
toc()

plan(sequential)

## 

stats_files <- list.files("output/comm_stats", full.names = T)

stats_df <- map_dfr(stats_files, readRDS)

stats_df <- 
  stats_df %>% 
  mutate(
    n_species = word(comm_description, 2, sep = "_"),
    freq_mean = word(comm_description, 4, sep = "_"),
    amp_mean = word(comm_description, 5, sep = "_"),
    freq_sd = word(comm_description, 6, sep = "_"),
    amp_cv = word(comm_description, 7, sep = "_"),
    .before = "stab"
  ) %>% 
  mutate(
    freq_mean = ifelse(freq_mean == "05", 0.5, freq_mean),
    freq_sd = case_when(
      freq_sd == "001" ~ "0.01",
      freq_sd == "01" ~ "0.1",
      .default = freq_sd
      ),
    amp_cv = ifelse(amp_cv == "05", 0.5, amp_cv),
    across(2:6, as.numeric)
  ) 

saveRDS(stats_df, "output/stats_df.rds")

