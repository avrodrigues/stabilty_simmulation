
library(ggdist)
library(ggplot2)


stats_df <- readRDS("output/stats_df.rds")


ggplot(stats_df) +
  geom_histogram(aes(x = stab))

ggplot(stats_df, aes(x = sync_g, y = stab)) +
  geom_point(color = "darkred", alpha = .1)
ggplot(stats_df, aes(x = sync_l, y = stab)) +
  geom_point(color = "darkred", alpha = .1)
ggplot(stats_df, aes(x = sync_g, y = sync_l)) +
  geom_point(color = "darkred", alpha = .1)




ggplot(stats_df, aes(x = as.factor(freq_mean), y = stab)) +
  stat_halfeye() 

ggplot(stats_df, aes(x = as.factor(n_species), y = stab)) +
  stat_halfeye() 

ggplot(stats_df, aes(x = as.factor(freq_sd), y = sync_g)) +
  stat_halfeye() 

ggplot(stats_df, aes(x = as.factor(amp_cv), y = sync_g)) +
  stat_halfeye() 


lm(sync_g ~ as.factor(n_species) + as.factor(amp_cv):as.factor(freq_sd), data = stats_df) |> 
  summary()

lm(stab ~ as.factor(n_species) + as.factor(freq_mean) + as.factor(freq_sd) : as.factor(amp_mean), data = stats_df) |> 
  summary()
