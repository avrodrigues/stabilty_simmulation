
<!-- README.md is generated from README.Rmd. Please edit that file -->

# stabilty_simmulation

<!-- badges: start -->
<!-- badges: end -->

In this repository I have simulations of fluctuation in community
abundance to understand the impacts of monitoring schemes on the
accuracy of stability metrics.

The simulation was done by:

- Simulating sine wave fluctuation of abundance at species level in a
  community for a fixed time.
- Setting the mean abundance of each species in the community following
  a log-normal distribution
- Sine wave was simulated using two parameter frequency and amplitude.
- Different combination of frequency and amplitude was defined at the
  community level trying to emulate differences in pace-of-life of
  different taxonomic groups, with a positive relationship between
  frequency and amplitude. Fast taxon can fluctuate in high frequency
  and also can have high amplitude since. These relates to the short
  lifespan and high reproductive output.
- The variability in frequency and amplitude among species in a
  community was also simulated.

The simulation was run using 360 combination of parameter with 100
repetition. Then in the `output/comm_sim` folder you can find 36000
files, one for each simulation run. In the `output/stat_df.rds` file you
find the community stability and asynchrony statistic for each
simulation run.

## Shiny app

You can check this shiny app to play with different parameters and see
the results for one community:
<https://avrodrigues.shinyapps.io/stab_sync/>

## Repo structure

    #> .
    #> ├── data
    #> ├── function
    #> │   └── helper_functions.R
    #> ├── output
    #> │   ├── comm_sim
    #> │   ├── comm_stats
    #> │   └── stats_df.rds
    #> ├── README.md
    #> ├── README.Rmd
    #> ├── renv
    #> │   ├── activate.R
    #> │   ├── library
    #> │   ├── settings.json
    #> │   └── staging
    #> ├── renv.lock
    #> ├── scripts
    #> │   ├── 01_simmulation_of_communities.R
    #> │   ├── 02_computing_metrics_reference.R
    #> │   ├── 02_ex_monitoring_discussion.R
    #> │   └── 03_stats_relationships.R
    #> └── stabilty_simmulation.Rproj
