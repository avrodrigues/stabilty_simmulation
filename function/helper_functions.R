
# Functions - Shiny app ---------------------------------------------------

sp_abund_dyn <- function(t, amp, freq, mean_abund){
  
  y <-  (amp * sin((t)*freq)) |> 
    round(0)
  
  y <- y + mean_abund
  
  y <- ifelse(y < 0, 0, y)
  data.frame(x = t/pi, y) 
}


#' Create temporal abundance fluctuation for a community
#' 
#' Fluctuations are based in a sine wave function
#'
#' @param t numeric vector with the times of the time series
#' @param amp_vec integer vector with the amplitude for each species
#' @param freq_vec integer vector with the frequency for each species
#' @param abund_vec a vector with the temporal mean abundance for each species
#' 
#' @details
#' The size of the vector in \code{amp_vec, freq_vec, abund_vec} represent the 
#' number of species in the community. In addition, the position in the vector 
#' should be the same a species in each vector
#' 
#'
#' @return data.frame
#' 
#' @examples
#' 
#' comm_abund_dyn(
#'   t = seq(1, 5*pi, by = 0.1),  
#'   amp_vec = c(5,5),  
#'   freq_vec = c(1,2),  
#'   abund_vec = c(50,50)
#'   )
#'   
#' @export
#'   
#' @importFrom purrr map
#' @importFrom purrr list_rbind

comm_abund_dyn <- function(t, amp_vec, freq_vec, abund_vec){
  
  purrr::map(seq_along(abund_vec), function(i){
    sp_abund_dyn(
      t,
      amp_vec[i],
      freq_vec[i],
      abund_vec[i]
    ) |> 
      mutate(sp = paste0("sp_",i))
  }) |> 
    purrr::list_rbind()
  
}


#' Compute metrics of stability and synchrony of communities
#'
#' @param comm_dyn a data frame with community dynamics. It is created using 
#'   the \code{comm_abund_dyn} function
#'
#' @return a list with three values. 
#' 
#'  stab: the stability metric
#'  sync_l: the Loureau's metric of synchrony
#'  sync_g: the Gross's metric of synchrony
#'  
#' @export
#'
#' @examples
#' 
stat_comm_dyn <- function(comm_dyn){
  stab <- community_stability(
    comm_dyn,
    time.var = "x",
    abundance.var = "y"
  ) |> round(3)
  
  sync_l <- synchrony(
    comm_dyn,
    time.var = "x",
    species.var = "sp",
    abundance.var = "y",
    metric = "Loreau",
  ) |> round(3)
  
  sync_g <- synchrony(
    comm_dyn,
    time.var = "x",
    species.var = "sp",
    abundance.var = "y",
    metric = "Gross",
  ) |> round(3)
  
  list(stab = stab, sync_l = sync_l, sync_g = sync_g)
}

#' Simulate the dynamics for one community
#' 
#' It uses sine wave as a fluctuation curve for each species in a community. 
#' The temporal mean abundance of each species is sampled from a log-normal 
#' species accumulation curve. No interaction among species or with the environment is 
#' included in the simulation. 
#'
#' @param n_species number of species in the simulation
#' @param n_ind number of individuals for the species abundance disribution sampling
#' @param mean_freq mean frequency in sine wave fluctuation among species
#' @param sd_freq standard deviation of frequency in sine wave fluctuation among species
#' @param mean_amp mean amplitude in sine wave fluctuation among species
#' @param cv_amp coefficient of variation of amplitude in sine wave fluctuation among species
#' @param total_years number of years to simulate
#' @param burn_in_years number of years to burn-in
#'
#' @return
#' @export
#'
#' @examples
#' 
#' 
simm_one_comm <- function(
    n_species, 
    n_ind, ## something to account for carrying capacity
    mean_freq, 
    sd_freq,
    mean_amp,
    cv_amp, 
    total_years, 
    burn_in_years
){
  
  # years with 12 sampling per year
  time_points <- seq(0, total_years*pi, 0.262)
  
  # burn-in
  time_points <- time_points[-c(1:burn_in_years*12)]
  
  abund_vec <- numeric()
  
  ## make the number of species equal to the resulted from the sad_sim function
  while(length(abund_vec) != n_species){
    abund_vec <- sim_sad(n_species, n_ind, sad_type = "lnorm")
  }
  
  sd_amp = cv_amp * mean_amp
  
  amp_vec <- rnorm(length(abund_vec), mean_amp, sd_amp)
  freq_vec <- rnorm(length(abund_vec), mean_freq, sd_freq)
  
  
  comm_abund_dyn(
    time_points,
    amp_vec,
    freq_vec,
    abund_vec
  )
  
}
