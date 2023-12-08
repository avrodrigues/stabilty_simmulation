
# load package ------------------------------------------------------------

library(tidyverse)
library(codyn)


source("function/ts_max_length.R")

# load data ---------------------------------------------------------------
birds_raw <- readRDS("data/raw/birds_data_tax_cleaned.rds")


# prepare data ------------------------------------------------------------


sel_sites_test <- ts_length_df |>
  filter(length > 20)


ts_data_20plus <- map(1:nrow(sel_sites_test), \(i){
  site_row <- sel_sites_test[i, ]

  birds_raw |>
    filter(
      SiteID == site_row$site,
      between(Year, site_row$begin, site_row$end)
    )

}) |>
  list_rbind()



tot_abund_df <- ts_data_20plus |>
  group_by(Year, SiteID) |>
  summarise(tot_abund = sum(Abundance))




ggplot() +
  geom_path(
    data = tot_abund_df |> filter(SiteID %in% c("MNP.5651", "MNP.5685", "MNP.5550")),
    aes(x = Year, y = tot_abund, color = SiteID),
    show.legend = T,
    size = 1.2
  ) +

  theme(legend.position = "bottom")

sp_abund <- ts_data_20plus |>
  group_by(Year, SiteID, Species) |>
  summarise(abund = sum(Abundance))


site_char <- "MNP.5578"

ggplot() +
  geom_path(
    data = tot_abund_df |> filter(SiteID %in% site_char),
    aes(x = Year, y = tot_abund),
    show.legend = F,
    size = 1.2
  )+
  geom_path(
    data = sp_abund |>
      filter(SiteID %in% site_char),
    aes(x = Year, y = abund, color = Species),
    show.legend = T
  )+


  theme(legend.position = "bottom")

stab <- community_stability(
  ts_data_20plus,
  time.var = "Year",
  abundance.var = "Abundance",
  replicate.var = "SiteID"
)

sync_l <- synchrony(
  ts_data_20plus,
  time.var = "Year",
  species.var = "Species",
  abundance.var = "Abundance",
  metric = "Loreau",
  replicate.var = "SiteID"
)

sync_g <- synchrony(
  ts_data_20plus,
  time.var = "Year",
  species.var = "Species",
  abundance.var = "Abundance",
  metric = "Gross",
  replicate.var = "SiteID"
)

