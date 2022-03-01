library(shiny)
library(tidyverse)
library(plotly)

source("shiny-helper-functions.R")
# data ----------------
late_fall_run_watersheds <- c("Upper Sacramento River", "Clear Creek", "Battle Creek")
late_fall_run_hypothesis <- read_rds("data/late-fall-run-juveniles-at-chipps-clean-new-metric.rds") %>%
  filter(watershed %in% late_fall_run_watersheds)

late_fall_run_valley_wide <- read_rds("data/valley-wide-LFR-juveniles-at-chipps.rds")
# options(shiny.reactlog=TRUE)

sac_valley_year_types <- waterYearType::water_year_indices %>%
  filter(location == "Sacramento Valley") %>%
  select(WY, Yr_type)

chipps_trawls_proportions <- read_rds("data/chipps-trawls-proportions.rds")
