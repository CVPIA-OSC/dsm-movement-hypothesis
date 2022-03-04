library(shiny)
library(tidyverse)
library(plotly)

source("shiny-helper-functions.R")
# data ----------------
late_fall_run_watersheds <- c("Upper Sacramento River", "Clear Creek", "Battle Creek")
fall_run_watersheds <- c(fallRunDSM::watershed_labels)
spring_run_watersheds <- c(springRunDSM::watershed_labels)
winter_run_watersheds <- c(winterRunDSM::watershed_labels)

late_fall_run_hypothesis <- read_rds("data/late-fall-run-juveniles-at-chipps-clean-new-metric.rds") %>%
  filter(watershed %in% late_fall_run_watersheds)

fall_run_hypothesis <- read_rds("data/fall-run-juveniles-at-chipps-proportion-outmigration.rds") %>%
  filter(watershed %in% fall_run_watersheds)

spring_run_hypothesis <- read_rds("data/spring-run-juveniles-at-chipps-proportion-outmigration.rds") %>%
  filter(watershed %in% spring_run_watersheds)

winter_run_hypothesis <- read_rds("data/winter-run-juveniles-at-chipps-proportion-outmigration.rds") %>%
  filter(watershed %in% winter_run_watersheds)

late_fall_run_valley_wide <- read_rds("data/valley-wide-LFR-juveniles-at-chipps.rds")
fall_run_valley_wide <- read_rds("data/valley-wide-FR-juveniles-at-chipps.rds")
spring_run_valley_wide <- read_rds("data/valley-wide-SR-juveniles-at-chipps.rds")
winter_run_valley_wide <- read_rds("data/valley-wide-WR-juveniles-at-chipps.rds")
# options(shiny.reactlog=TRUE)

sac_valley_year_types <- waterYearType::water_year_indices %>%
  filter(location == "Sacramento Valley") %>%
  select(WY, Yr_type)

chipps_trawls_proportions <- read_rds("data/chipps-trawls-proportions.rds")
