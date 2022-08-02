library(shiny)
library(tidyverse)
library(plotly)
library(RColorBrewer)

source("shiny-helper-functions.R")
# data ----------------
late_fall_run_watersheds <- c("Upper Sacramento River", "Clear Creek", "Battle Creek")
fall_run_watersheds <- c("Upper Sacramento River", "Antelope Creek", "Battle Creek",
                         "Bear Creek", "Big Chico Creek", "Butte Creek", "Clear Creek",
                         "Cottonwood Creek", "Cow Creek", "Deer Creek", "Elder Creek",
                         "Mill Creek", "Paynes Creek", "Stony Creek", "Thomes Creek",
                         "Upper-mid Sacramento River", "Sutter Bypass", "Bear River",
                         "Feather River", "Yuba River", "Lower-mid Sacramento River",
                         "Yolo Bypass", "American River", "Lower Sacramento River", "Calaveras River",
                         "Cosumnes River", "Mokelumne River", "Merced River", "Stanislaus River",
                         "Tuolumne River", "San Joaquin River")
spring_run_watersheds <- c("Upper Sacramento River", "Antelope Creek", "Battle Creek",
                           "Bear Creek", "Big Chico Creek", "Butte Creek", "Clear Creek",
                           "Cottonwood Creek", "Cow Creek", "Deer Creek", "Elder Creek",
                           "Mill Creek", "Paynes Creek", "Stony Creek", "Thomes Creek",
                           "Upper-mid Sacramento River", "Sutter Bypass", "Bear River",
                           "Feather River", "Yuba River", "Lower-mid Sacramento River",
                           "Yolo Bypass", "American River", "Lower Sacramento River", "Calaveras River",
                           "Cosumnes River", "Mokelumne River", "Merced River", "Stanislaus River",
                           "Tuolumne River", "San Joaquin River")
winter_run_watersheds <- c("Upper Sacramento River", "Antelope Creek", "Battle Creek",
                           "Bear Creek", "Big Chico Creek", "Butte Creek", "Clear Creek",
                           "Cottonwood Creek", "Cow Creek", "Deer Creek", "Elder Creek",
                           "Mill Creek", "Paynes Creek", "Stony Creek", "Thomes Creek",
                           "Upper-mid Sacramento River", "Sutter Bypass", "Bear River",
                           "Feather River", "Yuba River", "Lower-mid Sacramento River",
                           "Yolo Bypass", "American River", "Lower Sacramento River", "Calaveras River",
                           "Cosumnes River", "Mokelumne River", "Merced River", "Stanislaus River",
                           "Tuolumne River", "San Joaquin River")

late_fall_run_hypothesis <- read_rds("data/late-fall-run-juveniles-at-chipps-clean-new-metric.rds") %>%
  filter(watershed %in% late_fall_run_watersheds)

fall_run_hypothesis <- read_rds("data/fall-run-juveniles-at-chipps-proportion-outmigration__1.rds") %>%
  filter(watershed %in% fall_run_watersheds)

spring_run_hypothesis <- read_rds("data/spring-run-juveniles-at-chipps-proportion-outmigration.rds") %>%
  filter(watershed %in% spring_run_watersheds)

winter_run_hypothesis <- read_rds("data/winter-run-juveniles-at-chipps-proportion-outmigration.rds") %>%
  filter(watershed %in% winter_run_watersheds)

late_fall_run_valley_wide <- read_rds("data/valley-wide-LFR-juveniles-at-chipps.rds")
fall_run_valley_wide <- read_rds("data/valley-wide-FR-juveniles-at-chipps__1.rds")
spring_run_valley_wide <- read_rds("data/valley-wide-SR-juveniles-at-chipps.rds")
winter_run_valley_wide <- read_rds("data/valley-wide-WR-juveniles-at-chipps.rds")
# options(shiny.reactlog=TRUE)

sac_valley_year_types <- waterYearType::water_year_indices %>%
  filter(location == "Sacramento Valley") %>%
  select(WY, Yr_type)

yearly_chipps_trawls_proportions <- read_rds("data/yearly-chipps-trawls-proportions.rds") %>%
  mutate(prop_fish = round(prop_fish, 3))

