library(shiny)
library(tidyverse)
library(plotly)

source("shiny-helper-functions.R")
# data ----------------
late_fall_run_watersheds <- c("Upper Sacramento River", "Clear Creek", "Battle Creek")
late_fall_run_hypothesis <- read_rds("data/late-fall-run-juveniles-at-chipps-clean.rds") %>%
  filter(watershed %in% late_fall_run_watersheds)
# options(shiny.reactlog=TRUE)
