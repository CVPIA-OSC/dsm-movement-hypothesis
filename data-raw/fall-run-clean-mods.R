# clean fall run data with modified parameters

# Get new years and corresponding params
library(waterYearType)
library(tidyverse)

target_sac <- water_year_indices %>%
  filter(location == 'Sacramento Valley', between(WY, 1997, 2020)) %>%
  select(WY, Index)

options_sac <- water_year_indices %>%
  filter(location == 'Sacramento Valley', between(WY, 1980, 2000)) %>%
  pull(Index)

names(options_sac) <- 1980:2000

target_sj <- water_year_indices %>%
  filter(location == 'San Joaquin Valley', between(WY, 1997, 2020)) %>%
  select(WY, Index)

options_sj <- water_year_indices %>%
  filter(location == 'San Joaquin Valley', between(WY, 1980, 2000)) %>%
  pull(Index)

names(options_sj) <- 1980:2000

synth_year_mapping <- map_chr(seq(target_sj$Index),
                              ~names(
                                which.min(
                                  abs(target_sj$Index[.] - options_sj) +
                                    abs(target_sac$Index[.] - options_sac))
                              )
)

proxy_years <- data.frame(year = 1997:2020, proxy_year = synth_year_mapping,
                          sac_actual = target_sac$Index,
                          sac_synth = options_sac[synth_year_mapping],
                          sj_actual = target_sj$Index,
                          sj_synth = options_sj[synth_year_mapping])


proxy_spawn_index <- setNames(proxy_years$proxy_year, 1997:2020)
proxy_year_index <- proxy_spawn_index[-1]


params_mod <- DSMCalibrationData::set_synth_years(fallRunDSM::params,
                                                  spawn_years = proxy_spawn_index,
                                                  years = proxy_year_index)















