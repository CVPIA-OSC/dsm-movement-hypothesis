# clean fall run data with modified parameters

# Get new years and corresponding params
library(waterYearType)
library(tidyverse)
library(fallRunDSM)


target_sac <- water_year_indices %>%
  filter(location == 'Sacramento Valley', between(WY, 1997, 2018)) %>%
  select(WY, Index)

options_sac <- water_year_indices %>%
  filter(location == 'Sacramento Valley', between(WY, 1980, 2000)) %>%
  pull(Index)

names(options_sac) <- 1980:2000

target_sj <- water_year_indices %>%
  filter(location == 'San Joaquin Valley', between(WY, 1997, 2018)) %>%
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

proxy_years <- data.frame(year = 1997:2018, proxy_year = synth_year_mapping,
                          sac_actual = target_sac$Index,
                          sac_synth = options_sac[synth_year_mapping],
                          sj_actual = target_sj$Index,
                          sj_synth = options_sj[synth_year_mapping])


proxy_spawn_index <- setNames(proxy_years$proxy_year, 1997:2018)
proxy_year_index <- proxy_spawn_index[-1]


params_mod <- DSMCalibrationData::set_synth_years(fallRunDSM::params,
                                                  spawn_years = proxy_spawn_index,
                                                  years = proxy_year_index)

seeds_mod <- fall_run_model(mode = "seed", ..params = params_mod)
sim_mod <- fall_run_model(mode = "simulate", seeds = seeds_mod,
                          ..params = params_mod)

# Clean -----------------------------------

write_rds(sim_mod$north_delta_fish, "data/fall-run-juveniles-at-chipps-mod.rds")

fall_run_hypothesis_raw <- read_rds("data/fall-run-juveniles-at-chipps.rds")

fall_run_hypothesis_raw <- fall_run_hypothesis_raw %>%
  pivot_longer(
    cols = s:vl,
    names_to = "size_class",
    values_to = "count"
  )

sac_valley_watersheds <- fallRunDSM::watershed_labels[1:24]
san_joaquin_watersheds <- fallRunDSM::watershed_labels[25:31]

sac_valley_fall_run <- fall_run_hypothesis_raw %>%
  filter(watershed %in% sac_valley_watersheds)
san_joaquin_fall_run <-fall_run_hypothesis_raw %>%
  filter(watershed %in% san_joaquin_watersheds)

sac_valley_year_types <- waterYearType::water_year_indices %>%
  filter(location == "Sacramento Valley") %>%
  select(WY, Yr_type)
san_joaquin_year_types <- waterYearType::water_year_indices %>%
  filter(location == "San Joaquin Valley") %>%
  select(WY, Yr_type)

size_class_lookup <- c("s"= "small", "m" = "medium", "l" = "large", "vl" = "very large")
hypothesis_lookup <- c(
  "zero" = "Base fill + No Additional Movement",
  "one" = "Base fill + Snow Globe",
  "two" = "Base fill + Genetics",
  "three" = "Base fill + Temperature",
  "four" = "Base fill + Time",
  "five" = "Density fill + No Additional Movement",
  "six" = "Density fill + Snow Globe",
  "seven" = "Density fill + Genetics",
  "eight" = "Density fill + Temperature",
  "nine" = "Density fill + Time"
)

sac_valley_fall_run <- sac_valley_fall_run %>%
  mutate(size_class_label = factor(size_class_lookup[size_class], levels = c("small", "medium", "large", "very large")),
         month_label = factor(month.abb[month], levels = month.abb),
         hypothesis_label = factor(hypothesis_lookup[hypothesis], levels = hypothesis_lookup),
         cal_year = year + 1979) %>%
  left_join(sac_valley_year_types, by = c("cal_year" = "WY"))

san_joaquin_fall_run <- san_joaquin_fall_run %>%
  mutate(size_class_label = factor(size_class_lookup[size_class], levels = c("small", "medium", "large", "very large")),
         month_label = factor(month.abb[month], levels = month.abb),
         hypothesis_label = factor(hypothesis_lookup[hypothesis], levels = hypothesis_lookup),
         cal_year = year + 1979) %>%
  left_join(san_joaquin_year_types, by = c("cal_year" = "WY"))

fall_run_hypothesis <- bind_rows(sac_valley_fall_run, san_joaquin_fall_run)

fall_run_outmigration_prop<- fall_run_hypothesis %>%
  group_by(watershed, year, hypothesis) %>%
  mutate(total_fish = sum(count)) %>%
  ungroup() %>%
  group_by(watershed, year, month, hypothesis) %>%
  mutate(prop_fish = count / total_fish,
         prop_fish = ifelse(is.nan(prop_fish), 0, prop_fish)) %>%
  ungroup()

# Confirm we get 1
fall_run_outmigration_prop %>%
  filter(cal_year == 1995, watershed == "Upper Sacramento River",
         hypothesis == "zero") %>% pull(prop_fish) %>% sum()

write_rds(fall_run_outmigration_prop, "data/fall-run-juveniles-at-chipps-proportion-outmigration__1.rds")

# Valley-wide proportions -------------------------------------------------
# get valley total by summing across all the watersheds, preserve size_class
fr_valley_totals <- fall_run_hypothesis %>%
  mutate(region = case_when(
    watershed %in% sac_valley_watersheds ~ "Sacramento Valley",
    TRUE ~ "San Joaquin Valley"
  )) %>%
  group_by(region, cal_year, month_label, size_class_label , hypothesis_label) %>%
  summarise(valley_count = sum(count)) %>%
  ungroup()

# plot of valley-wide totals by hypothesis and size_class
fr_valley_wide_outmigration_props <- fr_valley_totals %>%
  group_by(region, cal_year, hypothesis_label) %>%
  mutate(annual_total = sum(valley_count)) %>% # get annual total per hypothesis
  ungroup() %>%
  group_by(region, cal_year, month_label, hypothesis_label) %>% # get annual outmigration propotions within a month
  mutate(prop_fish = valley_count / annual_total,
         prop_fish = ifelse(is.nan(prop_fish), 0, prop_fish)) %>%
  ungroup()

# confirm all looks good
fr_valley_wide_outmigration_props %>%
  filter(cal_year == 1999, region== "Sacramento Valley") %>%
  ggplot(aes(hypothesis_label, prop_fish, fill = size_class_label)) +
  geom_col() +
  facet_wrap(vars(month_label), nrow = 4)

write_rds(fr_valley_wide_outmigration_props, "data/valley-wide-FR-juveniles-at-chipps__1.rds")
