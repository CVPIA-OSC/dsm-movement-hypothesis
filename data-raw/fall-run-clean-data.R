library(tidyverse)
library(lubridate)

s <- fallRunDSM::fall_run_model()
ss <- fallRunDSM::fall_run_model(seeds = s, mode = "simulate")
write_rds(ss$north_delta_fish, "data/fall-run-juveniles-at-chipps.rds")

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


# 35mm and .74mm mods ---------------------------------------

params_growth_mod <- fallRunDSM::params
params_growth_mod$growth_rates <- fallRunDSM::growth(daily_growth_rate = .35)
params_growth_mod$growth_rates_floodplain <-
  fallRunDSM::growth_floodplain(daily_rates = c(.35, .74))


# compare the two
fallRunDSM::params$growth_rates |>
  as_tibble() |>
  mutate(transition_from = c("s", "m", "l", "vl")) |>
  pivot_longer(names_to = "transition_to", values_to = "probability", s:vl) |>
  mutate(
    transition_from = factor(transition_from, levels = c("s", "m", "l", "vl")),
    transition_to = factor(transition_to, levels = c("s", "m", "l", "vl"))
  ) |>
  ggplot() +
  geom_raster(aes(x = transition_from, y = transition_to, fill = probability),
              interpolate = F) +
  scale_fill_gradient(low = "#253df5", high = "#ff3636") +
  labs(x = "Transition from", y = "Transition to",
       fill = "Prob.", title = "Base Growth: Ichannel: .5mm/day -- Floodplain: 1.06mm/day") +
  theme_bw()

ggsave("plots/transition-matrix-base-growth.png")



params_growth_mod$growth_rates |>
  as_tibble() |>
  mutate(transition_from = c("s", "m", "l", "vl")) |>
  pivot_longer(names_to = "transition_to", values_to = "probability", s:vl) |>
  mutate(
    transition_from = factor(transition_from, levels = c("s", "m", "l", "vl")),
    transition_to = factor(transition_to, levels = c("s", "m", "l", "vl"))
  ) |>
  ggplot() +
  geom_raster(aes(x = transition_from, y = transition_to, fill = probability),
              interpolate = F) +
  scale_fill_gradient(low = "#253df5", high = "#ff3636") +
  labs(x = "Transition from", y = "Transition to",
       fill = "Prob.", title = "Modified Growth: Ichannel: .35mm/day -- Floodplain: .74mm/day") +
  theme_bw()

ggsave("plots/transition-matrix-35mm-growth.png")




seeds_growth_mod <- fallRunDSM::fall_run_model(..params = params_growth_mod)
sim_growth_mod <- fallRunDSM::fall_run_model(seeds = seeds_growth_mod,
                                             mode = "simulate",
                                             ..params = params_growth_mod)

write_rds(sim_growth_mod$north_delta_fish, "data/fall-run-juveniles-at-chipps-growth-mod.rds")

fall_run_hypothesis_raw <- read_rds("data/fall-run-juveniles-at-chipps-growth-mod.rds")

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
  filter(cal_year == 1992, watershed == "Upper Sacramento River",
         hypothesis == "zero") %>% pull(prop_fish) %>% sum()

write_rds(fall_run_outmigration_prop, "data/fall-run-juveniles-at-chipps-proportion-outmigration-growth-mod.rds")

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

write_rds(fr_valley_wide_outmigration_props, "data/valley-wide-FR-juveniles-at-chipps-growth-mod.rds")
