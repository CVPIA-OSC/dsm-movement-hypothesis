library(tidyverse)
library(lubridate)

spring_run_hypothesis_raw <- read_rds("data/spring-run-juveniles-at-chipps.rds")

spring_run_hypothesis_raw <- spring_run_hypothesis_raw %>%
  pivot_longer(
    cols = s:vl,
    names_to = "size_class",
    values_to = "count"
  )

sac_valley_watersheds <- fallRunDSM::watershed_labels[1:24]
san_joaquin_watersheds <- fallRunDSM::watershed_labels[25:31]

sac_valley_spring_run <- spring_run_hypothesis_raw %>%
  filter(watershed %in% sac_valley_watersheds)
san_joaquin_spring_run <-spring_run_hypothesis_raw %>%
  filter(watershed %in% san_joaquin_watersheds)

sac_valley_year_types <- waterYearType::water_year_indices %>%
  filter(location == "Sacramento Valley") %>%
  select(WY, Yr_type)
san_joaquin_year_types <- waterYearType::water_year_indices %>%
  filter(location == "San Joaquin Valley") %>%
  select(WY, Yr_type)

size_class_lookup <- c("s"= "small", "m" = "medium", "l" = "large", "vl" = "very large")
hypothesis_lookup <- c("one" = "Hypothesis 1", "two" = "Hypothesis 2", "three" = "Hypothesis 3")

sac_valley_spring_run <- sac_valley_spring_run %>%
  mutate(size_class_label = factor(size_class_lookup[size_class], levels = c("small", "medium", "large", "very large")),
         month_label = factor(month.abb[month], levels = c("Nov","Dec","Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct")),
         hypothesis_label = factor(hypothesis_lookup[hypothesis], levels = hypothesis_lookup),
         cal_year = year + 1979) %>%
  left_join(sac_valley_year_types, by = c("cal_year" = "WY"))

san_joaquin_spring_run <- san_joaquin_spring_run %>%
  mutate(size_class_label = factor(size_class_lookup[size_class], levels = c("small", "medium", "large", "very large")),
         month_label = factor(month.abb[month], levels = c("Nov","Dec","Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct")),
         hypothesis_label = factor(hypothesis_lookup[hypothesis], levels = hypothesis_lookup),
         cal_year = year + 1979) %>%
  left_join(san_joaquin_year_types, by = c("cal_year" = "WY"))

spring_run_hypothesis <- bind_rows(sac_valley_spring_run, san_joaquin_spring_run)

spring_run_outmigration_prop<- spring_run_hypothesis %>%
  group_by(watershed, year, hypothesis) %>%
  mutate(total_fish = sum(count)) %>%
  ungroup() %>%
  group_by(watershed, year, month, hypothesis) %>%
  mutate(prop_fish = count / total_fish,
         prop_fish = ifelse(is.nan(prop_fish), 0, prop_fish)) %>%
  ungroup()

write_rds(spring_run_outmigration_prop, "data/spring-run-juveniles-at-chipps-proportion-outmigration.rds")

# Valley-wide proportions -------------------------------------------------
# get valley total by summing across all the watersheds, preserve size_class
sr_valley_totals <- spring_run_hypothesis %>%
  mutate(region = case_when(
    watershed %in% sac_valley_watersheds ~ "Sacramento Valley",
    TRUE ~ "San Joaquin Valley"
  )) %>%
  group_by(region, cal_year, month_label, size_class_label , hypothesis_label) %>%
  summarise(valley_count = sum(count)) %>%
  ungroup()

# plot of valley-wide totals by hypothesis and size_class
sr_valley_wide_outmigration_props <- sr_valley_totals %>%
  group_by(region, cal_year, hypothesis_label) %>%
  mutate(annual_total = sum(valley_count)) %>% # get annual total per hypothesis
  ungroup() %>%
  group_by(region, cal_year, month_label, hypothesis_label) %>% # get annual outmigration propotions within a month
  mutate(prop_fish = valley_count / annual_total,
         prop_fish = ifelse(is.nan(prop_fish), 0, prop_fish)) %>%
  ungroup()

# confirm all looks good
sr_valley_wide_outmigration_props %>%
  filter(cal_year == 1999, region== "Sacramento Valley") %>%
  ggplot(aes(month_label, prop_fish, fill = size_class_label)) +
  geom_col() +
  facet_wrap(vars(hypothesis_label))

write_rds(sr_valley_wide_outmigration_props, "data/valley-wide-SR-juveniles-at-chipps.rds")
