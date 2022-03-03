library(tidyverse)
library(lubridate)

winter_run_hypothesis_raw <- read_rds("data/winter-run-juveniles-at-chipps.rds")

winter_run_hypothesis_raw <- winter_run_hypothesis_raw %>%
  pivot_longer(
    cols = s:vl,
    names_to = "size_class",
    values_to = "count"
  )

sac_valley_year_types <- waterYearType::water_year_indices %>%
  filter(location == "Sacramento Valley") %>%
  select(WY, Yr_type)

size_class_lookup <- c("s"= "small", "m" = "medium", "l" = "large", "vl" = "very large")
hypothesis_lookup <- c("one" = "Hypothesis 1", "two" = "Hypothesis 2", "three" = "Hypothesis 3")

winter_run_hypothesis <- winter_run_hypothesis_raw %>%
  mutate(size_class_label = factor(size_class_lookup[size_class], levels = c("small", "medium", "large", "very large")),
         month_label = factor(month.abb[month], levels = month.abb),
         hypothesis_label = factor(hypothesis_lookup[hypothesis], levels = hypothesis_lookup),
         cal_year = year + 1979) %>%
  left_join(sac_valley_year_types, by = c("cal_year" = "WY"))


winter_run_hypothesis %>%
  # select(-(Oct_Mar:Index)) %>%
  group_by(watershed, year, hypothesis) %>%
  mutate(total_fish = sum(count)) %>%
  ungroup() %>%
  group_by(watershed, year, month, hypothesis) %>%
  mutate(prop_fish = count / total_fish,
         prop_fish = ifelse(is.nan(prop_fish), 0, prop_fish)) %>%
  ungroup() %>%
  write_rds("data/winter-run-juveniles-at-chipps-clean-new-metric.rds")

# Valley-wide proportions -------------------------------------------------

# get valley total by summing across all the watersheds, preserve size_class
valley_totals <- winter_run_hypothesis %>%
  # select(-(Oct_Mar:Index)) %>%
  group_by(cal_year, month_label, size_class_label , hypothesis_label) %>%
  summarise(valley_count = sum(count)) %>%
  ungroup()

# plot of valley-wide totals by hypothesis and size_class
valley_wide_outmigration_props <- valley_totals %>%
  group_by(cal_year, hypothesis_label) %>%
  mutate(annual_total = sum(valley_count)) %>% # get annual total per hypothesis
  ungroup() %>%
  group_by(cal_year, month_label, hypothesis_label) %>% # get annual outmigration propotions within a month
  mutate(prop_fish = valley_count / annual_total,
         prop_fish = ifelse(is.nan(prop_fish), 0, prop_fish)) %>%
  ungroup()

# confirm all looks good
valley_wide_outmigration_props %>%
  filter(cal_year == 1999) %>%
  ggplot(aes(month_label, prop_fish, fill = size_class_label)) +
  geom_col() +
  facet_wrap(vars(hypothesis_label))

write_rds(valley_wide_outmigration_props, "data/valley-wide-WR-juveniles-at-chipps.rds")

