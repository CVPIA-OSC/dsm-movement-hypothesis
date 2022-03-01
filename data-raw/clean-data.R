library(tidyverse)
library(lubridate)

late_fall_run_hypothesis_raw <- read_rds("data/late-fall-run-juveniles-at-chipps.rds")

sac_valley_year_types <- waterYearType::water_year_indices %>%
  filter(location == "Sacramento Valley") %>%
  select(WY, Yr_type)

size_class_lookup <- c("s"= "small", "m" = "medium", "l" = "large", "vl" = "very large")
hypothesis_lookup <- c("one" = "Hypothesis 1", "two" = "Hypothesis 2", "three" = "Hypothesis 3")

late_fall_run_hypothesis <- late_fall_run_hypothesis_raw %>%
  mutate(size_class_label = factor(size_class_lookup[size_class], levels = c("small", "medium", "large", "very large")),
         month_label = factor(month.abb[month], levels = month.abb),
         hypothesis_label = factor(hypothesis_lookup[hypothesis], levels = hypothesis_lookup),
         cal_year = year + 1979) %>%
  left_join(sac_valley_year_types, by = c("cal_year" = "WY"))


late_fall_run_hypothesis %>%
  select(-(Oct_Mar:Index)) %>%
  group_by(watershed, year, hypothesis) %>%
  mutate(total_fish = sum(count)) %>%
  ungroup() %>%
  group_by(watershed, year, month, hypothesis) %>%
  mutate(prop_fish = count / total_fish,
         prop_fish = ifelse(is.nan(prop_fish), 0, prop_fish)) %>%
  ungroup() %>%
  write_rds("data/late-fall-run-juveniles-at-chipps-clean-new-metric.rds")


# Valley-wide proportions -------------------------------------------------

# get valley total by summing across all the watersheds, preserve size_class
valley_totals <- late_fall_run_hypothesis %>%
  select(-(Oct_Mar:Index)) %>%
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

write_rds(valley_wide_outmigration_props, "data/valley-wide-LFR-juveniles-at-chipps.rds")


d <- read_rds("data/late-fall-run-juveniles-at-chipps-clean-new-metric.rds")

dd <- d %>%
  group_by(cal_year, month_label, size_class_label , hypothesis_label) %>%
  summarise(count = sum(count)) %>%
  ungroup() %>%
  group_by(cal_year, hypothesis_label) %>%
  mutate(total_fish = sum(count)) %>% # get annual total per hypothesis
  ungroup() %>%
  group_by(cal_year, month_label, hypothesis_label) %>% # get annual outmigration propotions within a month
  mutate(prop_fish = count / total_fish,
         prop_fish = ifelse(is.nan(prop_fish), 0, prop_fish)) %>%
  ungroup()

colnames(dd) %in% colnames(d)

chipps_trawls <- read_csv("data/1976-2001_DJFMP_trawl_fish_and_water_quality_data.csv",
                          col_types = cols(
                            Location = col_character(),
                            RegionCode = col_double(),
                            StationCode = col_character(),
                            SampleDate = col_date(format = ""),
                            SampleTime = col_time(format = ""),
                            MethodCode = col_character(),
                            GearConditionCode = col_double(),
                            WeatherCode = col_character(),
                            DO = col_logical(),
                            WaterTemp = col_double(),
                            Turbidity = col_logical(),
                            Secchi = col_double(),
                            SpecificConductance = col_logical(),
                            TowNumber = col_double(),
                            SamplingDirection = col_character(),
                            TowDuration = col_double(),
                            FlowDebris = col_logical(),
                            SiteDisturbance = col_logical(),
                            AlternateSite = col_logical(),
                            SeineLength = col_logical(),
                            SeineWidth = col_logical(),
                            SeineDepth = col_logical(),
                            FlowmeterStart = col_double(),
                            FlowmeterEnd = col_double(),
                            FlowmeterDifference = col_double(),
                            Volume = col_double(),
                            OrganismCode = col_character(),
                            IEPFishCode = col_character(),
                            CommonName = col_character(),
                            MarkCode = col_character(),
                            StageCode = col_double(),
                            Expression = col_logical(),
                            ForkLength = col_double(),
                            RaceByLength = col_character(),
                            TagCode = col_character(),
                            RaceByTag = col_character(),
                            ArchivalID = col_logical(),
                            SpecialStudyID = col_logical(),
                            GeneticID = col_logical(),
                            Probability1 = col_logical(),
                            GeneticID2 = col_logical(),
                            Probability2 = col_logical(),
                            SexGeneID = col_logical(),
                            Ots28 = col_logical(),
                            Lab = col_logical(),
                            GeneticTest = col_logical(),
                            GeneticModel = col_logical(),
                            Count = col_double()
                          ))

chipps_trawls_proportions <- chipps_trawls %>%
  filter(Location == "Chipps Island",
         !is.na(RaceByTag)) %>%
  select(Location, RaceByTag, SampleDate, Count) %>%
  group_by(RaceByTag, year = year(SampleDate), month = month(SampleDate)) %>%
  summarise(Count = sum(Count)) %>%
  ungroup() %>%
  group_by(RaceByTag, year) %>%
  mutate(total_fish = sum(Count)) %>%
  ungroup() %>%
  mutate(prop_fish = Count / total_fish,
         date = as_date(paste0(year, "-", month, "-01")),
         month_label = factor(month.abb[month], levels = month.abb)) %>%
  select(cal_year = year, month_label, total_fish, prop_fish, RaceByTag) %>%
  group_by(RaceByTag, month_label) %>%
  summarise(avg_prop_fish = mean(prop_fish)) %>%
  ungroup() %>%
  arrange(RaceByTag, month_label)

x <- factor(month.abb, levels = month.abb)
base_ts <- data.frame(month_label = rep(x, 4),
                      RaceByTag = rep(c("Fall", "LateFall", "Spring", "Winter"), each = 12))

chipps_trawls_proportions_full <- base_ts %>%
  left_join(chipps_trawls_proportions) %>%
  as_tibble() %>%
  mutate(avg_prop_fish = ifelse(is.na(avg_prop_fish), 0, avg_prop_fish),
         avg_prop_fish = round(avg_prop_fish, 3))

write_rds(chipps_trawls_proportions_full, "data/chipps-trawls-proportions.rds")

chipps_trawls_proportions_full %>%
  filter(RaceByTag == "LateFall") %>%
  ggplot(aes(month_label, avg_prop_fish, fill = RaceByTag)) +
  geom_col(position = "dodge")
