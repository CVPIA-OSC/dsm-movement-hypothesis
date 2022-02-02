library(tidyverse)
library(plotly)


# data ----------------

# late_fall_run_hypothesis_raw <- read_rds("data/late-fall-run-juveniles-at-chipps.rds")
# # #
sac_valley_year_types <-waterYearType::water_year_indices %>%
  filter(location == "Sacramento Valley")
#
# size_class_lookup <- c("s"= "small", "m" = "medium", "l" = "large", "vl" = "very large")
# hypothesis_lookup <- c("one" = "Hypothesis 1", "two" = "Hypothesis 2", "three" = "Hypothesis 3")
# late_fall_run_hypothesis <-late_fall_run_hypothesis_raw %>%
#   mutate(size_class_label = factor(size_class_lookup[size_class], levels = c("small", "medium", "large", "very large")),
#          month_label = factor(month.abb[month], levels = month.abb),
#          hypothesis_label = factor(hypothesis_lookup[hypothesis], levels = hypothesis_lookup),
#          cal_year = year + 1979) %>%
#   left_join(sac_valley_year_types, by = c("cal_year" = "WY"))

write_rds(late_fall_run_hypothesis, "data/late-fall-run-juveniles-at-chipps-clean.rds")

# late_fall_run_watersheds <-late_fall_run_hypothesis %>%
#   filter(count > 0) %>%
#   distinct(watershed) %>%
#   pull()


region_selection <- "Sacramento Valley"

#filter count to true
# water year plots by region  ------------------------
year_type_selection <- "Critical"

water_year_region <- late_fall_run_hypothesis %>%
  filter(location == region_selection,
         Yr_type == year_type_selection,
         !count == 0) %>%
  group_by(month_label, hypothesis_label, size_class_label) %>%
  summarise(median_count = median(count)) %>%
  ggplot(aes(x = month_label, y = median_count, fill = size_class_label)) +
  geom_col() + facet_wrap(vars(hypothesis_label)) +
  labs(x = "", y = "Median Count", fill = "Size Class") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set2")

ggplotly(water_year_region)

#all year type plots by region --------------
all_region <- late_fall_run_hypothesis %>%
  filter(location == region_selection,
         !count == 0) %>%
  group_by(month_label, hypothesis_label, size_class_label) %>%
  summarise(median_count = median(count)) %>%
  ggplot(aes(x = month_label, y = median_count, fill = size_class_label)) +
  geom_col() + facet_wrap(vars(hypothesis_label)) +
  labs(x = "", y = "Median Count", fill = "Size Class") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set2")

ggplotly(all_region)

#single year plots by region ------------------
year_selection <- 1980

single_region <- late_fall_run_hypothesis %>%
  filter(location == region_selection,
         cal_year == year_selection,
         !count == 0) %>%
  ggplot(aes(x = month_label, y = count, fill = size_class_label)) +
  geom_col() + facet_wrap(vars(hypothesis_label)) +
  labs(x = "", y = "Median Count", fill = "Size Class") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set2")
ggplotly(single_region)

watershed_selection <- "Upper Sacramento River"

#water years plots by watershed -------------------
water_year_watershed <- late_fall_run_hypothesis %>%
  filter(watershed == watershed_selection,
         Yr_type == year_type_selection,
         !count == 0) %>%
  group_by(month_label, hypothesis_label, size_class_label) %>%
  summarise(median_count = median(count)) %>%
  ggplot(aes(x = month_label, y = median_count, fill = size_class_label)) +
  geom_col() + facet_wrap(vars(hypothesis_label)) +
  labs(x = "", y = "Median Count", fill = "Size Class") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set2")

ggplotly(water_year_watershed)

#all year plots by watershed -----------------

all_watershed <- late_fall_run_hypothesis %>%
  filter(watershed == watershed_selection,
         !count == 0) %>%
  group_by(month_label, hypothesis_label, size_class_label) %>%
  summarise(median_count = median(count)) %>%
  ggplot(aes(x = month_label, y = median_count, fill = size_class_label)) +
  geom_col() + facet_wrap(vars(hypothesis_label)) +
  labs(x = "", y = "Median Count", fill = "Size Class") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set2")

ggplotly(all_watershed)

#single year plots by watershed------------------
single_watershed <- late_fall_run_hypothesis %>%
  filter(watershed == watershed_selection,
         cal_year == year_selection) %>%
  ggplot(aes(x = month_label, y = count, fill = size_class_label)) +
  geom_col() + facet_wrap(vars(hypothesis_label)) +
  labs(x = "", fill = "Size Class") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set2")

ggplotly(single_watershed)


# plotly -----------------
#TODO: more research on faceting plotly needed
# late_fall_run_hypothesis %>%
#   filter(watershed == watershed_selection,
#          year == year_selection) %>%
#   mutate(size_class_label = factor(size_class_lookup[size_class], levels = c("small", "medium", "large", "very large")),
#          month_label = factor(month.abb[month], levels = month.abb)) %>%
#   group_by(hypothesis) %>%
#   group_map(~ plot_ly(
#     data=.,
#     x = ~ month_label,
#     y = ~ count,
#     color = ~ size_class_label,
#     type  = "bar")) %>%
#   subplot(nrows = 1, shareX = TRUE, shareY = TRUE) %>%
#   layout(showlegend = FALSE, showlegend2 = TRUE, showlegend3 = FALSE)


