library(tidyverse)
library(plotly)


# data ----------------

late_fall_run_hypothesis <- read_rds("data/late-fall-run-juveniles-at-chipps.rds")

#water year type plots--------------


#all years plots ------------------

#single year plots ------------------

year_selection <- 1982 - 1979
watershed_selection <- "Battle Creek"
size_class_lookup <- c("s"= "small", "m" = "medium", "l" = "large", "vl" = "very large")
month.abb[10]
late_fall_run_hypothesis %>%
  filter(watershed == watershed_selection,
         year == year_selection) %>%
  mutate(size_class_label = factor(size_class_lookup[size_class], levels = c("small", "medium", "large", "very large")),
         month_label = factor(month.abb[month], levels = month.abb)) %>%
  group_by(hypothesis) %>%
  group_map(~ plot_ly(
    data=.,
    x = ~ month_label,
    y = ~ count,
    color = ~ size_class_label,
    type  = "bar")) %>%
  subplot(nrows = 1, shareX = TRUE, shareY = TRUE) %>%
  layout(showlegend = FALSE, showlegend2 = TRUE, showlegend3 = FALSE)

hypothesis_lookup <- c("one" = "Hypothesis 1", "two" = "Hypothesis 2", "three" = "Hypothesis 3")
p <- late_fall_run_hypothesis %>%
  filter(watershed == watershed_selection,
         year == year_selection) %>%
  mutate(size_class_label = factor(size_class_lookup[size_class], levels = c("small", "medium", "large", "very large")),
         month_label = factor(month.abb[month], levels = month.abb),
         hypothesis_label = factor(hypothesis_lookup[hypothesis], levels = hypothesis_lookup)) %>%
  ggplot(aes(x = month_label, y = count, fill = size_class_label)) +
  geom_col() + facet_wrap(vars(hypothesis_label)) +
  labs(x= "", fill = "Size Class")+
  theme_minimal()+
  scale_fill_brewer(palette = "Set2")

ggplotly(p)
