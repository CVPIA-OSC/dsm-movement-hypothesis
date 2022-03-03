data_selection <-
  function(input_run,
           input_location_type,
           input_location,
           input_time_unit,
           input_year_type_selection ) {

    raw_data <- switch(input_run,
           "late_fall_run" = late_fall_run_hypothesis,
           "fall_run" = fall_run_hypothesis,
           "spring_run" = spring_run_hypothesis,
           "winter_run" = winter_run_hypothesis)

    if (input_location_type == "watershed") {
      selected_data <- raw_data %>%
        filter(watershed == input_location)
    } else if (input_location_type == "region") {
      # process valley wide data a little more to first get the valley wide
      # total before computing the proportion fish outmigrating
      selected_data <- raw_data %>%
        group_by(cal_year, month_label, size_class_label , hypothesis_label) %>%
        summarise(count = sum(count)) %>%
        ungroup() %>%
        group_by(cal_year, hypothesis_label) %>%
        mutate(total_fish = sum(count)) %>% # get annual total per hypothesis
        ungroup() %>%
        group_by(cal_year, month_label, hypothesis_label) %>% # get annual outmigration propotions within a month
        mutate(prop_fish = count / total_fish,
               prop_fish = ifelse(is.nan(prop_fish), 0, prop_fish)) %>%
        ungroup() %>%
        left_join(sac_valley_year_types, by=c("cal_year"="WY"))
    }

    if (input_time_unit == "Single Year") {
      selected_data %>%
        filter(cal_year == input_year_type_selection) %>%
        mutate(count_type = "Proportion", prop_fish = round(prop_fish, 3)) %>%
        select(
          x = month_label,
          y = prop_fish,
          fill = size_class_label,
          facet = hypothesis_label,
          count_type = count_type
        )
    } else if (input_time_unit == "Water Year Type") {
      selected_data %>%
        filter(Yr_type == input_year_type_selection) %>%
        group_by(month_label, hypothesis_label, size_class_label) %>%
        summarise(median_count = mean(prop_fish)) %>%
        mutate(count_type = "Average Proportion", median_count = round(median_count, 3)) %>%
        select(
          x = month_label,
          y = median_count,
          fill = size_class_label,
          facet = hypothesis_label,
          count_type = count_type
        )

    } else if (input_time_unit == "All Years") {
      selected_data %>%
        group_by(month_label, hypothesis_label, size_class_label) %>%
        summarise(median_count = mean(prop_fish)) %>%
        mutate(count_type = "Average Proportion", median_count = round(median_count, 3)) %>%
        select(
          x = month_label,
          y = median_count,
          fill = size_class_label,
          facet = hypothesis_label,
          count_type = count_type
        )
    }
  }

# }

# single_year_plot <- function(plot_data, x, y, fill, facet) {
#   ggplotly(
#     ggplot(data = plot_data, aes(x = x, y = y, fill = fill))+
#       geom_col() + facet_wrap(vars(facet)) +
#       theme_minimal() +
#       scale_fill_brewer(palette = "Set2"))
# }
