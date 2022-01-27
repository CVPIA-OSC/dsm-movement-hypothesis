#inputlocation
#input grouping -> water year type, all years, single year
#all years,-> find median
#single year -> year type selection - filter by year
#water year type -> year type selection - filter by water year find median
region_selection <- function(data, input_location_type, input_location){
  if(input_location_type == "watershed"){
    data <- data %>%
      filter(watershed == input_location)
  }else if (input_location_type == "region"){
    data <- data %>%
      filter(location == input_location)
  }
  return(data)
}

data_selection <- function(data, input_location_type, input_location, input_time_unit,
                                       input_year_type_selection = NULL) {



data <- region_selection(data, input_location_type, input_location)

if(input_time_unit == "Single Year"){
  data <- data %>%
    filter(
           cal_year == input_year_type_selection) %>%
    select(x = month_label,
           y = count,
           fill = size_class_label,
           facet = hypothesis_label)
}else if(input_time_unit == "Water Year Type"){
  data <- data %>%
    filter(
           Yr_type == input_year_type_selection) %>%
    group_by(month_label, hypothesis_label, size_class_label) %>%
    summarise(median_count = median(count)) %>%
    select(x = month_label,
           y = median_count,
           fill = size_class_label,
           facet = hypothesis_label)

}else if(input_time_unit == "All Years"){
  data <- data %>%
    # filter(watershed == input_location) %>%
    group_by(month_label, hypothesis_label, size_class_label) %>%
    summarise(median_count = median(count)) %>%
    select(x = month_label,
           y = median_count,
           fill = size_class_label,
           facet = hypothesis_label)
}

return(data)
}

# }

single_year_plot <- function(plot_data, x, y, fill, facet) {
  ggplotly(
    ggplot(data = plot_data, aes(x = x, y = y, fill = fill))+
      geom_col() + facet_wrap(vars(facet)) +
      theme_minimal() +
      scale_fill_brewer(palette = "Set2"))
}
