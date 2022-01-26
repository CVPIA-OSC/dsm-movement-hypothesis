single_year_data_selection <- function(data, input_watershed, year = NULL) {
  data %>%
    filter(watershed == input_watershed) %>%
    select(month_label,
           count,
           watershed,
           size_class_label,
           hypothesis_label)
  return(data)
}

single_year_plot <- function(plot_data, x, y, fill, facet) {
  ggplotly(
    ggplot(data = plot_data, aes(x = x, y = y, fill = fill))+
      geom_col() + facet_wrap(vars(facet)) +
      theme_minimal() +
      scale_fill_brewer(palette = "Set2"))
}
