shinyServer(function(input, output) {
  # location_type = "watershed"


  # Reactives -------------------------------
  location_choices <- reactive({
    switch(input$location_type,
           "watershed" = late_fall_run_watersheds,
           "region" = c("Sacramento Valley"))
  })

  observe({
    cat("input location: ", input$location[1], "\n")
  })

  top_plot_data <- reactive({

    req(input$location_type, input$location, input$time_unit, input$year_type_selection)

    data_selection(
      late_fall_run_hypothesis,
      input$location_type,
      input$location[1],
      input$time_unit,
      input$year_type_selection
    )
  })

  bottom_plot_data <- reactive({
    req(input$location_type, input$location, input$time_unit, input$year_type_selection)

    if (input$location_type == "watershed") {
      if (length(input$location) == 2) {
        data_selection(
          late_fall_run_hypothesis,
          input$location_type,
          input$location[2],
          input$time_unit,
          input$year_type_selection
        )}
      else{
        return(NULL)
      }
    } else {
      chipps_trawls_proportions %>%
        filter(RaceByTag == "LateFall") %>%
        mutate(x = month_label, y = avg_prop_fish, count_type = "Mean Proportion",
               fill = NULL)
    }
  })

  # Outputs ---------------------------------
  output$location_select_input_ui <- renderUI({
    req(input$location_type)
    tags$div(title = "Select up to two watersheds. A selected watershed's plot can be removed by deleting
                    it from this list using the backspace or delete key.",
             selectizeInput("location", label = "Select Location",
                            choices = location_choices(),
                            multiple = TRUE,
                            selected = location_choices()[1],
                            options = list(maxItems = 2))
    )
  })

  # observeEvent(input$time_unit, {
  output$metric_select_input_ui <- renderUI({
    # req(input$time_unit)
    if (input$time_unit == "Water Year Type") {
      selectInput(
        "year_type_selection",
        label = "Year Type Selection",
        choices = c(
          "Critical",
          "Dry",
          "Below Normal",
          "Above Normal",
          "Wet"
        ),
        selected = "Critical"
      )
    } else if (input$time_unit == "Single Year") {
      selectInput("year_type_selection",
                  label = "Year Type Selection",
                  choices = 1980:1999,
                  selected = 1980)
    } else{
      NULL
    }
    # })
  })


  output$hypothesis_plot_top <- renderPlotly({
    # req(exists(input$year_type_selection, inherits = FALSE), cancelOutput = TRUE)
    validate(need(nrow(top_plot_data()) > 0, "Selection yielded no results"))



    ggplotly(
      ggplot(data = top_plot_data(), aes(
        x = x,
        y = y,
        fill = fill,
        text =
          paste0("<b>",
                 fill,

                 "</b>",
                 "\n",
                 count_type,
                 ":",
                 y)
      )) +
        geom_col() + facet_wrap(vars(facet)) +
        scale_y_continuous(limits = c(0, 1)) +
        labs(
          x = "",
          fill = "",
          y = top_plot_data()$count_type[1],
          title = paste(input$location[1], "Juvenile Salmon at Chipps Island")
        ) +
        theme_minimal() +
        scale_fill_brewer(palette = "Set2") +
        theme(plot.margin = margin(1, 0, 0, 1.5, "cm")),
      tooltip =  "text"
    ) %>%layout(
      hovermode = "x"
    ) %>%
      plotly::config(displayModeBar = FALSE) %>%
      plotly::config(showLink = FALSE)
  })


  #
  output$hypothesis_plot_bottom <- renderPlotly({
    validate(need(nrow(bottom_plot_data()) > 0, ""))


    if (is.null(bottom_plot_data())) {
      return(NULL)
    }

    if (input$location_type == "watershed") {
      ggplotly(
        ggplot(data = bottom_plot_data(), aes(
          x = x,
          y = y,
          fill = fill,
          text =
            paste0("<b>",
                   fill,

                   "</b>",
                   "\n",
                   count_type,
                   ":",
                   y)
        )) +
          geom_col() + facet_wrap(vars(facet)) +
          scale_y_continuous(limits = c(0, 1)) +
          labs(
            x = "",
            fill = "",
            y = bottom_plot_data()$count_type[1],
            title = paste(input$location[2], "Juvenile Salmon at Chipps Island")
          )+
          theme_minimal() +
          scale_fill_brewer(palette = "Set2") +
          theme(plot.margin = margin(1, 0, 0, 1.5, "cm")),
        tooltip =  "text"
      ) %>%layout(
        hovermode = "x"
      ) %>%
        plotly::config(displayModeBar = FALSE) %>%
        plotly::config(showLink = FALSE)
    } else if (input$location_type == "region") {
      ggplotly(
        ggplot(data = bottom_plot_data(), aes(
          x = x,
          y = y,
          fill = RaceByTag,
          text =
            paste0(count_type, ": ", y)
        ), showlegend=FALSE) +
          geom_col(position = "dodge") +
          scale_y_continuous(limits = c(0, 1)) +
          labs(
            x = "",
            fill = "",
            y = bottom_plot_data()$count_type[1],
            title = paste("Late-Fall Average Outmigration (Chipps Trawls 1976-2001)")) +
          theme_minimal() +
          scale_fill_brewer(palette = "Set2") +
          theme(plot.margin = margin(1, 0, 0, 1.5, "cm"), legend.position = "none"),
        tooltip =  "text"
      ) %>%layout(
        hovermode = "x"
      ) %>%
        plotly::config(displayModeBar = FALSE) %>%
        plotly::config(showLink = FALSE)
    }

  })
})
