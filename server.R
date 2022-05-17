shinyServer(function(input, output) {
  # location_type = "watershed"


  # Reactives -------------------------------
  run_choices_watersheds <-reactive({
    switch(input$run,
           "Late-Fall Run" = late_fall_run_watersheds,
           "Fall Run" = fall_run_watersheds,
           "Spring Run" = spring_run_watersheds,
           "Winter Run" = winter_run_watersheds,)
  })
  location_choices <- reactive({
    switch(input$location_type,
           "watershed" = run_choices_watersheds(),
           "region" = c("Central Valley"))
  })


  top_plot_data <- reactive({

    req(input$run, input$location_type, input$location, input$time_unit, input$year_type_selection, input$plot_type)

    data_selection(
      input$run,
      input$location_type,
      input$location[1],
      input$time_unit,
      input$year_type_selection,
      input$plot_type
    )
  })

  bottom_plot_data <- reactive({
    req(input$location_type, input$location, input$time_unit, input$year_type_selection, input$plot_type)

    if (input$location_type == "watershed") {
      if (length(input$location) == 2) {
        data_selection(
          input$run,
          input$location_type,
          input$location[2],
          input$time_unit,
          input$year_type_selection,
          input$plot_type
        )}
      else{
        return(NULL)
      }
    } else {
      switch(
        input$run,
        "Late-Fall Run" = {
        yearly_chipps_trawls_proportions <- yearly_chipps_trawls_proportions %>%
          mutate(month_label = factor(month.abb[month_label], levels = c("Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec","Jan","Feb","Mar")))
        }, "Fall Run" = {
          yearly_chipps_trawls_proportions <- yearly_chipps_trawls_proportions %>%
            mutate(month_label = factor(month.abb[month_label], levels = month.abb))
        }, "Spring Run" = {
          yearly_chipps_trawls_proportions <- yearly_chipps_trawls_proportions %>%
            mutate(month_label = factor(month.abb[month_label], levels = c("Nov","Dec","Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct")))
        }, "Winter Run" = {
          yearly_chipps_trawls_proportions <- yearly_chipps_trawls_proportions %>%
            mutate(month_label = factor(month.abb[month_label], levels = c("Sep","Oct","Nov","Dec","Jan","Feb","Mar","Apr","May","Jun","Jul","Aug")))
        })

      switch(
        input$time_unit,
        "Single Year" = {
          yearly_chipps_trawls_proportions%>%
            filter(RaceByTag == input$run,
                   year == input$year_type_selection) %>%
            select(x= month_label, y= prop_fish, RaceByTag ) %>%
            mutate(count_type= "Proportion")
        },"All Years" = {
          yearly_chipps_trawls_proportions %>%
            filter(RaceByTag == input$run) %>%
            group_by(RaceByTag, month_label) %>%
            summarise(avg_prop_fish = mean(prop_fish)) %>%
            ungroup() %>%
            select(x= month_label, y= avg_prop_fish, RaceByTag ) %>%
            mutate(count_type= "Average Proportion",
                   y = round(y, 3))
        },"Water Year Type" = {
          yearly_chipps_trawls_proportions %>%
            filter(RaceByTag == input$run,
                   Yr_type == input$year_type_selection) %>%
            group_by(RaceByTag, month_label) %>%
            summarise(avg_prop_fish= mean(prop_fish)) %>%
            ungroup()%>%
            select(x= month_label, y= avg_prop_fish, RaceByTag ) %>%
            mutate(count_type= "Average Proportion",
                   y = round(y, 3))
        }
      )
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
          title = paste(input$location[1], input$run, "Juvenile Salmon at Chipps Island")
        ) +
        theme_minimal() +
        scale_fill_brewer(palette = "Set2") +
        theme(panel.spacing.y = unit(0.5, "cm"),
              plot.margin = margin(1, 1, 0, 1.5, "cm")),
      tooltip =  "text"
    ) %>%layout(
      hovermode = "x",
      legend = list(orientation = "h")
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
            title = paste(input$location[2],input$run, "Juvenile Salmon at Chipps Island")
          )+
          theme_minimal() +
          scale_fill_brewer(palette = "Set2") +
          theme(plot.margin = margin(1, 0, 0, 1.5, "cm")),
        tooltip =  "text"
      ) %>%layout(
        hovermode = "x",
        legend = list(orientation = "h")
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
            title = paste(input$run, "Proportion Outmigration at Chipps Trawls")) +
          theme_minimal() +
          scale_fill_brewer(palette = "Set2") +
          theme(plot.margin = margin(1, 0, 0, 1.5, "cm"), legend.position = "none"),
        # width = 1115,
        tooltip =  "text"
      ) %>%layout(
        hovermode = "x"
      ) %>%
        plotly::config(displayModeBar = FALSE) %>%
        plotly::config(showLink = FALSE)
    }

  })
})
