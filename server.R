shinyServer(function(input, output) {

    # Reactives -------------------------------
    location_choices <- reactive({
        switch(input$location_type,
        "watershed" = late_fall_run_watersheds,
        "region" = c("Sacramento Valley", "San Joaquin"))
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
    })
    # Outputs ---------------------------------
    output$location_select_input_ui <- renderUI({
        req(input$location_type)

        selectizeInput("location", label = "Select Location",
        choices = location_choices(),
        multiple = TRUE,
        selected = location_choices()[1],
        options = list(maxItems = 2))
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
                        choices = 1980:2000,
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
                    labs(
                        x = "",
                        fill = "",
                        y = top_plot_data()$count_type[1],
                        title = paste(input$location[1], "Juvenile Salmon at Chipps Island")
                    ) +
                    theme_minimal() +
                    scale_fill_brewer(palette = "Set2") +
                    theme(plot.margin = margin(0, 0, 0, 1.5, "cm")),
                tooltip =  "text"
            ) %>%layout(
                hovermode = "x"
            ) %>%
                plotly::config(displayModeBar = FALSE) %>%
                plotly::config(showLink = FALSE)
        })


#
    output$hypothesis_plot_bottom <- renderPlotly({
        validate(need(nrow(bottom_plot_data()) > 0, "Selection yielded no results"))


        if (is.null(bottom_plot_data())) {
            return(NULL)
        }
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
                labs(
                    x = "",
                    fill = "",
                    y = bottom_plot_data()$count_type[1],
                    title = paste(input$location[2], "Juvenile Salmon at Chipps Island")
                )+
            theme_minimal() +
            scale_fill_brewer(palette = "Set2") +
            theme(plot.margin = margin(0, 0, 2, 1.5, "cm")),
        tooltip =  "text"
        ) %>%layout(
        hovermode = "x"
        ) %>%
            plotly::config(displayModeBar = FALSE) %>%
            plotly::config(showLink = FALSE)
    })
})
