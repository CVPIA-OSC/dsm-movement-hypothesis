shinyServer(function(input, output) {

    # Reactives -------------------------------
    location_choices <- reactive({
        switch(input$location_type,
        "watershed" = fallRunDSM::watershed_labels,
        "region" = c("Sacramento Valley", "San Joaquin"))
    })

    # plot_data <- eventReactive(input$location, {
    #     total_locations <- length(input$location)
    #
    #     if (total_locations == 1) {
    #         return(list(top =  {
    #             data_selection(late_fall_run_hypothesis, input$location)
    #         },
    #         middle = NULL, bottom = NULL))
    #
    #     } else if (total_locations == 2) {
    #         return(list(top = {
    #             late_fall_run_hypothesis %>%
    #                 filter(watershed == input$location) %>%
    #                 select(x = month,
    #                        y = count,
    #                        watershed = watershed)
    #         },
    #         middle = {late_fall_run_hypothesis %>%
    #                 filter(watershed == input$location) %>%
    #                 select(x = month,
    #                        y = count,
    #                        watershed = watershed)},
    #         bottom = NULL))
    #     } else if (total_locations == 3) {
    #         return(list(top = {
    #             late_fall_run_hypothesis %>%
    #                 filter(watershed == input$location) %>%
    #                 select(x = month,
    #                        y = count,
    #                        watershed = watershed)
    #         },
    #         middle = {late_fall_run_hypothesis %>%
    #                 filter(watershed == input$location) %>%
    #                 select(x = month,
    #                        y = count,
    #                        watershed = watershed)},
    #         bottom = {late_fall_run_hypothesis %>%
    #                 filter(watershed == input$location) %>%
    #                 select(x = month,
    #                        y = count,
    #                        watershed = watershed) }))
    #     }
    #
    #
    # })

    top_plot_data <- reactive({
        if(length(input$location)== 1){
            return(
                data_selection(late_fall_run_hypothesis,
                               input$location_type,
                               input$location,
                               input$time_unit,
                               input$year_type_selection))
        }
        else{return(NULL)}
    })

    bottom_plot_data <- reactive({
        if(length(input$location)== 2){
            return(list(top = {
                data_selection(late_fall_run_hypothesis,
                               input$location_type,
                               input$location[1],
                               input$time_unit,
                               input$year_type_selection)
            },
            bottom = {
                data_selection(late_fall_run_hypothesis,
                               input$location_type,
                               input$location[2],
                               input$time_unit,
                               input$year_type_selection)
            }))
        }
        else{return(NULL)}
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

    output$metric_select_input_ui <- renderUI({
        req(input$time_unit)

        if(input$time_unit == "Water Year Type"){
            selectInput("year_type_selection", label = "Year Type Selection",
                        choices = c("Critical", "Dry", "Below Normal", "Above Normal", "Wet"))
        }else if(input$time_unit == "Single Year"){
            selectInput("year_type_selection", label = "Year Type Selection",
            choices = 1980:2000)
        }else{NULL}
    })

    output$hypothesis_plot_top <- renderPlotly({
        # if (is.null(top_plot_data()$bottom)) {return(NULL)}
        # single_year_plot(plot_data()$top,month_label, count, size_class_label, hypothesis_label)
        if(length(input$location)== 1){
            ggplotly(
                ggplot(data = top_plot_data(), aes(x = x, y = y, fill = fill)) +
                    geom_col() + facet_wrap(vars(facet)) +
                    labs(x = "", fill = "Size Class") +
                    theme_minimal() +
                    scale_fill_brewer(palette = "Set2"))
        } else if(length(input$location ==2)){
            ggplotly(
                ggplot(data = bottom_plot_data()$top, aes(x = x, y = y, fill = fill)) +
                    geom_col() + facet_wrap(vars(facet)) +
                    labs(x = "", fill = "Size Class") +
                    theme_minimal() +
                    scale_fill_brewer(palette = "Set2"))
        }

    })


    output$hypothesis_plot_bottom <- renderPlotly({
        if (is.null(bottom_plot_data())) {return(NULL)}
        ggplotly(
            ggplot(data = bottom_plot_data()$bottom, aes(x = x, y = y, fill = fill)) +
                geom_col() + facet_wrap(vars(facet)) +
                labs(x = "", fill = "Size Class") +
                theme_minimal() +
                scale_fill_brewer(palette = "Set2"))

    })
})
