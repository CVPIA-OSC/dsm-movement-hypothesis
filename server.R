shinyServer(function(input, output) {

    # Reactives -------------------------------
    location_choices <- reactive({
        switch(input$location_type,
        "watershed" = fallRunDSM::watershed_labels,
        "region" = c("Sacramento", "San Joaquin"))
    })

    plot_data <- eventReactive(input$location, {
        total_locations <- length(input$location)

        if (total_locations == 1) {
            return(list(top =  {
                single_year_data_selection(late_fall_run_hypothesis, input$location)
            },
            middle = NULL, bottom = NULL))

        } else if (total_locations == 2) {
            return(list(top = {
                late_fall_run_hypothesis %>%
                    filter(watershed == input$location) %>%
                    select(x = month,
                           y = count,
                           watershed = watershed)
            },
            middle = {late_fall_run_hypothesis %>%
                    filter(watershed == input$location) %>%
                    select(x = month,
                           y = count,
                           watershed = watershed)},
            bottom = NULL))
        } else if (total_locations == 3) {
            return(list(top = {
                late_fall_run_hypothesis %>%
                    filter(watershed == input$location) %>%
                    select(x = month,
                           y = count,
                           watershed = watershed)
            },
            middle = {late_fall_run_hypothesis %>%
                    filter(watershed == input$location) %>%
                    select(x = month,
                           y = count,
                           watershed = watershed)},
            bottom = {late_fall_run_hypothesis %>%
                    filter(watershed == input$location) %>%
                    select(x = month,
                           y = count,
                           watershed = watershed) }))
        }

    })

    # Outputs ---------------------------------
    output$location_select_input_ui <- renderUI({
        req(input$location_type)

        selectizeInput("location", label = "Select Location",
        choices = location_choices(),
        multiple = TRUE,
        selected = location_choices()[1],
        options = list(maxItems = 3))
    })

    output$hypothesis_plot_top <- renderPlotly({
        if (is.null(plot_data()$top)) {return(NULL)}
        # single_year_plot(plot_data()$top,month_label, count, size_class_label, hypothesis_label)
        ggplotly(
            ggplot(data = plot_data()$top, aes(x = month_label, y = count, fill = size_class_label)) +
                     geom_col() + facet_wrap(vars(hypothesis_label)) +
                     labs(x = "", fill = "Size Class") +
                     theme_minimal() +
                     scale_fill_brewer(palette = "Set2"))

    })

    output$hypothesis_plot_middle <- renderPlotly({
        if (is.null(plot_data()$middle)) {return(NULL)}
        plot_ly(data = plot_data()$middle, x = ~x, y = ~y, type = "bar", mode = "markers")
    })

    output$hypothesis_plot_bottom <- renderPlotly({
        if (is.null(plot_data()$bottom)) {return(NULL)}
        plot_ly(data = plot_data()$bottom, x = ~x, y = ~y, type = "bar", mode = "markers")

    })
})
