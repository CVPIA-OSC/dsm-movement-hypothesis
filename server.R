library(shiny)

shinyServer(function(input, output) {

    output$hypothesis_plot <- renderPlotly({
        plot_ly(data = mtcars, x = ~cyl, y = ~mpg, type = "scatter", mode = "markers")
    })    
})
