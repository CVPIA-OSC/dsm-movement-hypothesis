
shinyUI(fluidPage(
    # Application title
    titlePanel("DSM Movement Hypothesis"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            width = 3,
            tags$h3("Movement Hypothesis in the DSM"),
            tags$p("General information about movement rulesets in the model and inpact. Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore
            et dolore magna aliqua. Pellentesque habitant morbi tristique senectus et. Dui faucibus in ornare quam viverra.
            Vestibulum rhoncus est pellentesque elit ullamcorper dignissim cras tincidunt. Vestibulum mattis ullamcorper velit
            sed ullamcorper morbi tincidunt. Egestas tellus rutrum tellus pellentesque eu tincidunt tortor aliquam nulla."),

            tags$h3("How to use"),
            tags$p("How to use this Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore
            et dolore magna aliqua. Pellentesque habitant morbi tristique senectus et. Dui faucibus in ornare quam viverra.
            Vestibulum rhoncus est pellentesque elit ullamcorper dignissim cras tincidunt. Vestibulum mattis ullamcorper velit
            sed ullamcorper morbi tincidunt. Egestas tellus rutrum tellus pellentesque eu tincidunt tortor aliquam nulla."),

            tags$h3("Hypothesis Details"),
            tags$p(tags$b("Hypothesis 1:"),"Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore
            et dolore magna aliqua. Pellentesque habitant morbi tristique senectus et. Dui faucibus in ornare quam viverra.
            Vestibulum rhoncus est pellentesque elit ullamcorper dignissim cras tincidunt. Vestibulum mattis ullamcorper velit
            sed ullamcorper morbi tincidunt. Egestas tellus rutrum tellus pellentesque eu tincidunt tortor aliquam nulla.
            Volutpat blandit aliquam etiam erat velit scelerisque in. Cursus vitae congue mauris rhoncus aenean vel elit.
            Ultricies tristique nulla aliquet enim tortor at auctor urna nunc. Amet consectetur adipiscing elit duis.
            Feugiat in ante metus dictum at tempor commodo ullamcorper a. Euismod nisi porta lorem mollis. Gravida cum sociis
             natoque penatibus et magnis dis parturient montes. Sed turpis tincidunt id aliquet risus feugiat in ante metus.
             Venenatis cras sed felis eget velit aliquet sagittis id consectetur. Faucibus purus in massa tempor nec feugiat nisl
             pretium. Nam at lectus urna duis. Nunc aliquet bibendum enim facilisis gravida neque convallis. Ipsum dolor sit amet
              consectetur adipiscing elit. Lectus proin nibh nisl condimentum id venenatis a."),

            tags$p(tags$b("hypothesis 2:"), "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore
            et dolore magna aliqua. Pellentesque habitant morbi tristique senectus et. Dui faucibus in ornare quam viverra.
            Vestibulum rhoncus est pellentesque elit ullamcorper dignissim cras tincidunt. Vestibulum mattis ullamcorper velit
            sed ullamcorper morbi tincidunt. Egestas tellus rutrum tellus pellentesque eu tincidunt tortor aliquam nulla.
            Volutpat blandit aliquam etiam erat velit scelerisque in. Cursus vitae congue mauris rhoncus aenean vel elit.
            Ultricies tristique nulla aliquet enim tortor at auctor urna nunc. Amet consectetur adipiscing elit duis.
            Feugiat in ante metus dictum at tempor commodo ullamcorper a. Euismod nisi porta lorem mollis. Gravida cum sociis
             natoque penatibus et magnis dis parturient montes. Sed turpis tincidunt id aliquet risus feugiat in ante metus.
             Venenatis cras sed felis eget velit aliquet sagittis id consectetur. Faucibus purus in massa tempor nec feugiat nisl
             pretium. Nam at lectus urna duis. Nunc aliquet bibendum enim facilisis gravida neque convallis. Ipsum dolor sit amet
              consectetur adipiscing elit. Lectus proin nibh nisl condimentum id venenatis a.")


        ),

        mainPanel(
            fluidRow(
                column(width = 12,
                column(width = 2,
                radioButtons("location_type", label = "Select Location Type",
                choices = list("Watershed" = "watershed", "Region" = "region"), selected = "watershed")
                ),
                column(width = 3,
                uiOutput("location_select_input_ui")
                ),
                column(width = 3, radioButtons("time_unit", label = "Choose Grouping",
                                               inline = TRUE, choices = c("Water Year Type",
                                                                          "All Years", "Single Year"))))
            ),

            fluidRow(
                column(width = 12,
                plotlyOutput("hypothesis_plot_top"),
                plotlyOutput("hypothesis_plot_middle"),
                plotlyOutput("hypothesis_plot_bottom")
                )
            )
        )
    )
))
