

shinyUI(fluidPage(
    # Application title
    titlePanel("Late-Fall Run DSM Movement Hypothesis"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            width = 3,
            tags$h3("Movement Hypothesis in the DSM"),
            tags$p(
                "General information about movement rulesets in the model and inpact. Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore
            et dolore magna aliqua. Pellentesque habitant morbi tristique senectus et. Dui faucibus in ornare quam viverra.
            Vestibulum rhoncus est pellentesque elit ullamcorper dignissim cras tincidunt. Vestibulum mattis ullamcorper velit
            sed ullamcorper morbi tincidunt. Egestas tellus rutrum tellus pellentesque eu tincidunt tortor aliquam nulla."
            ),

            tags$h3("How to use"),
            tags$p(
                "How to use this Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore
            et dolore magna aliqua. Pellentesque habitant morbi tristique senectus et. Dui faucibus in ornare quam viverra.
            Vestibulum rhoncus est pellentesque elit ullamcorper dignissim cras tincidunt. Vestibulum mattis ullamcorper velit
            sed ullamcorper morbi tincidunt. Egestas tellus rutrum tellus pellentesque eu tincidunt tortor aliquam nulla."
            ),

            tags$h3("Hypothesis Details"),
            tags$p(
                tags$b("Hypothesis 1:"),
                "Fry leave natal tributaries and rear in the Sacramento River and the delta using habitat filling rules identical to fall-run."
            ),

            tags$p(
                tags$b("Hypothesis 2:"),
                "Fry leave natal tributaries and rear in the Sacramento River with 25% migrating below Red Bluff Diversion Dam to rear in the Sacramento River and the delta."
            ),

            tags$p(
              tags$b("Hypothesis 3:"),
              "Fry leave natal tributaries and rear in the Sacramento River and the delta but they do not pass a downstream segment if temperatures are greater than 18°C."
            )


        ),

        mainPanel(fluidRow(
            column(
                width = 12,
                column(
                    width = 2,
                    radioButtons(
                        "location_type",
                        label = "Select Location Type",
                        choices = list("Watershed" = "watershed"),
                        selected = "watershed"
                    )
                ),
                column(width = 3,
                       uiOutput("location_select_input_ui")),
                column(
                    width = 2,
                    radioButtons(
                        "time_unit",
                        label = "Choose Grouping",
                        choices = c("Single Year", "All Years", "Water Year Type")
                    )
                ),
                column(width = 3, uiOutput("metric_select_input_ui"))

            )
        ),

        fluidRow(
            column(
                width = 12,
                plotlyOutput("hypothesis_plot_top"),
                plotlyOutput("hypothesis_plot_bottom")
            )
        ))
    )
))
