

shinyUI(fluidPage(
    # Application title
    titlePanel("DSM Movement Hypothesis"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            width = 3,
            tags$h3("How to use"),
            tags$p(
                "Select a run, a watershed, and a grouping type to view outmigration fish proportions at Chipps Island.
                Selecting a grouping type changes the definition of the proportion slightly, details below."
            ),

            tags$h4("Grouping Types"),
            tags$ul(
              tags$li(tags$b("Single Year:"), "Data is filtered to the user selected year. Values shown
                      are the number of fish for a given watershed/size class/month combination divided by the total outmgrating fish within a watershed that season."),
              tags$li(tags$b("All Years:"), "All years of simulation are used to calculate average proportions. Values shown are the
                      average proportions for a given watershed/size class/month combination across all years for the selected watershed."),
              tags$li(tags$b("Water Year Type:"), "All years that meet the water type selected are used to compute the average
                      proportions. Values shown are the
                      average proportions for a given watershed/size class/month combination across all years for the selected watershed and year type.")
            ),

            tags$p(tags$b("Note:"),"Up to two watersheds can be selected from the 'Select Location' menu. A selected watershed's plot can be removed by deleting it from list of selections using the backspace or delete key."),

            tags$p("Outmigrating fish counts were obtained from runing the selected run in deterministic mode."),

            tags$h3("Hypothesis Details"),
            tags$p(
                tags$b("Hypothesis 1:"),
                "Fry leave natal tributaries and rear in the Sacramento River and the delta using habitat filling rules identical to the selected run."
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
                        "run",
                        label = "Select Run",
                        choices = list("Fall Run" = "Fall Run",
                                       "Late-Fall Run" = "Late-Fall Run",
                                       "Spring Run" = "Spring Run",
                                       "Winter Run" = "Winter Run")
                    )
                ),
                column(
                    width = 2,
                    radioButtons(
                        "location_type",
                        label = "Select Location Type",
                        choices = list("Watershed" = "watershed",
                                       "Central Valley" = "region"),
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
                plotlyOutput("hypothesis_plot_top", height = 500),
                plotlyOutput("hypothesis_plot_bottom", height = 500)
            )
        ))
    )
))
