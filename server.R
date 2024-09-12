#- server.R -#

server <- function(input, output, session) {

  OPTS$weather_date_max = yesterday()
  OPTS$climate_date_min = start_of_year()
  OPTS$climate_date_max = end_of_year()


  # Reactive values ----

  rv <- reactiveValues(

    # false if new weather data needs downloading
    weather_ready = length(weather_dates()) == 0,

    loc = NULL,
    loc_ready = FALSE
  )

  loc_data <- reactive({
    loc <- req(rv$loc)

    list(
      loc = loc,
      weather = weather %>%
        filter(lat == loc$lat, lng == loc$lng),
      c10 = climate$c10 %>%
        filter(lat == loc$lat, lng == loc$lng) %>%
        mutate(date = start_of_year() + yday - 1),
      c5 = climate$c5 %>%
        filter(lat == loc$lat, lng == loc$lng) %>%
        mutate(date = start_of_year() + yday - 1)
    )
  })


  # Initialize module servers ----

  mapServerValues <- mapServer()

  observe({
    rv$loc <- mapServerValues()$selected_grid
  })

  observe({
    if (is.null(rv$loc)) return()
    rv$loc_ready <- TRUE
  })

  plotServer(reactive(loc_data()))

  growthServer(reactive(loc_data()))

  timingServer(reactive(loc_data()))


  # Main UI ----

  output$main_ui <- renderUI({
    if (!rv$weather_ready) {
      load_data()
      rv$weather_ready <- TRUE
    }

    fluidRow(
      column(6,
        h3("Gridded weather and climate map"),
        mapUI(),
      ),
      column(6, uiOutput("sidebar_ui"))
    )
  })


  # Sidebar selector ----

  sidebar_pages <- list(
    map = list(
      h3("Map display options"),
      mapSidebarUI()
    ),
    growth = list(
      h3("Alfalfa growth projection"),
      growthUI()
    ),
    timing = list(
      h3("Alfalfa cut planning tool"),
      timingUI()
    ),
    charts = list(
      h3("Weather and climate charts"),
      plotUI()
    ),
    about = list(
      h3("About this app"),
      includeMarkdown("about.Rmd")
    )
  )

  output$sidebar_ui <- renderUI({
    page <- req(input$navbar)
    sidebar_pages[[page]]
  })

}
