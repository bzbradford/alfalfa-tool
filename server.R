#- server.R -#

server <- function(input, output, session) {

  OPTS$weather_date_max = yesterday()
  OPTS$climate_date_min = start_of_year()
  OPTS$climate_date_max = end_of_year()


  # Reactive values ----

  rv <- reactiveValues(
    # false if new weather data needs downloading
    weather_ready = length(weather_dates()) == 0,

    # true after a location has been selected
    loc_ready = FALSE,

    # true after map has loaded
    map_ready = FALSE,
  )

  loc_data <- reactive({
    loc <- req(mapServerValues()$selected_grid)

    list(
      loc = loc,
      weather = weather %>%
        filter(lat == loc$lat, lng == loc$lng),
      c10 = climate$c10 %>%
        filter(lat == loc$lat, lng == loc$lng) %>%
        mutate(date = start_of_year() + yday - 1, .after = yday),
      c5 = climate$c5 %>%
        filter(lat == loc$lat, lng == loc$lng) %>%
        mutate(date = start_of_year() + yday - 1, .after = yday)
    )
  })


  # Initialize module servers ----

  mapServerValues <- mapServer()
  growthServer(reactive(loc_data()))
  timingServer(reactive(loc_data()))
  plotServer(reactive(loc_data()))
  downloadsServer(
    grid_data = reactive(mapServerValues()$grid_data),
    loc_data = reactive(loc_data())
  )

  observe({
    req(mapServerValues()$selected_grid)
    rv$loc_ready <- TRUE
  })

  observe({
    req(mapServerValues()$grid_data)
    rv$map_ready <- TRUE
  })


  # Main UI ----

  output$main_ui <- renderUI({
    if (!rv$weather_ready) {
      load_data()
      rv$weather_ready <- TRUE
    }

    fluidRow(
      column(6, h3("Weather and climate map"), mapUI()),
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
    downloads = list(
      h3("Data downloads"),
      downloadsUI()
    ),
    about = list(
      h3("About this app"),
      includeMarkdown("about.Rmd")
    )
  )

  output$sidebar_ui <- renderUI({
    if (!rv$map_ready) {
      sidebar_pages[["map"]]
    } else {
      page <- req(input$navbar)
      # scroll to sidebar element on mobile view
      runjs("if (window.innerWidth < 768) document.getElementById('sidebar_ui').scrollIntoView();")
      sidebar_pages[[page]]
    }
  })

}
