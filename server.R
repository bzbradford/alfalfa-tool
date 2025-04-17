#- server.R -#

server <- function(input, output, session) {

  OPTS$weather_date_max <- yesterday()
  OPTS$climate_date_min <- start_of_year()
  OPTS$climate_date_max <- end_of_year()

  load_data <- function() {
    withProgress(
      message = "Loading datasets...",
      value = 0,
      min = 0,
      max = 3,
      {
        load_climate()
        load_weather()
        dates <- weather_dates()
        incProgress(ifelse(length(dates) > 0, 1, 2))
        update_weather(dates, progress = T)
      }
    )
  }


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

  weather_missing <- reactive({
    req(rv$weather_ready)
    all_dates <- seq.Date(OPTS$weather_date_min, OPTS$weather_date_max, 1)
    have_dates <- sort(unique(weather$date))
    diff <- setdiff(all_dates, have_dates)
    missing <- length(diff) > 0
    if (missing) message("Missing dates: ", paste(as_date(diff), collapse = ", "))
    missing
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
    # if the weather isn't updated on boot, load it
    if (isolate(!rv$weather_ready)) {
      load_data()
      rv$weather_ready <- TRUE
    }

    tagList(
      uiOutput("warning_ui"),
      fluidRow(
        column(6, h3("Weather and climate map"), mapUI()),
        column(6, uiOutput("sidebar_ui"))
      )
    )
  })

  output$warning_ui <- renderUI({
    req(weather_missing())
    div(style = "color: red; font-weight: bold;", OPTS$load_error_msg)
  })


  # Sidebar selector ----

  output$sidebar_ui <- renderUI({
    tagList(
      div(
        id = "map",
        h3("Map display options"),
        mapSidebarUI()
      ),
      div(
        id = "growth",
        style = "display: none;",
        h3("Alfalfa growth projection"),
        growthUI()
      ),
      div(
        id = "timing",
        style = "display: none;",
        h3("Alfalfa cut planning tool"),
        timingUI()
      ),
      div(
        id = "charts",
        style = "display: none;",
        h3("Weather and climate charts"),
        plotUI()
      ),
      div(
        id = "downloads",
        style = "display: none;",
        h3("Data downloads"),
        downloadsUI()
      ),
      div(
        id = "about",
        style = "display: none;",
        h3("About this app"),
        includeMarkdown("about.md")
      )
    )
  })

  # hide and show the appropriate content when switching tabs
  observe({
    req(rv$map_ready)
    id <- req(input$navbar)
    tabs <- c("map", "growth", "timing", "charts", "downloads", "about")
    lapply(tabs, function(tab) {
      if (tab != id) hide(tab) else show(tab)
    })
    #   # scroll to sidebar element on mobile view
    runjs("if (window.innerWidth < 768) document.getElementById('sidebar_ui').scrollIntoView();")
  })

}
