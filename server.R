#- server.R -#

server <- function(input, output, session) {

  OPTS$weather_date_max = yesterday()
  OPTS$climate_date_min = start_of_year()
  OPTS$climate_date_max = end_of_year()

  fill_weather <- function(dates = weather_dates()) {
    msg <- paste("Loading weather data for", first(dates))
    if (length(dates) > 1) msg <- paste(msg, "-", last(dates))
    withProgress({
      for (d in dates) {
        if (!exists("weather")) weather <<- tibble()
        wx <- get_weather_grid(d)
        incProgress(.5)
        weather <<- bind_rows(weather, wx)
        weather %>%
          filter(date >= start_of_year(as_date(d))) %>%
          remove_weather_cols() %>%
          write_fst(str_glue("data/weather-{year(d)}.fst"), compress = 99)
        incProgress(.5)
      }
    }, min = 0, max = length(dates), value = 0, message = msg)


    # build additional cols
    weather <<- weather %>% add_weather_cols()
  }


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
      # dates <- format(as_date(weather_dates()), "%b %d")
      # msg <- if (length(dates) == 1) {
      #   dates
      # } else {
      #   paste(c(first(dates), last(dates)), collapse = " - ")
      # }
      # msg <- paste0("Please wait, downloading weather data for ", msg, ".")
      # showModal(modalDialog(msg, title = strong("Loading weather..."), fade = F, footer = NULL))
      fill_weather()
      gc()
      rv$weather_ready <- TRUE
      # removeModal()
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
