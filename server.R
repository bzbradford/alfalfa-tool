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

  ## map server ----
  mapServerValues <- mapServer()

  observe({
    rv$loc <- mapServerValues()$selected_grid
  })

  observe({
    if (is.null(rv$loc)) return()
    rv$loc_ready <- TRUE
  })


  ## plot servers ----
  weatherPlotServer(reactive(loc_data()))
  climatePlotServer(reactive(loc_data()))
  customPlotServer(reactive(loc_data()))


  # Main UI ----

  ## main_ui ----
  output$main_ui <- renderUI({
    if (!rv$weather_ready) {
      dates <- format(as_date(weather_dates()), "%b %d")
      msg <- if (length(dates) == 1) {
        dates
      } else {
        paste(c(first(dates), last(dates)), collapse = " - ")
      }
      msg <- paste0("Please wait, downloading weather data for ", msg, ".")
      showModal(modalDialog(msg, fade = F, footer = NULL))
      fill_weather()
      gc()
      rv$weather_ready <- TRUE
      removeModal()
    }
    tagList(
      mapUI(),
      div(
        h3(style = "margin-top: 1em;", "Weather and climate details"),
        uiOutput("location_ui")
      )
    )
  })

  ## location_ui ----
  output$location_ui <- renderUI({
    validate(need(rv$loc_ready, "Please select a grid cell in the map above to view detailed weather data for that location. Use the crosshair icon on the map to automatically select your location."))

    tagList(
      uiOutput("lat_lng_ui"),
      tabsetPanel(
        tabPanel(
          "Weather plot",
          div(
            style = "min-height: 575px;",
            weatherPlotUI()
          )
        ),
        tabPanel(
          "Climate plot",
          div(
            style = "min-height: 640px;",
            climatePlotUI()
          )
        ),
        tabPanel(
          "Custom plot",
          div(
            style = "min-height: 760px;",
            customPlotUI()
          )
        )
        # tabPanel("Alfalfa cutting risk")
      )
    )
  })

  ## lat_lng_ui ----
  output$lat_lng_ui <- renderUI({
    loc <- req(rv$loc)
    p(strong("Selected grid:"), sprintf("%.1f°N, %.1f°W", loc$lat, loc$lng))
  })

}
