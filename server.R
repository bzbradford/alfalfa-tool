#- server.R -#

server <- function(input, output, session) {

  OPTS$weather_date_max = yesterday()
  OPTS$climate_date_min = start_of_year()
  OPTS$climate_date_max = end_of_year()


  # Reactive values ----

  rv <- reactiveValues(

    # false if new weather data needs downloading
    weather_ready = length(weather_dates()) == 0,

    selected_grid = NULL,
    selected_grid_ready = FALSE

  )

  # Initialize module servers ----

  ## map server ----
  mapServerValues <- mapServer()

  observe({
    rv$selected_grid <- mapServerValues()$selected_grid
  })

  observe({
    if (is.null(rv$selected_grid)) return()
    rv$selected_grid_ready <- TRUE
  })


  ## plot server ----
  plotServer(selected_grid = reactive(rv$selected_grid))


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
    validate(need(rv$selected_grid_ready, "Please select a grid cell in the map above to view detailed weather data for that location. Use the crosshair icon on the map to automatically select your location."))

    tagList(
      uiOutput("selected_grid_ui"),
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

  ## selected_grid_ui ----
  output$selected_grid_ui <- renderUI({
    loc <- req(rv$selected_grid)
    p(strong("Selected grid:"), sprintf("%.1f°N, %.1f°W", loc$lat, loc$lng))
  })

}
