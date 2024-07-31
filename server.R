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
  timingServer(reactive(loc_data()))


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
      showModal(modalDialog(msg, title = strong("Loading weather..."), fade = F, footer = NULL))
      fill_weather()
      gc()
      rv$weather_ready <- TRUE
      removeModal()
    }

    fluidRow(
      column(6,
        h3("Gridded weather and climate map"),
        mapUI(),
      ),
      column(6, uiOutput("sidebar_ui"))
    )
  })

  output$sidebar_ui <- renderUI({
    page <- req(input$navbar)

    side <- if (page == "Weather Map") {
      tagList(
        h3("Map display options"),
        mapSidebarUI()
      )
    } else if (page == "Weather Charts") {
      tagList(
        h3("Weather charts"),
        uiOutput("plots_ui")
      )
    } else if (page == "Timing Tool") {
      tagList(
        h3("Alfalfa cut timing tools"),
        timingUI()
      )
    } else if (page == "About") {
      tagList(
        h3("About this app"),
        uiOutput("about_ui")
      )
    }
  })


  ## plots_ui ----
  output$plots_ui <- renderUI({
    if (!rv$loc_ready) {
      tagList(
        p("Please select a location on the map to view detailed weather and climate charts.")
      )
    } else {
      tagList(
        # uiOutput("lat_lng_ui"),
        tabsetPanel(
          tabPanel("Weather plot", weatherPlotUI()),
          tabPanel("Climate plot", climatePlotUI()),
          tabPanel("Custom plot", customPlotUI()),
          # tabPanel("Alfalfa cutting risk")
        )
      )
    }
    # validate(need(rv$loc_ready, "Please select a grid cell on the map to view detailed weather data for that location. Use the crosshair icon on the map to automatically select your location."))
  })

  ## lat_lng_ui ----
  output$lat_lng_ui <- renderUI({
    loc <- req(rv$loc)
    p(strong("Selected grid:"), sprintf("%.1f°N, %.1f°W", loc$lat, loc$lng))
  })

  ## about_ui ----
  output$about_ui <- renderUI({
    p(em("Currently this tool allows for browsing current-year weather data, 5- and 10-year climate averages, and comparing the two. Ultimately, it will help alfalfa growers time their last fall cutting while understanding the probable timing of a fall killing freeze."))
  })

}
