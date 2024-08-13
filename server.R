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
        plotUI()
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


  ## about_ui ----
  output$about_ui <- renderUI({
    tagList(
      h4("Gridded weather and climate map"),
      p(HTML("Weather and climate data are presented on a 0.1 decimal degree grid (grid cells measure approximately 5 x 7 miles), with weather data originally sourced from NOAA and retrieved from <a href='https://agweather.cals.wisc.edu'>AgWeather</a> and climate data sourced from <a href='https://www.climatologylab.org/gridmet.html'>GridMET</a>. Clicking a grid cell will select that location for viewing in the charts and tools tabs.")),
      h4("Weather and climate charts"),
      p("To view detailed daily weather and climate data, you must first select a location on the map and switch to the charts tab. There are three types of charts, one focused on observed weather for this year and last, one for exploring climate averages, and a third with many options for customization. Elements are added to the charts in groups, but individual lines may be shown or hidden by clicking on items in the chart legend."),
      h4("Alfalfa cut timing tool"),
      p("Traditionally, timing alfalfa cuttings is done based on a calendar or a grower's knowledge or experience. This tool helps to plan or evaluate a cutting schedule based on actual and projected growing degree days to identify optimal cut timing for plant health. During the growing season, alfalfa should be cut when between 900 and 1100 growing degree days (base 41°F) have elapsed since Jan 1 or the last time the crop was cut. In the fall, the last cut should be scheduled such that either the crop has enough time to reach maturity again before a killing freeze, or the crop has very little time (<360 GDD) before the first killing freeze."),
      h4("More information"),
      tags$ul(
        tags$li(a("UW Extension Crops and Soils - Alfalfa", href = "https://cropsandsoils.extension.wisc.edu/article-topic/alfalfa/")),
        tags$li(a("Scott Newell - Alfalfa Outreach Specialist", href = "mailto:scott.newell@wisc.edu"))
      )
    )
  })
}
