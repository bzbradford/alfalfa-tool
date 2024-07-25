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

  # initialize module servers ----

  mapServerValues <- mapServer()

  observe({
    print(mapServerValues())
    rv$selected_grid <- mapServerValues()$selected_grid
  })

  observe({
    if (!is.null(rv$selected_grid)) {
      rv$selected_grid_ready <- TRUE
    }
  })


  # MAIN UI --------------------------------------------------------------------

  # this will appear as a spinning loader until weather data ready
  output$main_ui <- renderUI({
    req(rv$weather_ready)

    tagList(
      mapUI(),
      div(
        h3(
          style = "margin-top: 1em;",
          "Weather and climate details"
        ),
        uiOutput("location_ui")
      )
    )
  })

  # download fresh weather data if not up to date
  observe({
    if (!rv$weather_ready) {
      fill_weather()
      rv$weather_ready <- TRUE
    }
  })



  # PLOTS & DATA ---------------------------------------------------------------

  ## Set local data ----

  loc_data <- reactive({
    req(rv$selected_grid)

    loc <- rv$selected_grid

    list(
      weather = weather %>% filter(lat == loc$lat, lng == loc$lng),
      c10 = climate$c10 %>%
        filter(lat == loc$lat, lng == loc$lng) %>%
        mutate(date = start_of_year() + yday - 1),
      c5 = climate$c5 %>%
        filter(lat == loc$lat, lng == loc$lng) %>%
        mutate(date = start_of_year() + yday - 1)
    )
  })


  ## Main UI ----

  output$location_ui <- renderUI({
    validate(need(rv$selected_grid_ready, "Please select a grid cell in the map above to view detailed weather data for that location. Use the crosshair icon on the map to automatically select your location."))

    tagList(
      uiOutput("selected_grid_ui"),
      tabsetPanel(
        tabPanel(
          "Weather plot",
          div(
            style = "min-height: 575px;",
            uiOutput("weather_plot_ui")
          )
        ),
        tabPanel(
          "Climate plot",
          div(
            style = "min-height: 640px;",
            uiOutput("climate_plot_ui")
          )
        ),
        tabPanel(
          "Custom plot",
          div(
            style = "min-height: 760px;",
            uiOutput("custom_plot_ui")
          )
        )
        # tabPanel("Alfalfa cutting risk")
      )
    )
  })

  output$selected_grid_ui <- renderUI({
    loc <- rv$selected_grid
    req(loc)
    p(strong("Selected grid:"), sprintf("%.1f°N, %.1f°W", loc$lat, loc$lng))
  })


  ## Weather plot ----

  output$weather_plot_ui <- renderUI({
    tagList(
      div(class = "option-well",
        radioButtons(
          inputId = "weather_plot_smoothing",
          label = "Data smoothing options",
          choices = OPTS$data_smoothing_choices,
          inline = T
        )
      ),
      plotlyOutput("weather_plot", height = "500px"),
      div(class = "plot-caption", HTML("Click on any legend item in the plot to show or hide it. Weather data originally sourced from NOAA and retrieved from <a href='https://agweather.cals.wisc.edu/'>AgWeather</a>."))
    )
  })

  output$weather_plot <- renderPlotly({
    req(input$weather_plot_smoothing)

    loc <- rv$selected_grid
    w <- as.numeric(input$weather_plot_smoothing)
    plt_title <- case_match(w,
      1 ~ "Weather data",
      7 ~ "7-day average weather data",
      14 ~ "14-day average air temperature data"
    ) %>% sprintf("<b>%s for %.1f°N, %.1f°W</b>", ., loc$lat, loc$lng)
    df <- loc_data()$weather %>%
      mutate(across(
        all_of(OPTS$smoothable_weather),
        ~zoo::rollapply(.x, width = w, FUN = mean, na.rm = T, partial = T)
      ))

    plot_ly() %>%
      layout(
        title = list(
          text = plt_title,
          font = list(
            family = "Lato",
            size = 18)),
        legend = list(
          title = list(
            text = "<b>Plot elements</b>",
            size = 14)),
        xaxis = list(
          title = "Date",
          hoverformat = "%b %d, %Y (day %j)",
          domain = c(0, .95)),
        hovermode = "x unified"
      ) %>%
      add_temp_traces(df, "y1") %>%
      add_gdd_cum_traces(df, "y2")
  })


  ## Climate plot ----

  output$climate_plot_ui <- renderUI({
    tagList(
      div(class = "option-well",
        radioButtons(
          inputId = "climate_plot_period",
          label = "Climate dataset",
          choices = OPTS$climate_period_choices,
          inline = TRUE
        ),
        radioButtons(
          inputId = "climate_plot_smoothing",
          label = "Data smoothing options",
          choices = OPTS$data_smoothing_choices,
          selected = 7,
          inline = T
        )
      ),
      plotlyOutput("climate_plot", height = "500px"),
      div(class = "plot-caption",
        HTML("Click on any legend item in the plot to show or hide it. Today's date is indicated as a vertical dashed line. Climate data sourced from <a href='https://www.climatologylab.org/gridmet.html'>GridMET</a>.")
      )
    )
  })

  output$climate_plot <- renderPlotly({
    req(input$climate_plot_period)
    req(input$climate_plot_smoothing)

    loc <- rv$selected_grid
    p <- input$climate_plot_period
    w <- as.numeric(input$climate_plot_smoothing)
    plt_title <- case_match(w,
      1 ~ "Daily average",
      7 ~ "7-day average",
      14 ~ "14-day average"
    ) %>% paste(case_match(p,
      "c10" ~ "10-year climate data (2013-2023)",
      "c5" ~ "5-year climate data (2018-2023)"
    )) %>% sprintf("<b>%s</b>", .)
    df <- loc_data()[[p]] %>%
      mutate(across(
        all_of(OPTS$smoothable_climate),
        ~zoo::rollapply(.x, width = w, FUN = mean, na.rm = T, partial = T))) %>%
      mutate(date = start_of_year() + yday - 1)

    plot_ly() %>%
      layout(
        title = list(
          text = plt_title,
          font = list(
            family = "Lato",
            size = 18)),
        legend = list(
          title = list(
            text = "<b>Plot elements</b>",
            size = 14)),
        xaxis = list(
          title = "Date",
          dtick = "M1",
          tickformat = "%b",
          hoverformat = "%b %d (day %j)",
          domain = c(0, .95)),
        hovermode = "x unified",
        shapes = list(
          list(
            type = "line",
            x0 = Sys.Date(), x1 = Sys.Date(),
            y0 = 0, y1 = 1, yref = "paper",
            line = list(color = "black", dash = "dot"),
            opacity = .5
          )
        )
      ) %>%
      add_temp_traces(df, "y1") %>%
      add_frost_traces(df, "y2")
  })


  ## Custom plot ----

  output$custom_plot_ui <- renderUI({
    tagList(
      div(
        h4("Primary plot elements"),
        div(class = "option-well",
          div(class = "inline-flex",
            selectInput(
              inputId = "custom_plot_y1_elems",
              label = "Plot data",
              choices = OPTS$custom_plot_elems
            ),
            selectInput(
              inputId = "custom_plot_y1_climate_period",
              label = "Climate period (if applicable)",
              choices = OPTS$climate_period_choices
            ),
            selectInput(
              inputId = "custom_plot_y1_smoothing",
              label = "Data smoothing",
              choices = OPTS$data_smoothing_choices
            ),
          )
        )
      ),
      div(
        h4("Secondary plot elements"),
        div(class = "option-well",
          div(class = "inline-flex",
            selectInput(
              inputId = "custom_plot_y2_elems",
              label = "Plot data",
              choices = append(list("None" = "none"), OPTS$custom_plot_elems)
            ),
            selectInput(
              inputId = "custom_plot_y2_climate_period",
              label = "Climate period (if applicable)",
              choices = OPTS$climate_period_choices
            ),
            selectInput(
              inputId = "custom_plot_y2_smoothing",
              label = "Data smoothing",
              choices = OPTS$data_smoothing_choices
            )
          )
        )
      ),
      br(),
      plotlyOutput("custom_plot", height = "500px"),
      div(class = "plot-caption", HTML("Click on any legend item in the plot to show or hide it. Today's date is indicated as a vertical dashed line. Weather data originally sourced from NOAA and retrieved from <a href='https://agweather.cals.wisc.edu'>AgWeather</a>, climate data sourced from <a href='https://www.climatologylab.org/gridmet.html'>GridMET</a>."))
    )
  })


  output$custom_plot <- renderPlotly({
    req(
      input$custom_plot_y1_elems,
      input$custom_plot_y2_elems,
      input$custom_plot_y1_climate_period,
      input$custom_plot_y2_climate_period,
      input$custom_plot_y1_smoothing,
      input$custom_plot_y2_smoothing
    )

    # capture inputs
    elems <- list(
      y1 = input$custom_plot_y1_elems,
      y2 = input$custom_plot_y2_elems
    )
    i <- list(
      y1 = list(
        data = str_split_1(elems$y1, "_")[1],
        traces = str_split_1(elems$y1, "_")[2],
        climate = input$custom_plot_y1_climate_period,
        smoothing = as.numeric(input$custom_plot_y1_smoothing)
      ),
      y2 = list(
        data = str_split_1(elems$y2, "_")[1],
        traces = str_split_1(elems$y2, "_")[2],
        climate = input$custom_plot_y2_climate_period,
        smoothing = as.numeric(input$custom_plot_y2_smoothing)
      )
    )

    if (identical(i$y1, i$y2)) {
      elems$y2 <- "none"
    }

    # local vars
    loc <- rv$selected_grid
    plt_title <- sprintf("<b>Weather/climate data for %.1f°N, %.1f°W</b>", loc$lat, loc$lng)

    # base plot
    plt <- plot_ly() %>%
      layout(
        title = list(
          text = plt_title,
          font = list(
            family = "Lato",
            size = 18)),
        legend = list(orientation = "h"),
        xaxis = list(
          title = "Date",
          hoverformat = "%b %d, %Y (day %j)"),
        hovermode = "x unified",
        shapes = list(
          list(
            type = "line",
            x0 = Sys.Date(), x1 = Sys.Date(),
            y0 = 0, y1 = 1, yref = "paper",
            line = list(color = "black", dash = "dot"),
            opacity = .5
          )
        )
      )

    # add traces as necessary
    for (axis in c("y1", "y2")) {
      if (elems[[axis]] == "none") next

      opts <- i[[axis]]
      opts$dash <- FALSE
      opts$label <- {
        str <- str_to_sentence(opts$data)
        info <- paste(c(
          if (opts$data == "climate") list(c10 = "10-year", c5 = "5-year")[[opts$climate]],
          if (opts$smoothing != 1) paste0(opts$smoothing, "-day")
        ), collapse = ", ")
        if (info != "") info <- paste0("(", info, ")")
        paste0(str, info, ": ")
      }

      # if the same trace type on both sides so make 2nd dashed and don't repeat axis
      if (axis == "y2" & i$y1$traces == i$y2$traces) {
        opts$dash <- TRUE
        axis <- "y1"
      }

      # apply smoothing
      df <- if (opts$data == "weather") {
        loc_data()$weather %>%
          mutate(across(
            all_of(OPTS$smoothable_weather),
            ~zoo::rollapply(.x, width = opts$smoothing, FUN = mean, na.rm = T, partial = T)
          ))
      } else {
        loc_data()[[opts$climate]] %>%
          mutate(across(
            all_of(OPTS$smoothable_climate),
            ~zoo::rollapply(.x, width = opts$smoothing, FUN = mean, na.rm = T, partial = T)
          ))
      }

      # add traces
      plt <-
        if (opts$traces == "temp") {
          add_temp_traces(plt, df, axis, opts$label, opts$dash)
        } else if (opts$traces == "gdd") {
          add_gdd_daily_traces(plt, df, axis, opts$label, opts$dash)
        } else if (opts$traces == "gddcum") {
          add_gdd_cum_traces(plt, df, axis, opts$label, opts$dash)
        } else if (opts$traces == "frost") {
          add_frost_traces(plt, df, axis)
        } else {
          plt
        }
    }
    plt
  })


}
