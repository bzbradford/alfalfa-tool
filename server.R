#- server.R -#

server <- function(input, output, session) {

  OPTS$weather_date_max = yesterday()
  OPTS$climate_date_min = start_of_year()
  OPTS$climate_date_max = end_of_year()


  # Reactive values ------------------------------------------------------------

  rv <- reactiveValues(

    # false if new weather data needs downloading
    weather_ready = length(weather_dates()) == 0,

    # false until any grid point is selected
    selection_ready = FALSE,

    # lat/lng of selected grid cell
    selected_grid = NULL,

    # dataset for map grid
    grid_data = NULL,

    # palette used for map grid
    grid_pal = NULL,

    date_reset_nonce = rnorm(1)
  )


  # MAIN UI --------------------------------------------------------------------

  # this will appear as a spinning loader until weather data ready
  output$main_ui <- renderUI({
    req(rv$weather_ready)

    tagList(
      fluidRow(
        column(8,
          leafletOutput("map", width = "100%", height = "720px")
        ),
        column(4,
          div(
            class = "well", style = "margin-bottom: 0;",
            radioButtons(
              inputId = "map_data_type",
              label = "Choose data layer",
              choices = list(
                "Current weather" = "weather",
                "Climate normals" = "climate",
                "Weather vs climate" = "comparison"
              )
            ),
            uiOutput("map_opts_ui")
          )
        )
      ),
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


  # MAP OPTIONS UI -------------------------------------------------------------

  ## Map data type selector ----
  output$map_opts_ui <- renderUI({
    req(input$map_data_type)
    type <- input$map_data_type

    leafletProxy("map") %>%
      clearGroup(layers$grid)

    tagList(
      if (type == "weather") {
        uiOutput("weather_opts_ui")
      } else if (type == "climate") {
        uiOutput("climate_opts_ui")
      } else if (type == "comparison") {
        uiOutput("comparison_opts_ui")
      },
      uiOutput("date_btns_ui")
    )
  })


  ## Weather options ----

  output$weather_opts_ui <- renderUI({
    req(input$map_data_type)

    tagList(
      radioButtons(
        inputId = "weather_year",
        label = "Weather year",
        choices = list(cur_yr, cur_yr - 1),
        inline = T
      ),
      radioButtons(
        inputId = "weather_value",
        label = "Data value",
        choices = OPTS$grid_cols$weather
      ),
      uiOutput("weather_date_ui")
    )
  })


  ## Climate options ----

  output$climate_opts_ui <- renderUI({
    req(input$map_data_type)

    tagList(
      add_climate_period_ui("climate_period"),
      radioButtons(
        inputId = "climate_value",
        label = "Data value",
        choices = OPTS$grid_cols$climate
      ),
      uiOutput("climate_date_ui")
    )
  })


  ## Comparison options ----

  output$comparison_opts_ui <- renderUI({
    req(input$map_data_type)

    tagList(
      radioButtons(
        inputId = "climate_period",
        label = "Climate period",
        choices = OPTS$climate_period_choices
      ),
      radioButtons(
        inputId = "comparison_value",
        label = "Data value",
        choices = OPTS$grid_cols$comparison
      ),
      uiOutput("weather_date_ui")
    )
  })


  ## Date selector ----

  ### Weather/Comparison date ----

  output$weather_date_ui <- renderUI({
    req(rv$date_reset_nonce)
    req(input$weather_year)
    req(input$weather_value)

    prev_end <- isolate({
      last(coalesce(input$weather_date, max(weather$date)))
    })
    min_date <- make_date(input$weather_year, 1, 1)
    max_date <- min(make_date(input$weather_year, 12, 31), yesterday())
    end_value <- min(min_date + yday(prev_end) - 1, yesterday())
    value <- if (input$weather_value %in% c("gdd41cum", "gdd50cum")) {
      start_value <- isolate({
        if (length(input$weather_date) == 2) {
          min_date + yday(input$weather_date[1]) - 1
        } else {
          min_date
        }
      })
      c(start_value, end_value)
    } else {
      end_value
    }

    sliderInput(
      inputId = "weather_date",
      label = "Date",
      min = min_date,
      max = max_date,
      value = value,
      timeFormat = OPTS$weather_date_fmt
    )
  })


  ### Climate date ----

  output$climate_date_ui <- renderUI({
    sliderInput(
      inputId = "climate_date",
      label = "Date",
      min = start_of_year(),
      max = end_of_year(),
      value = coalesce(input$climate_date, max(weather$date)),
      timeFormat = OPTS$climate_date_fmt
    )
  })


  ### Date adjustment buttons ----

  output$date_btns_ui <- renderUI({
    req(input$map_data_type)

    tagList(
      tags$label(
        "for" = "date-btns",
        "Date adjustment"
      ),
      div(
        class = "date-btns",
        id = "date-btns",
        actionButton("date_earlier_7", "-7"),
        actionButton("date_earlier_1", "-1"),
        actionButton("date_later_1", "+1"),
        actionButton("date_later_7", "+7"),
        actionButton("date_reset", "Reset"),
      )
    )
  })

  move_date <- function(step) {
    req(input$map_data_type)

    if (input$map_data_type == "climate") {
      id <- "climate_date"
      value <- clamp(
        input$climate_date + step,
        OPTS$climate_date_min,
        OPTS$climate_date_max
      )
      fmt <- OPTS$climate_date_fmt
    } else {
      id <- "weather_date"
      dt <- input$weather_date
      value <- clamp(
        last(dt) + step,
        start_of_year(dt),
        min(end_of_year(dt), OPTS$weather_date_max)
      )
      if (length(dt) == 2) value <- c(dt[1], value)
      fmt <- OPTS$weather_date_fmt
    }
    updateSliderInput(
      inputId = id,
      value = value,
      timeFormat = fmt
    )
  }

  reset_date <- function() {
    req(input$map_data_type)

    value <- if (input$map_data_type == "climate") {
      id <- "climate_date"
      max(weather$date)
      fmt <- OPTS$climate_date_fmt
    } else {
      id <- "weather_date"
      fmt <- OPTS$weather_date_fmt
      if (length(input$weather_date) == 2) {
        c(
          start_of_year(input$weather_date[1]),
          start_of_year(input$weather_date[1]) + yday(yesterday()) - 1
        )
      } else {
        start_of_year(input$weather_date) + yday(yesterday()) - 1
      }
    }

    updateSliderInput(
      inputId = id,
      value = value,
      timeFormat = fmt
    )
  }

  observeEvent(input$date_earlier_1, move_date(-1))
  observeEvent(input$date_earlier_7, move_date(-7))
  observeEvent(input$date_later_1, move_date(1))
  observeEvent(input$date_later_7, move_date(7))
  observeEvent(input$date_reset, reset_date())



  # MAP RENDERING --------------------------------------------------------------

  basemaps <- tribble(
    ~label, ~provider,
    "ESRI Topo", providers$Esri.WorldTopoMap,
    "Satellite", providers$Esri.WorldImagery,
    "OpenStreetMap", providers$OpenStreetMap,
    "Grey Canvas", providers$CartoDB.Positron
  )

  addBasemaps <- function(map) {
    for (r in 1:nrow(basemaps)) {
      df <- slice(basemaps, r)
      map <- addProviderTiles(map, df$provider, group = df$label)
    }
    map
  }

  layers <- list(
    counties = "Counties/Regions",
    grid = "Data grid"
  )


  ## Initialize map ----

  output$map <- renderLeaflet({
    leaflet() %>%
      fitBounds(
        lat1 = 42.4,
        lat2 = 47.1,
        lng1 = -92.9,
        lng2 = -86.8
      ) %>%
      addBasemaps() %>%
      addMapPane("counties", 410) %>%
      addMapPane("grid", 420) %>%
      addMapPane("selected_grid", 430) %>%
      addLayersControl(
        baseGroups = basemaps$label,
        overlayGroups = unlist(layers, use.names = F),
        options = layersControlOptions(collapsed = F)
      ) %>%
      addFullscreenControl(pseudoFullscreen = T) %>%
      addEasyButtonBar(
        easyButton(
          position = "topleft",
          icon = "fa-crosshairs",
          title = "Show my location on the map",
          onClick = JS("
            function(btn, map) {
              map.locate({
                setView: true,
                enableHighAccuracy: false,
                maxZoom: 10
              }).on('locationfound', (event) => {
                Shiny.setInputValue('user_loc', event.latlng, {priority: 'event'})
              })
            }
          ")
        ),
        easyButton(
          position = "topleft",
          icon = "fa-globe",
          title = "Reset map view",
          onClick = JS("
            function(btn, map) {
              map.fitBounds([[47.1, -86.8], [42.4, -92.9]])
            }
          ")
        )
      ) %>%
      suspendScroll(
        sleepTime = 0,
        wakeTime = 1000,
        hoverToWake = T,
        sleepNote = F,
        sleepOpacity = 1
      ) %>%
      addPolygons(
        data = counties,
        group = layers$counties,
        label = ~ lapply(paste0("<b>", CountyName, " County</b><br>", DnrRegion), HTML),
        fillOpacity = 0.1,
        color = "grey",
        opacity = 0.5,
        fillColor = ~ colorFactor("Dark2", counties$DnrRegion)(DnrRegion),
        weight = 1,
        options = pathOptions(pane = "counties")
      )
  })


  ## Hide the legend ----

  observeEvent(TRUE, {
    delay(3000, {
      leafletProxy("map") %>%
        addLayersControl(
          baseGroups = basemaps$label,
          overlayGroups = unlist(layers, use.names = FALSE),
        )
    })
  })


  ## Set grid data ----

  pct_cols <- c("frost", "freeze", "frost_by", "freeze_by")

  # set grid color palette
  set_grid_fill <- function(.data, opts) {
    comp_cols <- c("min_temp", "max_temp", "gdd41", "gdd50")
    vals <- .data$value
    pal <- if (opts$col %in% pct_cols) {
      # percent frost/freeze
      colorNumeric("Blues", c(0, 1), reverse = T)
    } else {
      if (opts$type == "comparison" & opts$col %in% comp_cols) {
        # +/- comparisons vs climate. Centered on zero
        colorNumeric("Spectral", c(-max(abs(vals)), max(abs(vals))), reverse = T)
      } else {
        colorNumeric("Spectral", vals, reverse = T)
      }
    }
    rv$grid_pal <- pal
    .data %>%
      mutate(fill = pal(value))
  }

  # set grid labels
  # TODO: add other data to label?
  set_grid_labels <- function(.data, opts) {
    cols <- OPTS$grid_cols[[opts$type]]
    prefix <- setNames(names(cols), cols)[[opts$col]]
    .data %>% mutate(
      label = paste0(
        str_glue("<b>{lat}°N, {lng}°W</b><br>"),
        if (opts$type == "weather" & opts$col %in% c("frost", "freeze")) {
          sprintf("%s: %s", prefix, value)
        } else if (opts$col %in% pct_cols) {
          sprintf("%s: %.1f%%", prefix, value * 100)
        } else if (opts$type == "comparison") {
          sprintf("%s: %+.1f", prefix, value)
        } else {
          sprintf("%s: %.1f", prefix, value)
        }
      )
    )
  }


  ## Assign grid data ----

  prepare_weather_grid_data <- function(df, col, dt) {
    if (length(dt) == 1) {
      df %>%
        filter(date == dt) %>%
        rename(c("value" = col)) %>%
        select(lat, lng, value)
    } else if (length(dt) == 2) {
      df1 <- df %>%
        filter(date == dt[1]) %>%
        rename(c("value1" = col)) %>%
        select(lat, lng, value1)
      df2 <- df %>%
        filter(date == dt[2]) %>%
        rename(c("value2" = col)) %>%
        select(lat, lng, value2)
      left_join(df1, df2, join_by(lat, lng)) %>%
        mutate(value = value2 - value1) %>%
        select(lat, lng, value)
    }
  }

  prepare_climate_grid_data <- function(df, col, dt) {
    if (length(dt) == 1) {
      df %>%
        filter(yday == yday(dt)) %>%
        rename(c("value" = col)) %>%
        select(lat, lng, value)
    } else if (length(dt) == 2) {
      df1 <- df %>%
        filter(yday == yday(dt[1])) %>%
        rename(c("value1" = col)) %>%
        select(lat, lng, value1)
      df2 <- df %>%
        filter(yday == yday(dt[2])) %>%
        rename(c("value2" = col)) %>%
        select(lat, lng, value2)
      left_join(df1, df2, join_by(lat, lng)) %>%
        mutate(value = value2 - value1) %>%
        select(lat, lng, value)
    }
  }

  observe({
    req(input$map_data_type)

    opts = list(
      type = input$map_data_type
    )

    # set grid data
    grid <- if (opts$type == "weather") {

      req(input$weather_value)
      req(input$weather_date)

      opts$col <- input$weather_value
      opts$date <- input$weather_date

      weather %>%
        prepare_weather_grid_data(opts$col, opts$date)

    } else if (opts$type == "climate") {

      req(input$climate_period)
      req(input$climate_value)
      req(input$climate_date)

      opts$period <- input$climate_period
      opts$col <- input$climate_value
      opts$date <- input$climate_date

      climate[[opts$period]] %>%
        prepare_climate_grid_data(opts$col, opts$date)

    } else if (opts$type == "comparison") {

      req(input$climate_period)
      req(input$comparison_value)
      req(input$weather_date)

      opts$period <- input$climate_period
      opts$col <- input$comparison_value
      opts$date <- input$weather_date

      wx <- weather %>%
        prepare_weather_grid_data(opts$col, opts$date) %>%
        rename(c(wx_value = value))

      cl <- climate[[opts$period]] %>%
        prepare_climate_grid_data(opts$col, opts$date) %>%
        rename(c(cl_value = value))

      cl %>%
        left_join(wx, join_by(lat, lng)) %>%
        mutate(value = wx_value - cl_value)
    }

    rv$grid_data <- grid %>%
      mutate(grid_pt = coords_to_pt(lat, lng)) %>%
      set_grid_fill(opts) %>%
      set_grid_labels(opts)
  })


  ## Draw grid ----

  observe({
    map <- leafletProxy("map")
    grid <- rv$grid_data

    if (is.null(grid)) {
      map %>%
        clearGroup(layers$grid) %>%
        removeControl("legend")
    } else {
      map %>%
        addRectangles(
          data = grid,
          lat1 = ~lat - .05, lat2 = ~lat + .05,
          lng1 = ~lng - .05, lng2 = ~lng + .05,
          group = layers$grid,
          layerId = ~grid_pt,
          weight = 0,
          fillOpacity = .75,
          fillColor = ~fill,
          label = ~lapply(label, shiny::HTML),
          options = pathOptions(pane = "grid")
        ) %>%
        addLegend(
          layerId = "legend",
          position = "bottomright",
          pal = rv$grid_pal,
          bins = 5,
          values = grid$value
        )
    }
  })


  ## Set selected point on map click ----

  observeEvent(input$map_shape_click, {
    id <- input$map_shape_click$id
    req(id)
    if (id == "selected") return()
    rv$selected_grid <- pt_to_coords(id)
  })


  ## Draw selected grid on map ----

  observe({
    map <- leafletProxy("map")

    if (is.null(rv$selected_grid)) {
      map %>% removeShape("selected_grid")
      return()
    }

    if (!rv$selection_ready) { rv$selection_ready <- TRUE }

    loc <- rv$selected_grid
    map %>%
      removeShape("selected_grid") %>%
      addRectangles(
        data = rv$grid_data %>%
          filter(lat == loc$lat, lng == loc$lng),
        lat1 = ~lat - .05, lat2 = ~lat + .05,
        lng1 = ~lng - .05, lng2 = ~lng + .05,
        group = layers$grid,
        layerId = "selected",
        weight = .5, opacity = 1, color = "black",
        fillOpacity = 0,
        label = ~shiny::HTML(paste0(label, "<br><i>This location is shown in the charts below.</i>")),
        options = pathOptions(pane = "selected_grid")
      )
  })


  ## Draw user location on map ----

  observe({
    req(input$user_loc)

    loc <- input$user_loc
    loc_grid <- lapply(loc, function(x) round(x, 1))

    delay(250, {
      rv$selected_grid <- loc_grid
      leafletProxy("map") %>%
        addMarkers(
          lat = loc$lat, lng = loc$lng,
          layerId = "user_loc",
          label = HTML(str_glue("
          <b>Your location</b><br>
          Latitude: {loc$lat}<br>
          Longitude: {loc$lng}<br>
          <i>Click to remove marker.</i>
        "))
        )
    })
  })

  observeEvent(input$map_marker_click, {
    req(input$map_marker_click$id == "user_loc")
    leafletProxy("map") %>%
      removeMarker("user_loc")
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
    validate(need(rv$selection_ready, "Please select a grid cell in the map above to view detailed weather data for that location. Use the crosshair icon on the map to automatically select your location."))

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
      plotlyOutput("weather_plot"),
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
      plotlyOutput("climate_plot"),
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
      plotlyOutput("custom_plot"),
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
        legend = list(
          title = list(
            text = "<b>Plot elements</b>",
            size = 14)),
        xaxis = list(
          title = "Date",
          hoverformat = "%b %d, %Y (day %j)",
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
        paste(str, info, "- ")
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
