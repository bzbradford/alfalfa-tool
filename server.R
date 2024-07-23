#- server.R -#

server <- function(input, output, session) {

  OPTS <- list(
    weather_date_min = START_DATE,
    weather_date_max = yesterday(),
    weather_time_fmt = "%b %d, %Y (day %j)",
    climate_date_min = start_of_year(),
    climate_date_max = end_of_year(),
    climate_time_fmt = "%b %d (day %j)"
  )

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
        inputId = "weather_value",
        label = "Data value",
        choices = grid_cols$weather,
        selected = grid_cols$weather[1]
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
        choices = grid_cols$climate,
        selected = grid_cols$climate[1]
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
        choices = list(
          "10-year climate average (2013-2023)" = "c10",
          "5-year climate average (2018-2023)" = "c5"
        )
      ),
      radioButtons(
        inputId = "comparison_value",
        label = "Data value",
        choices = grid_cols$comparison,
        selected = grid_cols$comparison[1]
      ),
      uiOutput("weather_date_ui")
    )
  })


  ## Date selector ----

  ### Weather/Comparison date ----

  output$weather_date_ui <- renderUI({
    sliderInput(
      inputId = "weather_date",
      label = "Date",
      min = START_DATE,
      max = yesterday(),
      value = coalesce(input$weather_date, max(weather$date)),
      timeFormat = OPTS$weather_time_fmt
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
      timeFormat = OPTS$climate_time_fmt
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
    if (input$map_data_type == "climate") {
      id <- "climate_date"
      value <- clamp(
        input$climate_date + step,
        OPTS$climate_date_min,
        OPTS$climate_date_max
      )
      fmt <- OPTS$climate_time_fmt
    } else {
      id <- "weather_date"
      value <- clamp(
        input$weather_date + step,
        OPTS$weather_date_min,
        OPTS$weather_date_max
      )
      fmt <- OPTS$weather_time_fmt
    }
    if (step == 0) value <- yesterday()
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
  observeEvent(input$date_reset, move_date(0))



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
    cols <- grid_cols[[opts$type]]
    prefix <- setNames(names(cols), cols)[[opts$col]]
    .data %>% mutate(
      label = paste0(
        "<b>",
        if (opts$type == "weather" & opts$col %in% c("frost", "freeze")) {
          sprintf("%s: %s", prefix, value)
        } else if (opts$col %in% pct_cols) {
          sprintf("%s: %.1f%%", prefix, value * 100)
        } else if (opts$type == "comparison") {
          sprintf("%s: %+.1f", prefix, value)
        } else {
          sprintf("%s: %.1f", prefix, value)
        },
        "</b><br>",
        str_glue("Location: {lat}°N, {lng}°W")
      )
    )
  }


  ## Assign grid data ----

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
        filter(date == opts$date) %>%
        rename(c("value" = opts$col)) %>%
        select(lat, lng, value)

    } else if (opts$type == "climate") {

      req(input$climate_period)
      req(input$climate_value)
      req(input$climate_date)

      opts$period <- input$climate_period
      opts$col <- input$climate_value
      opts$date <- input$climate_date

      climate[[opts$period]] %>%
        filter(yday == yday(opts$date)) %>%
        rename(c("value" = opts$col)) %>%
        select(lat, lng, value)

    } else if (opts$type == "comparison") {

      req(input$climate_period)
      req(input$comparison_value)
      req(input$weather_date)

      opts$period <- input$climate_period
      opts$col <- input$comparison_value
      opts$date <- input$weather_date

      wx <- weather %>%
        filter(date == opts$date) %>%
        rename(c("wx_value" = opts$col)) %>%
        select(lat, lng, wx_value)
      cl <- climate[[opts$period]] %>%
        filter(yday == yday(opts$date)) %>%
        rename(c("cl_value" = opts$col)) %>%
        select(lat, lng, cl_value)
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

  custom_plot_elems <- list(
    "Weather - Temperature" = "weather_temp",
    "Weather - GDD/day" = "weather_gdd",
    "Weather - Cumulative GDD" = "weather_gddcum",
    "Climate - Temperature" = "climate_temp",
    "Climate - GDD/day" = "climate_gdd",
    "Climate - Cumulative GDD" = "climate_gddcum",
    "Climate - Frost/Freeze probability" = "climate_frost"
  )

  smoothable_weather <- c("min_temp", "max_temp", "mean_temp", "gdd41", "gdd50", "gdd41cum", "gdd50cum")
  smoothable_climate <- c(smoothable_weather, "frost", "freeze", "frost_by", "freeze_by")


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
        tabPanel("Weather plot", uiOutput("weather_plot_ui")),
        tabPanel("Climate plot", uiOutput("climate_plot_ui")),
        tabPanel("Custom plot", uiOutput("custom_plot_ui")),
        tabPanel("Alfalfa cutting risk")
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
      wellPanel(
        add_smoothing_ui("weather_plot_smoothing"),
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
      1 ~ "Weather record",
      7 ~ "7-day average weather record",
      14 ~ "14-day average air temperature record"
    ) %>% sprintf("%s for %.1f°N, %.1f°W", ., loc$lat, loc$lng)
    df <- loc_data()$weather %>%
      mutate(across(
        all_of(smoothable_weather),
        ~zoo::rollapply(.x, width = w, FUN = mean, na.rm = T, partial = T)
      ))

    plot_ly() %>%
      layout(
        title = plt_title,
        xaxis = list(
          title = "Date",
          domain = c(0, .9)),
        hovermode = "x unified"
      ) %>%
      add_temp_traces(df, "y1") %>%
      add_gdd_cum_traces(df, "y2")
  })



  ## climate_plot_ui ----
  output$climate_plot_ui <- renderUI({
    tagList(
      wellPanel(
        radioButtons(
          inputId = "climate_plot_period",
          label = "Climate dataset",
          choices = list(
            "10-year climate (2013-2023)" = "c10",
            "5-year climate (2018-2023)" = "c5"),
          inline = TRUE
        ),
        radioButtons(
          inputId = "climate_plot_smoothing",
          label = "Temperature smoothing options",
          choices = list(
            "Daily climate average" = 1,
            "Weekly rolling mean" = 7,
            "14-day rolling mean" = 14),
          selected = 7,
          inline = T
        ),
        uiOutput("plot_smoothing_opts_ui")
      ),
      plotlyOutput("climate_plot"),
      div(class = "plot-caption",
        HTML("Click on any legend item in the plot to show or hide it. Today's date is indicated as a vertical dashed line. Climate data sourced from <a href='https://www.climatologylab.org/gridmet.html'>GridMET</a>.")
      )
    )
  })

  ## climate_plot ----
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
      "c10" ~ "10-year climate record (2013-2023)",
      "c5" ~ "5-year climate record (2018-2023)"
    ))
    df <- loc_data()[[p]] %>%
      mutate(across(
        all_of(smoothable_climate),
        ~zoo::rollapply(.x, width = w, FUN = mean, na.rm = T, partial = T))) %>%
      mutate(date = start_of_year() + yday - 1)

    plot_ly() %>%
      layout(
        title = plt_title,
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
      wellPanel(
        fluidRow(
          column(
            width = 6,
            radioButtons(
              inputId = "custom_plot_y1_elems",
              label = "Primary plot elements",
              choices = custom_plot_elems
            ),
            add_climate_period_ui("custom_plot_y1_climate_period"),
            add_smoothing_ui("custom_plot_y1_smoothing", inline = F)
          ),
          column(
            width = 6,
            radioButtons(
              inputId = "custom_plot_y2_elems",
              label = "Secondary plot elements",
              choices = append(
                list("None" = "none"),
                custom_plot_elems
              )
            ),
            add_climate_period_ui("custom_plot_y2_climate_period"),
            add_smoothing_ui("custom_plot_y2_smoothing", inline = F)
          )
        )
      ),
      plotlyOutput("custom_plot_plot"),
      div(
        class = "plot-caption",
        "Click on any legend item in the plot to show or hide it. Weather data originally sourced from NOAA and retrieved from ", a("AgWeather", href = "https://agweather.cals.wisc.edu", .noWS = "outside"), "."
      )
    )
  })

  output$custom_plot_plot <- renderPlotly({
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
    print(i)
    if (identical(i$y1, i$y2)) {
      elems$y2 <- "none"
    }

    # local vars
    loc <- rv$selected_grid
    plt_title <- sprintf("Weather/climate data for %.1f°N, %.1f°W", loc$lat, loc$lng)

    # base plot
    plt <- plot_ly() %>%
      layout(
        title = plt_title,
        xaxis = list(
          title = "Date",
          domain = c(0, .9)),
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

    for (axis in c("y1", "y2")) {
      if (elems[[axis]] == "none") next

      opts <- i[[axis]]
      opts$dash <- FALSE
      opts$label <- str_to_sentence(opts$data)

      # if the same trace type on both sides so make 2nd dashed and don't repeat axis
      if (axis == "y2" & i$y1$traces == i$y2$traces) {
        opts$dash <- TRUE
        axis <- "y1"
      }

      df <- if (opts$data == "weather") {
        loc_data()$weather %>%
          mutate(across(
            all_of(smoothable_weather),
            ~zoo::rollapply(.x, width = opts$smoothing, FUN = mean, na.rm = T, partial = T)
          ))
      } else {
        loc_data()[[opts$climate]] %>%
          mutate(across(
            all_of(smoothable_climate),
            ~zoo::rollapply(.x, width = opts$smoothing, FUN = mean, na.rm = T, partial = T)
          ))
      }

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
