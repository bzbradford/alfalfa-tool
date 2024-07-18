
server <- function(input, output, session) {

  OPTS <- list(
    weather_date_min = START_DATE,
    weather_date_max = yesterday(),
    weather_time_fmt = "%b %d, %Y (day %j)",
    climate_date_min = start_of_year(),
    climate_date_max = end_of_year(),
    climate_time_fmt = "%b %d (day %j)"
  )

  # Reactive values ----

  weather_ready <- reactiveVal({
    length(weather_dates()) == 0
  })
  grid_data <- reactiveVal()
  selected_grid <- reactiveVal()
  has_selected_grid <- reactiveVal(FALSE)

  # Primary UI ----

  output$main_ui <- renderUI({
    req(weather_ready())

    tagList(
      sidebarLayout(
        mainPanel = mainPanel(
          leafletOutput("map", width = "100%", height = "720px")
        ),
        sidebarPanel =  sidebarPanel(
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
        ),
        position = "right"
      ),
      br(),
      div(
        h3(
          style = "margin-top: 0;",
          "Weather details and risk recommendation"
        ),
        uiOutput("location_content_ui")
      )
    )
  })

  observe({
    if (!weather_ready()) {
      fill_weather()
      weather_ready(TRUE)
    }
  })

  # Map UI ----

  ## map_opts_ui ----
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

  ## weather_opts_ui ----
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

  ## weather_date_ui ----
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

  ## climate_opts_ui ----
  output$climate_opts_ui <- renderUI({
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
        inputId = "climate_value",
        label = "Data value",
        choices = grid_cols$climate,
        selected = grid_cols$climate[1]
      ),
      uiOutput("climate_date_ui")
    )
  })

  ## climate_opts_ui ----
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

  ## comparison_opts_ui ----
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

  ## date_btns_ui ----
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

  ## date btn handlers ----
  observeEvent(input$date_earlier_1, move_date(-1))
  observeEvent(input$date_earlier_7, move_date(-7))
  observeEvent(input$date_later_1, move_date(1))
  observeEvent(input$date_later_7, move_date(7))
  observeEvent(input$date_reset, move_date(0))

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


  # Map rendering ----

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

  set_grid_fill <- function(.data, opts) {
    vals <- .data$value
    pal <- if (opts$col %in% pct_cols) {
      # percent frost/freeze
      colorNumeric("Blues", c(0, 1), reverse = T)
    } else {
      if (opts$type == "comparison" & opts$col %in% c("min_temp", "max_temp", "gdd41")) {
        # +/- comparisons vs climate. Centered on zero
        colorNumeric("Spectral", c(-max(abs(vals)), max(abs(vals))), reverse = T)
      } else {
        colorNumeric("Spectral", vals, reverse = T)
      }
    }
    .data %>% mutate(fill = pal(value))
  }

  set_grid_labels <- function(.data, opts) {
    cols <- grid_cols[[opts$type]]
    prefix <- setNames(names(cols), cols)[[opts$col]]
    .data %>% mutate(
      label = paste0(
        str_glue("<b>{lat}, {lng}</b><br>"),
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
        mutate(grid_pt = sprintf("%.1f %.1f", lat, lng)) %>%
        select(grid_pt, wx_value)
      cl <- climate[[opts$period]] %>%
        filter(yday == yday(opts$date)) %>%
        rename(c("cl_value" = opts$col)) %>%
        mutate(grid_pt = sprintf("%.1f %.1f", lat, lng)) %>%
        select(lat, lng, grid_pt, cl_value)
      cl %>%
        left_join(wx, join_by(grid_pt)) %>%
        mutate(value = wx_value - cl_value)

    }

    grid %>%
      mutate(grid_pt = sprintf("%.1f %.1f", lat, lng)) %>%
      set_grid_fill(opts) %>%
      set_grid_labels(opts) %>%
      grid_data()
  })


  ## Draw grid ----

  observe({
    map <- leafletProxy("map")
    grid <- grid_data()

    if (is.null(grid)) {
      map %>% clearGroup(layers$grid)
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
        )
    }
  })


  ## Set selected point on map click ----

  observeEvent(input$map_shape_click, {
    id <- input$map_shape_click$id
    req(id)
    if (id == "selected") return()
    coords <- as.numeric(str_split_1(id, " "))
    list(lat = coords[1], lng = coords[2]) %>%
      selected_grid()
    if (!has_selected_grid()) has_selected_grid(TRUE)
  })


  ## Draw selected point on map ----

  observe({
    map <- leafletProxy("map")
    if (is.null(selected_grid())) {
      map %>% removeShape("selected_grid")
    } else {
      pt <- grid_data() %>%
        filter(lat == selected_grid()$lat, lng == selected_grid()$lng)
      map %>%
        removeShape("selected_grid") %>%
        addRectangles(
          data = pt,
          lat1 = ~lat - .05, lat2 = ~lat + .05,
          lng1 = ~lng - .05, lng2 = ~lng + .05,
          group = layers$grid,
          layerId = "selected",
          weight = .5, opacity = 1, color = "black",
          fillOpacity = 0,
          label = ~shiny::HTML(paste("Selected:", label)),
          options = pathOptions(pane = "selected_grid")
        )
    }
  })


  # Draw user location on map ----

  observe({
    req(input$user_loc)

    loc <- input$user_loc
    delay(250, {
      lapply(loc, function(x) round(x, 1)) %>%
        selected_grid()
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


  # Location UI ----

  output$location_content_ui <- renderUI({
    validate(need(has_selected_grid(), "Please select a grid cell in the map above to view detailed weather data for that location. Use the crosshair icon on the map to automatically select your location."))

    tagList(
      uiOutput("selected_grid_ui"),
      tabsetPanel(
        tabPanel(
          "Weather",
          uiOutput("weather_plot_ui") %>% withSpinnerProxy()
        ),
        tabPanel(
          "Climate",
          uiOutput("climate_plot_ui") %>% withSpinnerProxy()
        ),
        tabPanel("Weather vs climate"),
        tabPanel("Alfalfa cutting risk")
      )
    )
  })

  output$selected_grid_ui <- renderUI({
    loc <- selected_grid()
    p(sprintf("Selected grid: %.1f°N, %.1f°W", loc$lat, loc$lng))
  })


  ## Weather plot ----

  output$weather_plot_ui <- renderUI({
    tagList(
      wellPanel(
        radioButtons(
          inputId = "weather_plot_smoothing",
          label = "Temperature smoothing options",
          choices = list(
            "Daily observations (no smoothing)" = 1,
            "Weekly rolling mean" = 7,
            "14-day rolling mean" = 14
          ),
          inline = T
        )
      ),
      plotlyOutput("weather_plot"),
      div(
        class = "plot-caption",
        "Click on any legend item in the plot to show or hide it. Weather data originally sourced from NOAA and retrieved from ", a("AgWeather", href = "https://agweather.cals.wisc.edu", .noWS = "outside"), "."
      )
    )
  })

  output$weather_plot <- renderPlotly({
    req(input$weather_plot_smoothing)

    loc <- selected_grid()
    w <- as.numeric(input$weather_plot_smoothing)
    plt_title <- case_match(w,
      1 ~ "Weather record",
      7 ~ "7-day average weather record",
      14 ~ "14-day average air temperature record"
    ) %>% sprintf("%s for %.1f°N, %.1f°W", ., loc$lat, loc$lng)
    df <- weather %>%
      filter(lat == loc$lat, lng == loc$lng) %>%
      mutate(across(
        c(min_temp, max_temp, mean_temp, gdd41, gdd50),
        ~zoo::rollapply(.x, width = w, FUN = mean, na.rm = T, partial = T)))

    df %>%
      plot_ly() %>%
      add_trace(
        name = "Min temp",
        x = ~date, y = ~min_temp,
        type = "scatter", mode = "lines",
        line = list(color = "cornflowerblue", shape = "spline"),
        hovertemplate = "%{y:.1f}°F",
        yaxis = "y1"
      ) %>%
      add_trace(
        name = "Mean temp",
        x = ~date, y = ~mean_temp,
        type = "scatter", mode = "lines",
        line = list(color = "orange", shape = "spline"),
        hovertemplate = "%{y:.1f}°F",
        yaxis = "y1"
      ) %>%
      add_trace(
        name = "Max temp",
        x = ~date, y = ~max_temp,
        type = "scatter", mode = "lines",
        line = list(color = "#c5050c", shape = "spline"),
        hovertemplate = "%{y:.1f}°F",
        yaxis = "y1"
      ) %>%
      add_trace(
        name = "Frost (<32F)",
        x = ~date, y = ~if_else(min_temp <= 32, 32, NA),
        type = "scatter", mode = "lines",
        line = list(color = "orchid"),
        hovertemplate = "Yes"
      ) %>%
      add_trace(
        name = "Hard freeze (<28F)",
        x = ~date, y = ~if_else(min_temp <= 28, 28, NA),
        type = "scatter", mode = "lines",
        line = list(color = "purple"),
        hovertemplate = "Yes"
      ) %>%
      add_trace(
        name = "GDD41 since Jan 1",
        x = ~date, y = ~gdd41cum,
        type = "scatter", mode = "lines",
        line = list(color = "green"),
        hovertemplate = "%{y:.1f}",
        yaxis = "y2",
        visible = "legendonly"
      ) %>%
      add_trace(
        name = "GDD50 since Jan 1",
        x = ~date, y = ~gdd50cum,
        type = "scatter", mode = "lines",
        line = list(color = "brown"),
        hovertemplate = "%{y:.1f}",
        yaxis = "y2",
        visible = "legendonly"
      ) %>%
      layout(
        title = plt_title,
        xaxis = list(
          title = "Date",
          domain = c(0, .9)),
        yaxis = list(
          title = "Temperature (F)"),
        yaxis2 = list(
          title = "Growing degree days",
          overlaying = "y",
          side = "right"),
        hovermode = "x unified"
      )
  })


  ## Climate plot ----

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
      div(
        class = "plot-caption",
        "Click on any legend item in the plot to show or hide it. Today's date is indicated as a vertical dashed line. Climate data sourced from ", a("GridMET", href = "https://www.climatologylab.org/gridmet.html", .noWS = "outside"), "."
      )
    )
  })

  output$climate_plot <- renderPlotly({
    req(input$climate_plot_period)
    req(input$climate_plot_smoothing)

    loc <- selected_grid()
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
    df <- climate[[p]] %>%
      filter(lat == loc$lat, lng == loc$lng) %>%
      mutate(across(
        c(min_temp, max_temp, mean_temp, frost, freeze, frost_by, freeze_by),
        ~zoo::rollapply(.x, width = w, FUN = mean, na.rm = T, partial = T))) %>%
      mutate(date = start_of_year() + yday - 1)

    df %>%
      plot_ly() %>%
      add_trace(
        name = "Min temp",
        x = ~date, y = ~min_temp,
        type = "scatter", mode = "lines",
        line = list(color = "cornflowerblue", shape = "spline"),
        hovertemplate = "%{y:.1f}°F",
        yaxis = "y1"
      ) %>%
      add_trace(
        name = "Mean temp",
        x = ~date, y = ~mean_temp,
        type = "scatter", mode = "lines",
        line = list(color = "orange", shape = "spline"),
        hovertemplate = "%{y:.1f}°F",
        yaxis = "y1"
      ) %>%
      add_trace(
        name = "Max temp",
        x = ~date, y = ~max_temp,
        type = "scatter", mode = "lines",
        line = list(color = "#c5050c", shape = "spline"),
        hovertemplate = "%{y:.1f}°F",
        yaxis = "y1"
      ) %>%
      add_trace(
        name = "Frost probability",
        x = ~date, y = ~frost*100,
        type = "scatter", mode = "lines",
        line = list(color = "orchid", shape = "spline"),
        hovertemplate = "%{y:.1f}%",
        yaxis = "y2"
      ) %>%
      add_trace(
        name = "Hard freeze probability",
        x = ~date, y = ~freeze*100,
        type = "scatter", mode = "lines",
        hovertemplate = "%{y:.1f}%",
        line = list(color = "purple", shape = "spline"),
        yaxis = "y2"
      ) %>%
      add_trace(
        name = "Cumul. frost prob.",
        x = ~date, y = ~frost_by*100,
        type = "scatter", mode = "lines",
        hovertemplate = "%{y:.1f}%",
        line = list(color = "orchid", shape = "spline", dash = "dot"),
        yaxis = "y2"
      ) %>%
      add_trace(
        name = "Cumul. freeze prob.",
        x = ~date, y = ~freeze_by*100,
        type = "scatter", mode = "lines",
        hovertemplate = "%{y:.1f}%",
        line = list(color = "purple", shape = "spline", dash = "dot"),
        yaxis = "y2"
      ) %>%
      layout(
        title = plt_title,
        xaxis = list(
          title = "Date",
          dtick = "M1",
          tickformat = "%b",
          hoverformat = "%b %d (day %j)",
          domain = c(0, .95)),
        yaxis = list(
          title = "Temperature (F)"),
        yaxis2 = list(
          title = "Frost/freeze probability",
          overlaying = "y",
          side = "right"),
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
  })

}
