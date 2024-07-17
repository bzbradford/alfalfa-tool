
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

  ## grid_data() ----
  grid_data <- reactiveVal()

  ## selected_grid() ----
  selected_grid <- reactiveVal()

  weather_ready <- reactiveVal({
    length(weather_dates()) == 0
  })


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
          label = ~shiny::HTML(label),
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
    validate(need(selected_grid(), "Please select a grid cell in the map above to view detailed weather data and alfalfa cutting analysis."))

    loc <- selected_grid()
    tagList(
      p(sprintf("Selected grid: %.1f°N, %.1f°W", loc$lat, loc$lng)),
      tabsetPanel(
        tabPanel(
          title = "Weather",
          uiOutput("weather_plot_ui")
        ),
        tabPanel(
          title = "Climate",
          uiOutput("climate_plot_ui")
        ),
        tabPanel(
          title = "Weather vs climate",
          uiOutput("comparison_plot_ui")
        ),
        tabPanel(
          title = "Alfalfa cutting risk",
          uiOutput("cutting_risk_ui")
        )
      )
    )
  })


  ## Weather ----

  output$weather_plot_ui <- renderUI({
    tagList(
      wellPanel(
        checkboxGroupInput(
          inputId = "weather_plot_vars",
          label = "Weather data",
          choices = grid_cols$weather,
          selected = grid_cols$weather,
          inline = TRUE
        )
      ),
      plotlyOutput("weather_plot")
    )
  })

  weather_plot_opts <- tribble(
    ~col, ~type, ~mode, ~yaxis,
    "max_temp", "scatter", "lines", "y1",
    "min_temp", "scatter", "lines", "y1",
    "gdd41", "bar", NA, "y2",
    "gdd50", "bar", NA, "y2",
    "gdd41cum", "scatter", "lines", "y3",
    "gdd50cum", "scatter", "lines", "y3",
    "frost", "bar", NA, "y4",
    "freeze", "bar", NA, "y4",
  ) %>% left_join(
    enframe(
      setNames(names(grid_cols$weather), grid_cols$weather),
      name = "col", value = "name"
    )
  )

  ## weather_plot ----
  output$weather_plot <- renderPlotly({
    vars <- input$weather_plot_vars

    df <- weather %>%
      filter(lat == selected_grid()$lat, lng == selected_grid()$lng) %>%
      mutate(across(c(frost, freeze), as.numeric))

    plt <- plot_ly(type = "scatter", mode = "lines") %>%
      layout(
        title = "Weather",
        xaxis = list(title = "Date"),
        yaxis = list(title = "Temperature (F)"),
        yaxis2 = list(
          title = "Daily GDD",
          overlaying = "y",
          side = "right"
        ),
        yaxis3 = list(
          title = "Cumulative GDD",
          overlaying = "y",
          side = "right",
          position = 1
        ),
        yaxis4 = list(
          title = "Frost/Freeze",
          overlaying = "y",
          side = "left",
          position = 0
        ),
        hovermode = "x unified"
      )

    for (row in 1:nrow(weather_plot_opts)) {
      opts <- as.list(slice(weather_plot_opts, row))
      if (opts$col %in% vars) {
        plt <- plt %>% add_trace(
          x = df$date, y = df[[opts$col]],
          type = opts$type,
          mode = { if (!is.na(opts$mode)) opts$mode },
          name = opts$name,
          yaxis = opts$yaxis
        )
      }
    }

    plt
  })

}
