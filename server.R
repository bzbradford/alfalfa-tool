
server <- function(input, output, session) {

  # Vars ----

  ## grid_cols ----
  grid_cols <- list(
    weather = list(
      "Max daily temp (F)" = "max_temp",
      "Min daily temp (F)" = "min_temp",
      "Daily GDD41 accumulation" = "gdd41",
      "Cumulative GDD41 since Jan 1" = "gdd41cum",
      "Daily GDD50 accumulation" = "gdd50",
      "Cumulative GDD50 since Jan 1" = "gdd50cum",
      "Frost (<32F) this day" = "frost",
      "Hard freeze (<28F) this day" = "freeze"
    ),
    climate = list(
      "Mean daily max temp (F)" = "max_temp",
      "Mean daily min temp (F)" = "min_temp",
      "Mean daily GDD41" = "gdd41",
      "Mean cumulative GDD41" = "gdd41cum",
      "Mean daily GDD50" = "gdd50",
      "Mean cumulative GDD50" = "gdd50cum",
      "Mean probability of frost on day" = "frost",
      "Mean probability of hard freeze on day" = "freeze",
      "Cumulative probability of frost" = "frost_by",
      "Cumulative probability of hard freeze" = "freeze_by"
    ),
    comparison = list(
      "Max daily temp vs climate average (F)" = "max_temp",
      "Min daily temp vs climate average (F)" = "min_temp",
      "Daily GDD41 vs climate average" = "gdd41",
      "Cumul. GDD41 vs climate average" = "gdd41cum",
      "Daily GDD50 vs climate average" = "gdd50",
      "Cumul. GDD50 vs climate average" = "gdd50cum"
    )
  )


  # Reactive values ----

  ## grid_data() ----
  grid_data <- reactiveVal()

  ## selected_grid() ----
  selected_grid <- reactiveVal()


  # Map UI ----

  ## output$map_opts_ui ----
  output$map_opts_ui <- renderUI({
    req(input$map_data_type)
    type <- input$map_data_type

    leafletProxy("map") %>%
      clearGroup(layers$grid)

    if (type == "weather") {
      uiOutput("weather_opts_ui")
    } else if (type == "climate") {
      uiOutput("climate_opts_ui")
    } else if (type == "comparison") {
      uiOutput("comparison_opts_ui")
    }
  })

  ## output$weather_opts_ui ----
  output$weather_opts_ui <- renderUI({
    req(input$map_data_type)

    isolate({
      date_value <- if (!is.null(input$weather_date)) {
        input$weather_date
      } else if (!is.null(input$climate_date)) {
        input$climate_date
      } else {
        yesterday()
      }
    })

    tagList(
      radioButtons(
        inputId = "weather_value",
        label = "Data value",
        choices = grid_cols$weather,
        selected = grid_cols$weather[1]
      ),
      sliderInput(
        inputId = "weather_date",
        label = "Date",
        min = START_DATE,
        max = yesterday(),
        value = date_value,
        timeFormat = "%b %d, %Y (day %j)"
      )
    )
  })

  ## output$climate_opts_ui ----
  output$climate_opts_ui <- renderUI({
    req(input$map_data_type)

    isolate({
      date_value <- if (!is.null(input$climate_date)) {
        input$climate_date
      } else if (!is.null(input$weather_date)) {
        # TODO: would weather_date ever be null?
        input$weather_date
      } else {
        yesterday()
      }
    })

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
      sliderInput(
        inputId = "climate_date",
        label = "Date",
        min = START_DATE,
        max = as_date(paste0(year(START_DATE), "-12-31")),
        value = date_value,
        timeFormat = "%b %d (day %j)"
      )
    )
  })

  ## output$comparison_opts_ui
  output$comparison_opts_ui <- renderUI({
    req(input$map_data_type)

    isolate({
      date_value <- if (!is.null(input$weather_date)) {
        input$weather_date
      } else {
        yesterday()
      }
    })

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
      sliderInput(
        inputId = "weather_date",
        label = "Date",
        min = START_DATE,
        max = yesterday(),
        value = date_value,
        timeFormat = "%b %d, %Y (day %j)"
      )
    )
  })


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
                maxZoom: 12
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
    selected_grid(tibble(lat = coords[1], lng = coords[2]))
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


  # observers ----
  # observe({
  #   message("selected_grid()")
  #   print(selected_grid())
  # })
  #
  # observe({
  #   message("grid_data()")
  #   print(grid_data())
  # })

}
