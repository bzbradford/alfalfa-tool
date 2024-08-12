#- map.R -#

mapUI <- function() {
  ns <- NS("map")

  div(
    div(
      style = "margin-bottom: 10px;",
      div(class = "map-title", textOutput(ns("map_title"))),
      leafletOutput(ns("map"), width = "100%", height = "750px")
    ),
    fluidRow(
      column(6, uiOutput(ns("searchbox_ui"))),
      column(6, uiOutput(ns("lat_lng_ui")))
    )
  )
}

mapSidebarUI <- function() {
  ns <- NS("map")
  uiOutput(ns("map_opts_ui"))
}

mapServer <- function() {
  moduleServer(
    id = "map",
    function(input, output, session) {
      ns <- session$ns


      # Reactive values ----

      rv <- reactiveValues(
        selected_grid = NULL,

        # stores date slider state
        date_start = NULL,
        date_end = NULL,

        # dataset for map grid
        grid_data = NULL,
        grid_opts = NULL,
        grid_domain = NULL,
        grid_pal = NULL,

        map_title = "TEST"
      )


      # SIDEBAR ----------------------------------------------------------------

      # clear map state on type change
      observeEvent(input$data_type, {
        req(input$data_type)
        rv$grid_data <- NULL
        rv$grid_opts <- NULL
        leafletProxy("map") %>% clearGroup(layers$grid)
      })


      ## Options UI ----

      output$map_opts_ui <- renderUI({
        div(
          class = "well",
          radioGroupButtons(
            ns("data_type"), "Choose data layer",
            choices = OPTS$map_type_choices,
            size = "sm"
          ),
          uiOutput(ns("type_opts_ui")),
          uiOutput(ns("value_opts_ui")),
          uiOutput(ns("smoothing_opts_ui")),
          div(
            style = "min-width: 100%;",
            uiOutput(ns("date_slider_ui")),
            uiOutput(ns("date_btns_ui")),
          ),
          uiOutput(ns("display_opts_ui"))
        )
      })

      # extended options per data type
      output$type_opts_ui <- renderUI({
        type <- req(input$data_type)

        div(
          class = "inline-flex",
          if (type %in% c("weather", "comparison")) {
            radioGroupButtons(
              ns("weather_year"), "Weather year",
              choices = OPTS$weather_years,
              selected = coalesce(input$weather_year, as.character(cur_yr)),
              size = "sm"
            )
          },
          if (type %in% c("climate", "comparison")) {
            choices <- OPTS$climate_period_choices
            radioGroupButtons(
              ns("climate_period"), "Climate period",
              choices = choices,
              selected = coalesce(input$climate_period, first(choices)),
              size = "sm"
            )
          }
        )
      })

      output$value_opts_ui <- renderUI({
        type <- req(input$data_type)

        if (type == "weather") {
          choices <- OPTS$grid_cols$weather
          radioGroupButtons(
            ns("weather_value"), "Data value",
            choices = choices,
            selected = coalesce(input$weather_value, first(choices)),
            size = "sm"
          )
        } else if (type == "climate") {
          choices <- OPTS$grid_cols$climate
          radioGroupButtons(
            ns("climate_value"), "Data value",
            choices = choices,
            selected = coalesce(input$climate_value, first(choices)),
            size = "sm"
          )
        } else if (type == "comparison") {
          choices <- OPTS$grid_cols$comparison
          radioGroupButtons(
            ns("comparison_value"), "Data value",
            choices = choices,
            selected = coalesce(input$comparison_value, first(choices)),
            size = "sm"
          )
        }
      })

      output$smoothing_opts_ui <- renderUI({
        type <- req(input$data_type)
        # value <- if (type == "weather") req(input$weather_value)
        # else if (type == "climate") req(input$climate_value)
        # else if (type == "comparison") req(input$comparison_value)
        # req(value %in% OPTS$smoothable_cols)
        choices <- OPTS$data_smoothing_choices

        radioGroupButtons(
          ns("smoothing"), "Data smoothing",
          choices = choices,
          selected = coalesce(input$smoothing, first(choices)),
          size = "sm"
        )
      })


      ## Date selector UI ----

      output$date_slider_ui <- renderUI({
        i <- list(type = req(input$data_type))
        opts <- list()

        if (i$type == "weather") {
          i$year <- req(input$weather_year)
          i$value <- req(input$weather_value)
          opts$fmt <- OPTS$weather_date_fmt
          opts$max <- min(yesterday(), end_of_year(i$year))
        }

        if (i$type == "climate") {
          i$year <- year(yesterday())
          i$period <- req(input$climate_period)
          i$value <- req(input$climate_value)
          opts$fmt <- OPTS$climate_date_fmt
          opts$max <- end_of_year(i$year)
        }

        if (i$type == "comparison") {
          i$year <- req(input$weather_year)
          i$period <- req(input$climate_period)
          i$value <- req(input$comparison_value)
          opts$fmt <- OPTS$weather_date_fmt
          opts$max <- min(yesterday(), end_of_year(i$year))
        }

        opts$min <- start_of_year(i$year)
        opts$value <- min(
          align_dates(coalesce(rv$date_end, yesterday()), i$year),
          opts$max
        )

        # enable double-ended slider for cumulative gdd
        if (i$value %in% OPTS$cumulative_cols) {
          opts$value <- c(
            align_dates(coalesce(rv$date_start, opts$min), i$year),
            opts$value
          )
        }

        sliderInput(
          ns("date_slider"), "Date",
          min = opts$min, max = opts$max,
          value = opts$value,
          timeFormat = opts$fmt
        )
      })

      # store date slider values in rv
      observe({
        dt <- req(input$date_slider)

        if (length(dt) == 2) {
          rv$date_start <- dt[1]
          rv$date_end <- dt[2]
        } else {
          rv$date_end <- dt
          rv$date_start <-
            if (is.null(rv$date_start)) {
              start_of_year(dt)
            } else {
              start_of_year(dt) + yday(rv$date_start) - 1
            }
        }
      })


      ## Date adjustment buttons ----

      output$date_btns_ui <- renderUI({
        tagList(
          tags$label("Date adjustment"),
          div(class = "date-btns",
            actionButton(ns("date_earlier_7"), "-7"),
            actionButton(ns("date_earlier_1"), "-1"),
            actionButton(ns("date_later_1"), "+1"),
            actionButton(ns("date_later_7"), "+7"),
            actionButton(ns("date_reset"), "Reset"),
          )
        )
      })

      move_date <- function(step) {
        cur_date <- as_date(rv$date_end)
        new_end_date <- clamp(
          cur_date + step,
          start_of_year(cur_date),
          end_of_year(cur_date)
        )
        new_start_date <- clamp(
          as.Date(rv$date_start),
          start_of_year(cur_date),
          new_end_date
        )
        rv$date_end <- new_end_date
        rv$date_start <- new_start_date
      }

      observeEvent(input$date_earlier_7, move_date(-7))
      observeEvent(input$date_earlier_1, move_date(-1))
      observeEvent(input$date_later_1, move_date(1))
      observeEvent(input$date_later_7, move_date(7))
      observeEvent(input$date_reset, {
        rv$date_start <- start_of_year()
        rv$date_end <- yesterday()
      })


      ## Manual gradient UI ----

      output$display_opts_ui <- renderUI({
        div(
          tags$label("Display options"),
          div(class = "map-display-opts",
            checkboxInput(
              ns("legend_autoscale"), "Autoscale map legend",
              value = TRUE
            ),
            uiOutput(ns("legend_range"))
          )
        )
      })

      output$legend_range <- renderUI({
        opts <- list()
        if (isTRUE(input$legend_autoscale)) {
          opts$style <- "display:none;"
          opts$min <- rv$grid_domain[1]
          opts$max <- rv$grid_domain[2]
        } else {
          opts$style <- "display:inline-flex; gap:10px;"
          opts$min <- input$legend_min
          opts$max <- input$legend_max
        }

        div(
          style = opts$style,
          numericInput(
            inputId = ns("legend_min"),
            label = "Gradient minimum",
            step = .1,
            value = opts$min,
            width = "150px"
          ),
          numericInput(
            inputId = ns("legend_max"),
            label = "Gradient maximum",
            step = .1,
            value = opts$max,
            width = "150px"
          )
        )
      })


      # MAP --------------------------------------------------------------------

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


      ## Map title ----

      output$map_title <- renderText({
        opts <- req(rv$grid_opts)
        cols <- OPTS$grid_cols[[opts$type]]
        setNames(names(cols), cols)[[opts$col]]
      })


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
                  map.locate({ setView: false }).on('locationfound', (event) => {
                    Shiny.setInputValue('map-user_loc', event.latlng, {priority: 'event'})
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
                  map.fitBounds([[47.1, -86.8], [42.4, -93.0]])
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


      ## Hide layer control after delay ----

      observeEvent(TRUE, {
        delay(3000, {
          leafletProxy(ns("map")) %>%
            addLayersControl(
              baseGroups = basemaps$label,
              overlayGroups = unlist(layers, use.names = FALSE),
            )
        })
      })


      # MAP GRID ---------------------------------------------------------------

      ## Grid helpers ----

      # handle filtering by date for weather data
      prepare_weather_grid_data <- function(df, col, dt, smoothing) {
        df <- df %>% select(all_of(c("lat", "lng", "date", "value" = col)))

        df1 <- if (smoothing > 1) {
          df %>%
            filter(between(date, dt[1] - smoothing / 2, dt[1] + smoothing / 2)) %>%
            summarize(value = mean(value), .by = c(lat, lng))
        } else {
          df %>% filter(date == dt[1])
        }
        df2 <- if (length(dt) == 2) df %>% filter(date == dt[2])

        prepare_grid_data(df1, df2, col)
      }

      # handle filtering by day of year for climate data
      prepare_climate_grid_data <- function(df, col, dt, smoothing) {
        df <- df %>% select(all_of(c("lat", "lng", "yday", "value" = col)))

        df1 <- if (smoothing > 1) {
          df %>%
            filter(between(yday, yday(dt[1] - smoothing / 2), yday(dt[1] + smoothing / 2))) %>%
            summarize(value = mean(value), .by = c(lat, lng))
        } else {
          df %>% filter(yday == yday(dt[1]))
        }
        df2 <- if (length(dt) == 2) df %>% filter(yday == yday(dt[2]))

        prepare_grid_data(df1, df2, col)
      }

      # returns a minimal dataset with lat, lng, and value cols
      prepare_grid_data <- function(df1, df2, col) {
        if (is.null(df2)) {
          df1 %>%
            select(lat, lng, value)
        } else {
          left_join(
            df1 %>% select(lat, lng, value1 = value),
            df2 %>% select(lat, lng, value2 = value),
            join_by(lat, lng)
          ) %>%
            mutate(value = value2 - value1) %>%
            select(lat, lng, value)
        }
      }

      # set grid labels
      # TODO: add other data to label?
      set_grid_labels <- function(.data, opts, selected = F) {
        cols <- OPTS$grid_cols[[opts$type]]
        prefix <- setNames(names(cols), cols)[[opts$col]]
        .data %>% mutate(
          label = paste0(
            str_glue("<b>{lat}°N, {lng}°W</b>"),
            { if (selected) " (Selected)"}, "<br>",
            if (opts$type == "weather" & opts$col %in% c("frost", "freeze")) {
              sprintf("%s: %s", prefix, value)
            } else if (opts$col %in% OPTS$percent_cols) {
              sprintf("%s: %.1f%%", prefix, value * 100)
            } else if (opts$type == "comparison") {
              sprintf("%s: %+.1f", prefix, value)
            } else {
              sprintf("%s: %.1f", prefix, value)
            }
          )
        )
      }

      ## Generate grid data ----
      observe({
        opts <- list()
        opts$type <- req(input$data_type)
        opts$smoothing <- req(input$smoothing) %>% as.numeric()
        opts$date <- req(input$date_slider)

        # set grid data
        grid <-
          if (opts$type == "weather") {
            opts$col <- req(input$weather_value)
            weather %>%
              prepare_weather_grid_data(opts$col, opts$date, opts$smoothing)
          } else if (opts$type == "climate") {
            opts$period <- req(input$climate_period)
            opts$col <- req(input$climate_value)
            climate[[opts$period]] %>%
              prepare_climate_grid_data(opts$col, opts$date, opts$smoothing)
          } else if (opts$type == "comparison") {
            opts$period <- req(input$climate_period)
            opts$col <- req(input$comparison_value)
            wx <- weather %>%
              prepare_weather_grid_data(opts$col, opts$date, opts$smoothing) %>%
              rename(c(wx_value = value))
            cl <- climate[[opts$period]] %>%
              prepare_climate_grid_data(opts$col, opts$date, opts$smoothing) %>%
              rename(c(cl_value = value))
            cl %>%
              left_join(wx, join_by(lat, lng)) %>%
              mutate(value = wx_value - cl_value)
          }

        rv$grid_data <- grid
        rv$grid_opts <- opts
        rv$grid_pal <- NULL
      })


      ## Create grid color palette ----

      # create the palette domain
      create_domain <- function(min_val, max_val) {
        signif(c(min_val, max_val), 3)
      }

      # color palette function for displaying grid
      observe({
        grid <- req(rv$grid_data)
        opts <- req(rv$grid_opts)
        opts$autoscale <- isTRUE(input$legend_autoscale)

        # percent frost/freeze palette
        if (opts$col %in% OPTS$percent_cols) {
          opts$domain <- c(0, 1)
          opts$colors <- "Blues"
        } else if (opts$type == "comparison" & opts$col %in% OPTS$comparison_cols) {
          # +/- comparisons vs climate. Centered on zero
          opts$domain <- create_domain(-max(abs(grid$value)), max(abs(grid$value)))
          opts$colors <- "Spectral"
        } else {
          opts$domain <- create_domain(min(grid$value), max(grid$value))
          opts$colors <- "Spectral"
        }

        # when autoscale unchecked, use min/max values from input for domain
        if (!opts$autoscale) {
          opts$domain <- c(
            req(input$legend_min),
            req(input$legend_max)
          )
        }

        rv$grid_domain <- opts$domain
        rv$grid_pal <- colorNumeric(opts$colors, opts$domain, reverse = T)
        # message("palette refreshed at ", Sys.time() - init_time)
      })


      ## Draw grid on map ----

      observe({
        opts <- list()
        grid <- req(rv$grid_data)
        opts$opts <- req(rv$grid_opts)
        opts$pal <- req(rv$grid_pal)
        opts$domain <- req(rv$grid_domain)

        grid <- grid %>%
          mutate(pal_value = mapply(clamp, value, opts$domain[1], opts$domain[2])) %>%
          mutate(fill = opts$pal(pal_value)) %>%
          set_grid_labels(opts$opts)

        leafletProxy(ns("map")) %>%
          addRectangles(
            data = grid,
            lat1 = ~lat - .05, lat2 = ~lat + .05,
            lng1 = ~lng - .05, lng2 = ~lng + .05,
            group = layers$grid,
            layerId = ~coords_to_pt(lat, lng),
            weight = 0,
            fillOpacity = .75,
            fillColor = ~fill,
            label = ~lapply(label, shiny::HTML),
            options = pathOptions(pane = "grid")
          ) %>%
          addLegend(
            layerId = "legend",
            position = "bottomright",
            pal = opts$pal,
            bins = 5,
            values = opts$domain
          )
      })


      # SELECTED GRID ----------------------------------------------------------

      # selects only if within bounds
      select_grid <- function(lat, lng) {
        req(between(lat, OPTS$min_lat, OPTS$max_lat))
        req(between(lng, OPTS$min_lng, OPTS$max_lng))
        rv$selected_grid <- list(
          lat = round(lat, 1),
          lng = round(lng, 1)
        )
      }

      ## Handle map click ----
      # Set selected point on map click
      observeEvent(input$map_shape_click, {
        id <- req(input$map_shape_click$id)
        if (id != "selected") {
          rv$selected_grid <- pt_to_coords(id)
        }
      })

      ## Draw selected grid ----
      # TODO: add a popup with more information?
      observe({
        map <- leafletProxy(ns("map"))

        if (is.null(rv$selected_grid)) {
          map %>% removeShape("selected_grid")
          return()
        }

        loc <- req(rv$selected_grid)
        grid <- req(rv$grid_data) %>%
          filter(lat == loc$lat, lng == loc$lng) %>%
          set_grid_labels(rv$grid_opts, selected = T)
        map %>%
          removeShape("selected_grid") %>%
          addRectangles(
            data = grid,
            lat1 = ~lat - .05, lat2 = ~lat + .05,
            lng1 = ~lng - .05, lng2 = ~lng + .05,
            group = layers$grid,
            layerId = "selected",
            weight = .5, opacity = 1, color = "black",
            fillOpacity = 0,
            label = ~HTML(label),
            options = pathOptions(pane = "selected_grid")
          )
      })

      ## Handle user location ----
      # Add user location pin map after map zooms in
      observe({
        loc <- req(input$user_loc)

        # center map on new location
        map <- leafletProxy(ns("map"))
        map %>% setView(
          lat = loc$lat,
          lng = loc$lng,
          zoom = 8
        )

        # select after delay for map panning
        delay(250, {
          select_grid(loc$lat, loc$lng)
          map %>%
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
          runjs("document.getElementById('map-searchbox').value = '';")
        })
      })

      # hide location pin on click
      observeEvent(input$map_marker_click, {
        req(input$map_marker_click$id == "user_loc")
        leafletProxy("map") %>%
          removeMarker("user_loc")
      })

      # show selected grid below map
      output$lat_lng_ui <- renderUI({
        loc <- rv$selected_grid
        msg <- if (is.null(loc)) "None" else sprintf("%.1f°N, %.1f°W", loc$lat, loc$lng)

        p(strong("Selected grid:"), msg)
      })


      # SEARCHBOX --------------------------------------------------------------

      output$searchbox_ui <- renderUI({
        div(
          HTML(paste0("<script async src='https://maps.googleapis.com/maps/api/js?key=", google_key, "&loading=async&libraries=places&callback=initAutocomplete'></script>")),
          textInput(ns("searchbox"), "Find a location")
        )
      })


      # RETURN -----------------------------------------------------------------

      return(reactive(list(
        selected_grid = rv$selected_grid
      )))

    } # end module
  )
}
