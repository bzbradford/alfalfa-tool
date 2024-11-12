#- map.R -#

mapUI <- function() {
  ns <- NS("map")

  div(
    div(
      style = "margin-bottom: 10px;",
      div(class = "map-title-container", uiOutput(ns("map_title"))),
      leafletOutput(ns("map"), width = "100%", height = "750px")
    ),
    fluidRow(
      column(6, uiOutput(ns("searchbox_ui"))),
      column(6, uiOutput(ns("coord_search_ui")))
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
        date_id = "date",
        date_set = NULL,
        date_vals = NULL,

        # grid data and opts
        grid_data = NULL
      )


      # SIDEBAR UI -------------------------------------------------------------

      ## Clear map grid on type change ----
      observeEvent(input$data_type, {
        req(input$data_type)
        leafletProxy("map") %>% clearGroup(layers$grid)
      })


      ## Main map options UI ----

      output$map_opts_ui <- renderUI({
        div(
          class = "well",
          useBusyIndicators(spinners = F, pulse = F, fade = F),
          uiOutput(ns("map_extent_ui")),
          uiOutput(ns("type_ui")),
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

      ## map_extent_ui ----
      output$map_extent_ui <- renderUI({
        radioGroupButtons(
          ns("map_extent"), "Map extent",
          choices = OPTS$map_extent_choices,
          size = "sm"
        )
      })


      ## Data type UI ----

      output$type_ui <- renderUI({
        choices <- OPTS$map_type_choices
        radioGroupButtons(
          ns("data_type"), "Choose data layer",
          choices = choices,
          selected = coalesce(input$data_type, first(choices)),
          size = "sm"
        )
      })


      ## Year/Period options UI ----

      output$type_opts_ui <- renderUI({
        type <- req(input$data_type)
        elems <- list()
        if (type %in% c("weather", "comparison")) {
          elems$weather <- uiOutput(ns("weather_year_ui"))
        }
        if (type %in% c("climate", "comparison")) {
          elems$climate <- uiOutput(ns("climate_period_ui"))
        }

        div(class = "inline-flex", elems)
      })

      output$weather_year_ui <- renderUI({
        radioGroupButtons(
          ns("weather_year"), "Weather year",
          choices = OPTS$weather_years,
          selected = coalesce(input$weather_year, as.character(cur_yr)),
          size = "sm"
        )
      })

      output$climate_period_ui <- renderUI({
        choices <- OPTS$climate_period_choices
        radioGroupButtons(
          ns("climate_period"), "Climate period",
          choices = choices,
          selected = coalesce(input$climate_period, first(choices)),
          size = "sm"
        )
      })


      ## Data value options UI ----

      output$value_opts_ui <- renderUI({
        type <- req(input$data_type)

        if (type == "weather") {
          uiOutput(ns("weather_value_ui"))
        } else if (type == "climate") {
          uiOutput(ns("climate_value_ui"))
        } else if (type == "comparison") {
          uiOutput(ns("comparison_value_ui"))
        }
      })

      output$weather_value_ui <- renderUI({
        choices <- OPTS$grid_cols$weather
        selectInput(
          ns("weather_value"), "Display value",
          choices = choices,
          selected = coalesce(input$weather_value, first(choices))
        )
      })

      output$climate_value_ui <- renderUI({
        choices <- OPTS$grid_cols$climate
        selectInput(
          ns("climate_value"), "Display value",
          choices = choices,
          selected = coalesce(input$climate_value, first(choices))
        )
      })

      output$comparison_value_ui <- renderUI({
        choices <- OPTS$grid_cols$comparison
        selectInput(
          ns("comparison_value"), "Display value",
          choices = choices,
          selected = coalesce(input$comparison_value, first(choices))
        )
      })


      ## Smoothing options UI ----

      output$smoothing_opts_ui <- renderUI({
        type <- req(input$data_type)
        value <- req(input[[paste0(type, "_value")]])
        if (value %in% OPTS$smoothable_cols) uiOutput(ns("smoothing_ui"))
      })

      output$smoothing_ui <- renderUI({
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
        type <- req(input$data_type)

        if (type %in% c("weather", "comparison")) {
          uiOutput(ns("weather_date_ui"))
        } else {
          uiOutput(ns("climate_date_ui"))
        }
      })

      ### Weather date slider ----
      output$weather_date_ui <- renderUI({
        opts <- list()
        opts$type <- req(input$data_type)
        opts$year <- req(input$weather_year)
        opts$value <- req(input[[paste0(opts$type, "_value")]])
        opts$min <- start_of_year(opts$year)
        opts$max <- min(yesterday(), end_of_year(opts$year))
        opts$prev_dates <- rv$date_set
        opts$end_date <-
          coalesce(
            opts$prev_dates$end,
            rev(weather_date())[1],
            yesterday()) %>%
          align_dates(opts$min) %>%
          clamp(opts$min, opts$max)
        if (opts$value %in% OPTS$cumulative_cols) {
          opts$start_date <-
            coalesce(
              opts$prev_dates$start,
              rev(weather_date())[2],
              opts$min) %>%
            align_dates(opts$min)
        }
        sliderInput(
          ns("weather_date"), "Date",
          min = opts$min, max = opts$max,
          value = as.Date(c(opts$start_date, opts$end_date)),
          timeFormat = OPTS$weather_date_fmt
        )
      })

      ### Climate date slider ----
      output$climate_date_ui <- renderUI({
        opts <- list()
        opts$type <- req(input$data_type)
        opts$period <- req(input$climate_period)
        opts$value <- req(input[[paste0(opts$type, "_value")]])
        opts$min <- start_of_year()
        opts$max <- end_of_year()
        opts$prev_dates <- rv$date_set
        opts$end_date <-
          coalesce(
            opts$prev_dates$end,
            rev(climate_date())[1],
            rev(weather_date())[1],
            yesterday()) %>%
          align_dates(opts$min) %>%
          clamp(opts$min, opts$max)
        if (opts$value %in% OPTS$cumulative_cols) {
          opts$start_date <-
            coalesce(
              opts$prev_dates$start,
              rev(climate_date())[2],
              rev(weather_date())[2],
              opts$min) %>%
            align_dates(opts$min)
        }
        sliderInput(
          ns("climate_date"), "Date",
          min = opts$min, max = opts$max,
          value = as.Date(c(opts$start_date, opts$end_date)),
          timeFormat = OPTS$climate_date_fmt
        )
      })


      ### Debounced date values ----

      weather_date <- reactive(input$weather_date) %>%
        debounce(OPTS$debounce_ms)

      climate_date <- reactive(input$climate_date) %>%
        debounce(OPTS$debounce_ms)

      grid_date <- reactive({
        if (req(input$data_type) == "climate") {
          climate_date()
        } else {
          weather_date()
        }
      })

      # store date slider values in rv
      observe({
        type <- req(input$data_type)
        dt <- req(grid_date())
        prev <- rv$date_vals
        dates <- list(start = NULL, end = NULL)

        dates <- if (length(dt) == 2) {
          list(start = dt[1], end = dt[2])
        } else {
          start_adjust <- ifelse(is.null(prev$start), 0, yday(prev$start) - 1)
          list(start = start_of_year(dt) + start_adjust, end = dt)
        }

        rv$date_set <- NULL
        rv$date_vals <- dates
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
        prev_dates <- rv$date_vals
        cur_date <- as_date(prev_dates$end)
        new_end_date <- clamp(
          cur_date + step,
          start_of_year(cur_date),
          end_of_year(cur_date)
        )
        new_start_date <- clamp(
          as.Date(prev_dates$start),
          start_of_year(cur_date),
          new_end_date
        )
        rv$date_set <- list(
          start = new_start_date,
          end = new_end_date
        )
      }

      observeEvent(input$date_earlier_7, move_date(-7))
      observeEvent(input$date_earlier_1, move_date(-1))
      observeEvent(input$date_later_1, move_date(1))
      observeEvent(input$date_later_7, move_date(7))
      observeEvent(input$date_reset, {
        rv$date_set <- list(
          start = start_of_year(),
          end = yesterday()
        )
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
          opts$min <- grid_pal()$domain[1]
          opts$max <- grid_pal()$domain[2]
        } else {
          opts$style <- "display:inline-flex; gap:10px;"
          opts$min <- input$legend_min
          opts$max <- input$legend_max
        }

        div(
          style = opts$style,
          numericInput(
            ns("legend_min"), "Gradient minimum",
            value = opts$min,
            step = .1,
            width = "150px"
          ),
          numericInput(
            ns("legend_max"), "Gradient maximum",
            value = opts$max,
            step = .1,
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
        grid = "Data grid",
        counties = "Counties/Regions"
      )


      ## Map title ----

      output$map_title <- renderUI({
        opts <- req(rv$grid_data$opts)
        title <- get_col_label(opts$type, opts$col)
        date_fmt <- ifelse(opts$type == "climate", "%b %d", "%b %d, %Y")
        date_str <- paste(format(opts$date, date_fmt), collapse = "-")
        title <- paste0(title, " - ", date_str)
        if (opts$smoothing != 1) {
          title <- paste0(title, " - ", opts$smoothing, "-day average")
        }
        div(class = "map-title", title)
      })


      ## Initialize map ----

      fit_extent <- function(map, extent = first(OPTS$map_extent_choices)) {
        bounds <- OPTS$map_extent[[extent]]
        map %>%
          fitBounds(
            lat1 = bounds$lat[1], lat2 = bounds$lat[2],
            lng1 = bounds$lng[1], lng2 = bounds$lng[2]
          )
      }

      output$map <- renderLeaflet({
        leaflet(options = leafletOptions(preferCanvas = T)) %>%
          fit_extent() %>%
          addBasemaps() %>%
          addMapPane("counties", 410) %>%
          addMapPane("grid", 420) %>%
          addMapPane("selected_grid", 430) %>%
          addLayersControl(
            baseGroups = basemaps$label,
            overlayGroups = unlist(layers, use.names = F),
            options = layersControlOptions(collapsed = T)
          ) %>%
          addEasyButtonBar(
            easyButton(
              position = "topleft",
              icon = "fa-location",
              title = "Show my location on the map",
              onClick = JS("
                function(btn, map) {
                  Shiny.setInputValue('map-easy_btn', 'user_loc', {priority: 'event'});
                }
              ")
            ),
            easyButton(
              position = "topleft",
              icon = "fa-search-location",
              title = "Zoom to selected grid (if any)",
              onClick = JS("
                function(btn, map) {
                  Shiny.setInputValue('map-easy_btn', 'zoom_grid', {priority: 'event'});
                }
              ")
            ),
            easyButton(
              position = "topleft",
              icon = "fa-globe",
              title = "Reset map view",
              onClick = JS("
                function(btn, map) {
                  Shiny.setInputValue('map-easy_btn', 'zoom_extent', {priority: 'event'});
                }
              ")
            )
          ) %>%
          # assign leaflet map object to global var 'map'
          htmlwidgets::onRender("() => { map = this; }") %>%
          suspendScroll(
            sleepTime = 0,
            wakeTime = 1000,
            hoverToWake = T,
            sleepNote = F,
            sleepOpacity = 1
          )
      })


      ## Add counties to map ----

      observe({
        extent <- req(input$map_extent)
        map <- leafletProxy("map")

        if (extent == "wi") {
          map %>%
            clearGroup(layers$counties) %>%
            addPolygons(
              data = counties_wi,
              group = layers$counties,
              label = ~label,
              color = "grey",
              weight = 0.5,
              opacity = 0.5,
              fillColor = ~colorFactor("Dark2", dnr_region)(dnr_region),
              fillOpacity = 0.05,
              options = pathOptions(pane = "counties")
            )
        } else {
          map %>%
            clearGroup(layers$counties) %>%
            addPolygons(
              data = counties_mw,
              group = layers$counties,
              label = ~label,
              color = "grey",
              weight = 1,
              opacity = 0.25,
              fillColor = ~colorFactor(OPTS$factor_colors, state)(state),
              fillOpacity = 0.1,
              options = pathOptions(pane = "counties")
            )
        }
      })


      ## Handle EasyButton clicks ----

      observeEvent(input$easy_btn, {
        btn <- req(input$easy_btn)

        if (btn == "user_loc") {
          runjs("
            map.getMap().locate({ setView: false }).on('locationfound', (event) => {
              Shiny.setInputValue('map-user_loc', event.latlng, {priority: 'event'})
            })
          ")
        } else if (btn == "zoom_grid") {
          loc <- req(rv$selected_grid)
          runjs(paste0(
            "map.getMap().setView([", loc$lat, ",", loc$lng, "], 10);"
          ))
        } else if (btn == "zoom_extent") {
          extent <- req(input$map_extent)
          zoom <- if (extent == "wi") 7 else 6
          runjs(paste0(
            "map.getMap().setView([45, -90], ", zoom, ");"
          ))
        }
      })


      # MAP GRID ---------------------------------------------------------------

      ## Grid helpers ----

      # handle filtering by date for weather data
      prepare_weather_grid_data <- function(opts) {
        df <- weather %>%
          filter_by_extent(opts$extent) %>%
          select(all_of(c("lat", "lng", "date", "value" = opts$col)))
        df1 <- if (opts$smoothing > 1 & (opts$col %in% OPTS$smoothable_cols)) {
          df %>%
            filter(between(date, opts$date[1] - opts$smoothing / 2, opts$date[1] + opts$smoothing / 2)) %>%
            summarize(value = mean(value), .by = c(lat, lng))
        } else filter(df, date == opts$date[1])
        df2 <- if (length(opts$date) == 2) filter(df, date == opts$date[2])
        prepare_grid_data(df1, df2, opts$col)
      }

      # handle filtering by day of year for climate data
      prepare_climate_grid_data <- function(opts) {
        df <- climate[[opts$period]] %>%
          filter_by_extent(opts$extent) %>%
          select(all_of(c("lat", "lng", "yday", "value" = opts$col)))
        df1 <- if (opts$smoothing > 1) {
          df %>%
            filter(between(yday, yday(opts$date[1] - opts$smoothing / 2), yday(opts$date[1] + opts$smoothing / 2))) %>%
            summarize(value = mean(value), .by = c(lat, lng))
        } else {
          df %>% filter(yday == yday(opts$date[1]))
        }
        df2 <- if (length(opts$date) == 2) df %>% filter(yday == yday(opts$date[2]))

        prepare_grid_data(df1, df2, opts$col)
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
            { if (selected) " (Selected)"},
            "<br>",
            if (opts$type == "weather" & opts$col %in% c("frost", "freeze")) {
              sprintf("%s: %s", prefix, value)
            } else if (opts$col %in% OPTS$percent_cols) {
              sprintf("%s: %.1f%%", prefix, value * 100)
            } else if (opts$type == "comparison") {
              sprintf("%s: %+.1f<br>Observed: %.1f, Average: %.1f", prefix, value, wx_value, cl_value)
            } else {
              sprintf("%s: %.1f", prefix, value)
            }
          )
        )
      }

      # create the palette domain
      create_domain <- function(min_val, max_val) {
        signif(c(min_val, max_val), 3)
      }




      ## Set grid data and opts ----
      observe({
        opts <- list()
        opts$extent <- req(input$map_extent)
        opts$type <- req(input$data_type)
        opts$col <- req(input[[paste0(opts$type, "_value")]])
        opts$smoothing <- req(input$smoothing) %>% as.numeric()
        opts$date <- req(grid_date())
        date_len <- ifelse(opts$col %in% OPTS$cumulative_cols, 2, 1)
        req(length(opts$date) == date_len)
        opts$value_label <- get_col_label(opts$type, opts$col)

        # set grid data
        grid <-
          if (opts$type == "weather") {
            prepare_weather_grid_data(opts)
          } else if (opts$type == "climate") {
            opts$period <- req(input$climate_period)
            prepare_climate_grid_data(opts)
          } else if (opts$type == "comparison") {
            opts$period <- req(input$climate_period)
            wx <- prepare_weather_grid_data(opts) %>% rename(c(wx_value = value))
            cl <- prepare_climate_grid_data(opts) %>% rename(c(cl_value = value))
            cl %>%
              left_join(wx, join_by(lat, lng)) %>%
              mutate(value = wx_value - cl_value)
          }

        # prepare grid labels
        grid <- grid %>% mutate(
          grid_str = sprintf("%.1f°N, %.1f°W", lat, lng),
          value_str =
            if (opts$type == "weather" & opts$col %in% c("frost", "freeze")) {
              sprintf("%s", value)
            } else if (opts$col %in% OPTS$percent_cols) {
              sprintf("%.1f%%", value * 100)
            } else if (opts$type == "comparison") {
              sprintf("%+.1f<br>Observed: %.1f, Average: %.1f", value, wx_value, cl_value)
            } else {
              sprintf("%.1f", value)
            }
          )

        rv$grid_data <- list(grid = grid, opts = opts)
      })


      ## Set grid domain and palette ----
      grid_pal <- reactive({
        grid <- req(rv$grid_data$grid)
        opts <- req(rv$grid_data$opts)
        opts$autoscale <- isTRUE(input$legend_autoscale)

        req(nrow(grid) > 0)

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

        list(
          domain = opts$domain,
          pal = colorNumeric(opts$colors, opts$domain, reverse = T)
        )
      })


      ## Draw grid on map ----
      observe({
        grid <- req(rv$grid_data$grid)
        opts <- req(rv$grid_data$opts)
        opts$pal <- grid_pal()$pal
        opts$domain <- grid_pal()$domain

        # make sure date slider is correctly single or double ended
        date_len <- ifelse(opts$col %in% OPTS$cumulative_cols, 2, 1)
        req(length(opts$date) == date_len)

        grid <- grid %>%
          mutate(pal_value = mapply(clamp, value, opts$domain[1], opts$domain[2])) %>%
          mutate(fill = opts$pal(pal_value))

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
            label = ~str_glue("<b>{grid_str}</b><br>{opts$value_label}: {value_str}") %>%
              lapply(shiny::HTML),
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
        new_lat = round(lat, 1)
        new_lng = round(lng, 1)
        matching_grid <- req(rv$grid_data$grid) %>%
          filter(lat == new_lat, lng == new_lng)
        if (nrow(matching_grid) > 0) {
          rv$selected_grid <- list(lat = new_lat, lng = new_lng)
        }
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
        loc <- req(rv$selected_grid)
        opts <- req(rv$grid_data$opts)
        grid <- req(rv$grid_data$grid) %>%
          filter(lat == loc$lat, lng == loc$lng)

        control_str <- str_glue("<b>Selected grid: {grid$grid_str}</b><br>{opts$value_label}: {grid$value_str}")
        grid_str <- str_glue("<b>{grid$grid_str}</b> (selected)<br>{opts$value_label}: {grid$value_str}")

        map %>%
          addControl(
            control_str,
            position = "bottomleft",
            layerId = "selected_coords"
          ) %>%
          addRectangles(
            data = grid,
            lat1 = ~lat - .05, lat2 = ~lat + .05,
            lng1 = ~lng - .05, lng2 = ~lng + .05,
            group = layers$grid,
            layerId = "selected",
            weight = .5, opacity = 1, color = "black",
            fillOpacity = 0,
            label = HTML(grid_str),
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
            removeMarker("user_loc") %>%
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
          runjs("
            document.getElementById('map-searchbox').value = '';
            document.getElementById('map-coord_search').value = '';
          ")
        })
      })

      # hide location pin on click
      observeEvent(input$map_marker_click, {
        req(input$map_marker_click$id == "user_loc")
        leafletProxy("map") %>%
          removeMarker("user_loc")
      })


      # LOWER UI ---------------------------------------------------------------

      observeEvent(input$map_extent, {
        extent <- req(input$map_extent)
        bounds <- OPTS$map_extent[[extent]]

        leafletProxy("map") %>%
          fit_extent(extent) %>%
          clearGroup(layers$grid)

        # move selection if now outside of extent
        if (!is.null(rv$selected_grid)) {
          loc <- rv$selected_grid
          closest_grid <- climate_grids %>%
            filter_by_extent(extent) %>%
            slice_min(abs(lat - loc$lat), n = 1) %>%
            slice_min(abs(lng - loc$lng), n = 1)
          rv$selected_grid <- list(
            lat = closest_grid$lat,
            lng = closest_grid$lng
          )
        }

        # update bounds on google places search
        if (extent == "wi") {
          runjs("autocomplete.setBounds(WI_BOUNDS);")
        } else {
          runjs("autocomplete.setBounds(MW_BOUNDS);")
        }
      })


      ## searchbox_ui ----
      output$searchbox_ui <- renderUI({
        div(
          HTML(paste0("<script async src='https://maps.googleapis.com/maps/api/js?key=", google_key, "&loading=async&libraries=places&callback=initAutocomplete'></script>")),
          textInput(ns("searchbox"), "Find a location by name")
        )
      })

      ## coord_search_ui ----
      output$coord_search_ui <- renderUI({
        runjs('
          $(document).keyup((event) => {
            if ($("#map-coord_search").is(":focus") && (event.key == "Enter")) {
              $("#map-coord_search_go").click();
            }
          });
        ')

        div(
          div(tags$label("Find a location by coordinates")),
          div(
            style = "display: inline-flex; gap: 5px; max-width: 100%;",
            textInput(
              ns("coord_search"), label = NULL,
              placeholder = "Enter coordinates"
            ),
            div(
              style = "margin-bottom: 10px;",
              actionButton(ns("coord_search_go"), "Go")
            )
          )
        )
      })

      observeEvent(input$coord_search_go, {
        str <- req(input$coord_search)
        try({
          coords <- parse_coords(str)
          coord_hash <- paste0("{lat:", coords$lat, ", lng:", coords$lng, "}")
          cmd <- paste0("Shiny.setInputValue('map-user_loc', ", coord_hash, ", {priority: 'event'})")
          runjs(cmd)
        })
      })



      # RETURN -----------------------------------------------------------------

      return(reactive(list(
        grid_data = rv$grid_data,
        selected_grid = rv$selected_grid
      )))

    } # end module
  )
}
