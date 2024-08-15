#- map.R -#

mapUI <- function() {
  ns <- NS("map")

  div(
    div(
      style = "margin-bottom: 10px;",
      div(class = "map-title", uiOutput(ns("map_title"))),
      leafletOutput(ns("map"), width = "100%", height = "750px")
    ),
    fluidRow(
      column(6, uiOutput(ns("searchbox_ui"))),
      column(6, uiOutput(ns("coord_search_ui")))
    ),
    uiOutput(ns("lat_lng_ui"))
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


      # test observers

      # observe({
      #   print(grid_data())
      # })

      # observe({
      #   print(list(
      #     grid_data = grid_data(),
      #     rv = list(
      #       date_id = rv$date_id,
      #       date_set = rv$date_set,
      #       date_vals = rv$date_vals
      #     )
      #   ))
      # })


      # Reactive values ----

      rv <- reactiveValues(
        selected_grid = NULL,

        # stores date slider state
        date_id = "date",
        date_set = NULL,
        date_vals = NULL
      )


      # SIDEBAR ----------------------------------------------------------------

      ## Clear map grid on type change ----
      observeEvent(input$data_type, {
        req(input$data_type)
        leafletProxy("map") %>% clearGroup(layers$grid)
      })

      ## Main map options UI ----

      output$map_opts_ui <- renderUI({
        div(
          class = "well",
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

      ## data type UI ----
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

      ## Data value options UI ----
      output$value_opts_ui <- renderUI({
        type <- req(input$data_type)
        opts <- list()

        if (type == "weather") {
          opts$id <- "weather_value"
          opts$choices <- OPTS$grid_cols$weather
          opts$selected <- input$weather_value
        } else if (type == "climate") {
          opts$id <- "climate_value"
          opts$choices <- OPTS$grid_cols$climate
          opts$selected <- input$climate_value
        } else if (type == "comparison") {
          opts$id <- "comparison_value"
          opts$choices <- OPTS$grid_cols$comparison
          opts$selected <- input$comparison_value
        }

        radioGroupButtons(
          ns(opts$id), "Display value",
          choices = opts$choices,
          selected = coalesce(opts$selected, first(opts$choices)),
          size = "sm"
        )
      })

      ## Smoothing options UI ----
      output$smoothing_opts_ui <- renderUI({
        type <- req(input$data_type)
        choices <- OPTS$data_smoothing_choices

        radioGroupButtons(
          ns("smoothing"), "Data smoothing",
          choices = choices,
          selected = coalesce(input$smoothing, first(choices)),
          size = "sm"
        )
      })

      ## Toggle smoothing buttons ----
      observe({
        type <- req(input$data_type)
        value <- if (type == "weather") req(input$weather_value)
        else if (type == "climate") req(input$climate_value)
        else if (type == "comparison") req(input$comparison_value)
        toggleState("smoothing", value %in% OPTS$smoothable_cols)
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

        # enable double-ended slider for cumulative gdd
        # if set_dates is defined, use those values
        # otherwise use existing or defaults
        set_dates <- rv$date_set
        if (i$value %in% OPTS$cumulative_cols) {
          opts$id <- "date2"
          opts$value <- align_dates(c(
            coalesce(set_dates$start, input$date2[1], opts$min),
            coalesce(set_dates$end, input$date2[2], input$date, yesterday())
          ), i$year)
        } else {
          opts$id <- "date"
          opts$value <- align_dates(
            coalesce(set_dates$end, input$date, input$date2[2], yesterday()),
            i$year
          )
        }

        rv$date_id <- opts$id
        sliderInput(
          ns(opts$id), "Date",
          min = opts$min, max = opts$max,
          value = opts$value,
          timeFormat = opts$fmt
        )
      })

      # store date slider values in rv
      observe({
        dt <- req(input[[req(rv$date_id)]])
        prev <- rv$date_vals
        dates <- list(start = NULL, end = NULL)

        if (length(dt) == 2) {
          dates$start <- dt[1]
          dates$end <- dt[2]
        } else {
          dates$end <- dt
          start_adjust <- ifelse(is.null(prev$start), 0, yday(prev$start) - 1)
          dates$start <- start_of_year(dt) + start_adjust
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
        counties = "Counties/Regions",
        grid = "Data grid"
      )


      ## Map title ----

      output$map_title <- renderUI({
        opts <- grid_data()$opts
        cols <- OPTS$grid_cols[[opts$type]]
        title <- setNames(names(cols), cols)[[opts$col]]
        if (opts$smoothing != 1) {
          title <- paste0(title, " - ", opts$smoothing, "-day average")
        }
        title
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
          # addFullscreenControl(pseudoFullscreen = T) %>%
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
        df1 <- if (smoothing > 1 & (col %in% OPTS$smoothable_cols)) {
          df %>%
            filter(between(date, dt[1] - smoothing / 2, dt[1] + smoothing / 2)) %>%
            summarize(value = mean(value), .by = c(lat, lng))
        } else filter(df, date == dt[1])
        df2 <- if (length(dt) == 2) filter(df, date == dt[2])
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

      # create the palette domain
      create_domain <- function(min_val, max_val) {
        signif(c(min_val, max_val), 3)
      }

      ## Set grid data and opts ----
      grid_data <- reactive({
        opts <- list()
        opts$type <- req(input$data_type)
        opts$smoothing <- req(input$smoothing) %>% as.numeric()
        opts$date <- req(input[[req(rv$date_id)]])

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

        list(grid = grid, opts = opts)
      })


      ## Set grid domain and palette ----
      grid_pal <- reactive({
        grid <- grid_data()$grid
        opts <- grid_data()$opts
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

        list(
          domain = opts$domain,
          pal = colorNumeric(opts$colors, opts$domain, reverse = T)
        )
      })


      ## Draw grid on map ----
      observe({
        grid <- grid_data()$grid
        opts <- grid_data()$opts
        opts$pal <- grid_pal()$pal
        opts$domain <- grid_pal()$domain

        # make sure date slider has updated
        if (opts$col %in% OPTS$cumulative_cols) {
          req(length(opts$date) == 2)
        } else {
          req(length(opts$date) == 1)
        }

        grid <- grid %>%
          mutate(pal_value = mapply(clamp, value, opts$domain[1], opts$domain[2])) %>%
          mutate(fill = opts$pal(pal_value)) %>%
          set_grid_labels(opts)

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
        if (in_extent(lat, lng)) {
          rv$selected_grid <- list(
            lat = round(lat, 1),
            lng = round(lng, 1)
          )
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

      ## Display grid coordinates on map ----
      observe({
        loc <- req(rv$selected_grid)

        leafletProxy(ns("map")) %>%
          removeControl("selected_coords") %>%
          addControl(
            sprintf("<b>Selected grid:</b> %.1f°N, %.1f°W", loc$lat, loc$lng),
            position = "bottomleft",
            layerId = "selected_coords"
          )
      })


      ## Draw selected grid ----
      # TODO: add a popup with more information?
      observe({
        map <- leafletProxy(ns("map"))

        if (is.null(rv$selected_grid)) {
          map %>% removeShape("selected_grid")
          req(F)
        }

        loc <- req(rv$selected_grid)
        opts <- grid_data()$opts
        grid <- grid_data()$grid %>%
          filter(lat == loc$lat, lng == loc$lng) %>%
          set_grid_labels(opts, selected = T)

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


      # SEARCHBOX --------------------------------------------------------------

      output$searchbox_ui <- renderUI({
        div(
          HTML(paste0("<script async src='https://maps.googleapis.com/maps/api/js?key=", google_key, "&loading=async&libraries=places&callback=initAutocomplete'></script>")),
          textInput(ns("searchbox"), "Find a location by name")
        )
      })

      output$coord_search_ui <- renderUI({
        runjs('
          $(document).keyup((event) => {
            if ($("#map-coord_search").is(":focus") && (event.key == "Enter")) {
              $("#map-coord_search_go").click();
            }
          });
        ')
        div(
          tags$label("Find a location by coordinates"),
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

      # show selected grid below map
      output$lat_lng_ui <- renderUI({
        loc <- rv$selected_grid
        msg <- if (is.null(loc)) "None" else sprintf("%.1f°N, %.1f°W", loc$lat, loc$lng)

        div(strong("Currently selected location:"), msg)
      })


      # RETURN -----------------------------------------------------------------

      return(reactive(list(
        selected_grid = rv$selected_grid
      )))

    } # end module
  )
}
