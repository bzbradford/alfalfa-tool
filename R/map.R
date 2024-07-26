#- map.R -#

mapUI <- function() {
  ns <- NS("map")

  tagList(
    h3(style = "margin-top: 1em;", "Gridded weather and climate data"),
    fluidRow(
      column(8,
        leafletOutput(ns("map"), width = "100%", height = "720px")
      ),
      column(4,
        div(
          class = "well", style = "margin-bottom: 0;",
          radioButtons(
            inputId = ns("map_data_type"),
            label = "Choose data layer",
            choices = list(
              "Current weather" = "weather",
              "Climate normals" = "climate",
              "Weather vs climate" = "comparison"
            )
          ),
          uiOutput(ns("map_opts_ui"))
        )
      )
    )
  )
}

mapServer <- function() {
  moduleServer(
    id = "map",
    function(input, output, session) {
      ns <- session$ns


      # Reactive values ----

      init_time <- Sys.time()

      rv <- reactiveValues(

        # stores date slider state
        date_start = NULL,
        date_end = NULL,

        # dataset for map grid
        grid_data = NULL,
        grid_opts = NULL,
        grid_domain = NULL,
        grid_pal = NULL
      )


      # SIDEBAR ----

      ## map_opts_ui ----
      output$map_opts_ui <- renderUI({
        type <- req(input$map_data_type)

        # clear map state on type change
        rv$grid_data <- NULL
        rv$grid_opts <- NULL
        leafletProxy("map") %>% clearGroup(layers$grid)

        tagList(
          if (type == "weather") {
            list(
              uiOutput(ns("weather_year_ui")),
              uiOutput(ns("weather_value_ui"))
            )
          } else if (type == "climate") {
            list(
              uiOutput(ns("climate_period_ui")),
              uiOutput(ns("climate_value_ui"))
            )
          } else {
            list(
              uiOutput(ns("weather_year_ui")),
              uiOutput(ns("climate_period_ui")),
              uiOutput(ns("comparison_value_ui"))
            )
          },
          uiOutput(ns("date_slider_ui")),
          uiOutput(ns("date_btns_ui")),
          uiOutput(ns("display_opts_ui"))
        )
      })

      ## weather_year ----
      output$weather_year_ui <- renderUI({
        radioButtons(
          inputId = ns("weather_year"),
          label = "Weather year",
          choices = list(cur_yr, cur_yr - 1),
          selected = coalesce(input$weather_year, as.character(cur_yr)),
          inline = T
        )
      })

      ## climate_period ----
      output$climate_period_ui <- renderUI({
        opts <- OPTS$climate_period_choices
        radioButtons(
          inputId = ns("climate_period"),
          label = "Climate period",
          choices = opts,
          selected = coalesce(input$climate_period, first(opts))
        )
      })

      ## weather_value ----
      output$weather_value_ui <- renderUI({
        opts <- OPTS$grid_cols$weather
        radioButtons(
          inputId = ns("weather_value"),
          label = "Data value",
          choices = opts,
          selected = coalesce(input$weather_value, first(opts))
        )
      })

      ## climate_value ----
      output$climate_value_ui <- renderUI({
        opts <- OPTS$grid_cols$climate
        radioButtons(
          inputId = ns("climate_value"),
          label = "Data value",
          choices = opts,
          selected = coalesce(input$climate_value, first(opts))
        )
      })

      ## comparison_value ----
      output$comparison_value_ui <- renderUI({
        opts <- OPTS$grid_cols$comparison
        radioButtons(
          inputId = ns("comparison_value"),
          label = "Data value",
          choices = opts,
          selected = coalesce(input$comparison_value, first(opts))
        )
      })

      ## date_slider ----
      output$date_slider_ui <- renderUI({
        i <- list(type = req(input$map_data_type))
        opts <- list()

        if (i$type == "weather") {
          i$year = req(input$weather_year)
          i$value = req(input$weather_value)
          opts$fmt = OPTS$weather_date_fmt
        } else if (i$type == "climate") {
          i$year = year(yesterday())
          i$period = req(input$climate_period)
          i$value = req(input$climate_value)
          opts$fmt = OPTS$climate_date_fmt
        } else {
          i$year = req(input$weather_year)
          i$period = req(input$climate_period)
          i$value = req(input$comparison_value)
          opts$fmt = OPTS$weather_date_fmt
        }

        opts$min = start_of_year(i$year)
        opts$max = min(yesterday(), end_of_year(i$year))
        opts$value = align_dates(coalesce(rv$date_end, yesterday()), i$year)
        if (i$value %in% OPTS$cumulative_cols) {
          opts$value = c(
            align_dates(coalesce(rv$date_start, opts$min), i$year),
            opts$value
          )
        }

        sliderInput(
          inputId = ns("date_slider"),
          label = "Date",
          min = opts$min,
          max = opts$max,
          value = opts$value,
          timeFormat = opts$fmt
        )
      })

      ## observe: date_slider ----
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


      ## display options ----
      ## legend_autoscale ----
      output$display_opts_ui <- renderUI({
        div(
          tags$label("Display options"),
          div(class = "map-display-opts",
            checkboxInput(
              inputId = ns("legend_autoscale"),
              label = "Autoscale map legend",
              value = coalesce(input$legend_autoscale, TRUE)
            ),
            uiOutput(ns("legend_range"))
          )
        )
      })

      ## legend_range ----
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
            label = "Min value",
            step = .1,
            value = opts$min
          ),
          numericInput(
            inputId = ns("legend_max"),
            label = "Max value",
            step = .1,
            value = opts$max
          )
        )
      })


      # MAP ----

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


      ## Hide layer control after delay ----

      observeEvent(TRUE, {
        delay(3000, {
          leafletProxy("map") %>%
            addLayersControl(
              baseGroups = basemaps$label,
              overlayGroups = unlist(layers, use.names = FALSE),
            )
        })
      })


      # MAP GRID ----

      ## Assign grid data ----

      # returns a minimal dataset with lat, lng, and value cols
      prepare_weather_grid_data <- function(df, col, dt) {
        if (length(dt) == 1) {
          df %>%
            filter(date == dt) %>%
            rename(c("value" = col)) %>%
            select(lat, lng, value)
        } else {
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
        } else {
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

      # prepares grid data
      observe({
        opts <- list()
        opts$type <- req(input$map_data_type)
        opts$date <- req(input$date_slider)

        # set grid data
        grid <-
          if (opts$type == "weather") {
            opts$col <- req(input$weather_value)
            weather %>%
              prepare_weather_grid_data(opts$col, opts$date)
          } else if (opts$type == "climate") {
            opts$period <- req(input$climate_period)
            opts$col <- req(input$climate_value)
            climate[[opts$period]] %>%
              prepare_climate_grid_data(opts$col, opts$date)
          } else if (opts$type == "comparison") {
            opts$period <- req(input$climate_period)
            opts$col <- req(input$comparison_value)
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

        grid <- grid %>%
          mutate(grid_pt = coords_to_pt(lat, lng)) %>%
          set_grid_labels(opts)

        rv$grid_data <- grid
        rv$grid_opts <- opts
      })

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


      ## Draw grid ----

      observe({
        opts <- list()
        opts$grid <- req(rv$grid_data)
        opts$pal <- req(rv$grid_pal)
        opts$domain <- req(rv$grid_domain)

        grid <- opts$grid %>%
          mutate(pal_value = mapply(clamp, value, opts$domain[1], opts$domain[2])) %>%
          mutate(fill = opts$pal(pal_value))

        leafletProxy(ns("map")) %>%
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
            pal = opts$pal,
            bins = 5,
            values = opts$domain
          )
      })


      # SELECTED GRID ----

      ## Set selected point on map click ----
      observeEvent(input$map_shape_click, {
        id <- req(input$map_shape_click$id)
        if (id != "selected") {
          rv$selected_grid <- pt_to_coords(id)
        }
      })


      ## Draw selected grid on map ----
      observe({
        map <- leafletProxy(ns("map"))

        if (is.null(rv$selected_grid)) {
          map %>% removeShape("selected_grid")
          return()
        }

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
        loc <- req(input$user_loc)
        loc_grid <- lapply(loc, function(x) round(x, 1))

        delay(250, {
          rv$selected_grid <- loc_grid
          leafletProxy(ns("map")) %>%
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

      return(reactive(list(
        selected_grid = rv$selected_grid
      )))

    } # end module
  )
}
