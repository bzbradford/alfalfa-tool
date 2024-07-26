#- plots.R -#

weatherPlotUI <- function() {
  ns <- NS("plot")

  tagList(
    div(class = "option-well",
      radioButtons(
        inputId = ns("weather_smoothing"),
        label = "Data smoothing options",
        choices = OPTS$data_smoothing_choices,
        inline = T
      )
    ),
    plotlyOutput(ns("weather_plot"), height = "600px"),
    div(class = "plot-caption", HTML("Click on any legend item in the plot to show or hide it. Weather data originally sourced from NOAA and retrieved from <a href='https://agweather.cals.wisc.edu/'>AgWeather</a>."))
  )
}

climatePlotUI <- function() {
  ns <- NS("plot")

  tagList(
    div(class = "option-well",
      radioButtons(
        inputId = ns("climate_period"),
        label = "Climate dataset",
        choices = OPTS$climate_period_choices,
        inline = TRUE
      ),
      radioButtons(
        inputId = ns("climate_smoothing"),
        label = "Data smoothing options",
        choices = OPTS$data_smoothing_choices,
        selected = 7,
        inline = T
      )
    ),
    plotlyOutput(ns("climate_plot"), height = "600px"),
    div(class = "plot-caption", HTML("Click on any legend item in the plot to show or hide it. Today's date is indicated as a vertical dashed line. Climate data sourced from <a href='https://www.climatologylab.org/gridmet.html'>GridMET</a>."))
  )
}

customPlotUI <- function() {
  ns <- NS("plot")

  tagList(
    div(
      h4("Primary plot elements"),
      div(class = "option-well",
        div(class = "inline-flex",
          selectInput(
            inputId = ns("custom_plot_y1_elems"),
            label = "Plot data",
            choices = OPTS$custom_plot_elems
          ),
          selectInput(
            inputId = ns("custom_plot_y1_climate_period"),
            label = "Climate period (if applicable)",
            choices = OPTS$climate_period_choices
          ),
          selectInput(
            inputId = ns("custom_plot_y1_smoothing"),
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
            inputId = ns("custom_plot_y2_elems"),
            label = "Plot data",
            choices = append(list("None" = "none"), OPTS$custom_plot_elems)
          ),
          selectInput(
            inputId = ns("custom_plot_y2_climate_period"),
            label = "Climate period (if applicable)",
            choices = OPTS$climate_period_choices
          ),
          selectInput(
            inputId = ns("custom_plot_y2_smoothing"),
            label = "Data smoothing",
            choices = OPTS$data_smoothing_choices
          )
        )
      )
    ),
    br(),
    plotlyOutput(ns("custom_plot"), height = "600px"),
    div(class = "plot-caption", HTML("Click on any legend item in the plot to show or hide it. Today's date is indicated as a vertical dashed line. Weather data originally sourced from NOAA and retrieved from <a href='https://agweather.cals.wisc.edu'>AgWeather</a>, climate data sourced from <a href='https://www.climatologylab.org/gridmet.html'>GridMET</a>."))
  )
}

plotServer <- function(selected_grid) {
  moduleServer(
    id = "plot",
    function(input, output, session) {
      ns <- session$ns

      rv <- reactiveValues(
        loc = NULL
      )

      observe({
        rv$loc <- req(selected_grid())
      })

      # set location data
      loc_data <- reactive({
        loc <- req(rv$loc)

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


      # Weather plot ----

      output$weather_plot <- renderPlotly({
        opts <- list()
        opts$loc <- req(rv$loc)
        opts$smoothing <- req(input$weather_smoothing) %>% as.numeric()

        plt_title <- case_match(
          opts$smoothing,
          1 ~ "Weather data",
          7 ~ "7-day average weather data",
          14 ~ "14-day average air temperature data"
        ) %>% sprintf("<b>%s for %.1f°N, %.1f°W</b>", ., opts$loc$lat, opts$loc$lng)
        df <- loc_data()$weather %>%
          mutate(across(
            all_of(OPTS$smoothable_weather),
            ~zoo::rollapply(.x, width = opts$smoothing, FUN = mean, na.rm = T, partial = T)
          ))

        plot_ly() %>%
          layout(
            title = list(
              text = plt_title,
              font = list(
                family = "Lato",
                size = 18)),
            legend = list(
              orientation = "h",
              xanchor = "center",
              x = .5, y = -.15),
            xaxis = list(
              title = "Date",
              hoverformat = "%b %d, %Y (day %j)",
              domain = c(0, .95)),
            hovermode = "x unified"
          ) %>%
          add_temp_traces(df, "y1") %>%
          add_gdd_cum_traces(df, "y2")
      })


      # Climate plot ----

      output$climate_plot <- renderPlotly({
        opts <- list()
        opts$loc <- req(rv$loc)
        opts$period <- req(input$climate_period)
        opts$smoothing <- req(input$climate_smoothing) %>% as.numeric()

        plt_title <- case_match(
          opts$smoothing,
          1 ~ "Daily average",
          7 ~ "7-day average",
          14 ~ "14-day average"
        ) %>% paste(case_match(
          opts$period,
          "c10" ~ "10-year climate data (2013-2023)",
          "c5" ~ "5-year climate data (2018-2023)"
        )) %>% sprintf("<b>%s</b>", .)
        df <- loc_data()[[opts$period]] %>%
          mutate(across(
            all_of(OPTS$smoothable_climate),
            ~zoo::rollapply(.x, width = opts$smoothing, FUN = mean, na.rm = T, partial = T))) %>%
          mutate(date = start_of_year() + yday - 1)

        plot_ly() %>%
          layout(
            title = list(
              text = plt_title,
              font = list(
                family = "Lato",
                size = 18)),
            legend = list(
              orientation = "h",
              xanchor = "center",
              x = .5, y = -.15),
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


      # Custom plot ----

      output$custom_plot <- renderPlotly({
        # capture inputs
        elems <- list(
          y1 = req(input$custom_plot_y1_elems),
          y2 = req(input$custom_plot_y2_elems)
        )
        i <- list(
          y1 = list(
            data = str_split_1(elems$y1, "_")[1],
            traces = str_split_1(elems$y1, "_")[2],
            climate = req(input$custom_plot_y1_climate_period),
            smoothing = as.numeric(req(input$custom_plot_y1_smoothing))
          ),
          y2 = list(
            data = str_split_1(elems$y2, "_")[1],
            traces = str_split_1(elems$y2, "_")[2],
            climate = req(input$custom_plot_y2_climate_period),
            smoothing = as.numeric(req(input$custom_plot_y2_smoothing))
          )
        )

        if (identical(i$y1, i$y2)) {
          elems$y2 <- "none"
        }

        # local vars
        loc <- req(rv$loc)
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
              orientation = "h",
              xanchor = "center",
              x = .5, y = -.15),
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

    } # end module
  )
}
