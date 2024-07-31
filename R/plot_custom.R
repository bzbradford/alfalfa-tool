
customPlotUI <- function() {
  ns <- NS("custom_plot")

  tagList(
    uiOutput(ns("options_ui")),
    div(
      uiOutput(ns("plot_title")),
      plotlyOutput(ns("plot"), height = "600px"),
      div(class = "plot-caption", HTML("Click on any legend item in the plot to show or hide it. Today's date is indicated as a vertical dashed line. Weather data originally sourced from NOAA and retrieved from <a href='https://agweather.cals.wisc.edu'>AgWeather</a>, climate data sourced from <a href='https://www.climatologylab.org/gridmet.html'>GridMET</a>."))
    )
  )
}

customPlotServer <- function(plot_data) {
  moduleServer(
    id = "custom_plot",
    function(input, output, session) {
      ns <- session$ns

      rv <- reactiveValues()

      observe({
        rv$loc <- plot_data()$loc
        rv$weather <- plot_data()$weather
        rv$c10 <- plot_data()$c10
        rv$c5 <- plot_data()$c5
      })

      output$options_ui <- renderUI({
        bsCollapse(
          bsCollapsePanel(
            "Plot options",
            h4("Primary plot elements"),
            div(
              class = "inline-flex",
              selectInput(
                inputId = ns("y1_elems"),
                label = "Plot data",
                choices = OPTS$custom_plot_elems
              ),
              uiOutput(ns("y1_opts"))
            ),
            h4("Secondary plot elements"),
            div(
              class = "inline-flex",
              selectInput(
                inputId = ns("y2_elems"),
                label = "Plot data",
                choices = append(
                  list("None" = "none"),
                  OPTS$custom_plot_elems
                )
              ),
              uiOutput(ns("y2_opts"))
            )
          ),
          open = "Plot options"
        )
      })

      output$y1_opts <- renderUI({
        i <- list(
          elems = input$y1_elems,
          year = first(c(input$y1_year, first(OPTS$weather_years))),
          period = first(c(input$y1_period, first(OPTS$climate_period_choices))),
          smoothing = first(c(input$y1_smoothing, first(OPTS$data_smoothing_choices)))
        )

        elems <- list()

        # show either the weather year or climate period
        if (grepl("weather", i$elems)) {
          elems$year <- selectInput(
            inputId = ns("y1_year"),
            label = "Weather year",
            choices = c(OPTS$weather_years, "All"),
            selected = i$year
          )
        } else {
          elems$period <- selectInput(
            inputId = ns("y1_period"),
            label = "Climate period",
            choices = OPTS$climate_period_choices,
            selected = i$period
          )
        }

        elems$smoothing <- selectInput(
          inputId = ns("y1_smoothing"),
          label = "Data smoothing",
          choices = OPTS$data_smoothing_choices,
          selected = i$smoothing
        )

        div(class = "inline-flex", elems)
      })

      output$y2_opts <- renderUI({
        i <- list(
          elems = input$y2_elems,
          year = first(c(input$y2_year, first(OPTS$weather_years))),
          period = first(c(input$y2_period, first(OPTS$climate_period_choices))),
          smoothing = first(c(input$y2_smoothing, first(OPTS$data_smoothing_choices)))
        )

        if (i$elems == "none") return()

        elems <- list()

        # show either the weather year or climate period
        if (grepl("weather", i$elems)) {
          elems$year <- selectInput(
            inputId = ns("y2_year"),
            label = "Weather year",
            choices = c(OPTS$weather_years, "All"),
            selected = i$year
          )
        } else {
          elems$period <- selectInput(
            inputId = ns("y2_period"),
            label = "Climate period",
            choices = OPTS$climate_period_choices,
            selected = i$period
          )
        }

        elems$smoothing <- selectInput(
          inputId = ns("y2_smoothing"),
          label = "Data smoothing",
          choices = OPTS$data_smoothing_choices,
          selected = i$smoothing
        )

        div(class = "inline-flex", elems)
      })


      # lapply(c("y1", "y2"), function(y) {
      #   id <- function(e) paste0(y, "_", e)
      #   output[[id("opts")]] <- renderUI({
      #     i <- list(
      #       elems = input[[id("elems")]],
      #       year = first(c(input[[id("year")]], first(OPTS$weather_years))),
      #       period = first(c(input[[id("period")]], first(OPTS$climate_period_choices))),
      #       smoothing = first(c(input[[id("smoothing")]], first(OPTS$data_smoothing_choices)))
      #     )
      #
      #     if (i$elems == "none") return()
      #
      #     elems <- list()
      #
      #     # show either the weather year or climate period
      #     if (grepl("weather", i$elems)) {
      #       elems$year <- selectInput(
      #         inputId = ns(id("year")),
      #         label = "Weather year",
      #         choices = c(OPTS$weather_years, "All"),
      #         selected = i$year
      #       )
      #     } else {
      #       elems$period <- selectInput(
      #         inputId = ns(id("period")),
      #         label = "Climate period",
      #         choices = OPTS$climate_period_choices,
      #         selected = i$period
      #       )
      #     }
      #
      #     elems$smoothing <- selectInput(
      #       inputId = ns(id("smoothing")),
      #       label = "Data smoothing",
      #       choices = OPTS$data_smoothing_choices,
      #       selected = i$smoothing
      #     )
      #
      #     div(class = "inline-flex", elems)
      #   })
      # })

      output$plot_title <- renderUI({
        loc <- req(rv$loc)
        title <- sprintf("Weather/climate data for %.1f°N, %.1f°W", loc$lat, loc$lng)
        h4(title, style = "text-align: center;")
      })

      make_trace_label <- function(opts) {
        type <- str_to_sentence(opts$data)
        info <- paste(c(
          if (opts$data == "climate") {
            list(c10 = "10-year", c5 = "5-year")[[opts$period]]
          } else {
            opts$year
          },
          if (opts$smoothing != 1) paste0(opts$smoothing, "-day")
        ), collapse = ", ")
        if (info != "") info <- paste0("(", info, ")")
        paste0(type, info, ": ")
      }

      output$plot <- renderPlotly({
        # capture inputs
        elems <- list(
          y1 = req(input$y1_elems),
          y2 = req(input$y2_elems)
        )
        i <- lapply2(c("y1", "y2"), function(y) {
          id <- function(e) paste0(y, "_", e)
          i <- list(
            data = str_split_1(elems[[y]], "_")[1],
            traces = str_split_1(elems[[y]], "_")[2]
          )
          if (i$data == "none") return()
          i$smoothing <- as.numeric(req(input[[id("smoothing")]]))
          if (i$data == "weather") {
            i$year <- req(input[[id("year")]])
          } else {
            i$period <- req(input[[id("period")]])
          }
          i
        })

        if (identical(i$y1, i$y2)) {
          elems$y2 <- "none"
        }

        # base plot
        plt <- plot_ly() %>%
          layout(
            legend = OPTS$plot_legend,
            xaxis = OPTS$plot_date_axis_climate,
            hovermode = "x unified"
          )

        # add traces as necessary
        for (axis in c("y1", "y2")) {
          if (elems[[axis]] == "none") next

          opts <- i[[axis]]
          opts$dash <- FALSE
          opts$label <- make_trace_label(opts)

          # if the same trace type on both sides so make 2nd dashed and don't repeat axis
          if (axis == "y2") {
            if (i$y1$traces == i$y2$traces) {
              opts$dash <- TRUE
              axis <- "y1"
            }
          }

          # apply smoothing
          df <- if (opts$data == "weather") {
            df <- rv$weather %>%
              filter(year == opts$year) %>%
              smooth_weather(opts$smoothing)
            if (opts$year != cur_yr) {
              df <- df %>% mutate(date = start_of_year() + yday - 1)
            }
            df
          } else {
            rv[[opts$period]] %>% smooth_climate(opts$smoothing)
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

        plt %>% layout(
          shapes = list(vline())
        )
      })

    } # end module
  )
}
