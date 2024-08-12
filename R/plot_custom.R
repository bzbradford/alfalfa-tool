
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


      # UI ----

      ## Plot options ----
      output$options_ui <- renderUI({
        bsCollapse(
          bsCollapsePanel(
            "Plot options",
            fluidRow(
              column(6,
                style = "padding-bottom: 0px;",
                selectInput(
                  inputId = ns("y1_elems"),
                  label = "Primary plot data",
                  choices = OPTS$custom_plot_elems,
                  width = "300px"
                ),
                uiOutput(ns("y1_opts"))
              ),
              column(6,
                selectInput(
                  inputId = ns("y2_elems"),
                  label = "Secondary plot data",
                  choices = append(
                    list("None" = "none"),
                    OPTS$custom_plot_elems
                  ),
                  width = "300px"
                ),
                uiOutput(ns("y2_opts"))
              )
            )

          ),
          open = "Plot options"
        )
      })

      ## Additonal plot options ----
      lapply(c("y1", "y2"), function(y) {
        id <- function(e) paste0(y, "_", e)
        output[[id("opts")]] <- renderUI({
          i <- list(
            elems = input[[id("elems")]],
            year = first(c(input[[id("year")]], first(OPTS$weather_years))),
            period = first(c(input[[id("period")]], first(OPTS$climate_period_choices))),
            smoothing = first(c(input[[id("smoothing")]], first(OPTS$data_smoothing_choices)))
          )

          if (i$elems == "none") return()

          # show either the weather year or climate period
          elems <- list()
          if (grepl("weather", i$elems)) {
            elems$year <- selectInput(
              ns(id("year")), "Weather year",
              choices = OPTS$weather_years,
              selected = i$year
            )
          } else {
            elems$period <- selectInput(
              ns(id("period")), "Climate period",
              choices = OPTS$climate_period_choices,
              selected = i$period
            )
          }

          elems$smoothing <- selectInput(
            ns(id("smoothing")), "Data smoothing",
            choices = OPTS$data_smoothing_choices,
            selected = i$smoothing
          )

          div(class = "inline-flex", elems)
        })
      })


      ## Plot title ----
      output$plot_title <- renderUI({
        loc <- req(rv$loc)
        title <- sprintf("Weather/climate data for %.1f°N, %.1f°W", loc$lat, loc$lng)
        h4(title, style = "text-align: center;")
      })


      # Render plot ----

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
        i <- sapply(c("y1", "y2"), function(y) {
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
        }, simplify = FALSE)

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
            rv$weather %>%
              filter(year == opts$year) %>%
              mutate(date = start_of_year() + yday - 1) %>%
              smooth_cols(opts$smoothing)
          } else {
            rv[[opts$period]] %>% smooth_cols(opts$smoothing)
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

        plt %>% add_today(yr = opts$year)
      })

    } # end module
  )
}
