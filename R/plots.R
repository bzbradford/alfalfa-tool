#- plots.R -#

plotUI <- function() {
  ns <- NS("plot")
  tagList(
    p("View detailed weather and climate charts for individual locations selected on the map."),
    uiOutput(ns("main_ui"))
  )
}

plotServer <- function(loc_data) {
  moduleServer(
    id = "plot",
    function(input, output, session) {
      ns <- session$ns

      rv <- reactiveValues(
        loc = NULL,
        loc_ready = FALSE
      )

      observe({
        rv$loc <- loc_data()$loc
      })

      observe({
        if (!is.null(rv$loc)) rv$loc_ready <- TRUE
      })


      # Main UI ----

      output$main_ui <- renderUI({
        validate(need(rv$loc_ready, OPTS$location_validation_msg))

        tagList(
          tabsetPanel(
            tabPanel("Weather plot", uiOutput(ns("weather_ui"))),
            tabPanel("Climate plot", uiOutput(ns("climate_ui"))),
            tabPanel("Custom plot", uiOutput(ns("custom_ui")))
          )
        )
      })


      # Weather plot ----

      ## weather_ui ----
      output$weather_ui <- renderUI({
        choices <- list(
          year = c(OPTS$weather_years, "All"),
          smoothing = OPTS$data_smoothing_choices,
          gdd = c("None", "Cumulative", "Daily")
        )

        tagList(
          bsCollapse(
            bsCollapsePanel(
              "Plot options",
              div(
                class = "inline-flex",
                radioButtons(
                  ns("weather_year"), "Weather year",
                  choices = choices$year,
                  inline = TRUE
                ),
                radioButtons(
                  ns("weather_smoothing"), "Data smoothing options",
                  choices = choices$smoothing,
                  inline = TRUE
                ),
                radioButtons(
                  ns("weather_gdd"), "Show growing degree days",
                  choices = choices$gdd,
                  inline = TRUE
                )
              )
            )
          ),
          plotlyOutput(ns("weather_plot"), height = "600px"),
          div(class = "plot-caption", OPTS$weather_plot_caption)
        )
      })

      ## weather_plot ----
      output$weather_plot <- renderPlotly({
        opts <- list(
          loc = loc_data()$loc,
          year = req(input$weather_year),
          smoothing = req(input$weather_smoothing),
          gdd_type = req(input$weather_gdd)
        )
        opts$title <- paste(
          { if (opts$year != "All") opts$year else paste(rev(OPTS$weather_years), collapse = "-") },
          sprintf("Weather data for %.1f°N, %.1f°W", opts$loc$lat, opts$loc$lng)
        )
        df <- loc_data()$weather

        if (opts$year != "All") df <- filter(df, year == opts$year)
        df <- df %>% smooth_cols(opts$smoothing)

        plt <- plot_ly() %>%
          layout(
            title = list(
              text = opts$title,
              yanchor = "bottom"),
            legend = OPTS$plot_legend,
            xaxis = OPTS$plot_date_axis_weather,
            hovermode = "x unified",
            margin = list(t = 50),
            modebar = list(
              remove = list("pan", "select", "lasso", "zoom", "autoscale")
            )
          ) %>%
          config(
            toImageButtonOptions = list(
              format = "png",
              filename = opts$title,
              height = 600,
              width = 1000,
              scale = 1.25
            )
          ) %>%
          add_temp_traces(df, "y1")

        if (opts$gdd_type == "Cumulative")
          plt <- add_gdd_cum_traces(plt, df, "y2")
        else if (opts$gdd_type == "Daily")
          plt <- add_gdd_daily_traces(plt, df, "y2")

        if (opts$year == "All") opts$year <- cur_yr
        plt %>% add_today(yr = opts$year, date_yr = opts$year)
      })


      # Climate plot ----

      ## climate_ui ----
      output$climate_ui <- renderUI({
        tagList(
          bsCollapse(
            bsCollapsePanel(
              "Plot options",
              div(
                class = "inline-flex",
                radioButtons(
                  ns("climate_period"), "Climate dataset",
                  choices = OPTS$climate_period_choices,
                  inline = TRUE
                ),
                radioButtons(
                  ns("climate_frost"), "Frost threshold",
                  choices = OPTS$climate_frost_choices,
                  inline = TRUE
                ),
                radioButtons(
                  ns("climate_smoothing"), "Data smoothing options",
                  choices = OPTS$data_smoothing_choices,
                  selected = 7,
                  inline = TRUE
                )
              )
            )
          ),
          plotlyOutput(ns("climate_plot"), height = "600px"),
          div(class = "plot-caption", OPTS$weather_plot_caption)
        )
      })

      ## climate_plot ----
      output$climate_plot <- renderPlotly({
        opts <- list(
          loc = loc_data()$loc,
          period = req(input$climate_period),
          frost = req(input$climate_frost),
          smoothing = req(input$climate_smoothing)
        )
        opts$title <- paste(
          invert(OPTS$climate_period_choices)[[opts$period]],
          sprintf("data for %.1f°N, %.1f°W", opts$loc$lat, opts$loc$lng)
        )

        df <- loc_data()[[opts$period]] %>%
          smooth_cols(opts$smoothing) %>%
          mutate(date = start_of_year() + yday - 1)

        plt <- plot_ly() %>%
          layout(
            title = list(
              text = opts$title,
              yanchor = "bottom"),
            legend = OPTS$plot_legend,
            xaxis = OPTS$plot_date_axis_climate,
            hovermode = "x unified",
            margin = list(t = 50),
            modebar = list(
              remove = list("pan", "select", "lasso", "zoom", "autoscale"))
          ) %>%
          config(
            toImageButtonOptions = list(
              format = "png",
              filename = opts$title,
              height = 600,
              width = 1000,
              scale = 1.25
            )
          ) %>%
          add_today() %>%
          add_temp_traces(df, "y1")

        if (opts$frost == "frost") {
          plt %>% add_frost_traces(df, "y2")
        } else if (opts$frost == "freeze") {
          plt %>% add_freeze_traces(df, "y2")
        } else if (opts$frost == "kill") {
          plt %>% add_kill_traces(df, "y2")
        }
      })


      # Custom plot ----

      ## custom_plot_ui ----
      output$custom_ui <- renderUI({
        tagList(
          bsCollapse(
            bsCollapsePanel(
              "Plot options",
              fluidRow(
                column(6,
                  style = "padding-bottom: 0px;",
                  selectInput(
                    ns("y1_elems"), "Primary plot data",
                    choices = OPTS$custom_plot_elems,
                    width = "300px"
                  ),
                  uiOutput(ns("y1_opts"))
                ),
                column(6,
                  selectInput(
                    ns("y2_elems"), "Secondary plot data",
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
          ),
          plotlyOutput(ns("custom_plot"), height = "600px"),
          div(class = "plot-caption", OPTS$weather_plot_caption)
        )
      })

      ## custom y1_opts // y2_opts ----
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

      ## custom_plot ----
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
        paste0(paste(type, info), ": ")
      }

      output$custom_plot <- renderPlotly({

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

        opts <- list(loc = req(rv$loc))
        opts$title <- sprintf("Weather/climate data for %.1f°N, %.1f°W", opts$loc$lat, opts$loc$lng)

        # base plot
        plt <- plot_ly() %>%
          layout(
            title = list(
              text = opts$title,
              yanchor = "bottom"),
            legend = OPTS$plot_legend,
            xaxis = OPTS$plot_date_axis_climate,
            hovermode = "x unified",
            margin = list(t = 50),
            modebar = list(
              remove = list("pan", "select", "lasso", "zoom", "autoscale"))
          ) %>%
          config(
            toImageButtonOptions = list(
              format = "png",
              filename = opts$title,
              height = 600,
              width = 1000,
              scale = 1.25
            )
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
            loc_data()$weather %>%
              filter(year == opts$year) %>%
              mutate(date = start_of_year() + yday - 1) %>%
              smooth_cols(opts$smoothing)
          } else {
            loc_data()[[opts$period]] %>% smooth_cols(opts$smoothing)
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
              add_frost_freeze_kill_traces(plt, df, axis)
            } else {
              plt
            }
        }

        plt %>% add_today(yr = coalesce(opts$year, ""))
      })

    } # end module
  )
}
