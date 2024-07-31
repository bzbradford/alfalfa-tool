#- plots.R -#

weatherPlotUI <- function() {
  ns <- NS("weather_plot")

  tagList(
    uiOutput(ns("options_ui")),
    div(
      uiOutput(ns("plot_title")),
      plotlyOutput(ns("plot"), height = "600px"),
      div(class = "plot-caption", HTML("Click on any legend item in the plot to show or hide it. Weather data originally sourced from NOAA and retrieved from <a href='https://agweather.cals.wisc.edu/'>AgWeather</a>."))
    )
  )
}

weatherPlotServer <- function(plot_data) {
  moduleServer(
    id = "weather_plot",
    function(input, output, session) {
      ns <- session$ns

      rv <- reactiveValues()

      observe({
        rv$loc <- plot_data()$loc
        rv$weather <- plot_data()$weather
      })

      output$options_ui <- renderUI({
        bsCollapse(
          bsCollapsePanel(
            "Plot options",
            div(
              class = "inline-flex",
              radioButtons(
                inputId = ns("year"),
                label = "Weather year",
                choices = c(OPTS$weather_years, "All"),
                selected = coalesce(input$weather_years, cur_yr),
                inline = T
              ),
              radioButtons(
                inputId = ns("smoothing"),
                label = "Data smoothing options",
                choices = OPTS$data_smoothing_choices,
                inline = T
              ),
              radioButtons(
                inputId = ns("gdd_type"),
                label = "Include GDD",
                choices = c("Cumulative", "Daily"),
                inline = T
              )
            )
          )
        )
      })

      output$plot_title <- renderUI({
        loc <- req(rv$loc)
        smoothing <- req(input$smoothing) %>% as.numeric()
        title <- case_match(
          smoothing,
          1 ~ "Weather data",
          7 ~ "7-day average weather data",
          14 ~ "14-day average air temperature data"
        ) %>%
          paste(sprintf("for %.1f°N, %.1f°W", loc$lat, loc$lng))
        h4(title, style = "text-align: center;")
      })

      output$plot <- renderPlotly({
        opts <- list()
        opts$loc <- req(rv$loc)
        opts$year <- req(input$year)
        opts$smoothing <- as.numeric(req(input$smoothing))
        opts$gdd_type <- req(input$gdd_type)

        df <- rv$weather
        if (opts$year != "All") df <- filter(df, year == opts$year)
        df <- df %>% smooth_weather(opts$smoothing)

        plt <- plot_ly() %>%
          layout(
            legend = OPTS$plot_legend,
            xaxis = OPTS$plot_date_axis_weather,
            hovermode = "x unified"
          ) %>%
          add_temp_traces(df, "y1")

        plt <- if (opts$gdd_type == "Cumulative") {
          add_gdd_cum_traces(plt, df, "y2")
        } else {
          add_gdd_daily_traces(plt, df, "y2")
        }
        plt
      })

    } # end module
  )
}
