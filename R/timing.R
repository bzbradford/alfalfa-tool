
timingUI <- function() {
  ns <- NS("timing")

  uiOutput(ns("main_ui"))
}

timingServer <- function(loc_data) {
  moduleServer(
    id = "timing",
    function(input, output, session) {
      ns <- session$ns

      rv <- reactiveValues()

      observe({
        rv$loc <- loc_data()$loc
        rv$weather <- loc_data()$weather
        rv$c10 <- loc_data()$c10
        rv$c5 <- loc_data()$c5
      })

      output$main_ui <- renderUI({
        validate(need(rv$loc, OPTS$location_validation_msg))

        tagList(
          uiOutput(ns("options_ui")),
          h4("Growing degree days and frost probability", align = "center"),
          uiOutput(ns("plot_title")),
          plotlyOutput(ns("plot"), height = "500px"),
          div(class = "plot-caption", HTML("Today's date is indicated as a vertical dashed line. Future degree-day accumulation estimated based on climate average GDD/day. Weather data originally sourced from NOAA and retrieved from <a href='https://agweather.cals.wisc.edu'>AgWeather</a>, climate data sourced from <a href='https://www.climatologylab.org/gridmet.html'>GridMET</a>."))

        )
      })

      output$options_ui <- renderUI({
        wellPanel(
          div(
            class = "inline-flex",
            radioButtons(
              inputId = ns("year"),
              label = "Year",
              choices = OPTS$weather_years
            ),
            radioButtons(
              inputId = ns("period"),
              label = "Climate period",
              choices = OPTS$climate_period_choices
            )
          ),
          uiOutput(ns("cut_dates_ui"))
        )
      })

      output$cut_dates_ui <- renderUI({
        yr <- req(input$year)
        initial_date <- max(start_of_year(yr), yesterday() - 28)
        div(
          class = "inline-flex",
          dateInput(
            inputId = ns("cut_date"),
            label = "Date of last cut",
            value = initial_date,
            format = "M d"
          ),
          div(
            actionButton(ns("add_date"), "+")
          )
        )
      })

      observe({
        rv$cut_dates <- req(input$cut_date)
      })


      plot_data <- reactive({
        opts <- list()
        opts$year <- req(input$year)
        opts$period <- req(input$period)
        opts$date <- req(rv$cut_dates)
        opts$yday <- yday(opts$date)

        cut_points <- c(-1, opts$yday, 366)
        wx <- req(rv$weather) %>%
          filter(year == opts$year) %>%
          select(date, yday, gdd41)
        cl <- req(rv[[opts$period]])
        cl_gdd <- cl %>%
          filter(yday > max(wx$yday)) %>%
          select(yday, gdd41) %>%
          mutate(date = start_of_year(opts$year) + yday - 1)
        cl_risk <- cl %>%
          select(yday, freeze_by)
        bind_rows(wx, cl_gdd) %>%
          left_join(cl_risk, join_by(yday)) %>%
          mutate(cutting = cut(yday, cut_points)) %>%
          mutate(gdd41cum = cumsum(gdd41)) %>%
          mutate(
            days_since_cut = n() - 1,
            gdd_since_cut = cumsum(gdd41),
            .by = cutting
          )
      })

      output$plot <- renderPlotly({
        opts <- list(year = req(input$year))

        plt <- plot_data() %>%
          plot_ly() %>%
          add_trace(
            name = "GDD41 since Jan 1",
            x = ~date, y = ~gdd41cum,
            type = "scatter", mode = "lines",
            hovertemplate = "%{y:.1f}",
            line = list(dash = "dot"),
            yaxis = "y1"
          ) %>%
          add_trace(
            name = "GDD41 since last cutting",
            x = ~date, y = ~gdd_since_cut,
            type = "scatter", mode = "lines",
            hovertemplate = "%{y:.1f}",
            yaxis = "y1"
          ) %>%
          add_trace(
            name = "Hard freeze prob.",
            x = ~date, y = ~freeze_by*100,
            type = "scatter", mode = "lines",
            hovertemplate = "%{y:.1f}%",
            line = list(
              color = "purple",
              shape = "spline",
              width = 1.5),
            yaxis = "y2"
          ) %>%
          layout(
            legend = OPTS$plot_legend,
            xaxis = OPTS$plot_date_axis_weather,
            yaxis = list(title = "Growing degree days (base 41F)"),
            yaxis2 = list(
              title = "Cumulative hard freeze probability",
              overlaying = "y",
              side = "right"
            ),
            hovermode = "x unified"
          )

        cut_zones <- list(
          rect(900, 1100, color = "green"),
          rect(0, 360, color = "blue")
        )

        if (opts$year == cur_yr) {
          plt %>% add_today(other_shapes = cut_zones)
        } else {
          plt %>% layout(shapes = cut_zones)
        }

      })

    } # end module
  )
}
