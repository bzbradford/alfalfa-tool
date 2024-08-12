
climatePlotUI <- function() {
  ns <- NS("climate_plot")
  uiOutput(ns("main_ui"))
}

climatePlotServer <- function(plot_data) {
  moduleServer(
    id = "climate_plot",
    function(input, output, session) {
      ns <- session$ns

      rv <- reactiveValues()

      observe({
        rv$loc <- plot_data()$loc
        rv$c10 <- plot_data()$c10
        rv$c5 <- plot_data()$c5
      })

      output$main_ui <- renderUI({
        validate(need(rv$loc, OPTS$location_validation_msg))

        tagList(
          uiOutput(ns("options_ui")),
          div(
            uiOutput(ns("plot_title")),
            plotlyOutput(ns("plot"), height = "600px"),
            div(class = "plot-caption", HTML("Click on any legend item in the plot to show or hide it. Today's date is indicated as a vertical dashed line. Climate data sourced from <a href='https://www.climatologylab.org/gridmet.html'>GridMET</a>."))
          )
        )
      })

      output$options_ui <- renderUI({
        bsCollapse(
          bsCollapsePanel(
            "Plot options",
            div(
              class = "inline-flex",
              radioButtons(
                inputId = ns("period"),
                label = "Climate dataset",
                choices = OPTS$climate_period_choices,
                inline = TRUE
              ),
              radioButtons(
                inputId = ns("smoothing"),
                label = "Data smoothing options",
                choices = OPTS$data_smoothing_choices,
                selected = 7,
                inline = TRUE
              )
            )
          )
        )
      })

      output$plot_title <- renderUI({
        loc <- req(rv$loc)
        period <- req(input$period)
        smoothing <- req(input$smoothing) %>% as.numeric()
        title <- case_match(
          smoothing,
          1 ~ "Daily average",
          7 ~ "7-day average",
          14 ~ "14-day average"
        ) %>% paste(case_match(
          period,
          "c10" ~ "10-year climate data (2013-2023)",
          "c5" ~ "5-year climate data (2018-2023)"
        )) %>%
          paste(sprintf("for %.1f°N, %.1f°W", loc$lat, loc$lng))
        h4(title, style = "text-align: center;")
      })

      output$plot <- renderPlotly({
        opts <- list()
        opts$loc <- req(rv$loc)
        opts$period <- req(input$period)
        opts$smoothing <- req(input$smoothing) %>% as.numeric()

        df <- rv[[opts$period]] %>%
          smooth_cols(opts$smoothing) %>%
          mutate(date = start_of_year() + yday - 1)

        plot_ly() %>%
          layout(
            legend = OPTS$plot_legend,
            xaxis = OPTS$plot_date_axis_climate,
            hovermode = "x unified"
          ) %>%
          add_today() %>%
          add_temp_traces(df, "y1") %>%
          add_frost_traces(df, "y2")
      })

    } # end module
  )
}
