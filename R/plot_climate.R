
climatePlotUI <- function() {
  ns <- NS("climate_plot")

  tagList(
    uiOutput(ns("options_ui")),
    div(
      uiOutput(ns("plot_title")),
      plotlyOutput(ns("plot"), height = "600px"),
      div(class = "plot-caption", HTML("Click on any legend item in the plot to show or hide it. Today's date is indicated as a vertical dashed line. Climate data sourced from <a href='https://www.climatologylab.org/gridmet.html'>GridMET</a>."))
    )
  )
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
          ),
          open = "Plot options"
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
        ))
        h4(title, style = "text-align: center;")
      })

      output$plot <- renderPlotly({
        opts <- list()
        opts$loc <- req(rv$loc)
        opts$period <- req(input$period)
        opts$smoothing <- req(input$smoothing) %>% as.numeric()

        df <- rv[[opts$period]] %>%
          mutate(across(
            all_of(OPTS$smoothable_climate),
            ~zoo::rollapply(.x, width = opts$smoothing, FUN = mean, na.rm = T, partial = T))) %>%
          mutate(date = start_of_year() + yday - 1)

        plot_ly() %>%
          layout(
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

    } # end module
  )
}
