# Projected crop growth

growthUI <- function() {
  ns <- NS("growth")

  div(
    p(OPTS$growth_info),
    uiOutput(ns("main_ui"))
  )
}

growthServer <- function(loc_data) {
  moduleServer(
    id = "growth",
    function(input, output, session) {
      ns <- session$ns

      # Reactive Values ----

      rv <- reactiveValues(
        data = NULL,
        ready = FALSE
      )

      # assign incoming data
      observe({
        rv$data <- loc_data()
      })

      # enables the main UI on location select
      # observe({
      #   if (!is.null(rv$data)) rv$ready <- TRUE
      # })


      # Interface ----

      ## main_ui ----
      output$main_ui <- renderUI({
        validate(need(rv$data, OPTS$location_validation_msg))

        tagList(
          uiOutput(ns("options_ui")),
          plotlyOutput(ns("plot"), height = "500px"),
          div(class = "plot-caption", OPTS$growth_plot_caption),
          uiOutput(ns("info_ui")),
        )
      })

      ## options_ui ----
      output$options_ui <- renderUI({
        btn <- function(id, label) {
          actionButton(ns(id), label, class = "btn-sm", style = "height:35px; margin:5px;")
        }

        div(
          class = "well", style = "padding-bottom: 0px;",
          div(
            class = "inline-flex",
            div(
              div(tags$label("Date of last cut")),
              div(
                class = "inline-flex", style = "gap: 5px;",
                uiOutput(ns("date_ui")),
                div(
                  btn("date_jan1", "Jan 1"),
                  btn("date_today", "Today"),
                  btn("date_reset", "Reset")
                )
              )
            ),
            div(
              radioButtons(
                ns("climate"), "Climate data:",
                choices = OPTS$climate_period_choices
              )
            )
          )
        )
      })

      ### date_ui ----
      output$date_ui <- renderUI({
        dateInput(
          inputId = ns("cut_date"),
          label = NULL,
          min = OPTS$growth_min_date,
          max = OPTS$growth_max_date,
          value = first_truthy(input$cut_date, OPTS$growth_default_date),
          format = "M d, yyyy",
          width = "150px"
        )
      })

      ## handle date buttons ----
      observeEvent(input$date_jan1, {
        updateDateInput(inputId = "cut_date", value = start_of_year())
      })
      observeEvent(input$date_today, {
        updateDateInput(inputId = "cut_date", value = today())
      })
      observeEvent(input$date_reset, {
        updateDateInput(inputId = "cut_date", value = OPTS$growth_default_date)
      })


      ## plot_data ----
      growth_data <- reactive({
        df <- buildGrowthData(
          weather_data = req(rv$data$weather),
          climate_data = req(rv$data[[req(input$climate)]]),
          start_date = req(input$cut_date)
        )
      })

      plot_data <- reactive({
        buildGrowthPlot(
          df = growth_data(),
          loc = req(rv$data$loc)
        )
      })

      ## Plot ----
      output$plot <- renderPlotly({
        plot_data()$plt
      })

      ## Info UI ----
      output$info_ui <- renderUI({
        # filter to match plot data
        df <- plot_data()$df
        div(
          class = "well",
          buildGrowthInfo(df)
        )
      })


    } # end module
  )
}
