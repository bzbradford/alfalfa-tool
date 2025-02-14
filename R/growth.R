# Projected crop growth

growthUI <- function() {
  ns <- NS("growth")
  tagList(
    p(OPTS$growth_info),
    uiOutput(ns("main_ui"))
  )
}

growthServer <- function(loc_data) {
  moduleServer(
    id = "growth",
    function(input, output, session) {
      ns <- session$ns

      rv <- reactiveValues(
        initial_date = OPTS$growth_default_date,
        loc_ready = FALSE
      )

      # assign incoming data
      observe({
        rv$loc <- loc_data()$loc
        rv$weather <- loc_data()$weather
        rv$c10 <- loc_data()$c10
        rv$c5 <- loc_data()$c5
      })

      # enables the main UI on location select
      observe({
        if (!is.null(rv$loc)) rv$loc_ready <- TRUE
      })


      # Interface ----

      ## main_ui ----
      output$main_ui <- renderUI({
        validate(need(rv$loc_ready, OPTS$location_validation_msg))

        tagList(
          uiOutput(ns("options_ui")),
          plotlyOutput(ns("plot"), height = "500px"),
          div(class = "plot-caption", OPTS$growth_plot_caption)
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
                ns("climate"), "GDD projection and freeze risk:",
                choices = OPTS$climate_period_choices
              )
            )
          )
        )
      })

      ## date_ui ----
      output$date_ui <- renderUI({
        min_date <- OPTS$growth_min_date
        max_date <- OPTS$growth_max_date
        value <- clamp(rv$initial_date, min_date, max_date)
        dateInput(
          inputId = ns("cut_date"),
          label = NULL,
          min = min_date,
          max = max_date,
          value = value,
          format = "M d", width = "150px"
        )
      })

      ## handle date buttons ----
      observeEvent(input$date_jan1, {
        rv$initial_date <- start_of_year()
      })
      observeEvent(input$date_today, {
        rv$initial_date <- today()
      })
      observeEvent(input$date_reset, {
        rv$initial_date <- OPTS$growth_default_date
      })


      # Generate plot data ----
      plot_data <- reactive({
        climate_period <- req(input$climate)

        args <- list(
          weather_data <- req(rv$weather),
          climate_data <- req(rv[[climate_period]]),
          start_date <- req(input$cut_date)
        )

        do.call(buildGrowthData, args)
      })


      # Render plot ----
      output$plot <- renderPlotly({
        args <- list(
          df = plot_data(),
          loc = req(rv$loc)
        )

        do.call(buildGrowthPlot, args)
      })

    } # end module
  )
}
