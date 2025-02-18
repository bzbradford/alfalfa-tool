# Full-season cut timing

timingUI <- function() {
  ns <- NS("timing")
  tagList(
    p(OPTS$timing_info),
    uiOutput(ns("main_ui"))
  )
}

timingServer <- function(loc_data) {
  moduleServer(
    id = "timing",
    function(input, output, session) {
      ns <- session$ns

      # Reactives ----

      rv <- reactiveValues(
        loc = NULL,
        loc_ready = FALSE,
        weather = NULL,
        c10 = NULL,
        c5 = NULL,
        initial_cut_dates = NULL,
        set_cut_dates = NULL,
        date_ui_ready = FALSE,
        plot_export_ready = TRUE
      )

      # store incoming location data in rv
      observe({
        rv$loc <- loc_data()$loc
        rv$weather <- loc_data()$weather
        rv$c10 <- loc_data()$c10
        rv$c5 <- loc_data()$c5
      })

      # hide UI until a location is selected
      observe({
        if (!is.null(rv$loc)) rv$loc_ready <- TRUE
      })

      # determine initial cut timing based on gdd accumulation
      schedule_cut_dates <- function() {
        df <- plot_data()
        freq <- as.numeric(req(input$cut_freq))
        cuts <- seq(freq, max(df$gdd_since_kill), by = freq)
        days <- sapply(cuts, function(gdd) {
          df[which.min(abs(gdd - df$gdd_since_kill)), ]$yday
        })
        days <- days[between(days, 60, 300)]
        pause_date_reader()
        rv$initial_cut_dates <- NULL
        rv$initial_cut_dates <- start_of_year() + unique(days) - 1
      }


      # Interface ----

      ## Main UI ----

      output$main_ui <- renderUI({
        validate(need(rv$loc_ready, OPTS$location_validation_msg))

        tagList(
          uiOutput(ns("options_ui")),
          plotlyOutput(ns("plot"), height = "500px"),
          div(class = "plot-caption", OPTS$timing_plot_caption)
        )
      })


      ## Options UI ----

      output$options_ui <- renderUI({
        div(
          class = "well",
          fluidRow(
            column(
              6,
              div(
                class = "inline-flex",
                radioButtons(
                  ns("year"), "Year",
                  choices = OPTS$weather_years
                ),
                radioButtons(
                  ns("period"), "Climate data",
                  choices = OPTS$climate_period_choices
                )
              )
            ),
            column(6, uiOutput(ns("auto_cut_ui")))
          ),
          div(
            tags$label("Cutting dates"),
            uiOutput(ns("cut_dates_ui"))
          )
        )
      })


      ## Auto cut scheduling ----

      output$auto_cut_ui <- renderUI({
        div(
          div(tags$label("Schedule cuts by cumulative GDD")),
          div(
            style = "display: inline-flex; gap: 20px;",
            selectInput(
              ns("cut_freq"),
              label = NULL,
              choices = OPTS$cut_freq_choices,
              selected = OPTS$cut_freq_default,
              width = "100px"
            ),
            actionButton(ns("apply_cut_freq"), "Apply", class = "btn-sm", style = "height: 34px;")
          )
        )
      })

      # handle 'apply' button
      observeEvent(input$apply_cut_freq, schedule_cut_dates())

      # auto schedule when no dates are set
      observe({
        if (is.null(rv$initial_cut_dates)) schedule_cut_dates()
      })


      ## Cutting dates UI ----

      cut_date_id <- function(i) paste0("cut_date-", i)
      remove_cut_date_id <- function(i) paste0("remove_cut_date-", i)

      output$cut_dates_ui <- renderUI({
        yr <- req(input$year)
        cut_dates <- req(rv$initial_cut_dates)
        min_date <- start_of_year(yr)
        max_date <- end_of_year(yr)
        cut_dates <- min_date + sort(unique(yday(cut_dates))) - 1
        n_dates <- length(cut_dates)
        inputs <- list()

        for (i in 1:n_dates) {
          id <- cut_date_id(i)
          elems <- list()
          elems$input <- dateInput(
            inputId = ns(id), label = NULL,
            min = min_date, max = max_date,
            value = clamp(cut_dates[i], min_date, max_date),
            format = "M d", width = "100px"
          )
          if (n_dates > 1) {
            elems$remove <- div(
              style = "text-align: center",
              actionLink(ns(remove_cut_date_id(i)), "Remove")
            )
          }
          inputs[[id]] <- div(elems)
        }

        btn <- function(id) {
          actionButton(
            ns(id), icon("plus"),
            class = "btn-sm", style = "height: 45px;",
            disabled = length(inputs) == OPTS$max_cut_dates
          )
        }

        div(
          class = "inline-flex",
          btn("add_cut_before"),
          inputs,
          btn("add_cut_after"),
          HTML("<script>Shiny.setInputValue('timing-date_ui_ready', true);</script>")
        )
      })


      ## Read and store cutting dates ----

      pause_date_reader <- function() {
        rv$date_ui_ready <- FALSE
        runjs("Shiny.setInputValue('timing-date_ui_ready', false);")
      }

      observe({
        rv$date_ui_ready <- req(input$date_ui_ready)
      })

      observe({
        req(rv$date_ui_ready)
        n_dates <- length(req(rv$initial_cut_dates))
        dates <- sapply(1:n_dates, function(i) {
          req(input[[cut_date_id(i)]])
        }) %>% as.Date()
        if (!identical(dates, sort(dates))) {
          pause_date_reader()
          rv$initial_cut_dates <- dates
        } else {
          rv$set_cut_dates <- dates
        }
      })


      ## Handle adding/removing dates ----

      # add another date halfway to Jan 1
      observeEvent(input$add_cut_before, {
        yr <- req(input$year)
        dates <- req(rv$set_cut_dates)
        new_date <- max(start_of_year(yr), as.Date(first(dates)) - 28)
        new_dates <- unique(c(new_date, dates))
        pause_date_reader()
        rv$initial_cut_dates <- new_dates
      })

      # add another date halfway to Dec 31
      observeEvent(input$add_cut_after, {
        yr <- req(input$year)
        dates <- req(rv$set_cut_dates)
        new_date <- min(as.Date(last(dates)) + 28, end_of_year(yr))
        new_dates <- unique(c(dates, new_date))
        pause_date_reader()
        rv$initial_cut_dates <- new_dates
      })

      # handle date removal
      lapply(1:OPTS$max_cut_dates, function(i) {
        id <- remove_cut_date_id(i)
        observeEvent(input[[id]], {
          dates <- req(rv$set_cut_dates)
          dates <- dates[-i]
          pause_date_reader()
          rv$initial_cut_dates <- dates
        })
      })


      # Plot data ----
      plot_data <- reactive({
        buildGrowthData(
          weather_data = req(rv$weather),
          climate_data = req(rv[[req(input$period)]]),
          start_date = start_of_year(req(input$year))
        )
      })


      # Plot ----

      output$plot <- renderPlotly({
        # make sure dates are in the right order
        cut_dates <- req(rv$set_cut_dates)
        req(identical(cut_dates, sort(unique(cut_dates))))

        buildTimingPlot(
          df = plot_data(),
          loc = req(rv$loc),
          weather_year = req(input$year),
          cut_dates = cut_dates
        )
      })

    } # end module
  )
}
