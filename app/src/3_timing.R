# Full-season cut timing

timingUI <- function() {
  ns <- NS("timing")

  div(
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
        data = NULL,
        initial_cut_dates = NULL,
        set_cut_dates = NULL,
        held = NULL, # day-of-year of cuts the user has locked in place
        date_ui_ready = FALSE,
        plot_export_ready = TRUE
      )

      # store incoming location data in rv
      observe({
        rv$data <- loc_data()
        rv$held <- NULL
        isolate(schedule_cut_dates())
      })

      # auto schedule when no dates are set
      observe({
        if (is.null(rv$initial_cut_dates)) schedule_cut_dates()
      })

      n_cut_dates <- reactive({
        length(req(rv$initial_cut_dates))
      })

      # determine cut timing from gdd accumulation, anchored on any held cuts
      schedule_cut_dates <- function() {
        df <- plot_data()
        echo(df)
        freq <- as.numeric(req(input$cut_freq))

        # convert between day-of-year and cumulative gdd since last kill
        gdd_on <- function(yd) df$gdd_since_kill[which.min(abs(df$yday - yd))]
        day_at <- function(gdd) df$yday[which.min(abs(df$gdd_since_kill - gdd))]

        # held cuts anchor the schedule (in gdd since kill); spring regrowth (0)
        # is the first anchor. gaps between anchors are divided evenly into
        # ~freq-sized steps, and the open-ended final gap is filled at freq.
        held <- sort(rv$held)
        anchors <- c(0, vapply(held, gdd_on, numeric(1)))
        max_gdd <- max(df$gdd_since_kill)
        interior <- unlist(lapply(seq_len(length(anchors) - 1), function(i) {
          a <- anchors[i]
          b <- anchors[i + 1]
          n <- round((b - a) / freq)
          if (n >= 2) a + (b - a) * seq_len(n - 1) / n else c()
        }))
        last_anchor <- anchors[length(anchors)]
        tail_gdd <- if (last_anchor + freq <= max_gdd) {
          seq(last_anchor + freq, max_gdd, by = freq)
        } else {
          c()
        }
        filled <- c(interior, tail_gdd)
        days <- if (length(filled)) vapply(filled, day_at, numeric(1)) else c()
        days <- days[between(days, 60, 300)]
        days <- sort(unique(c(held, days)))

        # keep the final fall cut out of the risky 360-800 gdd overwintering
        # window by dropping trailing (non-held) cuts until the last regrowth is
        # either minimal (<360) or substantial (>800) before the first fall kill

        # fall <- filter(df, kill, yday > 150)
        # if (nrow(fall) == 0) {
        #   fall <- slice_min(filter(df, yday > 200), abs(kill_by - .5))
        # }
        # if (nrow(fall) > 0) {
        #   kill_yday <- first(fall$yday)
        #   kill_gdd <- gdd_on(kill_yday - 1)
        #   while (any(days < kill_yday)) {
        #     last_cut <- max(days[days < kill_yday])
        #     regrowth <- kill_gdd - gdd_on(last_cut)
        #     if (last_cut %in% held || regrowth <= 360 || regrowth >= 800) {
        #       break
        #     }
        #     days <- days[days != last_cut]
        #   }
        # }

        pause_date_reader()
        rv$initial_cut_dates <- NULL
        rv$initial_cut_dates <- start_of_year(req(input$year)) + days - 1
      }

      # Interface ----

      ## Main UI ----

      output$main_ui <- renderUI({
        validate(need(rv$data, OPTS$location_validation_msg))

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

          # weather/climate selector
          fluidRow(
            column(
              6,
              radioButtons(
                ns("year"),
                "Weather year",
                choices = OPTS$weather_years
              ),
            ),
            column(
              6,
              radioButtons(
                ns("climate"),
                "Climate data",
                choices = OPTS$climate_period_choices
              )
            )
          ),

          # scheduler
          fluidRow(
            column(
              6,
              uiOutput(ns("schedule_by_gdd"))
            ),
            column(6, uiOutput(ns("schedule_by_cuts")))
          ),

          # date display
          div(
            tags$label("Planned cutting dates"),
            uiOutput(ns("cut_dates_ui"))
          )
        )
      })

      ## Cutting dates UI ----

      cut_date_id <- function(i) paste0("cut_date-", i)
      remove_cut_date_id <- function(i) paste0("remove_cut_date-", i)
      hold_id <- function(i) paste0("hold_cut_date-", i)

      output$cut_dates_ui <- renderUI({
        yr <- req(input$year)
        cut_dates <- req(rv$initial_cut_dates)
        held <- isolate(rv$held) # reflect held state without re-rendering on toggle
        min_date <- start_of_year(yr)
        max_date <- end_of_year(yr)
        cut_dates <- min_date + sort(unique(yday(cut_dates))) - 1
        n_dates <- length(cut_dates)
        inputs <- list()

        for (i in 1:n_dates) {
          id <- cut_date_id(i)
          inputs[[id]] <-
            div(
              class = "cut-date-container",
              div(
                style = "display: flex; align-items: center;",
                strong(paste0(i, ":")),
              ),
              dateInput(
                inputId = ns(id),
                label = NULL,
                min = min_date,
                max = max_date,
                value = clamp(cut_dates[i], min_date, max_date),
                format = "M d",
                width = "100px"
              ),
              div(
                class = "cut-date-controls",
                div(
                  class = "hold-toggle",
                  title = "Hold this cutting date when rescheduling",
                  checkboxInput(
                    ns(hold_id(i)),
                    label = tagList(
                      icon("lock-open", class = "fa-lg lock-off"),
                      icon("lock", class = "fa-lg lock-on")
                    ),
                    value = yday(cut_dates[i]) %in% held
                  )
                ),
                if (n_dates > 1) {
                  actionLink(
                    ns(remove_cut_date_id(i)),
                    icon("trash"),
                    class = "fa-lg remove-cut",
                    title = "Remove this cutting date"
                  )
                }
              )
            )
        }

        tagList(
          div(
            class = "cut-date-list",
            inputs,
            if (i < OPTS$max_cut_dates) {
              div(
                class = "cut-date-container",
                div(
                  actionButton(
                    class = "btn-sm",
                    ns("add_cut"),
                    "Add cut"
                  )
                )
              )
            }
          ),
          HTML(
            "<script>Shiny.setInputValue('timing-date_ui_ready', true);</script>"
          )
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
        n_dates <- n_cut_dates()
        dates <- sapply(1:n_dates, function(i) {
          req(input[[cut_date_id(i)]])
        }) |>
          as.Date()
        holds <- sapply(1:n_dates, function(i) isTRUE(input[[hold_id(i)]]))
        rv$held <- sort(unique(yday(dates[holds])))
        if (!identical(dates, sort(dates))) {
          pause_date_reader()
          rv$initial_cut_dates <- dates
        } else {
          rv$set_cut_dates <- dates
        }
      })

      ## Handle adding/removing dates ----

      # add another date halfway to Jan 1
      # observeEvent(input$add_cut_before, {
      #   yr <- req(input$year)
      #   dates <- req(rv$set_cut_dates)
      #   new_date <- max(start_of_year(yr), as.Date(first(dates)) - 28)
      #   new_dates <- unique(c(new_date, dates))
      #   pause_date_reader()
      #   rv$initial_cut_dates <- new_dates
      # })

      # add another date halfway to Dec 31
      observeEvent(input$add_cut, {
        yr <- req(input$year)
        dates <- req(rv$set_cut_dates)
        new_date <- min(as.Date(last(dates)) + 28, end_of_year(yr))
        new_dates <- unique(c(dates, new_date))
        pause_date_reader()
        rv$initial_cut_dates <- new_dates
        schedule_cut_dates()
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

      ## Schedule by GDD ----

      output$schedule_by_gdd <- renderUI({
        choices <- OPTS$cut_freq_choices
        choices <- set_names(choices, paste(choices, "GDD"))
        div(
          tags$label("Target growth interval:"),
          div(
            style = "display: inline-flex; gap: 20px;",
            selectInput(
              ns("cut_freq"),
              label = NULL,
              choices = choices,
              selected = OPTS$cut_freq_default,
              width = "120px"
            ),
            actionButton(
              class = "btn-sm",
              style = "height: 34px;",
              ns("apply_cut_freq"),
              "Apply"
            )
          )
        )
      })

      # handle 'apply' button
      observeEvent(input$apply_cut_freq, schedule_cut_dates())

      ## Schedule by number of cuts ----
      output$schedule_by_cuts <- renderUI({
        div(
          tags$label("Target number of cuts:"),
          div(
            style = "display: inline-flex; gap: 20px;",
            sliderInput(
              ns("cut_num"),
              label = NULL,
              value = n_cut_dates(),
              min = 2,
              max = OPTS$max_cut_dates,
              step = 1,
              ticks = FALSE,
              width = "120px"
            ),
            actionButton(
              class = "btn-sm",
              style = "height: 34px;",
              ns("apply_num_cuts"),
              "Apply"
            )
          )
        )
      })

      # Plot data ----

      plot_data <- reactive({
        buildGrowthData(
          weather_data = req(rv$data$weather),
          climate_data = req(rv$data[[req(input$climate)]]),
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
          loc = req(rv$data$loc),
          weather_year = req(input$year),
          cut_dates = cut_dates,
          held_ydays = rv$held
        )
      })
    } # end module
  )
}
