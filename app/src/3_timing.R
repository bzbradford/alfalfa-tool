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

      # store incoming location data in rv and reset schedule
      observe({
        rv$data <- loc_data()
        isolate({
          rv$initial_cut_dates <- NULL
          rv$set_cut_dates <- NULL
          rv$held <- NULL
          set_cut_dates(schedule_by_gdd())
        })
      })

      # auto schedule on load when no dates are set
      observe({
        req(is.null(rv$initial_cut_dates))
        dates <- schedule_by_gdd()
        set_cut_dates(dates)
      })

      n_cut_dates <- reactive({
        length(req(rv$initial_cut_dates))
      })

      cutting_summary <- reactive({
        yr <- req(input$year)
        cut_days <- yday(req(rv$set_cut_dates))
        cut_points <- c(-1, cut_days, 999)
        df <- plot_data() |>
          filter(year(date) == yr, yday > 31) |>
          mutate(cutting = cut(yday, cut_points)) |>
          mutate(
            days_since_cut = row_number() - 1,
            gdd_since_cut = cumsum(gdd41),
            .by = c(last_kill, cutting)
          )

        cut_annot <- df |>
          summarize(
            across(c(date, days_since_cut, gdd_since_cut), max),
            .by = cutting
          ) |>
          head(-1) |>
          select(-cutting) |>
          mutate(
            label = paste0(
              "<b>",
              format(date, "%b %d"),
              "</b><br>",
              days_since_cut,
              " days<br>",
              round(gdd_since_cut),
              " GDD"
            )
          )

        cut_annot
      })

      # pick how many equal sub-intervals best match the target gdd interval,
      # leaning toward more (shorter) intervals on a tie so growers get more cuts
      best_divisions <- function(gdd_length, target) {
        r <- gdd_length / target
        cands <- sort(
          unique(pmax(1, c(floor(r), ceiling(r)))),
          decreasing = TRUE
        )
        cands[which.min(abs(gdd_length / cands - target))]
      }

      # Determine cut timing, anchored on any held cuts. Schedules either by a
      # target gdd interval or by a target number of cuts, whichever is passed.
      # The final cut is placed just before the projected fall kill so the last
      # regrowth stays minimal, and cuts are added (not dropped) to keep more
      # healthy yield while mostly respecting the target interval.
      schedule_cut_dates <- function(
        df,
        gdd_interval = NULL,
        target_cuts = NULL
      ) {
        cur_yr <- req(input$year)

        # convert between day-of-year and cumulative gdd since last kill
        gdd_on <- function(yd) df$gdd_since_kill[which.min(abs(df$yday - yd))]
        day_at <- function(gdd) df$yday[which.min(abs(df$gdd_since_kill - gdd))]

        # season boundaries (in gdd since kill): spring regrowth (0), each held
        # cut, and the fall kill. Cuts are placed within the segments between them.
        held <- sort(rv$held)

        last_df <- df |>
          filter(yday > 180, date >= last_kill)
        possible_cut <- last_df |>
          filter(gdd_since_kill >= 800, between(kill_by, 0.1, 0.25))
        end_gdd <- if (nrow(possible_cut) > 0) {
          end_gdd <- min(possible_cut$gdd_since_kill)
        } else {
          max(df$gdd_since_kill)
        }

        pts <- sort(unique(c(0, vapply(held, gdd_on, numeric(1)), end_gdd)))
        seg_lo <- head(pts, -1)
        seg_hi <- tail(pts, -1)
        seg_len <- seg_hi - seg_lo
        n_seg <- length(seg_len)
        is_tail <- seq_len(n_seg) == n_seg # final segment ends at the fall kill

        # decide how many equal sub-intervals to divide each segment into
        if (!is.null(target_cuts)) {
          # by number of cuts: one interval per segment, then hand each additional
          # cut to whichever segment currently has the widest interval
          divs <- rep(1L, n_seg)
          for (j in seq_len(max(0, target_cuts - n_seg))) {
            i <- which.max(seg_len / divs)
            divs[i] <- divs[i] + 1L
          }
        } else {
          # by gdd interval: split each segment to keep intervals near the target,
          # but leave a short (<360 gdd) final fall regrowth uncut
          divs <- vapply(
            seq_len(n_seg),
            function(i) {
              if (is_tail[i] && seg_len[i] < 360) {
                return(0L)
              }
              as.integer(best_divisions(seg_len[i], gdd_interval))
            },
            integer(1)
          )
        }

        # place cuts: interior segments exclude their (held) upper boundary; the
        # tail includes it, putting the last cut just before the fall kill
        gdd_cuts <- unlist(lapply(seq_len(n_seg), function(i) {
          if (divs[i] < 1) {
            return(NULL)
          }
          steps <- if (is_tail[i]) seq_len(divs[i]) else seq_len(divs[i] - 1)
          seg_lo[i] + seg_len[i] * steps / divs[i]
        }))

        end_day <- day_at(end_gdd)
        gen_days <- if (length(gdd_cuts)) {
          vapply(gdd_cuts, day_at, numeric(1))
        } else {
          numeric(0)
        }
        gen_days <- gen_days[between(gen_days, 60, end_gdd)]
        days <- sort(unique(c(held, gen_days)))

        # regenerate the date ui
        start_of_year(cur_yr) + days - 1
      }

      schedule_by_gdd <- reactive({
        gdd_freq <- as.numeric(req(input$cut_freq))
        df <- plot_data()
        schedule_cut_dates(df, gdd_interval = gdd_freq)
      })

      schedule_by_cuts <- reactive({
        n_cuts <- req(input$cut_num)
        df <- plot_data()
        schedule_cut_dates(df, target = n_cuts)
      })

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
              selectInput(
                ns("year"),
                "Weather year",
                choices = OPTS$weather_years
              ),
            ),
            column(
              6,
              selectInput(
                ns("climate"),
                "Climate data",
                choices = OPTS$climate_period_choices
              )
            )
          ),

          # scheduler
          fluidRow(
            column(6, uiOutput(ns("schedule_by_gdd"))),
            column(6, uiOutput(ns("schedule_by_cuts")))
          ),

          # date display
          uiOutput(ns("cut_dates_ui"))
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
          inputs[[id]] <- div(
            class = "cut-date-container",

            div(
              class = "cut-date-header",
              span(
                class = "cut-date-number",
                i
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
            ),

            dateInput(
              inputId = ns(id),
              label = NULL,
              min = min_date,
              max = max_date,
              value = clamp(cut_dates[i], min_date, max_date),
              format = "M d",
              width = "100px"
            )
          )
        }

        div(
          tags$label("Planned cutting dates"),
          div(class = "cut-date-list", inputs),
          tags$script("Shiny.setInputValue('timing-date_ui_ready', true);")
        )
      })

      ## Read and store cutting dates ----

      # save dates into rv which will propagate into the date UI
      # and ultimately be set into rv$set_cut_dates
      set_cut_dates <- function(d) {
        if (!identical(d, rv$initial_cut_dates)) {
          pause_date_reader()
          rv$initial_cut_dates <- d
        }
      }

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

        # check that date inputs are in the right order
        sorted_dates <- sort(unique(dates))
        if (!identical(dates, sorted_dates)) {
          set_cut_dates(sorted_dates)
          return()
        }

        holds <- sapply(1:n_dates, function(i) isTRUE(input[[hold_id(i)]]))
        rv$held <- sort(unique(yday(dates[holds])))
        rv$set_cut_dates <- dates
      })

      ## Handle adding/removing dates ----

      # add another cut by rescheduling for one more than the current count
      observeEvent(input$add_cut, {
        df <- plot_data()
        dates <- schedule_cut_dates(df, target_cuts = n_cut_dates() + 1)
        set_cut_dates(dates)
      })

      # handle date removal
      lapply(1:OPTS$max_cut_dates, function(i) {
        id <- remove_cut_date_id(i)
        observeEvent(input[[id]], {
          dates <- req(rv$set_cut_dates)
          dates <- dates[-i]
          set_cut_dates(dates)
        })
      })

      ## Schedule by GDD ----

      output$schedule_by_gdd <- renderUI({
        choices <- OPTS$cut_freq_choices
        choices <- set_names(choices, paste(choices, "GDD"))
        div(
          class = "cut-schedule-opts",
          tags$label("Target growth interval:"),
          div(
            selectInput(
              ns("cut_freq"),
              label = NULL,
              choices = choices,
              selected = OPTS$cut_freq_default,
              width = "100%"
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
      observeEvent(input$apply_cut_freq, {
        set_cut_dates(schedule_by_gdd())
      })

      ## Schedule by number of cuts ----
      output$schedule_by_cuts <- renderUI({
        div(
          class = "cut-schedule-opts",
          tags$label("Target number of cuts:"),
          div(
            sliderInput(
              ns("cut_num"),
              label = NULL,
              value = n_cut_dates(),
              min = 2,
              max = OPTS$max_cut_dates,
              step = 1,
              ticks = FALSE,
              width = "100%"
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

      # handle 'apply' button for number of cuts
      observeEvent(input$apply_num_cuts, {
        set_cut_dates(schedule_by_cuts())
      })

      # Plot data ----
      plot_data <- reactive({
        buildGrowthData(
          weather_data = req(rv$data$weather),
          climate_data = req(rv$data[[req(input$climate)]]),
          start_date = start_of_year(req(input$year))
        )
      })

      observe({
        rv$plot_args <- list(
          df = plot_data(),
          loc = req(rv$data$loc),
          weather_year = req(input$year),
          cut_dates = req(rv$set_cut_dates),
          held_ydays = rv$held
        )
      })

      output$plot <- renderPlotly({
        do.call(buildTimingPlot, req(rv$plot_args))
      })
    } # end module
  )
}
