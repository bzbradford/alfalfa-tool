
timingUI <- function() {
  ns <- NS("timing")
  uiOutput(ns("main_ui"))
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
        date_ui_ready = FALSE
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
        df <- weather_data()
        freq <- as.numeric(req(input$cut_freq))
        cuts <- seq(freq, max(df$gdd_since_kill), by = freq)
        days <- sapply(cuts, function(gdd) {
          df[which.min(abs(gdd - df$gdd_since_kill)),]$yday
        })
        days <- days[between(days, 60, 300)]
        pause_date_reader()
        rv$initial_cut_dates <- start_of_year() + unique(days) - 1
      }

      ## Observed/projected data ----

      weather_data <- reactive({
        opts <- list(
          year = req(input$year),
          period = req(input$period)
        )
        wx <- req(rv$weather) %>%
          filter(year == opts$year) %>%
          select(date, yday, gdd41, kill) %>%
          mutate(last_kill = if_else(kill, yday, NA)) %>%
          fill(last_kill)
        cl <- req(rv[[opts$period]])
        cl_gdd <- cl %>%
          filter(yday > max(wx$yday)) %>%
          select(yday, gdd41) %>%
          mutate(date = start_of_year(opts$year) + yday - 1)
        cl_risk <- cl %>% select(yday, kill_by)
        bind_rows(wx, cl_gdd) %>%
          left_join(cl_risk, join_by(yday)) %>%
          fill(last_kill) %>%
          mutate(gdd41cum = cumsum(gdd41)) %>%
          mutate(gdd_since_kill = cumsum(gdd41), .by = last_kill)
      })

      ## Plot data ----
      observe({
        cut_dates <- req(rv$set_cut_dates)
        req(identical(cut_dates, sort(unique(cut_dates))))
        cut_days <- yday(cut_dates)
        cut_points <- c(-1, cut_days, 999)
        df <- weather_data() %>%
          mutate(cutting = cut(yday, cut_points)) %>%
          mutate(
            days_since_cut = row_number() - 1,
            gdd_since_cut = cumsum(gdd41),
            .by = c(last_kill, cutting)
          )
        rv$plot_data <- df
      })


      # Interface ----

      ## Main UI ----

      output$main_ui <- renderUI({
        validate(need(rv$loc_ready, OPTS$location_validation_msg))

        tagList(
          uiOutput(ns("options_ui")),
          h4("Full-season alfalfa cut and kill schedule", align = "center"),
          uiOutput(ns("plot_title")),
          plotlyOutput(ns("plot"), height = "500px"),
          div(class = "plot-caption", HTML("Today's date is indicated as a vertical dashed line. Future degree-day accumulation estimated based on climate average GDD/day. Weather data originally sourced from NOAA and retrieved from <a href='https://agweather.cals.wisc.edu'>AgWeather</a>, climate data sourced from <a href='https://www.climatologylab.org/gridmet.html'>GridMET</a>. Green zone represents optimal cut timing (900-1100 GDD since last cutting), blue zone (0-360 GDD) represents maximum grow-back since last cut and before first freeze. Ideally alfalfa should not be allowed to grow outside of this zone after the last fall cutting."))
        )
      })


      ## Options UI ----

      output$options_ui <- renderUI({
        div(
          class = "well",
          fluidRow(
            column(6,
              div(class = "inline-flex",
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
              ns("cut_freq"), label = NULL,
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


      # Plot ----

      output$plot <- renderPlotly({
        opts <- list()
        opts$year <- req(input$year)
        opts$climate = req(input$period)
        df <- req(rv$plot_data)
        cl <- req(rv[[opts$climate]]) %>%
          mutate(date = start_of_year(opts$year) + yday - 1)
        killing_freezes <- df %>% filter(kill)

        cut_annot <- df %>%
          summarize(
            across(c(date, days_since_cut, gdd_since_cut), max),
            .by = cutting
          ) %>%
          head(-1) %>%
          select(-cutting) %>%
          mutate(label = paste0(
            "<b>", format(date, "%b %d"), "</b><br>",
            days_since_cut, " days<br>",
            round(gdd_since_cut), " GDD"
          ))

        last_spring_kill <- df %>%
          filter(kill, yday < 150) %>%
          tail(1) %>%
          mutate(label = paste0("<b>", format(date, "%b %d"), "</b><br>Last spring kill"))

        first_fall_kill <- df %>%
          mutate(across(c(days_since_cut, gdd_since_cut), lag)) %>%
          filter(kill, yday > 150) %>%
          head(1) %>%
          mutate(label = paste0(
            "<b>", format(date, "%b %d"), "</b><br>",
            "First fall kill<br>",
            days_since_cut, " days<br>",
            round(gdd_since_cut), " GDD"
          ))

        kill_annot <- bind_rows(last_spring_kill, first_fall_kill)

        plt <- df %>%
          plot_ly() %>%
          add_trace(
            name = "Days since last kill or cut",
            x = ~date, y = ~days_since_cut,
            type = "scatter", mode = "none", hovertemplate = "%{y:.0f}",
            showlegend = F
          ) %>%
          add_trace(
            name = "Killing freeze (<24°F)",
            x = killing_freezes$date, y = 200,
            type = "bar", hovertemplate = "Yes",
            marker = list(color = "blue"), width = 1000 * 60 * 60 * 24
          ) %>%
          add_trace(
            name = "Cumul. GDD41 (climate average)",
            x = cl$date, y = cl$gdd41cum, yaxis = "y1",
            type = "scatter", mode = "lines", hovertemplate = "%{y:.1f}",
            line = list(color = "#ad2b2f", shape = "spline")
          ) %>%
          add_trace(
            name = "Cumul. GDD41 (observed/projected)",
            x = ~date, y = ~gdd41cum, yaxis = "y1",
            type = "scatter", mode = "lines", hovertemplate = "%{y:.1f}",
            line = list(color = "#ff802e", shape = "spline", dash = "dot")
          ) %>%
          add_trace(
            name = "GDD41 since last kill/cut",
            x = ~date, y = ~gdd_since_cut, yaxis = "y1",
            type = "scatter", mode = "lines", hovertemplate = "%{y:.1f}",
            line = list(color = "#00a038")
          ) %>%
          add_trace(
            name = "Cumul. killing freeze prob.",
            x = ~date, y = ~kill_by * 100, yaxis = "y2",
            type = "scatter", mode = "lines", hovertemplate = "%{y:.1f}%",
            line = list(color = "purple", shape = "spline", width = 1.5)
          ) %>%
          add_annotations(
            data = cut_annot, x = ~date, y = ~gdd_since_cut, text = ~label,
            arrowsize = .5,
            font = list(size = 10)
          ) %>%
          add_annotations(
            data = kill_annot, x = ~date, y = ~gdd_since_cut, text = ~label,
            arrowsize = .5,
            font = list(size = 10)
          ) %>%
          layout(
            legend = OPTS$plot_legend,
            xaxis = OPTS$plot_date_axis_weather,
            yaxis = list(
              title = "Growing degree days (base 41°F)",
              fixedrange = T
            ),
            yaxis2 = list(
              title = "Cumul. killing freeze prob. (<24°F)",
              overlaying = "y",
              side = "right",
              zeroline = F,
              showgrid = F,
              fixedrange = T
            ),
            hovermode = "x unified"
          )

        cut_zones <- list(
          rect(900, 1100, color = "green"),
          rect(0, 360, color = "blue")
        )

        plt %>% add_today(yr = opts$year, date_yr = opts$year, other_shapes = cut_zones)
      })


    } # end module
  )
}
