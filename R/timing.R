
timingUI <- function() {
  ns <- NS("timing")

  uiOutput(ns("main_ui"))
}

timingServer <- function(loc_data) {
  moduleServer(
    id = "timing",
    function(input, output, session) {
      ns <- session$ns

      rv <- reactiveValues(
        initial_cut_dates = NULL,
        set_cut_dates = NULL
      )

      # store incoming location data in rv
      observe({
        rv$loc <- loc_data()$loc
        rv$weather <- loc_data()$weather
        rv$c10 <- loc_data()$c10
        rv$c5 <- loc_data()$c5
      })

      # determine initial cut timing based on climate average
      schedule_cut_dates <- function() {
        cl <- rv[[req(input$period)]]
        freq <- as.numeric(req(input$cut_freq))
        col <- "gdd41cum"
        cuts <- seq(freq, 4500, by = freq)
        days <- sapply(cuts, function(gdd) {
          cl[which.min(abs(gdd - cl[[col]])),]$yday
        })
        days <- days[between(days, 60, 300)]
        start_of_year() + unique(days) - 1
      }


      # Interface ----

      ## Main UI ----

      output$main_ui <- renderUI({
        validate(need(rv$loc, OPTS$location_validation_msg))

        tagList(
          uiOutput(ns("options_ui")),
          h4("Growing degree days and frost probability", align = "center"),
          uiOutput(ns("plot_title")),
          plotlyOutput(ns("plot"), height = "500px"),
          div(class = "plot-caption", HTML("Today's date is indicated as a vertical dashed line. Future degree-day accumulation estimated based on climate average GDD/day. Weather data originally sourced from NOAA and retrieved from <a href='https://agweather.cals.wisc.edu'>AgWeather</a>, climate data sourced from <a href='https://www.climatologylab.org/gridmet.html'>GridMET</a>. Green zone represents optimal cut timing (900-1100 GDD since last cutting), blue zone (0-360 GDD) represents maximum grow-back since last cut and before first freeze. Ideally alfalfa should not be allowed to grow outside of this zone after the last fall cutting."))
        )
      })


      ## Options UI ----

      output$options_ui <- renderUI({
        div(
          class = "well",
          div(
            class = "inline-flex",
            radioButtons(
              ns("year"), "Year",
              choices = OPTS$weather_years,
            ),
            radioButtons(
              ns("period"), "Climate period",
              choices = OPTS$climate_period_choices
            ),
            uiOutput(ns("auto_cut_ui"))
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
          div(
            tags$label("Schedule cuts by cumulative GDD"),
          ),
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

      observeEvent(input$apply_cut_freq, {
        # this forces the date UI to use the scheduler to find initial dates
        rv$initial_cut_dates <- NULL
      })


      ## Cutting dates UI ----

      cut_date_id <- function(i) paste0("cut_date-", i)
      remove_cut_date_id <- function(i) paste0("remove_cut_date-", i)

      output$cut_dates_ui <- renderUI({
        if (is.null(rv$initial_cut_dates)) {
          rv$initial_cut_dates <- schedule_cut_dates()
          return()
        }

        opts <- list()
        opts$year <- req(input$year)
        opts$cut_dates <- req(rv$initial_cut_dates)
        min_date <- start_of_year(opts$year)
        max_date <- end_of_year(opts$year)
        cut_dates <- min_date + sort(unique(yday(opts$cut_dates))) - 1
        n_dates <- length(cut_dates)
        inputs <- list()

        for (i in 1:n_dates) {
          id <- cut_date_id(i)
          inputs[[id]] <- div(
            dateInput(
              inputId = ns(id),
              label = NULL,
              min = min_date,
              max = max_date,
              value = clamp(cut_dates[i], min_date, max_date),
              format = "M d",
              width = "100px"
            ), {
              # add remove link only if >1 date inputs
              if (n_dates > 1) {
                div(
                  style = "text-align: center",
                  actionLink(ns(remove_cut_date_id(i)), "Remove")
                )
              }
            }
          )
        }

        btn <- function(id) {
          actionButton(
            ns(id), icon("plus"),
            class = "btn-sm",
            style = "height: 45px;",
            disabled = length(inputs) == OPTS$max_cut_dates
          )
        }

        div(
          class = "inline-flex",
          btn("add_cut_before"),
          inputs,
          btn("add_cut_after")
        )
      })


      ## Read and store cutting dates ----

      observe({
        dates <- req(rv$initial_cut_dates)
        dates <- sapply(1:length(dates), function(i) {
          id <- cut_date_id(i)
          req(input[[id]])
        })
        rv$set_cut_dates <- as.Date(dates)
      })


      ## Handle adding/removing dates ----

      # add another date halfway to Jan 1
      observeEvent(input$add_cut_before, {
        yr <- req(input$year)
        dates <- req(rv$set_cut_dates)
        new_date <- max(start_of_year(yr), as.Date(first(dates)) - 28)
        new_dates <- unique(c(new_date, dates))
        rv$initial_cut_dates <- new_dates
        rv$set_cut_dates <- NULL
      })

      # add another date halfway to Dec 31
      observeEvent(input$add_cut_after, {
        yr <- req(input$year)
        dates <- req(rv$set_cut_dates)
        new_date <- min(as.Date(last(dates)) + 28, end_of_year(yr))
        new_dates <- unique(c(dates, new_date))
        rv$initial_cut_dates <- new_dates
        rv$set_cut_dates <- NULL
      })

      # handle date removal
      lapply(1:OPTS$max_cut_dates, function(i) {
        id <- remove_cut_date_id(i)
        observeEvent(input[[id]], {
          dates <- req(rv$set_cut_dates)
          dates <- dates[-i]
          rv$initial_cut_dates <- dates
          rv$set_cut_dates <- NULL
        })
      })


      # Plot ----

      ## Generate plot data ----

      plot_data <- reactive({
        opts <- list()
        opts$year <- req(input$year)
        opts$period <- req(input$period)
        opts$dates <- req(rv$set_cut_dates)
        req(identical(opts$dates, sort(unique(opts$dates))))
        opts$days <- yday(opts$dates)

        cut_points <- c(-1, opts$days, 999)
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
            days_since_cut = row_number() - 1,
            gdd_since_cut = cumsum(gdd41),
            .by = cutting
          )
      })


      ## Render plot ----

      output$plot <- renderPlotly({
        opts <- list(
          year = req(input$year),
          climate = req(input$period)
        )

        df <- plot_data()
        cl <- req(rv[[opts$climate]]) %>%
          mutate(date = start_of_year(opts$year) + yday - 1)

        plt <- df %>%
          plot_ly() %>%
          add_trace(
            name = "Days since last cutting",
            x = ~date, y = ~days_since_cut,
            type = "scatter", mode = "none",
            hovertemplate = "%{y:.0f}",
            showlegend = F
          ) %>%
          add_trace(
            name = "Cumul. GDD41 (climate average)",
            x = cl$date, y = cl$gdd41cum,
            type = "scatter", mode = "lines",
            hovertemplate = "%{y:.1f}",
            line = list(
              color = "#ad2b2f",
              shape = "spline"),
            yaxis = "y1"
          ) %>%
          add_trace(
            name = "Cumul. GDD41 (observed/projected)",
            x = ~date, y = ~gdd41cum,
            type = "scatter", mode = "lines",
            hovertemplate = "%{y:.1f}",
            line = list(
              color = "#ff802e",
              shape = "spline",
              dash = "dot"),
            yaxis = "y1"
          ) %>%
          add_trace(
            name = "GDD41 since last cutting",
            x = ~date, y = ~gdd_since_cut,
            type = "scatter", mode = "lines",
            line = list(color = "#00a038"),
            hovertemplate = "%{y:.1f}",
            yaxis = "y1"
          ) %>%
          add_trace(
            name = "Cumul. hard freeze probability",
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
              side = "right",
              zeroline = F,
              showgrid = F
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
