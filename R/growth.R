# Projected crop growth

growthUI <- function() {
  ns <- NS("growth")
  tagList(
    p("This tool estimates alfalfa growth since the last cutting using observed weather values prior and climate averages. Date estimates for certain degree-day thresholds and killing freeze risks will be provided."),
    uiOutput(ns("main_ui"))
  )
}

growthServer <- function(loc_data) {
  moduleServer(
    id = "growth",
    function(input, output, session) {
      ns <- session$ns

      rv <- reactiveValues(
        initial_date = yesterday() - 28,
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
          div(class = "plot-caption", "Today's date is indicated as a vertical dashed line. Green zone represents optimal cut timing (900-1100 GDD since last cutting), blue zone (0-360 GDD) represents maximum grow-back since last cut and before first freeze. Ideally alfalfa should not be allowed to grow outside of this zone after the last fall cutting. Click and drag on plot to zoom in, double-click to reset. Click the camera icon in the plot menu to download a copy.")
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
        dateInput(
          inputId = ns("cut_date"), label = NULL,
          min = start_of_year(), max = end_of_year(),
          value = clamp(rv$initial_date, start_of_year(), end_of_year()),
          format = "M d", width = "150px"
        )
      })

      observeEvent(input$date_jan1, { rv$initial_date <- start_of_year() })
      observeEvent(input$date_today, { rv$initial_date <- yesterday() + 1 })
      observeEvent(input$date_reset, { rv$initial_date <- yesterday() - 28 })


      # Generate plot data ----

      observe({
        opts <- list(
          cut_date = req(input$cut_date),
          climate = req(input$climate)
        )
        data <- list()
        wx <- req(rv$weather) %>%
          filter(year == year(opts$cut_date)) %>%
          select(date, yday, gdd41, kill) %>%
          mutate(last_kill = if_else(kill, yday, NA)) %>%
          fill(last_kill)
        cl <- req(rv[[opts$climate]])
        cl_gdd <- cl %>%
          filter(yday > max(wx$yday, 0)) %>%
          select(yday, gdd41) %>%
          mutate(date = start_of_year(opts$cut_date) + yday - 1)
        cl_risk <- cl %>% select(yday, kill_by)

        # main data
        df <- bind_rows(wx, cl_gdd) %>%
          filter(date >= opts$cut_date) %>%
          left_join(cl_risk, join_by(yday)) %>%
          fill(kill, last_kill) %>%
          mutate(gdd41 = if_else(date == opts$cut_date, 0, gdd41)) %>%
          mutate(gdd_since_cut = cumsum(gdd41), .by = last_kill) %>%
          mutate(days_since_cut = row_number() - 1)

        rv$plot_data <- df
      })


      # Plot ----

      output$plot <- renderPlotly({
        opts <- list()
        opts$loc <- req(rv$loc)
        opts$cut_date <- req(input$cut_date)
        opts$title <- sprintf("Alfalfa growth projection since %s for %.1f°N, %.1f°W", format(opts$cut_date, "%b %d, %Y"), opts$loc$lat, opts$loc$lng)

        df <- req(rv$plot_data)

        # cutting thresholds
        thresholds <- seq(800, 1200, by = 100)
        thresholds <- thresholds[thresholds < last(df$gdd_since_cut)]
        threshold_days <- sapply(thresholds, function(gdd) {
          df[which.min(abs(gdd - df$gdd_since_cut)),]$yday
        })

        end_day <- if (length(thresholds) > 0) {
          max(yday(yesterday()) + 1, last(threshold_days) + 30)
        } else {
          yday(end_of_year())
        }

        df_clip <- df %>% filter(yday <= end_day)

        cut_annot <- df_clip %>%
          filter(yday %in% threshold_days) %>%
          mutate(label = paste0(
            "<b>", format(date, "%b %d"), "</b><br>",
            round(gdd_since_cut), " GDD<br>",
            days_since_cut, " days"
          ))

        kill_date <- df %>%
          filter(yday > 200) %>%
          slice_min(abs(kill_by - .5)) %>%
          pull(date)

        kill_annot <- df_clip %>%
          filter(date == first(kill_date)) %>%
          mutate(label = paste0(
            "<b>", format(date, "%b %d"), "</b><br>",
            "50% kill<br>probability<br>",
            days_since_cut, " days<br>",
            round(gdd_since_cut), " GDD"
          ))

        # base plot
        plt <- df_clip %>%
          plot_ly() %>%
          add_trace(
            name = "Days since last kill or cut",
            x = ~date, y = ~days_since_cut,
            type = "scatter", mode = "none", hovertemplate = "%{y:.0f}",
            showlegend = F
          )

        # add observed hard freezes if any
        freezes <- df %>% filter(kill)
        if (nrow(freezes) > 0) {
          plt <- plt %>% add_trace(
            name = "Killing freeze (<24°F)",
            x = freezes$date, y = 200,
            type = "bar", hovertemplate = "Yes",
            marker = list(color = "blue"),
            width = 1000 * 60 * 60 * 24
          )
        }

        # add gdd traces
        plt <- plt %>%
          add_trace(
            name = "GDD41",
            x = ~date, y = ~gdd41, yaxis = "y1",
            type = "bar", hovertemplate = "%{y:.1f}",
            marker = list(color = "#00a038")
          ) %>%
          add_trace(
            name = "GDD41 since last kill/cut",
            x = ~date, y = ~gdd_since_cut, yaxis = "y1",
            type = "scatter", mode = "lines", hovertemplate = "%{y:.1f}",
            line = list(color = "#00a038")
          )

        # add cumulative freeze probability
        if (max(df$kill_by) > 0) {
          plt <- plt %>%
            add_trace(
              name = "Cumul. killing freeze prob.",
              x = ~date, y = ~kill_by * 100, yaxis = "y2",
              type = "scatter", mode = "lines", hovertemplate = "%{y:.1f}%",
              line = list(color = "purple", width = 1.5)
            )
          opts$y2 <- list(
            title = "Cumul. killing freeze prob. (<24°F)",
            overlaying = "y",
            side = "right",
            zeroline = F,
            showgrid = F,
            fixedrange = T,
            range = c(0, 100)
          )
        }

        # add cut annotation if any
        if (nrow(cut_annot) > 0) {
          plt <- plt %>%
            add_annotations(
              data = cut_annot,
              x = ~date, y = ~gdd_since_cut,
              text = ~label,
              ax = -25, ay = -35,
              arrowsize = .5,
              font = list(size = 10)
            )
        }

        # add kill annotation if any
        if (nrow(kill_annot) > 0) {
          plt <- plt %>%
            add_annotations(
              data = kill_annot,
              x = ~date, y = 50, yref = "y2",
              text = ~label,
              ax = 40, ay = 30,
              arrowsize = .5,
              font = list(size = 10)
            )
        }

        plt <- plt %>%
          layout(
            legend = OPTS$plot_legend,
            title = list(
              text = opts$title,
              yanchor = "bottom"),
            xaxis = list(
              title = "Date",
              dtick = 1000 * 60 * 60 * 24 * 7,
              ticks = "outside",
              showgrid = T,
              gridwidth = .5,
              tickformat = "%b %d",
              hoverformat = "%b %d, %Y (day %j)",
              domain = c(0, .95)),
            yaxis = list(
              title = "Growing degree days (base 41°F)",
              fixedrange = T,
              gridwidth = .5,
              range = c(0, max(df$gdd_since_cut) * 1.1)),
            yaxis2 = opts$y2,
            hovermode = "x unified",
            margin = list(t = 50),
            modebar = list(remove = list("pan", "select", "lasso", "zoom", "autoscale"))
          ) %>%
          config(
            toImageButtonOptions = list(
              format = "png",
              filename = opts$title,
              height = 600,
              width = 1000,
              scale = 1.25
            )
          )

        cut_zones <- list(
          rect(800, 1200, color = "green"),
          rect(900, 1100, color = "green"),
          rect(0, 360, color = "blue")
        )

        plt %>% add_today(other_shapes = cut_zones)
      })

    }
  )
}

