# Projected crop growth

growthUI <- function() {
  ns <- NS("growth")
  uiOutput(ns("main_ui"))
}

growthServer <- function(loc_data) {
  moduleServer(
    id = "growth",
    function(input, output, session) {
      ns <- session$ns

      rv <- reactiveValues(
        initial_date = yesterday() - 28
      )

      observe({
        rv$loc <- loc_data()$loc
        rv$weather <- loc_data()$weather
        rv$c10 <- loc_data()$c10
        rv$c5 <- loc_data()$c5
      })

      observe({
        if (!is.null(rv$loc)) rv$loc_ready <- TRUE
      })

      output$main_ui <- renderUI({
        validate(need(rv$loc_ready, OPTS$location_validation_msg))

        tagList(
          uiOutput(ns("options_ui")),
          uiOutput(ns("plot_title")),
          plotlyOutput(ns("plot"), height = "500px"),
          div(class = "plot-caption", HTML("Today's date is indicated as a vertical dashed line. Green zone represents optimal cut timing (900-1100 GDD since last cutting), blue zone (0-360 GDD) represents maximum grow-back since last cut and before first freeze. Ideally alfalfa should not be allowed to grow outside of this zone after the last fall cutting."))
        )
      })

      output$options_ui <- renderUI({
        btn <- function(id, label) {
          actionButton(ns(id), label, class = "btn-sm", style = "height:35px; margin:5px;")
        }

        div(
          class = "well", style = "padding-bottom: 0px;",
          fluidRow(
            column(6,
              div(tags$label("Date of last cut")),
              div(
                class = "inline-flex",
                uiOutput(ns("date_ui")),
                div(
                  btn("date_today", "Today"),
                  btn("date_reset", "Reset")
                )
              )
            ),
            column(6,
              radioButtons(
                ns("climate"), "Project forward with:",
                choices = OPTS$climate_period_choices
              )
            )
          )
        )
      })

      output$date_ui <- renderUI({
        dateInput(
          inputId = ns("cut_date"), label = NULL,
          min = start_of_year(), max = end_of_year(),
          value = clamp(rv$initial_date, start_of_year(), end_of_year()),
          format = "M d", width = "150px"
        )
      })

      observeEvent(input$date_today, {
        rv$initial_date <- yesterday() + 1
      })

      observeEvent(input$date_reset, {
        rv$initial_date <- yesterday() - 28
      })

      output$plot_title <- renderUI({
        loc <- req(rv$loc)
        title <- sprintf("Alfalfa growth projection for %.1f°N, %.1f°W", loc$lat, loc$lng)
        h4(title, align = "center")
      })

      observe({
        opts <- list(
          cut_date = req(input$cut_date),
          climate = req(input$climate)
        )
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
        df <- bind_rows(wx, cl_gdd) %>%
          filter(date >= opts$cut_date) %>%
          left_join(cl_risk, join_by(yday)) %>%
          fill(kill, last_kill) %>%
          mutate(gdd41 = if_else(date == opts$cut_date, 0, gdd41)) %>%
          mutate(gdd_since_cut = cumsum(gdd41), .by = last_kill) %>%
          mutate(days_since_cut = row_number() - 1)
        rv$plot_data <- df
      })

      output$plot <- renderPlotly({
        df <- req(rv$plot_data)

        thresholds <- seq(800, 1200, by = 100)
        threshold_days <- sapply(thresholds, function(gdd) {
          df[which.min(abs(gdd - df$gdd_since_cut)),]$yday
        })
        kill_date <- df %>%
          filter(yday > 200) %>%
          slice_min(abs(kill_by - .5)) %>%
          pull(date)

        df <- df %>%
          filter(yday <= max(yday(yesterday()) + 1, last(threshold_days) + 30))

        # cutting date annotations
        cut_annot <- df %>%
          filter(yday %in% threshold_days) %>%
          mutate(label = paste0(
            "<b>", format(date, "%b %d"), "</b><br>",
            round(gdd_since_cut), " GDD<br>",
            days_since_cut, " days"
          ))

        # killing freeze annotation
        kill_annot <- df %>%
          filter(date == first(kill_date)) %>%
          mutate(label = paste0(
            "<b>", format(date, "%b %d"), "</b><br>",
            "50% kill<br>probability<br>",
            days_since_cut, " days<br>",
            round(gdd_since_cut), " GDD"
          ))

        plt <- df %>%
          plot_ly() %>%
          add_trace(
            name = "Days since last kill or cut",
            x = ~date, y = ~days_since_cut,
            type = "scatter", mode = "none", hovertemplate = "%{y:.0f}",
            showlegend = F
          )

        freezes <- df %>% filter(kill)
        if (nrow(freezes) > 0) {
          plt <- plt %>% add_trace(
            name = "Killing freeze (<24°F)",
            x = freezes$date, y = 200,
            type = "bar", hovertemplate = "Yes",
            marker = list(color = "blue"), width = 1000 * 60 * 60 * 24
          )
        }

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
          ) %>%
          add_trace(
            name = "Cumul. killing freeze prob.",
            x = ~date, y = ~kill_by * 100, yaxis = "y2",
            type = "scatter", mode = "lines", hovertemplate = "%{y:.1f}%",
            line = list(color = "purple", width = 1.5)
          ) %>%
          add_annotations(
            data = cut_annot, x = ~date, y = ~gdd_since_cut, text = ~label,
            arrowsize = .5,
            font = list(size = 10)
          )

        if (nrow(kill_annot) > 0) {
          plt <- plt %>%
            add_annotations(
              data = kill_annot, x = ~date, y = ~gdd_since_cut, text = ~label,
              arrowsize = .5,
              font = list(size = 10)
            )
        }

        plt <- plt %>%
          layout(
            legend = OPTS$plot_legend,
            xaxis = OPTS$plot_date_axis_weather,
            yaxis = list(
              title = "Growing degree days (base 41°F)",
              fixedrange = T,
              range = c(0, max(df$gdd_since_cut))
            ),
            yaxis2 = list(
              title = "Cumul. killing freeze prob. (<24°F)",
              overlaying = "y",
              side = "right",
              zeroline = F,
              showgrid = F,
              fixedrange = T,
              range = c(0, 100)
            ),
            hovermode = "x unified"
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

