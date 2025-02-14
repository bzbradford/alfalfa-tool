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

        div(class = "well", style = "padding-bottom: 0px;",
          div(class = "inline-flex",
            div(
              div(tags$label("Date of last cut")),
              div(class = "inline-flex", style = "gap: 5px;",
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
        min_date <- today() - 180
        max_date <- today() + 180
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

      observeEvent(input$date_jan1, { rv$initial_date <- start_of_year() })
      observeEvent(input$date_today, { rv$initial_date <- yesterday() + 1 })
      observeEvent(input$date_reset, { rv$initial_date <- yesterday() - 28 })


      # Generate plot data ----
      observe({
        climate_period <- req(input$climate)
        args <- list(
          weather_data = req(rv$weather),
          climate_data = req(rv[[climate_period]]),
          cut_date = req(input$cut_date)
        )
        rv$plot_data <- do.call(buildGrowthPlotData, args)
      })


      # Plot ----
      output$plot <- renderPlotly({
        args <- list(
          df = req(rv$plot_data),
          loc = req(rv$loc)
        )
        do.call(buildGrowthPlot, args)
      })

    }
  )
}


#' @param weather_data weather data for a single location
#' @param climate_data climate data for a single location
#' @param cut_date date of last cutting
#'
buildGrowthPlotData <- function(weather_data, climate_data, cut_date) {
  # select weather after the cut date
  wx <- weather_data %>%
    filter(date >= cut_date) %>%
    select(date, gdd41, kill) %>%
    mutate(source = "weather")

  cl <- climate_data %>%
    select(yday, gdd41_cl = gdd41, kill_by)

  tibble(date = seq.Date(cut_date, cut_date + 365, 1), yday = yday(date)) %>%
    left_join(wx, join_by(date)) %>%
    left_join(cl, join_by(yday)) %>%
    mutate(gdd41 = coalesce(gdd41, gdd41_cl)) %>%
    # mutate(kill = if_else(is.na(kill) & kill_by >= .9, TRUE, kill)) %>%
    replace_na(list(kill = F)) %>%
    mutate(last_kill = if_else(kill | row_number() == 1, date, NA)) %>%
    fill(last_kill) %>%
    mutate(gdd41 = if_else(kill, 0, gdd41)) %>%
    mutate(
      gdd_since_cut = cumsum(gdd41),
      days_since_cut = as.integer(date - last_kill),
      .by = last_kill
    )
}


#' @param df data from `buildGrowthPlotData`
#' @param loc list with lat, lng
#'
buildGrowthPlot <- function(df, loc) {
  opts <- list(
    cut_date = min(df$date),
    end_date = max(df$date)
  )
  opts$title <- sprintf(
    "Alfalfa growth projection since %s for %.1f°N, %.1f°W",
    format(opts$cut_date, "%b %d, %Y"), loc$lat, loc$lng
  )

  # cutting thresholds
  thresholds <- seq(800, 1200, by = 100)
  thresholds <- thresholds[thresholds < last(df$gdd_since_cut)]
  threshold_dates <- sapply(thresholds, function(gdd) {
    df[which.min(abs(gdd - df$gdd_since_cut)),]$date
  })
  if (length(threshold_dates) > 0) {
    threshold_dates <- as_date(threshold_dates)
    df <- df %>% filter(date <= max(threshold_dates) + 30)
  }
  opts$yrange <- c(0, max(df$gdd_since_cut) * 1.1)

  cut_annot <- df %>%
    filter(date %in% threshold_dates) %>%
    mutate(label = paste0(
      "<b>", format(date, "%b %d"), "</b><br>",
      round(gdd_since_cut), " GDD<br>",
      days_since_cut, " days"
    ))

  # identify fall kill probability to annotate the 50% date
  kill_date <- df %>%
    filter((lag(kill_by) < .5 & kill_by >= .5) | (kill_by <= .5 & lead(kill_by) > .5)) %>%
    slice_min(abs(kill_by - .5)) %>%
    pull(date)

  # annotate fall kill probability
  kill_annot <- df %>%
    filter(date %in% kill_date) %>%
    filter(yday > 200) %>%
    mutate(label = paste0(
      "<b>", format(date, "%b %d"), "</b><br>",
      "50% kill<br>probability<br>",
      days_since_cut, " days<br>",
      round(gdd_since_cut), " GDD"
    ))

  # base plot
  plt <- df %>%
    plot_ly() %>%
    add_trace(
      name = "Days since last kill or cut",
      x = ~date, y = ~days_since_cut,
      type = "scatter", mode = "none", hovertemplate = "%{y:.0f}",
      showlegend = F
    )

  # add observed hard freezes if any
  freezes <- df %>% filter(kill, source == "weather")
  if (nrow(freezes) > 0) {
    plt <- plt %>% add_trace(
      name = "Killing freeze (<24°F)",
      x = freezes$date, y = .025, yaxis = "y2",
      type = "bar", hovertemplate = "Yes",
      marker = list(color = "blue", line = list(opacity = 0)),
      width = 1000 * 60 * 60 * 24 # 1 day in ms
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
        x = ~date, y = ~kill_by, yaxis = "y2",
        type = "scatter", mode = "lines", hovertemplate = "%{y:.0%}",
        line = list(color = "purple", width = 1.5)
      )
    opts$y2 <- list(
      title = "Cumul. killing freeze prob. (<24°F)",
      overlaying = "y",
      side = "right",
      zeroline = F,
      showgrid = F,
      fixedrange = T,
      tickformat = ".0%",
      range = c(0, 1)
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
        x = ~date, y = ~kill_by * 100, yref = "y2",
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
        # dtick = 1000 * 60 * 60 * 24 * 7,
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
        range = opts$yrange),
      yaxis2 = opts$y2,
      hovermode = "x unified",
      margin = list(t = 50),
      modebar = list(remove = list("pan", "select", "lasso", "zoom", "autoscale"))
    ) %>%
    config(
      toImageButtonOptions = append(OPTS$plot_export_opts, list(filename = opts$title))
    )

  cut_zones <- list(
    rect(800, 1200, color = "green"),
    rect(900, 1100, color = "green"),
    rect(0, 360, color = "blue")
  )

  if (today() %in% df$date) {
    plt %>% add_today(other_shapes = cut_zones)
  } else {
    plt %>% layout(shapes = cut_zones)
  }
}





