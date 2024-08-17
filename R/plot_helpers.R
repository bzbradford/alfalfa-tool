#- plot_helpers.R -#

# plotly horizontal line annotation
hline <- function(y = 0, color = "black", dash = "dash", alpha = .5) {
  list(
    type = "line", xref = "paper",
    x0 = 0, x1 = 1, y0 = y, y1 = y,
    line = list(color = color, dash = dash),
    opacity = alpha
  )
}

# plotly rectanglular annotation
rect <- function(ymin, ymax, color = "red") {
  list(
    type = "rect",
    fillcolor = color,
    line = list(color = color),
    opacity = 0.1,
    xref = "x domain",
    x0 = 0, x1 = 1,
    y0 = ymin, y1 = ymax,
    layer = "below"
  )
}

add_today <- function(plt, yr = cur_yr, date_yr = cur_yr, other_shapes = list()) {
  x <- align_dates(yesterday() + 1, date_yr)
  text <- if (yr == cur_yr) "Today" else format(x, "%b %d")
  vline <- list(list(
    type = "line", yref = "y domain",
    x0 = x, x1 = x, y0 = 0, y1 = .95,
    line = list(color = "black", dash = "dash"),
    opacity = .25
  ))
  text <- list(list(
    yref = "y domain",
    x = x, y = 1,
    text = text,
    showarrow = F,
    opacity = .5
  ))
  shapes <- c(other_shapes, vline)
  plt %>% layout(shapes = shapes, annotations = text)
}

set_axis <- function(plt, yaxis, title) {
  axis <-  list(
    title = title,
    zeroline = F,
    fixedrange = T
  )
  if (yaxis == "y1") {
    plt %>% layout(yaxis = axis)
  } else {
    axis$overlaying = "y"
    axis$side = "right"
    axis$showgrid = F
    plt %>% layout(yaxis2 = axis)
  }
}

add_temp_traces <- function(plt, df, yaxis = "y1", label = "", dash = F) {
  dash <- ifelse(dash, "dashdot", "none")
  width <- OPTS$plot_line_width
  plt %>%
    add_trace(
      name = paste0(label, "Min temp"), x = df$date, y = df$min_temp, yaxis = yaxis,
      type = "scatter", mode = "lines", hovertemplate = "%{y:.1f}°F",
      line = list(color = OPTS$plot_colors$min_temp, width = width, shape = "spline", dash = dash)
    ) %>%
    add_trace(
      name = paste0(label, "Mean temp"), x = df$date, y = df$mean_temp, yaxis = yaxis,
      type = "scatter", mode = "lines", hovertemplate = "%{y:.1f}°F",
      line = list(color = OPTS$plot_colors$mean_temp, width = width, shape = "spline", dash = dash)
    ) %>%
    add_trace(
      name = paste0(label, "Max temp"), x = df$date, y = df$max_temp, yaxis = yaxis,
      type = "scatter", mode = "lines", hovertemplate = "%{y:.1f}°F",
      line = list(color = OPTS$plot_colors$max_temp, width = width, shape = "spline", dash = dash)
    ) %>%
    add_trace(
      name = paste0(label, "Frost (<32°F)"),
      x = df$date, y = if_else(df$min_temp <= 32, 32, NA), yaxis = yaxis,
      type = "scatter", mode = "lines", hovertemplate = "Yes",
      line = list(color = OPTS$plot_colors$frost, width = width, dash = dash)
    ) %>%
    add_trace(
      name = paste0(label, "Hard freeze (<28°F)"),
      x = df$date, y = if_else(df$min_temp <= 28, 28, NA), yaxis = yaxis,
      type = "scatter", mode = "lines", hovertemplate = "Yes",
      line = list(color = OPTS$plot_colors$freeze, width = width, dash = dash)
    ) %>%
    add_trace(
      name = paste0(label, "Killing freeze (<24°F)"),
      x = df$date, y = if_else(df$min_temp <= 24, 24, NA), yaxis = yaxis,
      type = "scatter", mode = "lines", hovertemplate = "Yes",
      line = list(color = OPTS$plot_colors$kill, width = width, dash = dash)
    ) %>%
    set_axis(yaxis, "Temperature (°F)")
}

add_gdd_daily_traces <- function(plt, df, yaxis = "y1", label = "", dash = F) {
  dash <- ifelse(dash, "dashdot", "none")
  width <- OPTS$plot_line_width
  plt %>%
    add_trace(
      name = paste(label, "GDD41/day"), x = df$date, y = df$gdd41, yaxis = yaxis,
      type = "scatter", mode = "lines", hovertemplate = "%{y:.1f}",
      line = list(color = OPTS$plot_colors$gdd41, width = width, dash = dash)
    ) %>%
    add_trace(
      name = paste(label, "GDD50/day"), x = df$date, y = df$gdd50, yaxis = yaxis,
      type = "scatter", mode = "lines", hovertemplate = "%{y:.1f}",
      line = list(color = OPTS$plot_colors$gdd50, width = width, dash = dash)
    ) %>%
    set_axis(yaxis, "GDD accumulation/day")
}

add_gdd_cum_traces <- function(plt, df, yaxis = "y1", label = "", dash = F) {
  dash <- ifelse(dash, "dashdot", "none")
  width <- OPTS$plot_line_width

  plt %>%
    add_trace(
      name = paste0(label, "GDD41 (cumul.)"), x = df$date, y = df$gdd41cum, yaxis = yaxis,
      type = "scatter", mode = "lines", hovertemplate = "%{y:.1f}",
      line = list(color = OPTS$plot_colors$gdd41cum, width = width, dash = dash)
    ) %>%
    add_trace(
      name = paste0(label, "GDD50 (cumul.)"), x = df$date, y = df$gdd50cum, yaxis = yaxis,
      type = "scatter", mode = "lines", hovertemplate = "%{y:.1f}",
      line = list(color = OPTS$plot_colors$gdd50cum, width = width, dash = dash)
    ) %>%
    set_axis(yaxis, "Cumulative GDD")
}

add_frost_traces <- function(plt, df, yaxis = "y1") {
  width <- OPTS$plot_line_width
  color <- OPTS$plot_colors$frost
  plt %>%
    add_trace(
      name = "Frost probability", x = df$date, y = df$frost * 100, yaxis = yaxis,
      type = "scatter", mode = "lines", hovertemplate = "%{y:.1f}%",
      line = list(color = color, width = width, shape = "spline")
    ) %>%
    add_trace(
      name = "Cumul. frost prob.", x = df$date, y = df$frost_by * 100, yaxis = yaxis,
      type = "scatter", mode = "lines", hovertemplate = "%{y:.1f}%",
      line = list(color = color, width = width, shape = "spline", dash = "dot")
    ) %>%
    set_axis(yaxis, "Frost probability (<32°F)")
}

add_freeze_traces <- function(plt, df, yaxis = "y1") {
  width <- OPTS$plot_line_width
  color <- OPTS$plot_color$freeze
  plt %>%
    add_trace(
      name = "Hard freeze probability", x = df$date, y = df$freeze * 100, yaxis = yaxis,
      type = "scatter", mode = "lines", hovertemplate = "%{y:.1f}%",
      line = list(color = color, width = width, shape = "spline")
    ) %>%
    add_trace(
      name = "Cumul. freeze prob.", x = df$date, y = df$freeze_by * 100, yaxis = yaxis,
      type = "scatter", mode = "lines", hovertemplate = "%{y:.1f}%",
      line = list(color = color, width = width, shape = "spline", dash = "dot")
    ) %>%
    set_axis(yaxis, "Hard freeze probability (<28°F)")
}

add_kill_traces <- function(plt, df, yaxis = "y1") {
  width <- OPTS$plot_line_width
  color <- OPTS$plot_colors$kill
  plt %>%
    add_trace(
      name = "Killing freeze prob.", x = df$date, y = df$kill * 100, yaxis = yaxis,
      type = "scatter", mode = "lines", hovertemplate = "%{y:.1f}%",
      line = list(color = color, width = width, shape = "spline")
    ) %>%
    add_trace(
      name = "Cumul. killing freeze prob.", x = df$date, y = df$kill_by * 100, yaxis = yaxis,
      type = "scatter", mode = "lines", hovertemplate = "%{y:.1f}%",
      line = list(color = color, width = width, shape = "spline", dash = "dot")
    ) %>%
    set_axis(yaxis, "Killing freeze probability (<24°F)")
}

add_frost_freeze_kill_traces <- function(plt, df, yaxis = "y1") {
  width <- OPTS$plot_line_width
  plt %>%
    add_trace(
      name = "Frost probability", x = df$date, y = df$frost * 100, yaxis = yaxis,
      type = "scatter", mode = "lines", hovertemplate = "%{y:.1f}%",
      line = list(color = OPTS$plot_colors$frost, width = width, shape = "spline")
    ) %>%
    add_trace(
      name = "Hard freeze probability", x = df$date, y = df$freeze * 100, yaxis = yaxis,
      type = "scatter", mode = "lines", hovertemplate = "%{y:.1f}%",
      line = list(color = OPTS$plot_colors$freeze, width = width, shape = "spline")
    ) %>%
    add_trace(
      name = "Killing freeze probability", x = df$date, y = df$kill * 100, yaxis = yaxis,
      type = "scatter", mode = "lines", hovertemplate = "%{y:.1f}%",
      line = list(color = OPTS$plot_colors$kill, width = width, shape = "spline")
    ) %>%
    add_trace(
      name = "Cumul. frost prob.", x = df$date, y = df$frost_by * 100, yaxis = yaxis,
      type = "scatter", mode = "lines", hovertemplate = "%{y:.1f}%",
      line = list(color = OPTS$plot_colors$frost, width = width, shape = "spline", dash = "dot")
    ) %>%
    add_trace(
      name = "Cumul. freeze prob.", x = df$date, y = df$freeze_by * 100, yaxis = yaxis,
      type = "scatter", mode = "lines", hovertemplate = "%{y:.1f}%",
      line = list(color = OPTS$plot_colors$freeze, width = width, shape = "spline", dash = "dot")
    ) %>%
    add_trace(
      name = "Cumul. kill prob.", x = df$date, y = df$kill_by * 100, yaxis = yaxis,
      type = "scatter", mode = "lines", hovertemplate = "%{y:.1f}%",
      line = list(color = OPTS$plot_colors$kill, width = width, shape = "spline", dash = "dot")
    ) %>%
    set_axis(yaxis, "Frost/freeze/kill probability")
}
