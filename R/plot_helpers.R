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

add_today <- function(plt, yr = cur_yr, other_shapes = list()) {
  x <- yesterday() + 1
  if (yr == cur_yr) {
    # x <- yesterday() + 1
    text <- "Today"
  } else {
    # x <- yesterday() + 1
    text <- format(x, "%b %d")
  }

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

  plt %>%
    layout(shapes = shapes, annotations = text)
}

set_axis <- function(plt, yaxis, title) {
  axis <-  list(
    title = title,
    zeroline = F
  )
  if (yaxis == "y1") {
    plt %>% layout(yaxis = axis)
  } else {
    axis$overlaying = "y"
    axis$side = "right"
    plt %>% layout(yaxis2 = axis)
  }
}

add_temp_traces <- function(plt, df, yaxis = "y1", label = "", dash = F) {
  dash <- ifelse(dash, "dashdot", "none")
  width <- OPTS$plot_line_width

  plt %>%
    add_trace(
      name = paste0(label, "Min temp"),
      x = df$date, y = df$min_temp,
      type = "scatter", mode = "lines",
      line = list(
        color = "cornflowerblue",
        width = width,
        shape = "spline",
        dash = dash),
      hovertemplate = "%{y:.1f}°F",
      yaxis = yaxis
    ) %>%
    add_trace(
      name = paste0(label, "Mean temp"),
      x = df$date, y = df$mean_temp,
      type = "scatter", mode = "lines",
      line = list(
        color = "orange",
        width = width,
        shape = "spline",
        dash = dash),
      hovertemplate = "%{y:.1f}°F",
      yaxis = yaxis
    ) %>%
    add_trace(
      name = paste0(label, "Max temp"),
      x = df$date, y = df$max_temp,
      type = "scatter", mode = "lines",
      line = list(
        color = "#c5050c",
        width = width,
        shape = "spline",
        dash = dash),
      hovertemplate = "%{y:.1f}°F",
      yaxis = yaxis
    ) %>%
    add_trace(
      name = paste0(label, "Frost (<32F)"),
      x = df$date, y = if_else(df$min_temp <= 32, 32, NA),
      type = "scatter", mode = "lines",
      line = list(
        color = "orchid",
        width = width,
        dash = dash),
      hovertemplate = "Yes",
      yaxis = yaxis
    ) %>%
    add_trace(
      name = paste0(label, "Hard freeze (<28F)"),
      x = df$date, y = if_else(df$min_temp <= 28, 28, NA),
      type = "scatter", mode = "lines",
      line = list(
        color = "purple",
        width = width,
        dash = dash),
      hovertemplate = "Yes",
      yaxis = yaxis
    ) %>%
    set_axis(yaxis, "Temperature (F)")
}

add_gdd_daily_traces <- function(plt, df, yaxis = "y1", label = "", dash = F) {
  dash <- ifelse(dash, "dashdot", "none")
  width <- OPTS$plot_line_width

  plt %>%
    add_trace(
      name = paste(label, "GDD41/day"),
      x = df$date, y = df$gdd41,
      type = "scatter", mode = "lines",
      line = list(
        color = "green",
        width = width,
        dash = dash),
      hovertemplate = "%{y:.1f}",
      yaxis = yaxis
    ) %>%
    add_trace(
      name = paste(label, "GDD50/day"),
      x = df$date, y = df$gdd50,
      type = "scatter", mode = "lines",
      line = list(
        color = "peru",
        width = width,
        dash = dash),
      hovertemplate = "%{y:.1f}",
      yaxis = yaxis
    ) %>%
    set_axis(yaxis, "GDD accumulation/day")
}

add_gdd_cum_traces <- function(plt, df, yaxis = "y1", label = "", dash = F) {
  dash <- ifelse(dash, "dashdot", "none")
  width <- OPTS$plot_line_width

  plt %>%
    add_trace(
      name = paste0(label, "GDD41 (cumul.)"),
      x = df$date, y = df$gdd41cum,
      type = "scatter", mode = "lines",
      line = list(
        color = "olivedrab",
        width = width,
        dash = dash),
      hovertemplate = "%{y:.1f}",
      yaxis = yaxis
    ) %>%
    add_trace(
      name = paste0(label, "GDD50 (cumul.)"),
      x = df$date, y = df$gdd50cum,
      type = "scatter", mode = "lines",
      line = list(
        color = "chocolate",
        width = width,
        dash = dash),
      hovertemplate = "%{y:.1f}",
      yaxis = yaxis
    ) %>%
    set_axis(yaxis, "Cumulative GDD")
}

add_frost_traces <- function(plt, df, yaxis = "y1") {
  width <- OPTS$plot_line_width

  plt %>%
    add_trace(
      name = "Frost probability",
      x = df$date, y = df$frost*100,
      type = "scatter", mode = "lines",
      line = list(
        color = "orchid",
        width = width,
        shape = "spline"),
      hovertemplate = "%{y:.1f}%",
      yaxis = yaxis
    ) %>%
    add_trace(
      name = "Hard freeze probability",
      x = df$date, y = df$freeze*100,
      type = "scatter", mode = "lines",
      hovertemplate = "%{y:.1f}%",
      line = list(
        color = "purple",
        width = width,
        shape = "spline"),
      yaxis = yaxis
    ) %>%
    add_trace(
      name = "Cumul. frost prob.",
      x = df$date, y = df$frost_by*100,
      type = "scatter", mode = "lines",
      hovertemplate = "%{y:.1f}%",
      line = list(
        color = "orchid",
        width = width,
        shape = "spline",
        dash = "dot"),
      yaxis = yaxis
    ) %>%
    add_trace(
      name = "Cumul. freeze prob.",
      x = df$date, y = df$freeze_by*100,
      type = "scatter", mode = "lines",
      hovertemplate = "%{y:.1f}%",
      line = list(
        color = "purple",
        width = width,
        shape = "spline",
        dash = "dot"),
      yaxis = yaxis
    ) %>%
    set_axis(yaxis, "Frost/freeze probability")
}
