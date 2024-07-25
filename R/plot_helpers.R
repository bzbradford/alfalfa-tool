#- plot_helpers.R -#

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
  width <- 1

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
  width <- 1

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
  width <- 1

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
  width <- 1

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
