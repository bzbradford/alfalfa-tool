#- ui.R -#

ui <- navbarPage(
  title = "Alfalfa Weather and Cutting Tool",
  id = "navbar",
  theme = shinytheme("flatly"),
  position = "fixed-top",
  collapsible = TRUE,

  header = tagList(
    tags$head(
      tags$meta(charset = "UTF-8"),
      tags$meta(name = "description", content = "A weather tool for scheduling alfalfa cutting and managing fall frost risk in Wisconsin and the Upper Midwest"),
      tags$meta(name = "keywords", content = "uw, wisconsin, alfalfa, weather, frost, freeze, growing degree days, risk, tool"),
      tags$link(rel = "shortcut icon", href = "favicon.ico"),
      tags$link(rel = "stylesheet", type = "text/css", href = "style.css"),
      tags$script(src = "script.js"),
      includeHTML("www/google-analytics.html"),
      useShinyjs()
    ),
    div(
      id = "main",
      withSpinnerProxy(
        uiOutput("main_ui")
      )
    )
  ),

  tabPanel("Weather Map", icon = icon("map"), value = "map"),
  tabPanel("Growth Projection", icon = icon("leaf"), value = "growth"),
  tabPanel("Season Planning", icon = icon("tractor"), value = "timing"),
  tabPanel("Weather Charts", icon = icon("chart-line"), value = "charts"),
  tabPanel("About", icon = icon("question"), value = "about"),

  footer = tags$footer(
    class = "footer-container",
    div(
      class = "footer-content",
      div(
        class = "footer-credits",
        p(
          "App developed by",
          a("Ben Bradford", href = "https://entomology.wisc.edu/directory/ben-bradford/", target = "_blank", .noWS = "after"),
          ", UW-Madison Entomology",
          br(),
          "Last updated:", format(file.info(".")$mtime, "%Y-%m-%d."),
          a("View source code", href = "https://github.com/bzbradford/alfalfa-tool", target = "_blank", .noWS = "after"), ".",
        ),
        p(
          "Feedback welcome!",
          a("Click here to take our survey.", href = "https://uwmadison.co1.qualtrics.com/jfe/form/SV_81RoNUXYxlicCa2", target = "_blank")
        )
      ),
      div(
        class = "footer-badges",
        img(src = "uw-logo.svg", width = "200px")
      )
    )
  )
)
