#- ui.R -#

ui <- navbarPage(
  title = "Alfalfa Weather Tool",
  id = "navbar",
  theme = shinytheme("flatly"),
  position = "fixed-top",
  collapsible = TRUE,
  header = tagList(
    tags$head(
      tags$meta(charset = "UTF-8"),
      tags$meta(name = "description", content = "A weather tool for managing frost risk for fall alfalfa cutting in Wisconsin"),
      tags$meta(name = "keywords", content = "uw, wisconsin, alfalfa, weather, frost, freeze, growing degree days, risk, tool"),
      tags$link(rel = "shortcut icon", href = "favicon.ico"),
      # tags$link(rel = "stylesheet", type = "text/css", href = "style.css"),
      tags$style(style),
      useShinyjs(),
      tags$script(src = "script.js"),
      # includeHTML("google-analytics.html"),
      # tags$script(src = "html2canvas.min.js"),
      # tags$script(src = "saveAs.js")
    ),
    div(
      style = "min-height: 750px; margin: 0 20px;",
      uiOutput("main_ui") %>% withSpinnerProxy()
    )
  ),
  tabPanel("Weather Map", icon = icon("map")),
  tabPanel("Weather Charts", icon = icon("chart-line")),
  tabPanel("Timing Tool", icon = icon("tractor")),
  tabPanel("About", icon = icon("question")),
  footer = tags$footer(
    id = "footer-content",
    br(),
    hr(),
    p(
      style = "color: grey; font-size: smaller; font-style: italic;",
      align = "center",
      "Alfalfa weather tool developed by",
      a("Ben Bradford", href = "https://entomology.wisc.edu/directory/ben-bradford/", target = "_blank", .noWS = "after"),
      ", UW-Madison Entomology", br(),
      paste(
        "Last updated:", format(file.info(".")$mtime, "%Y-%m-%d")
      ), br(),
      a("Source code", href = "https://github.com/bzbradford/alfalfa-tool", target = "_blank")
    )
  )
)
