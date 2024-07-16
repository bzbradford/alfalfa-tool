ui <- fluidPage(

  ## Setup ----

  title = "Alfalfa Weather Tool",
  theme = shinytheme("flatly"),
  tags$head(
    tags$meta(charset = "UTF-8"),
    tags$meta(name = "description", content = "A weather tool for managing frost risk for fall alfalfa cutting in Wisconsin"),
    tags$meta(name = "keywords", content = "uw, wisconsin, alfalfa, weather, frost, freeze, growing degree days, risk, tool"),
    # tags$link(rel = "shortcut icon", href = "favicon.ico"),
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css"),
    # includeHTML("google-analytics.html"),
    # tags$script(src = "html2canvas.min.js"),
    # tags$script(src = "saveAs.js")
  ),
  useShinyjs(),

  div(
    id = "main-content",

    ## Page heading ----
    tags$header(
      h2("Alfalfa Weather Risk Tool", align = "center"),
    ),

    ## Map and sidebar ----
    tagList(
      div(
        style = "margin: 0.5em 1em;", align = "center",
        p(em("Currently this tool allows for browsing current-year weather data, 5- and 10-year climate averages, and comparing the two. Ultimately, it will help alfalfa growers time their last fall cutting while understanding the probable timing of a fall killing freeze."))
      ),
      br(),
      sidebarLayout(
        sidebarPanel(
          radioButtons(
            inputId = "map_data_type",
            label = "Choose data layer",
            choices = list(
              "Current weather" = "weather",
              "Climate normals" = "climate",
              "Weather vs climate" = "comparison"
            )
          ),
          uiOutput("map_opts_ui")
        ),
        mainPanel(
          div(
            leafletOutput("map", width = "100%", height = "720px") %>% withSpinnerProxy(hide.ui = T)
          )
        ),
        position = "right"
      )
    )
  ),


  # Footer ----

  tags$footer(
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
