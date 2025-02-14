#- downloads.R -#

downloadsUI <- function() {
  ns <- NS("downloads")
  tagList(
    h4("Entire grid"),
    p("Download a copy of the data currently shown on the map as a CSV."),
    uiOutput(ns("grid_download_ui")),
    br(),
    h4("Single location"),
    p("Download a copy of the weather and climate data for a single location."),
    uiOutput(ns("loc_download_ui"))
  )
}

downloadsServer <- function(grid_data, loc_data) {
  moduleServer(
    id = "downloads",
    function(input, output, session) {
      ns <- session$ns

      climate_btn_names <- invert(OPTS$climate_period_choices)

      rv <- reactiveValues(
        loc_ready = FALSE
      )

      observe({
        loc_data()
        rv$loc_ready <- TRUE
      })


      # Grid download ----

      grid_csv_name <- reactive({
        opts <- req(grid_data()$opts)
        paste0(
          toupper(opts$extent),
          " ",
          opts$type,
          " grid - ",
          gsub("_", " ", opts$col),
          " for ",
          opts$date,
          {
            if (opts$smoothing > 1) paste0(" (", opts$smoothing, "-day avg)")
          }
        )
      })

      grid_csv_header <- reactive({
        opts <- req(grid_data()$opts) %>% lapply(as.character)
        params <- list(
          "Map extent" = invert(OPTS$map_extent_choices)[[opts$extent]],
          "Map type" = invert(OPTS$map_type_choices)[[opts$type]],
          "Climate type" = ifelse(
            is.null(opts$period),
            "Not applicable",
            invert(OPTS$climate_period_choices)[[opts$period]]
          ),
          "Value" = opts$value_label,
          "Date" = opts$date,
          "Moving average" = invert(OPTS$data_smoothing_choices)[[opts$smoothing]]
        )
        params <- paste0(names(params), ": ", params)
        params <- na.omit(params)
        tibble(line = c("Alfalfa weather tool gridded data", NA, params, NA))
      })

      grid_csv_content <- reactive({
        grid <- req(grid_data()$grid)
        grid %>%
          complete(crossing(lat, lng), fill = list(value = NA)) %>%
          arrange(desc(lat), lng) %>%
          select("lat\\lng" = lat, name = lng, value) %>%
          pivot_wider()
      })

      output$grid_download_ui <- renderUI({
        downloadButton(ns("grid_csv"), "Download grid", class = "btn-sm")
      })

      output$grid_csv <- downloadHandler(
        filename = function() paste(grid_csv_name(), "csv", sep = "."),
        content = function(file) {
          header <- grid_csv_header()
          content <- grid_csv_content()
          write_excel_csv(header, file, col_names = F, na = "")
          write_excel_csv(content, file, append = T, col_names = T, na = "")
        }
      )


      # Location downloads ----

      loc_str <- reactive({
        loc <- req(loc_data()$loc)
        sprintf("%.1f°N, %.1f°W", loc$lat, loc$lng)
      })

      output$loc_download_ui <- renderUI({
        validate(need(rv$loc_ready, OPTS$location_validation_msg))
        tagList(
          p("Selected location:", loc_str()),
          div(
            style = "display: flex; flex-wrap: wrap; flex-direction: row; gap: 10px;",
            downloadButton(
              ns("loc_weather_csv"),
              paste0("Weather (", min(OPTS$weather_years), "-", max(OPTS$weather_years), ")"),
              class = "btn-sm"
            ),
            downloadButton(ns("loc_c5_csv"), climate_btn_names$c5, class = "btn-sm"),
            downloadButton(ns("loc_c10_csv"), climate_btn_names$c10, class = "btn-sm")
          )
        )
      })

      output$loc_weather_csv <- downloadHandler(
        filename = function() paste0("Weather for ", loc_str(), ".csv"),
        content = function(file) {
          header <- bind_rows(
            tibble(name = paste0("Weather data for ", loc_str())),
            tibble(name = NA),
            enframe(invert(OPTS$grid_cols$weather)) %>% unnest(value),
            tibble(name = NA)
          )
          content <- loc_data()$weather
          write_excel_csv(header, file, col_names = F, na = "")
          write_excel_csv(content, file, append = T, col_names = T, na = "")
        }
      )

      climate_csv_handler <- function(period) {
        downloadHandler(
          filename = function() paste0(period, "-year climate for ", loc_str(), ".csv"),
          content = function(file) {
            header <- bind_rows(
              tibble(name = paste0(period, "-year climate data for ", loc_str())),
              tibble(name = NA),
              enframe(invert(OPTS$grid_cols$climate)) %>% unnest(value),
              tibble(name = NA)
            )
            content <- loc_data()[[paste0("c", period)]]
            write_excel_csv(header, file, col_names = F, na = "")
            write_excel_csv(content, file, append = T, col_names = T, na = "")
          }
        )
      }

      output$loc_c5_csv <- climate_csv_handler(5)
      output$loc_c10_csv <- climate_csv_handler(10)

      # output$loc_c5_csv <- downloadHandler(
      #   filename = function() paste0("5-year climate for ", loc_str(), ".csv"),
      #   content = function(file) write_csv(loc_data()$c5, file)
      # )
      #
      # output$loc_c10_csv <- downloadHandler(
      #   filename = function() paste0("10-year climate for ", loc_str(), ".csv"),
      #   content = function(file) write_csv(loc_data()$c10, file)
      # )
    }
  )
}
