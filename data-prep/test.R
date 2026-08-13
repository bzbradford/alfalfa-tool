# Inspect climate data ---------------------------------------------------------

## load data ----

climate <- list(
  "5yr" = "data/climate/climate_5yr_2020_2025.fst",
  "10yr" = "data/climate/climate_10yr_2015_2025.fst",
  "30yr" = "data/climate/climate_30yr_1995_2025.fst"
) |>
  lapply(read_fst) |>
  lapply(as_tibble) |>
  bind_rows(.id = "type") |>
  mutate(across(type, fct_inorder))


## build grid ----

climate_grid <- climate |>
  distinct(lat, lng) |>
  mutate(
    geometry = st_sfc(points_to_grid(lat, lng), crs = 4326),
    .by = c(lat, lng)
  ) |>
  st_set_geometry("geometry")

# should show correct grid which omits canada and great lakes
climate_grid |>
  leaflet() |>
  addProviderTiles(providers$CartoDB.Positron) |>
  addPolygons(weight = 0.25, opacity = 0.5, fillOpacity = 0)

# should show same
climate |>
  distinct(lat, lng) |>
  leaflet() |>
  addProviderTiles(providers$CartoDB.Positron) |>
  addRectangles(
    lat1 = ~ lat - 0.05,
    lat2 = ~ lat + 0.05,
    lng1 = ~ lng - 0.05,
    lng2 = ~ lng + 0.05,
    weight = 0.25,
    opacity = 0.5,
    fillOpacity = 0
  )


# inspect climate data ----

climate |>
  filter(type == "30yr") |>
  filter(yday == 150) |>
  rename(value = max_temp) |>
  leaflet() |>
  addProviderTiles(providers$CartoDB.Positron) |>
  addRectangles(
    lat1 = ~ lat - .05,
    lat2 = ~ lat + .05,
    lng1 = ~ lng - .05,
    lng2 = ~ lng + .05,
    weight = .5,
    opacity = .1,
    color = "black",
    fillOpacity = .75,
    fillColor = ~ colorNumeric("viridis", value)(value),
    label = ~ round(value, 1)
  )

climate |>
  filter(lat == sample(lat, 1), lng == sample(lng, 1)) |>
  ggplot(aes(x = yday, y = gdd41, color = type)) +
  geom_line() +
  geom_smooth(method = "gam") +
  labs(title = "Mean GDD41 per day")

climate |>
  filter(lat == sample(lat, 1), lng == sample(lng, 1)) |>
  ggplot(aes(x = yday, y = freeze, color = type)) +
  geom_line() +
  geom_smooth(method = "gam")

climate |>
  filter(lat == sample(lat, 1), lng == sample(lng, 1)) |>
  mutate(date = as_date("2024-1-1") + yday - 1) |>
  plot_ly(x = ~date, color = ~type) |>
  add_lines(y = ~frost_by, name = ~ paste(type, "prob of frost by date")) |>
  add_lines(y = ~freeze_by, name = ~ paste(type, "prob of freezing by date"))


# County map ----

us_counties <- read_sf("cb-2018-conus-county-5m.geojson")
us_states <- read_sf("cb-2018-conus-state-5m.geojson")

climate_grids <- climate_10yr |>
  distinct(lat, lng) |>
  mutate(keep = T) |>
  st_as_sf(coords = c("lng", "lat"), crs = 4326)

keep_counties <- climate_grids |>
  st_join(us_counties) |>
  drop_na(NAME) |>
  distinct(STATEFP, COUNTYFP)

select_counties <- us_counties |>
  inner_join(keep_counties) |>
  st_join(us_states |> select(state = NAME, geometry), largest = T) |>
  select(state, county = NAME, geometry) |>
  arrange(state, county)

select_counties |> saveRDS("mw-counties.rds")

select_counties |>
  leaflet() |>
  addPolygons(
    label = ~ paste0(state, ": ", county, " County"),
    fillColor = ~ colorFactor(grDevices::rainbow(n_distinct(state)), state)(
      state
    ),
    weight = 1
  )

wi_counties <- read_rds("counties-wi.rds") |>
  select(county = CountyName, dnr_region = DnrRegion, geometry)

wi_counties |>
  saveRDS("counties-wi.rds")
