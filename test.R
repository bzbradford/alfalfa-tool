


# Growth plot -------------------------------------------------------------

test_loc <- list(lat = 44.3, lng = -90.2)
test_loc <- list(lat = 45, lng = -89)
test_wx <- weather %>% filter(lat == test_loc$lat, lng == test_loc$lng)
test_cl <- climate$c10 %>% filter(lat == test_loc$lat, lng == test_loc$lng)
test_data <- buildGrowthData(test_wx, test_cl, as_date("2025-1-1"))

buildGrowthPlot(test_data, test_loc)

lapply(OPTS$growth_thresholds, \(t) eval(str_glue("sign(lag(gdd_since_kill) - {t}) != sign(gdd_since_kill - {t}) ~ {t}" )))



df <- test_data %>%
  filter(!is.na(growth_threshold))


buildGrowthAnnotation




buildGrowthInfo(test_data)

unique(year(test_data$date))
