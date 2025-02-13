
## App features

### Gridded weather map

Weather and climate data are presented on a 0.1 decimal degree grid (grid cells measure approximately 5 x 7 miles), with weather data originally sourced from NOAA and retrieved from <a href="https://agweather.cals.wisc.edu" target="_blank">AgWeather</a> and climate data sourced from <a href="https://www.climatologylab.org/gridmet.html" target="_blank">GridMET</a> using the <a href="https://github.com/mikejohnson51/climateR" target="_blank">ClimateR</a> package. Clicking a grid cell will select that location for viewing in the charts and tools tabs.

### Alfalfa growth projection tool

This tool uses weather and climate data to estimate alfalfa growth initial spring growth or last cutting and identifies dates when the next cut should take place based on accumulated degree days. This tool also displays observed killing freezes (spring) and fall killing freeze probabilities based on climate averages. A location on the map must be selected before using this tool.

### Full-season alfalfa cut scheduling tool

Traditionally, timing alfalfa cuttings is done based on a calendar or a grower's knowledge or experience. This tool helps to plan or evaluate a cutting schedule based on actual and projected growing degree days to identify optimal cut timing for plant health. During the growing season, alfalfa should be cut when between 900 and 1100 growing degree days (base 41°F) have elapsed since Jan 1 or the last time the crop was cut. In the fall, the last cut should be scheduled such that either the crop has enough time to reach maturity again before a killing freeze, or the crop has very little time (<360 GDD) to grow back before the first killing freeze.

### Weather and climate charts

There are three types of charts, one focused on observed weather for this year and last, one for exploring climate averages, and a 'custom' chart where you can compare any two kinds of weather or climate values. Elements are added to the charts in groups, but individual lines may be shown or hidden by clicking on items in the chart legend.

## Methodology

### Frost and freeze calculations

Spring and fall frost/freeze risk is calculated using climate averages and shown as either the probability of frost/freeze on a given day of year, or the cumulative probability that a frost/freeze will have occurred on or before that date (fall) or will not occur again after that date (spring). For instance, a cumulative killing freeze probability of 50% on Oct. 25 means that in the historical data used, half of those years saw a killing freeze on or before Oct. 25 and half did not. This can aid in setting your risk tolerance. Frost occurs when the daily minimum is below 32°F, hard freeze occurs when the minimum is below 28°F, and a killing freeze occurs when the minimum is below 24°F.
