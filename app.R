library(magrittr)
library(dplyr)
library(ggplot2)
library(lubridate)


# Load data ---------------------------------------------------------------

bigfoot <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-09-13/bigfoot.csv')



# Check data --------------------------------------------------------------

head(bigfoot)

glimpse(bigfoot)



# Summary -----------------------------------------------------------------

big_summ <- bigfoot %>%
  group_by(
    state,
    county,
    date,
    lat = latitude,
    lon = longitude,
    season,
    precip_type,
    classification
    ) %>%
  summarise(
    N_obs = n(),
    temp_high = mean(temperature_high),
    temp_mid = mean(temperature_mid),
    temp_low = mean(temperature_low),
    humidity = mean(humidity),
    cloud_cover = mean(cloud_cover),
    precip_int = mean(precip_intensity),
    precip_prob = mean(precip_probability),
    wind = mean(wind_speed),
    .groups = "drop"
    )


# plot 1 ------------------------------------------------------------------

graph1 <- function(data, x_var, y_var, label_var) {
  data %>%
    filter(!season == "Unknown") %>%
    ggplot() +
    aes(x = {{x_var}}, y = {{y_var}}, fill = {{label_var}}) +
    geom_col() +
    theme_minimal(12) +
    coord_flip()
}


# plot 2 ------------------------------------------------------------------

graph2 <- function(data, x_var, y_var) {
  data %>%
    filter(year(date) >= 1950) %>%
    group_by(year = year(date)) %>%
    summarise(total = sum({{y_var}})) %>%
    ggplot() +
    aes(x = {{x_var}}, y = total) +
    geom_line() +
    theme_minimal(12)
}


# table 1 -----------------------------------------------------------------

table <- function(data) {
  data %>%
  group_by(state, county) %>%
  summarise(
    total = sum(N_obs),
    .groups = "drop") %>%
  reactable::reactable(
    groupBy = "state",
    columns = list(
      state = reactable::colDef("State"),
      county = reactable::colDef("County"),
      total = reactable::colDef("Observations", aggregate = "sum")
    )
  )
}


# Map ---------------------------------------------------------------------


map_bigfoot <- function(data) {
  data %>%
    filter(!state == "Alaska") %>%
    group_by(county) %>%
    summarise(
      across(c(lat, lon), first),
      n = sum(N_obs),
      .groups = "drop"
    ) %>%
    mutate(lab = paste0(county, ": ", n)) %>%
    leaflet::leaflet() %>%
    leaflet::addTiles() %>%
    leaflet::addCircles(
      lat = ~lat,
      lng = ~lon,
      weight = 1.5,
      radius = ~n * 1000,
      color = "#67001f",
      popup = ~lab
    )
}
