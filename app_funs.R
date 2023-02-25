library(lubridate)



# plot 1 ------------------------------------------------------------------

graph1 <- function(data) {
  data %>%
    dplyr::filter(!season == "Unknown") %>%
    dplyr::group_by(state, classification) %>%
    dplyr::summarise(N = sum(N_obs), .groups = "drop") %>%
    highcharter::hchart(
      type = "bar",
      highcharter::hcaes(
        x = state,
        y = N,
        group = classification
      )
    )


    # ggplot2::ggplot() +
    # ggplot2::aes(x = {{x_var}}, y = {{y_var}}, fill = {{label_var}}) +
    # ggplot2::geom_col() +
    # ggplot2::theme_minimal(12) +
    # ggplot2::coord_flip()
}


# plot 2 ------------------------------------------------------------------

graph2 <- function(data, x_var, y_var) {
  data %>%
    dplyr::filter(year(date) >= 1950) %>%
    dplyr::group_by(Year = year(date)) %>%
    dplyr::summarise(Sightings = sum({{y_var}})) %>%
    highcharter::hchart(
      type = "line",
      highcharter::hcaes(
        x = Year,
        y = Sightings)
      )

    # ggplot2::ggplot() +
    # ggplot2::aes(x = {{x_var}}, y = total) +
    # ggplot2::geom_line() +
    # ggplot2::theme_minimal(12)
}


# table 1 -----------------------------------------------------------------

table <- function(data) {
  data %>%
    dplyr::group_by(state, county) %>%
    dplyr::summarise(
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
    dplyr::filter(!state == "Alaska") %>%
    dplyr::group_by(county) %>%
    dplyr::summarise(
      across(c(lat, lon), dplyr::first),
      n = sum(N_obs),
      .groups = "drop"
    ) %>%
    dplyr::mutate(lab = paste0(county, ": ", n)) %>%
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
