library(lubridate)



# plot 1 ------------------------------------------------------------------

graph1 <- function(data) {
  data %>%
    dplyr::group_by(
      state,
      classification
      ) %>%
    dplyr::summarise(
      N = sum(N_obs),
      .groups = "drop"
      ) %>%
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

graph2 <- function(data) {
  data %>%
    dplyr::group_by(
      Year = year(date)
      ) %>%
    dplyr::summarise(
      Sightings = sum(N_obs)
      ) %>%
    highcharter::hchart(
      type = "areaspline",
      highcharter::hcaes(
        x = Year,
        y = Sightings),
      name = "Sightings"
      ) %>%
    highcharter::hc_add_theme(highcharter::hc_theme_ft()
                              )

    # ggplot2::ggplot() +
    # ggplot2::aes(x = {{x_var}}, y = total) +
    # ggplot2::geom_line() +
    # ggplot2::theme_minimal(12)
}


# table 1 -----------------------------------------------------------------

table <- function(data) {
  data %>%
    dplyr::group_by(
      state,
      county
      ) %>%
    dplyr::summarise(
      total = sum(N_obs),
      .groups = "drop") %>%
    reactable::reactable(
      groupBy = "state",
      columns = list(
        state = reactable::colDef("State"),
        county = reactable::colDef("County"),
        total = reactable::colDef("Sightings",
                                  aggregate = "sum"
                                  )
      )
    )
}


# Map ---------------------------------------------------------------------


map_bigfoot <- function(data) {
  data %>%
    dplyr::group_by(county) %>%
    dplyr::summarise(
      across(c(lat, lon), dplyr::first),
      n = sum(N_obs),
      .groups = "drop"
    ) %>%
    dplyr::mutate(lab = paste0(county, ": ", n)) %>%
    leaflet::leaflet() %>%
    leaflet::addTiles() %>%
    leaflet::addCircleMarkers(
      lat = ~lat,
      lng = ~lon,
      weight = 1.5,
      radius = ~n/2,
      color = "#67001f",
      popup = ~lab
    )
}




# map_bigfoot2 <- function(data) {
#
#   df <- bf_data %>%
#     dplyr::group_by(name = county) %>%
#     dplyr::summarise(
#       across(c(lat, lon), dplyr::first),
#       z = sum(N_obs),
#       .groups = "drop"
#     )
#
#   highcharter::hcmap("countries/us/us-all") %>%
#   highcharter::hc_add_series(df,
#                              type = "mapbubble",
#                              name = "County",
#                              minSize = "1%",
#                              maxSize = "5%"
#                              ) %>%
#   highcharter::hc_mapNavigation(enabled = TRUE)
# }



