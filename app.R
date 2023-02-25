library(magrittr)
library(lubridate)
library(shiny)


# Load data ---------------------------------------------------------------

bigfoot <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-09-13/bigfoot.csv')



# Check data --------------------------------------------------------------

head(bigfoot)

dplyr::glimpse(bigfoot)



# Summary -----------------------------------------------------------------

big_summ <- bigfoot %>%
  dplyr::group_by(
    state,
    county,
    date,
    lat = latitude,
    lon = longitude,
    season,
    precip_type,
    classification
    ) %>%
  dplyr::summarise(
    N_obs = dplyr::n(),
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
    dplyr::filter(!season == "Unknown") %>%
    ggplot2::ggplot() +
    ggplot2::aes(x = {{x_var}}, y = {{y_var}}, fill = {{label_var}}) +
    ggplot2::geom_col() +
    ggplot2::theme_minimal(12) +
    ggplot2::coord_flip()
}


# plot 2 ------------------------------------------------------------------

graph2 <- function(data, x_var, y_var) {
  data %>%
    dplyr::filter(year(date) >= 1950) %>%
    dplyr::group_by(year = year(date)) %>%
    dplyr::summarise(total = sum({{y_var}})) %>%
    ggplot2::ggplot() +
    ggplot2::aes(x = {{x_var}}, y = total) +
    ggplot2::geom_line() +
    ggplot2::theme_minimal(12)
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



# Shiny -------------------------------------------------------------------

library(shiny)

ui <- fluidPage(
  fluidRow(
    column(
      width = 6,
      tags$h2("Bigfoot Sightings in USA")
    )
  ),
  fluidRow(
    column(
      width = 6,
      plotOutput("grafico"),
      reactable::reactableOutput("tabela")
    ),
    column(
      width = 6,
      leaflet::leafletOutput("mapa")
    )
  )

)

server <- function(input, output, session) {

  output$grafico <- renderPlot({
    graph2(big_summ, year, N_obs)
  })

  output$mapa <- leaflet::renderLeaflet({
    map_bigfoot(big_summ)
  })

  output$tabela <- reactable::renderReactable({
    table(big_summ)
  })
}

shinyApp(ui, server)



