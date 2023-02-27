library(magrittr)


# Load data ---------------------------------------------------------------

bigfoot <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-09-13/bigfoot.csv')



# Check data --------------------------------------------------------------

head(bigfoot)

dplyr::glimpse(bigfoot)



# Summary -----------------------------------------------------------------

bf_data <- bigfoot %>%
  dplyr::filter(
    !state == "Alaska"
    & year(date) >= 1950
  ) %>%
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

