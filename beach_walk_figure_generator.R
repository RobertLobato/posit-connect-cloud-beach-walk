library(httr)
library(jsonlite)
library(lubridate)
library(tidyverse)
library(ggtext)
library(ggimage)

# Coordinates for Jacksonville Beach, FL
lat <- 30.2947
lon <- -81.3931
location <- "32250"

# Date
date <- Sys.Date()

# Weather API Key (sign up at https://openweathermap.org/api)
WEATHER_API_KEY <- Sys.getenv("WEATHER_API_KEY")

# Get first light time (civil twilight begin)
get_first_light <- function(lat, lon, date) {
  url <- paste0("https://api.sunrise-sunset.org/json?lat=", lat,
                "&lng=", lon, "&date=", date, "&formatted=0")
  res <- GET(url)
  data <- content(res, "parsed")
  first_light <- data$results$civil_twilight_begin
  return(with_tz(ymd_hms(first_light), tzone = "America/New_York"))
}

# Get tide info from NOAA CO-OPS API
get_tide_info <- function(date) {
  station_id <- "8720291"  # Jacksonville Beach station
  url <- paste0("https://api.tidesandcurrents.noaa.gov/api/prod/datagetter?",
                "product=predictions&application=web_services",
                "&begin_date=", date,
                "&end_date=", date,
                "&datum=MLLW&station=", station_id,
                "&time_zone=lst_ldt&units=english&interval=hilo&format=json")
  res <- GET(url)
  data <- content(res, "parsed")
  return(data$predictions)
}

# Get tide info from NOAA CO-OPS API
get_tide_info_tibble <- function(date) {
  # station_id <- "8720218"  # Mayport Docs
  station_id <- "8720291"  # Jacksonville Beach station
  url <- paste0("https://api.tidesandcurrents.noaa.gov/api/prod/datagetter?",
                "product=predictions&application=web_services",
                "&begin_date=", date - days(1),
                "&end_date=", date + days(1),
                "&datum=MSL&station=", station_id,
                "&time_zone=lst_ldt&units=english&interval=30&format=json")
  res <- GET(url)
  data_text <- content(res, "text")
  data_flattened <- fromJSON(data_text, flatten = TRUE)
  data_tibble <- as_tibble(data_flattened$predictions)
  return(data_tibble)
}

get_tide_hilo_tibble <- function(date) {
  # station_id <- "8720218"  # Mayport Docs
  station_id <- "8720291"  # Jacksonville Beach station
  url <- paste0("https://api.tidesandcurrents.noaa.gov/api/prod/datagetter?",
                "product=predictions&application=web_services",
                "&begin_date=", date - days(1),
                "&end_date=", date + days(1),
                "&datum=MSL&station=", station_id,
                "&time_zone=lst_ldt&units=english&interval=hilo&format=json")
  res <- GET(url)
  data_text <- content(res, "text")
  data_flattened <- fromJSON(data_text, flatten = TRUE)
  data_tibble <- as_tibble(data_flattened$predictions)
  return(data_tibble)
}

# Get weather info from WeatherAPI.com (not Google but better documented)
get_weather_info <- function(location, api_key) {
  url <- paste0("http://api.weatherapi.com/v1/current.json?key=",
                api_key, "&q=", URLencode(location))
  res <- GET(url)
  data <- content(res, "parsed")
  list(
    condition = data$current$condition$text,
    temp = data$current$temp_f,
    humidity = data$current$humidity,
    wind = data$current$wind_mph,
    feels_like = data$current$feelslike_f
  )
}

get_weather_forecast_info <- function(location, api_key) {
  url <- paste0("http://api.weatherapi.com/v1/forecast.json?key=",
                api_key, "&days=1&q=", URLencode(location))
  res <- GET(url)
  data_text <- content(res, "text")
  data_flattened <- fromJSON(data_text, flatten = TRUE)
  data_tibble <- as_tibble(data_flattened$forecast$forecastday)
  forecast_tibble <- unnest(data_tibble, cols = hour)
}

# Main
first_light <- get_first_light(lat, lon, date)
# cat(paste("🌅 First Light (civil twilight begins):", first_light, "\n\n"))
#
# cat("🌊 Tide Info:\n")
# tides <- get_tide_info(date)
# for (tide in tides) {
#   cat(" -", tide$t, ":", tide$type, "tide\n")
# }
#
# cat("\n🌦️ Weather Info:\n")
# weather <- get_weather_info(location, WEATHER_API_KEY)
# cat(" - Condition:", weather$condition, "\n")
# cat(" - Temperature:", weather$temp, "°F\n")
# cat(" - Humidity:", weather$humidity, "%\n")
# cat(" - Wind Speed:", weather$wind, "mph\n")

first_light_hms <- ymd_hms(first_light, tz = "America/New_York")

tides <- get_tide_info_tibble(date) %>%
  mutate(dttm = ymd_hm(t, tz = "America/New_York"),
         height = as.numeric(v))

tides_hilo <- get_tide_hilo_tibble(date) %>%
  mutate(dttm = ymd_hm(t, tz = "America/New_York"),
         height = as.numeric(v))

weather_forecast <- get_weather_forecast_info(location, WEATHER_API_KEY) %>%
  mutate(dttm = ymd_hm(time, tz = "America/New_York")) %>%
  relocate(dttm, .after = time) %>%
  filter(dttm == ceiling_date(first_light, unit = "hour"))

annotation_text <- tribble(~label_text,
                           paste0("First Light @ ",
                                  format(first_light, "%H:%M"), "<br>",
                                  "Temp: ", weather_forecast$temp_f, "°F<br>",
                                  "Wind: ", weather_forecast$wind_mph," mph ",
                                  weather_forecast$wind_dir, "<br>",
                                  "Sky: ", weather_forecast$condition.text,"<br>",
                                  "Precip %: ", weather_forecast$chance_of_rain)
)

p1 <- tides %>%
  ggplot(aes(x = dttm)) +
  geom_hline(yintercept = 0,
             color = "grey80") +
  geom_path(aes(y = height),
            linetype = "dashed",
            color = "#b4d5fe",
            linewidth = 1.5) +
  geom_point(data = tides_hilo,
             aes(y = height),
             size = 2) +
  annotate("rect",
           xmin = first_light_hms, xmax = first_light_hms + minutes(90),
            ymin = -Inf, ymax = Inf,
            fill = "darkorange",
            alpha = 0.4) +
  geom_richtext(data = annotation_text,
                x = first_light_hms + minutes(100),
                y = max(tides$height)*1.2,
                fill = NA, label.colour = NA,
                label = annotation_text$label_text,
                # color = "red",
                family = "Roboto Condensed",
                hjust = 0, vjust = 0.5) +
  geom_image(data = tibble(dttm = first_light_hms + minutes(45),
                           height = 0.5),
             aes(image = "walk_icon.png",
                 x = dttm, y = height),
             size = 0.15) +
  scale_x_datetime(date_labels = "%H:%M<br>%d %b",
                   breaks = tides_hilo$dttm,
                   limits = c(first_light_hms - hours(6),
                              first_light_hms + hours(10))) +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.25))) +
  theme_minimal() +
  labs(x = "", y = "Feet Above/Below MSL",
       title = "**Morning Walk Tide and Weather**",
       subtitle = "Jacksonville Beach, FL") +
  theme(text = element_text(family = "Roboto Condensed"),
        plot.title = element_markdown(size = 14),
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank(),
        axis.text = element_markdown())

ggsave("beach_walk.png", plot = p1, width = 7, height = 5, bg = "#fffff3")
