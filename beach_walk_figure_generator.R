library(httr)
library(jsonlite)
library(lubridate)
library(tidyverse)
library(ggtext)
library(ggimage)

library(sysfonts)
font_add_google("Roboto Condensed")

color_text <- function (text_string, color = "#AD122A"){
  sprintf("<span style='color: %s;'>%s</span>", color, text_string)
}

# Coordinates for Jacksonville Beach, FL
lat <- 30.2947
lon <- -81.3931
location <- "32250"

# Date
date <- Sys.Date()

# Get first light time (civil twilight begin)
get_first_light <- function(lat, lon, date) {
  url <- paste0("https://api.sunrise-sunset.org/json?lat=", lat,
                "&lng=", lon, "&date=", date, "&formatted=0")
  res <- GET(url)
  data <- content(res, "parsed")
  first_light <- data$results$nautical_twilight_begin
  return(with_tz(ymd_hms(first_light), tzone = "America/New_York"))
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

get_nws_forecast_info <- function(){
  # Step 1: Get gridpoint data for Jacksonville Beach, FL
  point_url <- "https://api.weather.gov/points/30.2947,-81.3931"

  # Include a User-Agent header as required by NWS
  response1 <- GET(point_url, user_agent("your-email@example.com"))

  # Parse JSON response
  point_data <- content(response1, as = "parsed", type = "application/json")

  # Extract forecast URL
  forecast_url <- point_data$properties$forecastHourly

  # Step 2: Get the forecast data
  response2 <- GET(forecast_url, user_agent("your-email@example.com"))

  # Parse the forecast data
  forecast_data <- content(response2, as = "parsed", type = "application/json")
  forecast_text <- content(response2, "text")
  forecast_flattened <- fromJSON(forecast_text, flatten = TRUE)
  forecast_data_tibble <- as_tibble(forecast_flattened$properties$periods) %>%
    mutate(startTime = ymd_hms(startTime, tz = "America/New_York"),
           endTime = ymd_hms(endTime, tz = "America/New_York"))
}

##################################
# Main
first_light <- get_first_light(lat, lon, date)

first_light_hms <- ymd_hms(first_light, tz = "America/New_York")

tides <- get_tide_info_tibble(date) %>%
  mutate(dttm = ymd_hm(t, tz = "America/New_York"),
         height = as.numeric(v))

tides_hilo <- get_tide_hilo_tibble(date) %>%
  mutate(dttm = ymd_hm(t, tz = "America/New_York"),
         height = as.numeric(v))

weather_forecast <- get_nws_forecast_info() %>%
  filter(startTime <= first_light_hms,
         endTime > first_light_hms)

annotation_text <- tribble(~label_text,
                           paste0("First Light @ ",
                                  format(first_light, "%H:%M"), "<br>",
                                  "Temp: ", weather_forecast$temperature, "&deg;F<br>",
                                  "Wind: ", weather_forecast$windSpeed, " ",
                                  weather_forecast$windDirection, "<br>",
                                  "Sky: ", weather_forecast$shortForecast,"<br>",
                                  "Precip %: ", weather_forecast$probabilityOfPrecipitation.value)
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
                 x = dttm, y = max(tides$height)*1.2),
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

annotation_text_dark <- tribble(~label_text,
                           paste0(color_text( paste0("First Light @ ",
                                                     format(first_light, "%H:%M")), "#FF8C00"),
                                  "<br>",
                                  "Temp: ", weather_forecast$temperature, "&deg;F<br>",
                                  "Wind: ", weather_forecast$windSpeed, " ",
                                  weather_forecast$windDirection, "<br>",
                                  "Sky: ", weather_forecast$shortForecast,"<br>",
                                  "Precip %: ", weather_forecast$probabilityOfPrecipitation.value)
)

p1_dark <- tides %>%
  ggplot(aes(x = dttm)) +
  scale_x_datetime(date_labels = "%H:%M<br>%d %b",
                   breaks = tides_hilo$dttm,
                   limits = c(first_light_hms - hours(6),
                              first_light_hms + hours(10))) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.3))) +
  theme_minimal() +
  labs(x = "", y = "Feet Above/Below MSL") +
  theme(text = element_text(family = "Roboto Condensed",
                            color = "white"),
        plot.background = element_rect(fill = "black"),
        plot.title = element_markdown(size = 14),
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(color = "grey20"),
        axis.text = element_markdown(color = "white")) +
  geom_hline(yintercept = 0,
             color = "grey40") +
  geom_path(aes(y = height),
            linetype = "dashed",
            color = "#3869a6",
            # alpha = 0.4,
            linewidth = 1.5) +
  geom_point(data = tides_hilo,
             aes(y = height),
             color = "white",
             size = 2) +
  annotate("rect",
           xmin = first_light_hms,
           xmax = first_light_hms + minutes(90),
           ymin = -Inf, ymax = Inf,
           fill = "gray70",
           alpha = 0.3) +
  geom_richtext(data = annotation_text_dark,
                x = first_light_hms + minutes(120),
                y = max(tides$height)*1.6,
                fill = NA, label.colour = NA,
                label = annotation_text_dark$label_text,
                color = "white",
                family = "Roboto Condensed",
                size = 6,
                hjust = 0, vjust = 0.5) +
  geom_image(data = tibble(dttm = first_light_hms + minutes(45),
                           height = 0.5),
             aes(image = "walk_icon_white.png",
                 x = dttm, y = max(tides$height)*1.4),
             size = 0.12)

ggsave("beach_walk_dark.png", plot = p1_dark, width = 5, height = 5)
