# Load libraries
library(dplyr)
library(tidygeocoder)
library(geosphere)

# Load raw data
games <- read.csv("data/raw/Games.csv")

# Calculate point differential
games <- games %>%
  mutate(point_diff = awayScore - homeScore)

# Clean city names
city_lookup <- games %>%
  select(hometeamCity) %>% rename(city = hometeamCity) %>%
  bind_rows(games %>% select(awayteamCity) %>% rename(city = awayteamCity)) %>%
  distinct() %>%
  mutate(city_clean = case_when(
    city == "Los Angeles"             ~ "Los Angeles, California",
    city == "LA"                      ~ "Los Angeles, California",
    city == "Phoenix"                 ~ "Phoenix, Arizona",
    city == "New York"                ~ "New York, New York",
    city == "Cleveland"               ~ "Cleveland, Ohio",
    city == "Toronto"                 ~ "Toronto, Ontario, Canada",
    city == "Charlotte"               ~ "Charlotte, North Carolina",
    city == "Brooklyn"                ~ "Brooklyn, New York",
    city == "Oklahoma City"           ~ "Oklahoma City, Oklahoma",
    city == "Detroit"                 ~ "Detroit, Michigan",
    city == "Boston"                  ~ "Boston, Massachusetts",
    city == "Indiana"                 ~ "Indianapolis, Indiana",
    city == "Washington"              ~ "Washington, D.C.",
    city == "Milwaukee"               ~ "Milwaukee, Wisconsin",
    city == "Portland"                ~ "Portland, Oregon",
    city == "Memphis"                 ~ "Memphis, Tennessee",
    city == "Chicago"                 ~ "Chicago, Illinois",
    city == "Philadelphia"            ~ "Philadelphia, Pennsylvania",
    city == "San Antonio"             ~ "San Antonio, Texas",
    city == "Minnesota"               ~ "Minneapolis, Minnesota",
    city == "Denver"                  ~ "Denver, Colorado",
    city == "Utah"                    ~ "Salt Lake City, Utah",
    city == "Dallas"                  ~ "Dallas, Texas",
    city == "Golden State"            ~ "San Francisco, California",
    city == "Miami"                   ~ "Miami, Florida",
    city == "Houston"                 ~ "Houston, Texas",
    city == "New Orleans"             ~ "New Orleans, Louisiana",
    city == "Sacramento"              ~ "Sacramento, California",
    city == "Atlanta"                 ~ "Atlanta, Georgia",
    city == "Orlando"                 ~ "Orlando, Florida",
    city == "New Jersey"              ~ "New Jersey, USA",
    city == "Seattle"                 ~ "Seattle, Washington",
    city == "Vancouver"               ~ "Vancouver, British Columbia, Canada",
    city == "Kansas City"             ~ "Kansas City, Missouri",
    city == "San Diego"               ~ "San Diego, California",
    city == "Buffalo"                 ~ "Buffalo, New York",
    city == "Kansas City-Omaha"       ~ "Kansas City / Omaha, USA",
    city == "Capital"                 ~ "Washington, D.C.",
    city == "Baltimore"               ~ "Baltimore, Maryland",
    city == "Cincinnati"              ~ "Cincinnati, Ohio",
    city == "San Francisco"           ~ "San Francisco, California",
    city == "St. Louis"               ~ "St. Louis, Missouri",
    city == "Syracuse"                ~ "Syracuse, New York",
    city == "Minneapolis"             ~ "Minneapolis, Minnesota",
    city == "Ft. Wayne Zollner"       ~ "Fort Wayne, Indiana",
    city == "Rochester"               ~ "Rochester, New York",
    city == "Tri-Cities"              ~ "Tri-Cities, Washington / Oregon",
    city == "Guangzhou"               ~ "Guangzhou, Guangdong, China",
    city == "South East Melbourne"    ~ "Melbourne, Victoria, Australia",
    city == "Hapoel"                  ~ "Tel Aviv, Israel",
    TRUE                              ~ city
  ))

# Geocode cities
city_coords <- city_lookup %>%
  geocode(city_clean, method = "osm")

# Merge coordinates with games
games2 <- games %>%
  left_join(city_coords %>% select(city, lat_home = lat, lon_home = long),
            by = c("hometeamCity" = "city")) %>%
  left_join(city_coords %>% select(city, lat_away = lat, lon_away = long),
            by = c("awayteamCity" = "city"))

# Compute distances, zones, travel direction, and year
games3 <- games2 %>%
  mutate(
    dist_meters = distHaversine(cbind(lon_home, lat_home), cbind(lon_away, lat_away)),
    dist_miles = dist_meters * 0.000621371,
    year = as.numeric(substr(gameDate, 1, 4)),
    winning_team = case_when(
      winner == awayteamId ~ "away",
      winner == hometeamId ~ "home",
      TRUE ~ NA_character_
    ),
    home_zone = case_when(
      lon_home < -105 ~ "West",
      lon_home > -90 ~ "East",
      TRUE ~ "Central"
    ),
    away_zone = case_when(
      lon_away < -105 ~ "West",
      lon_away > -90 ~ "East",
      TRUE ~ "Central"
    ),
    away_travel = case_when(
      away_zone == "East" & home_zone == "West" ~ "East to West",
      away_zone == "West" & home_zone == "East" ~ "West to East",
      away_zone == home_zone ~ "In-Zone",
      TRUE ~ "Other/International"
    ),
    away_travel_high = factor(case_when(
      away_travel == "East to West" ~ "East to West",
      away_travel == "West to East" ~ "West to East",
      TRUE ~ "Neither"
    ), levels = c("Neither", "West to East", "East to West"))
  )

# Filter for regular season games
games_clean <- games3 %>%
  filter(gameType == "Regular Season", !is.na(dist_miles))

# Splitting into EDA (30%) and modeling (70%) data sets
set.seed(123)   # ensures reproducibility
n <- nrow(games3)
eda_index <- sample(1:n, size = floor(0.30 * n), replace = FALSE)
games_clean_eda   <- games3[eda_index, ]
games_clean_model <- games3[-eda_index, ]

# Save cleaned data set
saveRDS(games_clean_eda, "data/processed/games_clean_eda.rds")
saveRDS(games_clean_model, "data/processed/games_clean_model.rds")
