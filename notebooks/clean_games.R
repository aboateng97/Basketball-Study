# Load libraries
library(dplyr)
library(tidygeocoder)
library(geosphere)

# Load raw data
games <- read.csv("../data/raw/Games.csv")

# Calculate point differential
games <- games %>%
  mutate(point_diff = awayScore - homeScore)

# Clean city names
city_lookup <- games %>%
  select(hometeamCity) %>% rename(city = hometeamCity) %>%
  bind_rows(games %>% select(awayteamCity) %>% rename(city = awayteamCity)) %>%
  distinct() %>%
  mutate(city_clean = case_when(
    city == "Los Angeles" ~ "Los Angeles, California",
    city == "LA" ~ "Los Angeles, California",
    # ... (all other mappings)
    TRUE ~ city
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

# Save cleaned dataset
saveRDS(games_clean, "../data/processed/games_clean.rds")
