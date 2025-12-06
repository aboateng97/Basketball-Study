install.packages("tidygeocoder")
install.packages("geosphere")
install.packages("fixest")


### Load packages
library(tidyverse)
library(lubridate)
library(tidygeocoder)
library(geosphere)
library(fixest)

### Load dataset
games <- read_csv("Games.csv")

### Keep only Regular Season games
games_clean <- games %>%
  filter(gameType == "Regular Season") %>%
  mutate(
    # Extract season from gameDateTimeEst
    gameDate = ymd_hms(gameDateTimeEst),
    season = year(gameDate),
    
    # Outcome variable
    point_diff = awayScore - homeScore
  )

### Geocode unique cities
all_cities <- games_clean %>%
  select(hometeamCity) %>%
  rename(city = hometeamCity) %>%
  bind_rows(games_clean %>% 
              select(awayteamCity) %>% 
              rename(city = awayteamCity)) %>%
  distinct(city)

# Geocode city coordinates
city_coords <- all_cities %>%
  geocode(city, method = "osm", lat = latitude, long = longitude)

### Merge coordinates back into main dataset
games_geo <- games_clean %>%
  left_join(city_coords, by = c("hometeamCity" = "city")) %>%
  rename(home_lat = latitude, home_long = longitude) %>%
  left_join(city_coords, by = c("awayteamCity" = "city")) %>%
  rename(away_lat = latitude, away_long = longitude)

### Compute distance (miles) using Haversine formula
games_geo <- games_geo %>%
  mutate(distance_miles =
           distHaversine(
             cbind(home_long, home_lat),
             cbind(away_long, away_lat)
           ) / 1609.34)   # meters → miles

### -------- RUN MODELS -------- ###

## Model 1: Simple regression
model1 <- lm(point_diff ~ distance_miles, data = games_geo)
summary(model1)

## Model 2: Add season fixed effects
model2 <- feols(
  point_diff ~ distance_miles | season, 
  data = games_geo
)
summary(model2)

## Model 3: Add home + away team fixed effects
model3 <- feols(
  point_diff ~ distance_miles | season + hometeamName + awayteamName,
  data = games_geo
)
summary(model3)

