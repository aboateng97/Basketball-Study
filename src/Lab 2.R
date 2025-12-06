getwd()
setwd("~/Downloads")
# Load the CSV file
games <- read.csv("/Users/afrahboateng/Downloads/Games.csv")

# Check that it loaded
head(games)

library(dplyr)
library(tidygeocoder)
library(geosphere)
library(ggplot2)

games <- games %>%
  mutate(point_diff = awayScore - homeScore)

#There are 45 unique hometeamCity
unique(games$hometeamCity)

#There are 49 unique awayteamCity
unique(games$awayteamCity)

# Find the total Number of Unique Cities in the Dataset
unique_cities <- games %>%
  select(hometeamCity, awayteamCity) %>%         
  pivot_longer(cols = everything(),              
               names_to = "type",                
               values_to = "city") %>% 
  distinct(city)                                 

# View the unique cities
print(unique_cities)

# Count how many unique cities there are
num_unique_cities <- nrow(unique_cities)
print(num_unique_cities)

##There are 51 total unique cities (home and away)

# Clean city names by creating new city variable
city_lookup <- games %>%
  select(hometeamCity) %>% rename(city = hometeamCity) %>%
  bind_rows(games %>% select(awayteamCity) %>% rename(city = awayteamCity)) %>%
  distinct() %>%
  mutate(city_clean = case_when(
    city == "Los Angeles"            ~ "Los Angeles, California",
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
    city == "San Francisco"            ~ "San Francisco, California",
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

# Geocode to get lat/long
city_coords <- city_lookup %>%
  geocode(city_clean, method = "osm")   # or “iq” or “google” if you have API
# This will return lat & long columns

# Inspect:
head(city_coords)

#merge home team city coords to assign lat lon to each game
games2 <- games %>%
  left_join(city_coords %>% select(city, lat_home = lat, lon_home = long),
            by = c("hometeamCity" = "city")) %>%
  # Then merge away team city coords
  left_join(city_coords %>% select(city, lat_away = lat, lon_away = long),
            by = c("awayteamCity" = "city"))

#Count NAs in each column for games2
na_counts <- games2 %>%
  summarise(
    na_lat_home = sum(is.na(lat_home)),
    na_lon_home = sum(is.na(lon_home)),
    na_lat_away = sum(is.na(lat_away)),
    na_lon_away = sum(is.na(lon_away))
  )

print(na_counts)

#Find which rows have any NAs in those columns for games2
rows_with_na <- games2 %>%
  filter(
    is.na(lat_home) | is.na(lon_home) | is.na(lat_away) | is.na(lon_away)
  ) %>%
  select(hometeamCity, awayteamCity, lat_home, lon_home, lat_away, lon_away)


## 71 Rows with NA lat lon, all due to Tri-cities, which i don't think even exists anymore.
## We can ignore these; they will be dropped from the regression automatically

games3 <- games2 %>%
  mutate(
    dist_meters = distHaversine(cbind(lon_home, lat_home),
                                cbind(lon_away, lat_away)),
    dist_miles = dist_meters * 0.000621371,
    year = as.numeric(substr(gameDate, 1, 4)),
    winning_team = case_when(
      winner == awayteamId ~ "away",
      winner == hometeamId ~ "home",
      TRUE ~ NA_character_
    ),
    
    # ------------ Longitude-based zones ----------
    home_zone = case_when(
      lon_home < -105 ~ "West",
      lon_home > -90  ~ "East",
      TRUE            ~ "Central"   # in between → Central
    ),
    away_zone = case_when(
      lon_away < -105 ~ "West",
      lon_away > -90  ~ "East",
      TRUE            ~ "Central"
    )
  )


games4 <- games3 %>%
  filter(gameType == "Regular Season", !is.na(dist_miles)) %>%
  mutate(
    away_travel = case_when(
      away_zone == "East"    & home_zone == "West"    ~ "East to West",
      away_zone == "West"    & home_zone == "East"    ~ "West to East",
      away_zone == "East"    & home_zone == "Central" ~ "East to Central",
      away_zone == "Central" & home_zone == "East"    ~ "Central to East",
      away_zone == "Central" & home_zone == "West"    ~ "Central to West",
      away_zone == "West"    & home_zone == "Central" ~ "West to Central",
      away_zone == home_zone                         ~ "In-Zone",
      TRUE                                          ~ "Other/International"
    ),
    away_travel_high = factor(
      case_when(
        away_travel == "East to West" ~ "East to West",
        away_travel == "West to East" ~ "West to East",
        TRUE                          ~ "Neither"
      ),
      levels = c("Neither", "West to East", "East to West")
    )
  )


home_win_pct <- games4 %>%
  group_by(year) %>%
  summarise(
    home_win_percent = mean(winning_team == "home") * 100
  )

ggplot(home_win_pct, aes(x = year, y = home_win_percent)) +
  geom_point(size = 2) +  # only the dots
  labs(
    title = "Home Win Percentage by Year",
    x = "Year",
    y = "Home Win Percentage (%)"
  ) +
  theme_minimal()

ggplot(home_win_pct, aes(x = year, y = home_win_percent)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(
    title = "Home Win Percentage by Year",
    x = "Year",
    y = "Home Win Percentage (%)"
  ) +
  scale_x_continuous(breaks = seq(min(home_win_pct$year),
                                  max(home_win_pct$year),
                                  by = 5)) +
  theme_minimal()

ggplot(games3, aes(x = dist_miles, y = point_diff)) +
  geom_point(alpha = 0.5, color = "blue", size = 0.5) +  # smaller, semi-transparent points
  labs(
    x = "Distance between teams (miles)",
    y = "Point Differential (Away - Home)",
    title = "Scatterplot of Point Differential vs. Distance Traveled"
  ) +
  theme_minimal()

# Simple linear regression
lm_model <- lm(point_diff ~ dist_miles + year, data = games4)

# View the summary of the model
summary(lm_model)

# Multiple linear regression
lm_model2 <- lm(point_diff ~ dist_miles + away_travel_high + year, data = games4)

# View the summary of the model
summary(lm_model2)

lm_model3 <- lm(point_diff ~ attendance, data = games3)

# View the summary of the model
summary(lm_model3)

ggplot(games4, aes(x = year, y = point_diff)) +
  geom_point(alpha = 0.5, size = 0.5, color = "blue") +
  geom_smooth(method = "lm", color = "red", se = TRUE) +
  labs(
    x = "Year",
    y = "Point Differential (Away - Home)",
    title = "Scatterplot of Point Differential vs. Year"
  ) +
  theme_minimal()

ggplot(games3, aes(x = attendance, y = point_diff)) +
  geom_point(alpha = 0.5, size = 0.5, color = "darkgreen") +  # tiny, semi-transparent points
  geom_smooth(method = "lm", color = "red", se = TRUE) +      # regression line with confidence interval
  labs(
    x = "Attendance",
    y = "Point Differential (Away - Home)",
    title = "Scatterplot of Point Differential vs. Attendance"
  ) +
  theme_minimal()


