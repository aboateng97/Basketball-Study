# Load libraries
library(tidyverse)
library(geosphere)
library(cowplot)

# Load cleaned dataset
games <- readRDS("data/processed/games_clean_eda.rds")
head(games)

# Summary statistics
cat("Number of games:", nrow(games), "\n")
cat("Number of columns:", ncol(games), "\n")
cat("Average point differential (Away - Home):", round(mean(games$point_diff),2), "\n")
cat("Average attendance:", round(mean(games$attendance, na.rm = TRUE)), "\n")
cat("Average distance traveled:", round(mean(games$dist_miles, na.rm = TRUE),2), "miles\n")
cat("Maximum distance traveled:", round(max(games$dist_miles, na.rm = TRUE),2), "miles\n")
cat("Minimum distance traveled:", round(min(games$dist_miles, na.rm = TRUE),2), "miles\n")
cat("Median distance traveled:", round(median(games$dist_miles, na.rm = TRUE),2), "miles\n")

# Histogram of point differentials
hist_points <- ggplot(games, aes(x = point_diff)) +
  geom_histogram(bins = 50, fill = "darkblue", alpha = 0.7) +
  geom_vline(aes(xintercept = mean(point_diff, na.rm = TRUE)), color = "darkblue", linetype = "dashed") +
  labs(title = "Distribution of Point Differentials",
       x = "Point Differential (Away - Home)",
       y = "Frequency",
       subtitle = paste("Mean point differential:", round(mean(games$point_diff),2))) +
  theme_minimal()

# Histogram of travel distances
hist_distance <- ggplot(games, aes(x = dist_miles)) +
  geom_histogram(bins = 50, fill = "darkblue", alpha = 0.7) +
  geom_vline(aes(xintercept = mean(dist_miles, na.rm = TRUE)), color = "darkblue", linetype = "dashed") +
  labs(title = "Distribution of Travel Distances",
       x = "Distance (miles)",
       y = "Frequency",
       subtitle = paste("Mean distance:", round(mean(games$dist_miles, na.rm = TRUE),2), "miles")) +
  theme_minimal()

# Combine the two histograms side by side
plot_grid(hist_points, hist_distance, labels = c("A", "B"))

# Number of games per year
games_per_year <- games %>%
  group_by(year) %>%
  summarise(num_games = n())

games_per_year_plot <- ggplot(games_per_year, aes(x = year, y = num_games)) +
  geom_line(color = "darkblue", size = 1) +
  geom_point(color = "darkblue", size = 2) +
  labs(title = "Number of Games per Season Over Time",
       x = "Year",
       y = "Number of Games") +
  theme_minimal()

# Longest travel trips
top_trips <- games %>%
  arrange(desc(dist_miles)) %>%
  select(hometeamCity, awayteamCity, dist_miles, year) %>%
  slice(1:10)

cat("Top 10 Longest Trips in the Dataset:\n")
print(top_trips)

long_trips_plot <- ggplot(top_trips, aes(x = reorder(paste(awayteamCity, "->", hometeamCity), dist_miles), y = dist_miles)) +
  geom_col(fill = "darkblue") +
  coord_flip() +
  labs(title = "Longest Travel Trips (Top 10)",
       x = "Matchup (Away -> Home)",
       y = "Distance (miles)") +
  theme_minimal()

# Combine games per year and top trips
plot_grid(games_per_year_plot, long_trips_plot, labels = c("C", "D"))

# Scatterplots of point differential vs key variables
p1 <- ggplot(games, aes(x = dist_miles, y = point_diff)) +
  geom_point(alpha = 0.3, color = "darkblue") +
  geom_smooth(method = "lm", color = "darkblue") +
  labs(title = "Point Differential vs Distance Travelled",
       x = "Distance (miles)", y = "Point Differential (Away - Home)") +
  theme_minimal()

p2 <- ggplot(games, aes(x = attendance, y = point_diff)) +
  geom_point(alpha = 0.3, color = "darkblue") +
  geom_smooth(method = "lm", color = "darkblue") +
  labs(title = "Point Differential vs Attendance",
       x = "Attendance", y = "Point Differential (Away - Home)") +
  theme_minimal()

p3 <- ggplot(games, aes(x = away_travel_high, y = point_diff)) +
  geom_point(alpha = 0.5, size = 0.5, color = "darkblue") +
  geom_smooth(method = "lm", color = "darkblue") +
  labs(title = "Point Differential vs Away Travel Direction",
       x = "Away Travel", y = "Point Differential (Away - Home)") +
  theme_minimal()

plot_grid(p1, p2, p3, ncol = 1, align = "v")

# Additional summaries
cat("Highest scoring game:", max(games$awayScore + games$homeScore, na.rm = TRUE), "\n")
cat("Lowest scoring game:", min(games$awayScore + games$homeScore, na.rm = TRUE), "\n")
