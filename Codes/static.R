# Import Data -------------------------------------------------------------
vb_matches <- read.csv(file.choose(), stringsAsFactors=TRUE)


# Libraries ---------------------------------------------------------------
install.packages("dplyr")
install.packages("ggplot2")
install.packages("plotly")
install.packages("shiny")


library(dplyr)
library(ggplot2)
library(plotly)
library(shiny)

# Static Visuals ----------------------------------------------------------

# plot 1 ------------------------------------------------------------------

# Extract player ages from the data set
player_age <- c(vb_matches$w_p1_age, vb_matches$w_p2_age, vb_matches$l_p1_age, vb_matches$l_p2_age)

# Create a histogram of player ages
ggplot() +
  geom_histogram(aes(x = player_age), bins = 20, fill = "#008080", color = "black") +
  labs(title = "Player Age Distribution",
       x = "Age",
       y = "Frequency") +
  theme_minimal()

# plot 2 ------------------------------------------------------------------

# Combine heights of all players into a single vector
all_heights <- c(vb_matches$w_p1_hgt, vb_matches$w_p2_hgt, vb_matches$l_p1_hgt, vb_matches$l_p2_hgt)

# Create a dataframe for plotting
height_df <- data.frame(Height = all_heights)

# Plot histogram
ggplot(height_df, aes(x = Height)) +
  geom_histogram(binwidth = 1, fill = "#008080", color = "black") +
  labs(title = "Height Distribution of Players",
       x = "Height (in inches)",
       y = "Frequency") +
  theme_minimal()

# plot 3 ------------------------------------------------------------------

# Calculate the total blocks for each player
player_blocks = vb_matches %>%
  select(w_player1, w_p1_tot_blocks) %>%
  group_by(w_player1) %>%
  summarise(Total_Blocks = sum(w_p1_tot_blocks)) %>%
  rename(Player = w_player1)

# Select the top N players based on total blocks
N = 10
top_players_blocks <- player_blocks %>%
  top_n(N, Total_Blocks)

# Plotting using ggplot2
ggplot(top_players_blocks, aes(x = reorder(Player, Total_Blocks), y = Total_Blocks)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(x = "Player", y = "Total Blocks", title = paste("Top", N, "Players by Total Blocks")) +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 10)) +
  coord_flip()

# plot 4 ------------------------------------------------------------------

# Calculate the total kills for each player
player_performance = vb_matches %>%
  group_by(w_player1) %>%
  summarise(Total_Kills = sum(w_p1_tot_kills)) %>%
  rename(Player = w_player1)

# Select the top N players based on total kills
N = 10
top_players = player_performance %>%
  top_n(N, Total_Kills)

# Plotting using ggplot2
ggplot(top_players, aes(x = reorder(Player, Total_Kills), y = Total_Kills)) +
  geom_bar(stat = "identity", fill = "lightgreen") +
  labs(x = "Player", y = "Total Kills", title = "Top 10 Players by Total Kills") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 10)) +
  coord_flip()

# plot 5 ------------------------------------------------------------------

# Plot total matches played by year
ggplot(vb_matches, aes(x = factor(year))) +
  geom_bar(fill = "#008080") +
  labs(title = "Total Matches Played by Year",
       x = "Year",
       y = "Total Matches Played") +
  theme_minimal()

# plot 6 ------------------------------------------------------------------

# Define the top_n value
top_n <- 2

# Calculate the total aces for each player and select the top players
top_players_aces <- vb_matches %>%
  group_by(w_player1) %>%
  summarise(total_aces = sum(w_p1_tot_aces), .groups = 'drop') %>%
  top_n(top_n, total_aces) %>%
  pull(w_player1)

# Filter the dataset to include only the top players
top_players_data <- vb_matches %>%
  filter(w_player1 %in% top_players_aces)

# Aggregate the performance metrics over time (years)
performance_over_time <- top_players_data %>%
  group_by(w_player1, year) %>%
  summarise(total_aces = sum(w_p1_tot_aces), .groups = 'drop')

# Plot the performance over time using line plots
ggplot(performance_over_time, aes(x = year, y = total_aces, color = w_player1, group = w_player1)) +
  geom_line() +
  geom_point() +
  labs(title = 'Top 2 Players Performance Over Time (Total Aces)',
       x = 'Year',
       y = 'Total Aces',
       color = 'Player') +
  theme_minimal() +
  expand_limits(y = 0) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.2))) # Ensures the y-axis starts at 0 and adds a small padding on top

# plot 7 ------------------------------------------------------------------

# Calculate average performance metrics for each height
avg_performance_by_height <- vb_matches %>%
  group_by(w_p1_hgt) %>%
  summarise(avg_total_kills = mean(w_p1_tot_kills),
            avg_total_blocks = mean(w_p1_tot_blocks))

# Plot average total kills vs. player height
p1 <- ggplot(avg_performance_by_height, aes(x = w_p1_hgt, y = avg_total_kills)) +
  geom_line(color = "skyblue", size = 1) +
  geom_point(color = "skyblue", size = 3) +
  labs(title = "Average Total Kills vs. Player Height",
       x = "Player Height (inches)",
       y = "Average Total Kills") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_continuous(breaks = seq(min(avg_performance_by_height$w_p1_hgt), max(avg_performance_by_height$w_p1_hgt), by = 1)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) # Remove gridlines
print(p1)

# plot 8 ------------------------------------------------------------------

# Plot average total blocks vs. player height
p2 <- ggplot(avg_performance_by_height, aes(x = w_p1_hgt, y = avg_total_blocks)) +
  geom_line(color = "lightgreen", size = 1) +
  geom_point(color = "lightgreen", size = 3) +
  labs(title = "Average Total Blocks vs. Player Height",
       x = "Player Height (inches)",
       y = "Average Total Blocks") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_continuous(breaks = seq(min(avg_performance_by_height$w_p1_hgt), max(avg_performance_by_height$w_p1_hgt), by = 1)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) # Remove gridlines

# Print the plots
print(p2)

# plot 9 ------------------------------------------------------------------

# Group by winner and loser countries and calculate win/loss counts
winner_counts <- vb_matches %>%
  group_by(w_p1_country) %>%
  summarise(win_count = n()) %>%
  rename(country = w_p1_country)

loser_counts <- vb_matches %>%
  group_by(l_p1_country) %>%
  summarise(loss_count = n()) %>%
  rename(country = l_p1_country)

# Merge the win and loss counts
country_stats <- full_join(winner_counts, loser_counts, by = "country") %>%
  mutate(win_count = ifelse(is.na(win_count), 0, win_count),
         loss_count = ifelse(is.na(loss_count), 0, loss_count),
         total_matches = win_count + loss_count,
         winning_percentage = (win_count / total_matches) * 100)

# Sort by winning percentage and select the top 10 countries
top_10_countries <- country_stats %>%
  arrange(desc(winning_percentage)) %>%
  head(10)

# Plot the winning percentage by country
ggplot(top_10_countries, aes(x = winning_percentage, y = reorder(country, winning_percentage))) +
  geom_bar(stat = "identity", fill = "#008080" ) +  # Change fill color here
  labs(title = "Top 10 Countries by Winning Percentage",
       x = "Winning Percentage",
       y = "Country") +
  theme_minimal()

# plot 10 ------------------------------------------------------------------
# Load the required libraries if not already loaded
library(dplyr)
library(ggplot2)
library(tidyr)  # For data manipulation
library(tibble) # For rownames_to_column function

# Read the data
vb_matches <- read.csv("C:/Users/LENOVO-H/Desktop/data visualization/Final/cleaned_data (1).csv", stringsAsFactors=TRUE)

# Select relevant performance metrics for creating the heatmap
metrics <- c("w_p1_tot_attacks", "w_p1_tot_kills", "w_p1_tot_errors", 
             "w_p1_tot_hitpct", "w_p1_tot_aces", "w_p1_tot_serve_errors", 
             "w_p1_tot_blocks", "w_p1_tot_digs")

# Subset the data for selected metrics
performance_data <- vb_matches %>% 
  select(all_of(metrics))

# Calculate correlations between performance metrics
performance_correlation <- cor(performance_data)

# Convert the correlation matrix into a long format suitable for plotting
correlation_df <- as.data.frame(performance_correlation) %>%
  rownames_to_column(var = "Metric1") %>%
  gather(key = "Metric2", value = "Correlation", -Metric1)

# Plot heatmap
heatmap <- ggplot(correlation_df, aes(x = Metric1, y = Metric2, fill = Correlation)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "skyblue", mid = "white", high = "lightcoral", midpoint = 0,
                       limits = c(-1, 1), name = "Correlation") +
  theme_minimal() +
  labs(title = "Correlation Heatmap of Performance Metrics",
       x = "Performance Metrics",
       y = "Performance Metrics") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Print heatmap
print(heatmap)

# plot 11 ------------------------------------------------------------------

# Calculate total matches played and wins for each player
player_wins <- vb_matches %>%
  # Extract winners and losers with their heights
  select(w_player1, w_p1_hgt, l_player1, l_p1_hgt) %>%
  # Gather into a long format where each row is a player match result
  gather(key = "result", value = "player", w_player1, l_player1) %>%
  # Determine if the player was a winner (1) or a loser (0)
  mutate(winner = ifelse(result == "w_player1", 1, 0)) %>%
  # Combine player heights back into the same column
  gather(key = "height_type", value = "height", w_p1_hgt, l_p1_hgt) %>%
  # Keep only rows where the height matches the player type
  filter((result == "w_player1" & height_type == "w_p1_hgt") |
           (result == "l_player1" & height_type == "l_p1_hgt")) %>%
  # Group by player and summarize their total matches, total wins, and average height
  group_by(player) %>%
  summarise(
    total_matches = n(),
    total_wins = sum(winner),
    avg_height = mean(height, na.rm = TRUE)
  )

# Calculate the winning percentage
player_wins <- player_wins %>%
  mutate(winning_percentage = (total_wins / total_matches) * 100)

# Plot the relationship between player height and winning percentage
ggplot(player_wins, aes(x = avg_height, y = winning_percentage)) +
  geom_point(color = "#008080", size = 3, alpha = 0.7) +
  labs(
    title = "Relationship between Player Height and Winning Percentage",
    x = "Average Height (inches)",
    y = "Winning Percentage"
  ) +
  theme_minimal()



