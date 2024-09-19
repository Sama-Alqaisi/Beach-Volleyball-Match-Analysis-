
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

# Interactive visuals -----------------------------------------------------

# plot 1 ------------------------------------------------------------------

# Calculate number of matches by tournament year and gender
matches_by_year_gender <- vb_matches %>%
  group_by(year, gender) %>%
  summarise(num_matches = n()) %>%
  ungroup()
plot <- plot_ly(matches_by_year_gender, x = ~year, y = ~num_matches, color = ~gender, type = 'scatter', mode = 'lines+markers',
                colors = c('blue', 'green')) %>%
  layout(title = 'Number of Matches by Tournament Year and Gender',
         xaxis = list(title = 'Tournament Year'),
         yaxis = list(title = 'Number of Matches'),
         hoverlabel = list(namelength = 0)) %>%
  add_trace(text = ~paste('Gender: ', gender, '<br>Year: ', year, '<br>Number of Matches: ', num_matches),
            hoverinfo = 'text')
plot

# plot 2 ------------------------------------------------------------------
vb_matches$w_rank <- as.numeric(as.character(vb_matches$w_rank))
categorize_performance <- function(rank) {
  ifelse(rank <= 10, 'Top Performers',
         ifelse(rank <= 30, 'Mid-tier', 'Lower-ranked'))
}
vb_matches$performance_category <- categorize_performance(vb_matches$w_rank)
plot <- plot_ly(vb_matches, x = ~w_p1_age, y = ~w_rank, color = ~performance_category,
                type = 'scatter', mode = 'markers',
                text = ~paste('Player: ', w_player1, '<br>Age: ', w_p1_age, '<br>Rank: ', w_rank),
                hoverinfo = 'text') %>%
  layout(title = 'Player Rank vs. Age',
         xaxis = list(title = 'Player Age'),
         yaxis = list(title = 'Player Rank'))
plot

# plot 3 ------------------------------------------------------------------
vb_matches$w_rank <- as.numeric(as.character(vb_matches$w_rank))
categorize_performance <- function(rank) {
  ifelse(rank <= 10, 'Top Performers',
         ifelse(rank <= 30, 'Mid-tier', 'Lower-ranked'))
}
vb_matches$performance_category <- categorize_performance(vb_matches$w_rank)
plot <- plot_ly(vb_matches, x = ~w_p1_tot_attacks, y = ~w_rank, color = ~performance_category,
                type = 'scatter', mode = 'markers', marker = list(size = 10),
                text = ~paste('Player: ', w_player1, '<br>Total Attacks: ', w_p1_tot_attacks, '<br>Player Rank: ', w_rank),
                hoverinfo = 'text') %>%
  layout(title = 'Player Rank vs. Total Attacks',
         xaxis = list(title = 'Total Attacks'),
         yaxis = list(title = 'Player Rank'))
plot



