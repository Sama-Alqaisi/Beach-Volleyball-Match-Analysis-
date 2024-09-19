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



player_digs <- vb_matches %>%
  select(w_player1, w_p1_tot_digs, year) %>%
  group_by(w_player1, year) %>%
  summarise(Total_Digs = sum(w_p1_tot_digs)) %>%
  rename(Player = w_player1)

top_players_digs <- player_digs %>%
  group_by(Player) %>%
  summarise(Total_Digs = sum(Total_Digs)) %>%
  top_n(5, Total_Digs)

ui <- fluidPage(
  titlePanel("Top Players by Total Digs"),
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput("player_checkbox", "Select Players:", choices = top_players_digs$Player)
    ),
    mainPanel(
      plotOutput("player_digs_plot")
    )
  )
)

server <- function(input, output) {
  output$player_digs_plot <- renderPlot({
    filtered_data <- player_digs %>%
      filter(Player %in% input$player_checkbox)
    
    ggplot(filtered_data, aes(x = year, y = Total_Digs, color = Player)) +
      geom_line() +
      labs(x = "Year", y = "Total Digs", title = "Top Players by Total Digs") +
      theme_minimal()
  })
}

shinyApp(ui = ui, server = server)
