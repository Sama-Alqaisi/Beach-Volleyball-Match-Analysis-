
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

# plot 4 ------------------------------------------------------------------

ui <- fluidPage(
  titlePanel("Player Statistics Over Time"),
  sidebarLayout(
    sidebarPanel(
      selectInput("player", "Select Player:", choices = unique(vb_matches$w_player1)),
      selectInput("statistic", "Select Statistic:", 
                  choices = c("Total Kills" = "w_p1_tot_kills", 
                              "Total Blocks" = "w_p1_tot_blocks", 
                              "Serve Errors" = "w_p1_tot_serve_errors", 
                              "Total Attacks" = "w_p1_tot_attacks"))
    ),
    mainPanel(
      plotlyOutput("linePlot")
    )
  )
)

server <- function(input, output) {
  output$linePlot <- renderPlotly({
    req(input$player, input$statistic)  
    
    player_data <- vb_matches %>%
      filter(w_player1 == input$player)
    
    line_data <- player_data %>%
      group_by(year) %>%
      summarise(stat_value = sum(get(input$statistic), na.rm = TRUE)) %>%
      ungroup()
    
    plot <- plot_ly(
      data = line_data,
      x = ~year,                
      y = ~stat_value,          
      type = 'scatter',
      mode = 'lines+markers',
      marker = list(size = 10),
      text = ~paste('Year: ', year, '<br>Value: ', stat_value),
      hoverinfo = 'text'
    ) %>%
      layout(title = paste("Trend of", input$statistic, "for", input$player, "Over Years"),
             xaxis = list(title = "Year"),
             yaxis = list(title = paste("Total", input$statistic)))
    
    plot
  })
}

shinyApp(ui = ui, server = server)


