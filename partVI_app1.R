library(shiny)
library(tidyverse)
library(plotly)

# Data for app
genre_ratings <- titles_genre_long %>%
  inner_join(title_ratings, by = "tconst")

ui1 <- fluidPage(
  titlePanel("IMDb Ratings Explorer"),
  sidebarLayout(
    sidebarPanel(
      selectInput("selected_genres", "Select Genres:",
                  choices = sort(unique(genre_ratings$genres)),
                  selected = "Drama", multiple = TRUE),
      sliderInput("year_range", "Select Release Year Range:",
                  min = min(genre_ratings$startYear, na.rm = TRUE),
                  max = max(genre_ratings$startYear, na.rm = TRUE),
                  value = c(1980, 2020)),
      radioButtons("rating_stat", "Statistic to Plot:",
                   choices = c("Average" = "mean", "Median" = "median"),
                   selected = "mean")
    ),
    mainPanel(
      plotlyOutput("ratingPlot"),
      dataTableOutput("titleTable")
    )
  )
)

server1 <- function(input, output) {
  
  filtered_data <- reactive({
    genre_ratings %>%
      filter(genres %in% input$selected_genres,
             startYear >= input$year_range[1],
             startYear <= input$year_range[2])
  })
  
  output$ratingPlot <- renderPlotly({
    stat_fun <- if (input$rating_stat == "mean") mean else median
    
    summary_data <- filtered_data() %>%
      group_by(startYear, genres) %>%
      summarise(stat_rating = stat_fun(averageRating, na.rm = TRUE),
                .groups = "drop")
    
    p <- ggplot(summary_data, aes(x = startYear, y = stat_rating, color = genres)) +
      geom_line(linewidth = 1) +
      labs(title = paste(input$rating_stat, "IMDb Ratings by Year"),
           x = "Year", y = paste(input$rating_stat, "Rating")) +
      theme_minimal()
    
    ggplotly(p)
  })
  
  output$titleTable <- renderDataTable({
    filtered_data() %>%
      select(primaryTitle = title_basics$title, startYear, genres, averageRating, numVotes)
  })
  
}

shinyApp(ui = ui1, server = server1)
