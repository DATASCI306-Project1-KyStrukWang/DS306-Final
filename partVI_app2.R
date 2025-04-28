library(shiny)
library(tidyverse)
library(DT)

# Data for app
principals_joined <- title_principals %>%
  inner_join(name_basics, by = "nconst") %>%
  inner_join(title_basics, by = "tconst") %>%
  inner_join(title_ratings, by = "tconst")

ui2 <- fluidPage(
  titlePanel("IMDb Principals Explorer"),
  sidebarLayout(
    sidebarPanel(
      selectInput("category_select", "Select Categories:",
                  choices = sort(unique(title_principals$category)),
                  selected = "actor", multiple = TRUE)
    ),
    mainPanel(
      DT::DTOutput("principalTable")   # <-- fixed
    )
  )
)

server2 <- function(input, output) {
  
  filtered_principals <- reactive({
    principals_joined %>%
      filter(category %in% input$category_select)
  })
  
  output$principalTable <- DT::renderDT({
    filtered_principals() %>%
      select(primaryName, category, primaryTitle, startYear, averageRating, numVotes) %>%
      arrange(desc(averageRating))
  })
}

shinyApp(ui = ui2, server = server2)
