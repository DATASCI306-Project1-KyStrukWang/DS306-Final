library(shiny)
library(dplyr)
library(stringr)

kevin_bacon_nconst <- "nm0000102"

ui <- fluidPage(
  titlePanel("Six Degrees of Kevin Bacon"),
  sidebarLayout(
    sidebarPanel(
      textInput("title", "Enter Movie or TV Show Title:", value = ""),
      actionButton("search_button", "Search Title"),
      uiOutput("cast_list"),
      actionButton("select_button", "Select Actor"),
      actionButton("reset_button", "Reset Game")
    ),
    mainPanel(
      h3("Instructions:"),
      p("Find a connection to Kevin Bacon by selecting actors who have appeared together."),
      h4(textOutput("game_status")),
      verbatimTextOutput("selected_path")
    )
  )
)

server <- function(input, output, session) {
  
  state <- reactiveValues(
    available_actors = NULL,
    selected_actor_nconst = NULL,
    path = character(),
    move_count = 0,
    game_over = FALSE,
    message = "Game has not started yet."
  )
  
  observeEvent(input$search_button, {
    req(input$title)
    
    selected_title <- title_basics %>%
      filter(str_detect(primaryTitle, fixed(input$title, ignore_case = TRUE))) %>%
      slice(1)  # Take first match if multiple titles
    
    req(nrow(selected_title) > 0)
    
    initial_cast <- title_principals %>%
      filter(tconst == selected_title$tconst) %>%
      left_join(name_basics, by = "nconst") %>%
      distinct(nconst, primaryName)
    
    state$available_actors <- initial_cast
    state$path <- character()
    state$move_count <- 0
    state$game_over <- FALSE
    state$message <- "Game started! Select a cast member."
    
    output$cast_list <- renderUI({
      selectInput("cast_member", "Select a Cast Member:", choices = initial_cast$primaryName)
    })
  })
  
  # Select an actor
  observeEvent(input$select_button, {
    req(input$cast_member)
    req(!state$game_over)
    
    selected_name <- input$cast_member
    selected_nconst <- state$available_actors %>%
      filter(primaryName == selected_name) %>%
      pull(nconst)
    
    # Update path
    state$path <- c(state$path, selected_name)
    state$move_count <- state$move_count + 1
    state$selected_actor_nconst <- selected_nconst
    
    if (selected_nconst == kevin_bacon_nconst) {
      state$message <- "Congratulations! You found Kevin Bacon!"
      state$game_over <- TRUE
      return()
    }
    
    # Find all movies the actor has been in
    titles_of_actor <- title_principals %>%
      filter(nconst == selected_nconst) %>%
      pull(tconst)
    
    # Find all co-actors in those movies
    co_actors <- title_principals %>%
      filter(tconst %in% titles_of_actor, nconst != selected_nconst) %>%
      left_join(name_basics, by = "nconst") %>%
      distinct(nconst, primaryName)
    
    if (kevin_bacon_nconst %in% co_actors$nconst) {
      state$message <- "Congratulations! You found Kevin Bacon!"
      state$path <- c(state$path, "Kevin Bacon")
      state$game_over <- TRUE
      return()
    }
    
    if (state$move_count >= 6) {
      state$message <- "Too many moves. You lose! Try again."
      state$game_over <- TRUE
      return()
    }
    
    state$available_actors <- co_actors
    
    output$cast_list <- renderUI({
      selectInput("cast_member", "Select a Cast Member:", choices = co_actors$primaryName)
    })
  })
  
  # Reset
  observeEvent(input$reset_button, {
    state$available_actors <- NULL
    state$selected_actor_nconst <- NULL
    state$path <- character()
    state$move_count <- 0
    state$game_over <- FALSE
    state$message <- "Game reset. Enter a new title to start."
    updateTextInput(session, "title", value = "")
    output$cast_list <- renderUI({ NULL })
  })
  
  # Outputs
  output$selected_path <- renderPrint({
    state$path
  })
  
  output$game_status <- renderText({
    state$message
  })
}

shinyApp(ui = ui, server = server)