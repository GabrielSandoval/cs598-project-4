## ui.R
library(shiny)
library(shinydashboard)
library(recommenderlab)
library(data.table)
library(ShinyRatingInput)
library(shinyjs)

source('functions/helpers.R')

genre_list = c("Action", "Adventure", "Animation", 
               "Children's", "Comedy", "Crime",
               "Documentary", "Drama", "Fantasy",
               "Film-Noir", "Horror", "Musical", 
               "Mystery", "Romance", "Sci-Fi", 
               "Thriller", "War", "Western")
shinyUI(
    dashboardPage(
          skin = "blue",
          dashboardHeader(title = "Content-Based Filtering"),

          dashboardSidebar(disable = TRUE),

          dashboardBody(includeCSS("css/movies.css"),
              fluidRow(
                box(width = 12, title = "Step 1: Select a Genre", status = "info", solidHeader = TRUE, collapsible = TRUE,
                  selectInput(
                    'genre',
                    'genre',
                    genre_list,
                    selectize = FALSE,
                  )
                    # div(class = "rateitems",
                    #     uiOutput('ratings')
                    # )
                )
              ),
              fluidRow(
                useShinyjs(),
                box(
                  width = 12, status = "info", solidHeader = TRUE,
                  title = "Step 2: Discover movies you might like",
                  br(),
                  withBusyIndicatorUI(
                    actionButton("btn", "Click here to get your recommendations", class = "btn-warning")
                  ),
                  br(),
                  tableOutput("results")
                )
             )
          )
    )
)
