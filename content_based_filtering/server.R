## server.R

library(dplyr)
library(stringr)

## NOTE:
#
# - To run app locally:
#   shiny::runApp()
#
# - To deploy app:
#   library(rsconnect)
#   deployApp()
#
##

myurl = "https://liangfgithub.github.io/MovieData/"

ratings = read.csv(paste0(myurl, 'ratings.dat?raw=true'),
                   sep = ':',
                   colClasses = c('integer', 'NULL'),
                   header = FALSE)
colnames(ratings) = c('UserID', 'MovieID', 'Rating', 'Timestamp')

movies = readLines(paste0(myurl, 'movies.dat?raw=true'))
movies = strsplit(movies, split = "::", fixed = TRUE, useBytes = TRUE)
movies = matrix(unlist(movies), ncol = 3, byrow = TRUE)
movies = data.frame(movies, stringsAsFactors = FALSE)
colnames(movies) = c('MovieID', 'Title', 'Genres')
movies$MovieID = as.integer(movies$MovieID)

# convert accented characters
movies$Title = iconv(movies$Title, "latin1", "UTF-8")

# extract Year
movies$Year = as.numeric(unlist(
  lapply(movies$Title, function(x) substr(x, nchar(x)-4, nchar(x)-1))))

small_image_url = "https://liangfgithub.github.io/MovieImages/"
movies$image_url = sapply(movies$MovieID, function(x) paste0(small_image_url, x, '.jpg?raw=true'))

recency_function = function(year) {
  minYear = min(movies$Year)
  maxYear = max(movies$Year)

  return(1 + (year - minYear) / (maxYear - minYear))
}

genre_threshold = function(genre){
  if (genre %in% c("Comedy", "Drama", "Action", "Thriller", "Sci-Fi", "Romance", "Adventure")) {
    return(1000)
  } else if (genre %in% c("Crime", "Horror", "Children's", "War")) {
    return(500)
  } else if (genre %in% c("Animation", "Musical", "Mystery", "Fantasy", "Western", "Film-Noir", "Documentary")) {
    return(200)
  }
}

get_recommendations_by_genre = function(chosen_genre) {
  recommendations = ratings %>%
  group_by(MovieID) %>%
  summarize(ratings_per_movie = n(),
            Score = round(mean(Rating), dig=3)) %>%
  inner_join(movies, by = 'MovieID') %>%
  filter(ratings_per_movie > genre_threshold(chosen_genre)) %>%
  filter(str_detect(Genres, chosen_genre)) %>%
  top_n(10, Score) %>%
  mutate(Image = paste0(small_image_url,
                        MovieID,
                        '.jpg?raw=true')) %>%
  mutate(Score = round(Score * recency_function(Year), dig=3)) %>%
  select('MovieID', 'Image', 'Title', 'Score') %>%
  arrange(desc(Score))

  return(recommendations)
}

shinyServer(function(input, output, session) {

  # Calculate recommendations when the sbumbutton is clicked
  df <- eventReactive(input$btn, {
    withBusyIndicatorServer("btn", { # showing the busy indicator
        # hide the rating container
        useShinyjs()
        jsCode <- "document.querySelector('[data-widget=collapse]').click();"
        runjs(jsCode)

        # get the user's rating data
        # print(input$genre)

        # ======= CODE HERE =======

        recommendations = get_recommendations_by_genre(input$genre)

        # print(recommendations)

        recom_results <- data.table(Rank = 1:10,
                                    MovieID = recommendations$MovieID,
                                    Title = recommendations$Title,
                                    Image = recommendations$Image,
                                    Score = recommendations$Score)

        #  ========================

    }) # still busy

  }) # clicked on button

  output$urls <- renderUI({
    content_based_url = a("Content-Based Filtering", href="https://grs4.shinyapps.io/content_based_filtering")
    collaborative_url = a("Collaborative Filtering", href="https://grs4.shinyapps.io/collaborative_filtering")
    div(
      tagList("System I: ", content_based_url),
      br(),
      tagList("System II: ", collaborative_url)
    )
  })

  # display the recommendations
  output$results <- renderUI({
    num_rows <- 2
    num_movies <- 5
    recom_results <- df()
    # print(recom_results)

    lapply(1:num_rows, function(i) {
      list(fluidRow(lapply(1:num_movies, function(j) {
        idx = (i-1) * num_movies + j
        movie = recom_results[idx,]

        box(width = 2, status = "success", solidHeader = TRUE, title = paste0("Rank ", movie$Rank),

          div(style = "text-align:center",
              a(img(src = movie$Image, height = 150))
             ),
          div(style="text-align:center; font-size: 100%",
              strong(movie$Title)
             ),
          div(style="text-align:center; font-size: 100%",
              strong(paste0("Score: ", movie$Score))
             )

        )
      }))) # columns
    }) # rows

  }) # renderUI function

}) # server function
