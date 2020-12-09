## server.R

library(dplyr)
library(stringr)

# load functions
# source('functions/cf_algorithm.R') # collaborative filtering
# source('functions/similarity_measures.R') # similarity measures

createRatingMatrix = function(ratings){
  # First create a utility matrix stored as a sparse matrix.
  i = paste0('u', ratings$UserID)
  j = paste0('m', ratings$MovieID)
  x = ratings$Rating
  tmp = data.frame(i, j, x, stringsAsFactors = T)
  Rmat = sparseMatrix(as.integer(tmp$i), as.integer(tmp$j), x = tmp$x)
  rownames(Rmat) = levels(tmp$i)
  colnames(Rmat) = levels(tmp$j)
  Rmat = new('realRatingMatrix', data = Rmat)
  return(Rmat)
}

get_recommendations = function(value_list) {
  selected_movies = value_list[grep(pattern = "select_", names(value_list))]
  movie_ids = as.integer(stringr::str_extract(names(selected_movies), "\\d+"))

  i = 1
  for(movie in 1:length(selected_movies)) {
    rating = selected_movies[i]
    movie_id = movie_ids[i]
    i = i + 1

    if(is.na(rating) || rating == "") {
      next
    }

    movie_id = as.integer(movie_id)
    rating = as.integer(rating)

    print(paste0("MOVIE_ID: ",  movie_id, "; RATING: ", rating))
    ratings[dim(ratings)[1]+1,] = c(99999, movie_id, rating)
  }

  Rmat = createRatingMatrix(ratings)
  rec = Recommender(Rmat, method = "UBCF",
                    parameter = list(normalize = 'Z-score',
                                     method = 'Cosine',
                                     nn = 25))
  recom = predict(rec, Rmat['u99999'], type = 'ratings')
  recom = as(recom, 'matrix')[1, ]
  ordered_recom = recom[order(recom, decreasing=TRUE)]
  recommendations = names(ordered_recom)[1:10]
  print(recommendations)
  recommendations = lapply(recommendations, function(x) {
                      as.integer(str_remove(x, "m"))
                    })

  return(recommendations)
}

# read in data
myurl = "https://liangfgithub.github.io/MovieData/"
movies = readLines(paste0(myurl, 'movies.dat?raw=true'))
movies = strsplit(movies, split = "::", fixed = TRUE, useBytes = TRUE)
movies = matrix(unlist(movies), ncol = 3, byrow = TRUE)
movies = data.frame(movies, stringsAsFactors = FALSE)
colnames(movies) = c('MovieID', 'Title', 'Genres')
movies$MovieID = as.integer(movies$MovieID)
movies$Title = iconv(movies$Title, "latin1", "UTF-8")

small_image_url = "https://liangfgithub.github.io/MovieImages/"
movies$image_url = sapply(movies$MovieID,
                          function(x) paste0(small_image_url, x, '.jpg?raw=true'))

myurl = "https://liangfgithub.github.io/MovieData/"
ratings = read.csv(paste0(myurl, 'ratings.dat?raw=true'),
                   sep = ':',
                   colClasses = c('integer', 'NULL'),
                   header = FALSE)
colnames(ratings) = c('UserID', 'MovieID', 'Rating', 'Timestamp')
ratings$Timestamp = NULL

shinyServer(function(input, output, session) {

  # show the books to be rated
  output$ratings <- renderUI({
    num_rows <- 20
    num_movies <- 6 # movies per row

    lapply(1:num_rows, function(i) {
      list(fluidRow(lapply(1:num_movies, function(j) {
        list(box(width = 2,
                 div(style = "text-align:center", img(src = movies$image_url[(i - 1) * num_movies + j], height = 150)),
                 #div(style = "text-align:center; color: #999999; font-size: 80%", books$authors[(i - 1) * num_books + j]),
                 div(style = "text-align:center", strong(movies$Title[(i - 1) * num_movies + j])),
                 div(style = "text-align:center; font-size: 150%; color: #f0ad4e;", ratingInput(paste0("select_", movies$MovieID[(i - 1) * num_movies + j]), label = "", dataStop = 5)))) #00c0ef
      })))
    })
  })

  # Calculate recommendations when the sbumbutton is clicked
  df <- eventReactive(input$btn, {
    withBusyIndicatorServer("btn", { # showing the busy indicator
        # hide the rating container
        useShinyjs()
        jsCode <- "document.querySelector('[data-widget=collapse]').click();"
        runjs(jsCode)

        # get the user's rating data
        value_list <- reactiveValuesToList(input)
        recommended_movie_ids <- get_recommendations(value_list)

        recommendations = movies %>%
        filter(MovieID %in% recommended_movie_ids) %>%
        mutate(Image = paste0(small_image_url,
                              MovieID,
                              '.jpg?raw=true')) %>%
        arrange(factor(MovieID, levels = recommended_movie_ids)) %>%
        select('MovieID', 'Image', 'Title')

        print(recommendations)
        recom_results <- data.table(Rank = 1:10,
                                    MovieID = recommendations$MovieID,
                                    Image = recommendations$Image,
                                    Title = recommendations$Title)

    }) # still busy

  }) # clicked on button


  # display the recommendations
  output$results <- renderUI({
    num_rows <- 2
    num_movies <- 5
    recom_results <- df()

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
          )
        )
      }))) # columns
    }) # rows

  }) # renderUI function

}) # server function
