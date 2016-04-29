stat_users_by_genre = function(data.Ratings, vect.MovieGenres){
  
  # Nombre d'utilisateurs
  vect.Users = sort(unique(data.Ratings$userID))
  nb.Users = length(vect.Users)
  
  # Nombre de genres cinématographiques
  nb.Genres = length(vect.MovieGenres)
  
  mat.stat.byGenre = as.data.frame(matrix(0, nrow = nb.Users, ncol = (nb.Genres+1)))
  colnames(mat.stat.byGenre) = c("userID", vect.MovieGenres)
  
  for(userIND in 1:nb.Users){
    cat(sprintf("%.0f|", userIND))
    userID = vect.Users[userIND]
    meanRatings = mean(data.Ratings$rating[data.Ratings$userID == userID])
    
    # Matrice contenant les couples (movieID/rating) de ses films
    mat.RatedMovies = data.Ratings[data.Ratings$userID == userID, c("movieID", "rating")]     
  
    # Caractéristiques sur ses genres préférés
    mat.StatByGenresbyUser = as.data.frame(matrix(0, nrow = nb.Genres, ncol = 3)) 
    colnames(mat.StatByGenresbyUser) = c("nb.Ratings", "sum", "mean")
    rownames(mat.StatByGenresbyUser) = vect.MovieGenres 
    
    for(movieID in mat.RatedMovies[,"movieID"]){ #boucle sur tous les films notés par userID
      rating =  mat.RatedMovies[mat.RatedMovies[,"movieID"] == movieID, "rating"] #note obtenu
      genresOfMovie = genres_of_movie(movieID, recap.Movies, vect.MovieGenres)    #genres du film
      
      for(genre in genresOfMovie){ #boucle sur tous les genres du film movieID
        mat.StatByGenresbyUser[genre, "nb.Ratings"] = mat.StatByGenresbyUser[genre, "nb.Ratings"] + 1 #incrémentation
        mat.StatByGenresbyUser[genre, "sum"] = mat.StatByGenresbyUser[genre, "sum"] + rating        #somme des notes
      }
    }
    
    mat.StatByGenresbyUser$mean = mat.StatByGenresbyUser$sum / mat.StatByGenresbyUser$nb.Ratings - meanRatings #obtention de la moyenne par division de la somme par le nombre nb.Ratings
    
    mat.stat.byGenre[userIND,1] = userID
    mat.stat.byGenre[userIND,2:(nb.Genres+1)] = mat.StatByGenresbyUser$mean
  }
  
  return(mat.stat.byGenre)
}