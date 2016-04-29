genres_of_movie = function(movieID, recap.Movies, vect.MovieGenres){
  # INPUT movieID       : l'identifiant du film
  #       recap.Movies  : la base des films et des statistiques
  #       vect.MoviesGenres : vecteur contenant les noms des genres cinématographiques
  
  #extraction des genres (TRUE ou FALSE) du films
  genresOfMovies = sapply(vect.MovieGenres, function(x) recap.Movies[recap.Movies$movieID == movieID, x] == 1)
  
  #ensemble des genres du films
  genresOfMovies = genresOfMovies[genresOfMovies == TRUE]
  
  #ensemble des noms des genres du films
  genresOfMovies = names(genresOfMovies)
  
  return(genresOfMovies)
}