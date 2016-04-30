display_user_characteristics = function(userID, recap.Users, data.Ratings, recap.Movies){
#INPUT      userID            : l'identifiant de l'utilisateur 
#           recap.Users       : la base des utilisateurs et des statistiques
#           data.Ratings      : la base des notes
#           recap.Movies      : la base des films et des statistiques
#OUTPUT                       : affichage des caractéristiques de l'utilisateur

  # Vecteur des genres du film
  vect.MovieGenres = c("unknown", "Action", "Adventure", "Animation", "Children.s", "Comedy", 
                       "Crime", "Documentary", "Drama", "Fantasy", "Film.noir", "Horror", 
                       "Musical", "Mystery", "Romance", "Sci.fi", "Thriller", "War", "Western")
  
  # Caractéristiques de l'individu
  age = recap.Users$age[recap.Users$userID == userID] 
  sex = recap.Users$sex[recap.Users$userID == userID]
  
  if(sex == 'M'){
    sex = "un homme"
  }else{
    sex = "une femme"
  }
    
  # Caractéristiques générales sur ses notes
  nb.Ratings = recap.Users$nb.Ratings[recap.Users$userID == userID] #nombre de films notés
  meanRatings = recap.Users$mean[recap.Users$userID == userID]      #moyenne de ses notes
  
  # Caractéristiques sur ses films préférés
  mat.RatedMovies = data.Ratings[data.Ratings$userID == userID, c("movieID", "rating")]     #matrice contenant les couples (movieID/rating) de ses films
  mat.RatedMovies = mat.RatedMovies[order(mat.RatedMovies[,"rating"], decreasing = TRUE),]  #tri dans l'ordre décroissant en fonction des notes

  # Caractéristiques sur ses genres préférés
  nb.Genres = length(vect.MovieGenres)
  mat.StatByGenres = as.data.frame(matrix(0, nrow = nb.Genres, ncol = 3)) #data frame qui contiendra les statistiques sur les genres cinématographiques
  colnames(mat.StatByGenres) = c("genre", "nb.Ratings", "mean") #nom du genre, nombre de films notés de ce genre, moyenne pour les films de ce genre
  mat.StatByGenres$genre = vect.MovieGenres
  
  for(movieID in mat.RatedMovies[,"movieID"]){ #boucle sur tous les films notés par userID
    rating =  mat.RatedMovies[mat.RatedMovies[,"movieID"] == movieID, "rating"] #note obtenu
    genresOfMovie = genres_of_movie(movieID, recap.Movies, vect.MovieGenres)    #genres du film

    for(genre in genresOfMovie){ #boucle sur tous les genres du film movieID
      mat.StatByGenres[mat.StatByGenres$genre == genre, "nb.Ratings"] = mat.StatByGenres[mat.StatByGenres$genre == genre, "nb.Ratings"] + 1 #incrémentation
      mat.StatByGenres[mat.StatByGenres$genre == genre, "mean"] = mat.StatByGenres[mat.StatByGenres$genre == genre, "mean"] + rating        #somme des notes
    }
  }
  
  mat.StatByGenres$mean = mat.StatByGenres$mean / mat.StatByGenres$nb.Ratings #obtention de la moyenne par division de la somme par le nombre nb.Ratings
  
  #tri par ordre décroissant des genres en fonction de leur moyenne
  mat.StatByGenres = mat.StatByGenres[order(mat.StatByGenres[,"mean"], decreasing = TRUE),] #tri dans l'ordre décroissant des notes
  
  #Affichage des caractéristiques générales de l'utilisateur
  cat(sprintf("\nVous êtes %s de %.0f ans et vous avez noté %.0f films avec une moyenne à %.2f \n", sex, age, nb.Ratings, meanRatings))
  
  #Affichage des caractéristiques sur ses films préférés (son top 10% des meilleurs films)
  nb.FavoriteMovies = max(min(round(10/100 * nb.Ratings,1),20),5) #nombre de films considérés, majoré par 20 et minoré à 5
  cat(sprintf("\nVos %.0f films préférés sont : \n", nb.FavoriteMovies))
  for(movieID in mat.RatedMovies$movieID[1:nb.FavoriteMovies]){
    title = recap.Movies$title[recap.Movies$movieID == movieID]
    mean = recap.Movies$mean[recap.Movies$movieID == movieID]
    rating = data.Ratings$rating[data.Ratings$userID == userID & data.Ratings$movieID == movieID]
    genresOfMovies = genres_of_movie(movieID, recap.Movies, vect.MovieGenres)
    cat(sprintf("%s avec une note à %.0f (moyenne à %.2f), de genre(s) %s\n", title, rating, mean, paste(genresOfMovies, collapse=" ")))
  }
  
  #Affichage des caractéristiques sur ses genres préférés
  cat(sprintf("\nVos 5 genres préférés sont : \n"))
  for(genre in mat.StatByGenres$genre[1:5]){
    nb.Ratings = mat.StatByGenres$nb.Ratings[mat.StatByGenres$genre == genre]
    meanGenre = mat.StatByGenres$mean[mat.StatByGenres$genre == genre]
    cat(sprintf("%s dont vous avez vu %.0f film(s) avec une note moyenne à %.2f \n", genre, nb.Ratings, meanGenre))
  }
  
}
