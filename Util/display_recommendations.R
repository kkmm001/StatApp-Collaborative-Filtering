display_recommendations = function(mat.RecommendedMovies, nb.recommandations, recap.Movies){
  #INPUT  mat.RecommendedMovies : matrice contenant les films recommandés
  #       nb.recommandations    : le nombre de recommandations par personne
  #       recap.Movies          : la base de données des films et des statistiques
  #OUTPUT                       : AFFICHAGE DES RESULTATS
  
  # vecteur des genres du film
  vect.MovieGenres = c("unknown", "Action", "Adventure", "Animation", "Children.s", "Comedy", 
                       "Crime", "Documentary", "Drama", "Fantasy", "Film.noir", "Horror", 
                       "Musical", "Mystery", "Romance", "Sci.fi", "Thriller", "War", "Western")

  # affichage
  cat(sprintf("\n Les %.0f films recommandés pour vous : \n", nb.recommandations))
  
  for (recom in 1:nb.recommandations){
    #l'identifiant du film
    movieID = mat.RecommendedMovies$movieID[recom] 
    
    #détermination des genres du film
    genresOfMovies = sapply(vect.MovieGenres, function(x) recap.Movies[recap.Movies$movieID == movieID, x] == 1)
    genresOfMovies = genresOfMovies[genresOfMovies == TRUE]
    genresOfMovies = names(genresOfMovies)
    
    cat(sprintf("%.0f \t %-40s \t noté %.2f/5 \t prédit à %.2f/5 \ vu %0.f fois \t genres : %s \n", 
                recom,                                                    #numéro de la recommandation
                recap.Movies$title[recap.Movies$movieID == movieID],      #titre du film
                recap.Movies$mean[recap.Movies$movieID == movieID],       #note moyenne du film
                mat.RecommendedMovies$prating[recom],                     #note prédite
                recap.Movies$nb.Ratings[recap.Movies$movieID == movieID], #nombre de notes reçues
                paste(genresOfMovies, collapse=" ")                       #genres du films
                )
    )
  }  
  
}
