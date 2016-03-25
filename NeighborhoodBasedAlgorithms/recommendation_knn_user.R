recommendation_knn_user = function(userID, recap.Users, recap.Movies, data.Ratings, similarity, list.dejaVu, Q, nb.recommandations){
  #INPUT  userID
  #       recap.Users
  #       recap.Movies
  #       data.Ratings
  #       similarity
  #       list.dejaVu
  #       Q
  #       nb.recommandations
  #OUTPUT                     : retourne les nb.recommandations pour l'utilisateur userID
  vect.Users = sort(unique(recap.Users$userID))
  vect.Movies = sort(unique(recap.Movies$movieID[recap.Movies$nb.Ratings >= Q]))
  
  userIND = which(vect.Users == userID)
  
  vect.Recommandable = vect.Movies[!(vect.Movies %in% list.dejaVu[[userID]])]
  
  mat.QNeighbors = matrix(NA, nrow = length(vect.Recommandable), ncol = Q)
  for(movieIND in 1:length(vect.Recommandable)){
    movieID = vect.Recommandable[movieIND]
    vect.QNeighbors_movie = Q_nearest_neighbors(userID, movieID, Q, list.dejaVu, vect.Users, similarity)
    for(q in 1:Q){
      mat.QNeighbors[movieIND,q] = vect.QNeighbors_movie[q]
    }
  }
  
  vect.Prediction = matrix(NA, nrow = length(vect.Recommandable), ncol = 1)
  for(movieIND in 1 : length(vect.Recommandable)){
    vect.Prediction[movieIND] = mean(data.Ratings$rating[data.Ratings$userID %in% mat.QNeighbors[movieIND,]])
  }
  
  vect.RecommendedMovies = matrix(NA, nrow = nb.recommandations, ncol = 1)
  for(movieIND in 1:nb.recommandations){
    vect.RecommendedMovies[movieIND] = vect.Recommandable[order(vect.Prediction, decreasing = TRUE)][movieIND]
  }
  
  return(vect.RecommendedMovies)
}
