recommendation_knn_user = function(userID, vect.Users, vect.Movies, data.Ratings, vect.similarity, list.dejaVu){
  
  userIND = which(vect.Users == userID)
  vect.Recommandable = vect.Movies[!(vect.Movies %in% list.dejaVu[[userID]])]
  
  mat.QNeighbors = matrix(NA, nrow = length(vect.Recommandable), ncol = Q)
  for(movieIND in 1:length(vect.Recommandable)){
    movieID = vect.Recommandable[movieIND]
    vect.QNeighbors_movie = Q_nearest_neighbors(list.dejaVu, vect.similarity, movieID, Q)
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
