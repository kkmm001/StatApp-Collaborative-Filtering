knn_user_recommendation = function(userID, recap.Users, recap.Movies, data.Ratings, mat.sim, list.dejaVu, Q, nb.recommandations, predicteur){
  #INPUT  userID              : identifiant de l'utilisateur
  #       recap.Users         : base de données et des statistiques des utilisateurs
  #       recap.Movies        : base de données et des statistiques des films
  #       data.Ratings        : base des notes
  #       mat.sim             : matrice de similarité entre les individus
  #       list.dejaVu         : liste des films déjà notés par individu
  #       Q                   : nombre de plus proches voisins
  #       nb.recommandations  : nombre de films recommandés
  #       predicteur          : la fonction de prédiction
  #OUTPUT                     : retourne les recommandations pour l'utilisateur userID
  
  # Plus spécifiquement, cette fonction retourne le vecteur de taille nb.recommandations, contenant les identifiants des films recommandés
  # à partir de l'algorithme des Q plus proches (au sens de mat.sim) voisins présents dans recap.Users pour l'individu userID, 
  # calculé à partir (en fonction du prédicteur) des notes de la base data.Ratings. 
  
  # Ensemble des utilisateurs (et donc des potentiels futurs voisins de userID)
  vect.Users = sort(unique(recap.Users$userID))
  
  # Ensemble des films
  vect.Movies = sort(unique(recap.Movies$movieID))
  
  # Indice de userID dans les matrices 
  userIND = which(vect.Users == userID)
  
  # Ensemble des films qui sont susceptibles d'être recommandés à userID
  vect.Recommandable = vect.Movies[!(vect.Movies %in% list.dejaVu[[userID]])]
  
  # Génération du vecteur vect.Ratings.byNN : vecteur contenant les notes des Q plus proches voisins pour un film donné
  vect.Ratings.byNN = as.vector(matrix(NA, nrow = 1, ncol = Q))
  
  # Génération du vecteur vect.Prediction : vecteur contenant les prédictions pour tous les films susceptibles d'être recommandés
  vect.Prediction = matrix(NA, nrow = length(vect.Recommandable), ncol = 1)
  
  cat(length(vect.Recommandable))
  
  # Complétion des vecteurs vect.Ratings.byNN et vect.Prediction
  for(movieIND in 1:length(vect.Recommandable)){
    cat(sprintf("|%0.f",movieIND))
    
    movieID = vect.Recommandable[movieIND]
    qnn = Q_nearest_neighbors(userID, movieID, Q, list.dejaVu, vect.Users, mat.sim)
    #vect.QNeighbors_movie = qnn[,1] #vecteur contenant les identifiants des Q plus proches voisins
    #vect.Similarity.byNN = qnn[,2] #vecteur contenant les similarités des Q plus proches voisins
    
    #vecteur contenant les notes des Q plus proches voisins pour un film donné
    for(q in 1:Q){
      vect.Ratings.byNN[q] = data.Ratings$rating[(data.Ratings$userID == qnn[q,1]) & (data.Ratings$movieID == movieID)]
    }
 
    vect.Prediction[movieIND] =  knn_user_predicteur(qnn$similarities, vect.Ratings.byNN, recap.Users, userID, predicteur, qnn$neighbors)
  }
  
  mat.RecommendedMovies = as.data.frame(matrix(NA, nrow = nb.recommandations, ncol = 2))
  colnames(mat.RecommendedMovies) = c("movieID", "prating")
  for(movieIND in 1:nb.recommandations){
    mat.RecommendedMovies$movieID[movieIND] = vect.Recommandable[order(vect.Prediction, decreasing = TRUE)][movieIND]
    mat.RecommendedMovies$prating[movieIND] = vect.Prediction[order(vect.Prediction, decreasing = TRUE)[movieIND]]
  }
  
  return(mat.RecommendedMovies)
}
