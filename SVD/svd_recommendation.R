svd_recommendation = function(userID, recap.Users, recap.Movies, data.Ratings, list.SVD.Item, list.SVD.User, tau,  nb.recommandations, nbMin.Ratings, howToFill, list.dejaVu){
  # INPUT   userID              : identifiant de l'utilisateur
  #     	  recap.Users         : base de données et des statistiques des utilisateurs
  #         recap.Movies        : base de données et des statistiques des films
  #     	  data.Ratings        : base des notes
  #     	  list.SVD.User       : liste des matrices lors de la décomposition de la matrice des notes remplie par la méthode howToFill = User
  #     	  list.SVD.Item       : liste des matrices lors de la décomposition de la matrice des notes remplie par la méthode howToFill = Item
  #     	  tau                 : taux d'inertie conservé lors de la réduction de matrice
  #         nb.recommandations  : nombre de films recommandés
  #         nbMin.Ratings       : le nombre minimal de visionnage pour qu'un film soit recommandable
  #         howToFill           : méthode naïve de complétion de matrice (vaut "Item" ou "User")
  #         list.dejaVu         : la liste des films notés par utilisateur
  # OUTPUT                      : retourne les recommandations pour l'utilisateur userID 
  
  # Plus spécifiquement, cette fonction retourne le vecteur de taille nb.recommandations, contenant les identifiants  et les notes 
  # des films recommandés à partir de l'algorithme SVD pour l'individu userID
  
  # Décompostion SVD en fonction de la valeur de howToFill
  if(howToFill=="Item"){
    list.SVD=list.SVD.Item
  }else{
    list.SVD=list.SVD.User
  }
  
  # Dimension du problème
  vect.Users = sort(unique(data.Ratings$userID))
  userIND = which(vect.Users == userID)      
  
  # Filtre des films ayant dépassés un certain seuil
  vect.RecommandableMovies = sort(unique(recap.Movies$movieID[recap.Movies$nb.Ratings >= nbMin.Ratings]))
        
  # Ensemble des films qui sont susceptibles d'être recommandés à userID
  vect.RecommandableMovies = vect.RecommandableMovies[!(vect.RecommandableMovies %in% list.dejaVu[[userID]])]
  nb.RecommandableMovies = length(vect.RecommandableMovies)
  
  # Génération de la data frame mat.RecommendedMovies : matrice contenant les prédictions pour tous les films susceptibles 
  # d'être recommandés et leur identifiants
  mat.RecommendedMovies = as.data.frame(matrix(NA, nrow = nb.RecommandableMovies, ncol = 2))
  colnames(mat.RecommendedMovies) = c("movieID", "prating")
  
  # Creation des matrice US et SV se basant sur la méthode SVD necessaire à la prediction
  mat.US = matUS_matSV(list.SVD,tau)$US
  mat.SV = matUS_matSV(list.SVD,tau)$SV
  
  # Création le de la moyenne de l'utilisateur userID
  meanOfUser = recap.Users$mean[recap.Users$userID == userID]
  
  # Complétion de la data frame mat.RecommendedMovies
  for(movieIND in 1:nb.RecommandableMovies){
    movieID = vect.RecommandableMovies[movieIND]
    mat.RecommendedMovies$prating[movieIND] = meanOfUser + mat.US[userIND,] %*% mat.SV[,movieIND]
    mat.RecommendedMovies$movieID[movieIND] = movieID
  }

  # Tri de mat.RecommendedMovies
  mat.RecommendedMovies = mat.RecommendedMovies[order(mat.RecommendedMovies$prating, decreasing = TRUE),]
  
  return(mat.RecommendedMovies[1:nb.recommandations,])
}

