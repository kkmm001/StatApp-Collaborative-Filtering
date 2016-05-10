svd_recommendation = function(userID, recap.Users, recap.Movies, data.Ratings, list.SVD_Item, list.SVD_User, tau,  nb.recommandations, nbMin.Ratings, howToFill, list.dejaVu){
  # INPUT   userID              : identifiant de l'utilisateur
  #     	  recap.Users         : base de données et des statistiques des utilisateurs
  #         recap.Movies        : base de données et des statistiques des films
  #     	  data.Ratings        : base des notes
  #     	  list.SVD_User       : liste des matrices lors de la décomposition de la matrice des notes remplie par la méthode howToFill = User
  #     	  list.SVD_Item       : liste des matrices lors de la décomposition de la matrice des notes remplie par la méthode howToFill = Item
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
    list.SVD=list.SVD_Item
  }else{
    list.SVD=list.SVD_User
  }
  
  # Dimension du problème
  vect.Users = sort(unique(data.Ratings$userID))
  userIND = which(vect.Users == userID) 
  
  # Dimension du problème
  vect.Movies = sort(unique(data.Ratings$movieID))

  # Filtre des films ayant dépassés un certain seuil
  vect.RecommandableMovies = sort(unique(recap.Movies$movieID[recap.Movies$nb.Ratings >= nbMin.Ratings]))
        
  # Ensemble des films qui sont susceptibles d'être recommandés à userID
  vect.RecommandableMovies = vect.RecommandableMovies[!(vect.RecommandableMovies %in% list.dejaVu[[userID]])]
  nb.RecommandableMovies = length(vect.RecommandableMovies)
  
  # Génération de la data frame mat.RecommendedMovies : matrice contenant les prédictions pour tous les films susceptibles 
  # d'être recommandés et leur identifiants
  mat.RecommendedMovies = as.data.frame(matrix(NA, nrow = length(vect.RecommandableMovies), ncol = 2))
  colnames(mat.RecommendedMovies) = c("movieID", "prating")
  
  # Creation des matrice US et SV se basant sur la méthode SVD necessaire à la prediction
  mat.US = matUS_matSV(list.SVD,tau)$US
  mat.SV = matUS_matSV(list.SVD,tau)$SV
  
  # La moyenne de l'utilisateur userID
  meanOfUser = recap.Users$mean[recap.Users$userID == userID]
  
  # Complétion de la data frame mat.RecommendedMovies
  for(movieREC in 1:length(vect.RecommandableMovies)){ # INDICE du film dans vect.RecommandableMovies
    movieID = vect.RecommandableMovies[movieREC] #ID du film
    movieIND = which(vect.Movies == movieID)      #INDICE du film dans la matrice
    mat.RecommendedMovies$prating[movieREC] = get_limited_value(meanOfUser + mat.US[userIND,] %*% mat.SV[,movieIND]) #TODO IN ou IND
    mat.RecommendedMovies$movieID[movieREC] = movieID
  }

  # Tri de mat.RecommendedMovies
  mat.RecommendedMovies = mat.RecommendedMovies[order(mat.RecommendedMovies$prating, decreasing = TRUE),]

  
  return(mat.RecommendedMovies[1:nb.recommandations,])
}

