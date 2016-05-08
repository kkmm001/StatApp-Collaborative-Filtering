svd_filledMatrix = function(howToFill, data.Ratings, stat.Users, stat.Movies){
  # INPUT   howToFill     : note par défaut pour les éléments notés ("Item" ou "User")
  #         data.Ratings  : la base des notes
  #         stat.Users    : statistiques sur les utilisateurs
  #         stat.Movies   : statistiques sur les films
  # OUTPUT                : la décomposition SVD de la matrice remplie
  
  # Cette fonction remplie la matrice mat.Y des notes. Si la note existe, elle la garde, sinon, elle
  # considère la moyenne (par utilisteur ou par film selon la valeur de howToFill). Puis, elle retourne
  # la décomposition SVD de la matrice ainsi remplie.
  
  # Dimension du problème
  vect.Users = sort(unique(data.Ratings$userID))
  nb.Users = length(vect.Users) # nombre d'individus différents dans la data frame data.Ratings
  
  vect.Movies = sort(unique(data.Ratings$movieID))
  nb.Movies = length(vect.Movies) # nombre de films différents dans la data frame data.Ratings
  
  nb.Ratings = dim(data.Ratings)[1]
  
  # Crétion de la matrice Y (matrice des notes)
  mat.Y = matrix(NA, nrow = nb.Users, ncol = nb.Movies) 
  
  # Complétion par la moyenne pour toutes les valeurs de la matrice
  if(howToFill == "User"){
    for(userIND in 1:nb.Users){
      userID = vect.Users[userIND]
      mat.Y[userIND, ] = stat.Users$mean[stat.Users$userID == userID]
    }
  } else{
    for(movieIND in 1:nb.Movies){
      movieID = vect.Movies[movieIND]
      mat.Y[,movieIND] = stat.Movies$mean[stat.Movies$movieID == movieID]
    }
  }
  
  # Ecrasement des valeurs où la note existe
  for(i in 1:nb.Ratings){
    userID = data.Ratings$userID[i]
    movieID = data.Ratings$movieID[i]
    rating = data.Ratings$rating[i]
    
    userIND = which(vect.Users == userID)
    movieIND = which(vect.Movies == movieID)
    mat.Y[userIND, movieIND] = rating
  }

  # Normalisation par la note moyenne des utilisateurs
  mat.Y = mat.Y - as.numeric(stat.Users$mean) #la soustraction se fait par ligne
  
  # Décomposition SVD de la matrice remplie
  list.SVD=svd(mat.Y)  
  
  return(list.SVD)
}
