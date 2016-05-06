# Fonction pour obtenir la moyenne des notes d'un individu
get_meanRatings = function(userID, stat.Users){
  if(is.na(userID)){
    return(NA)
  }
  else{
    res = stat.Users$mean[stat.Users$userID == userID]
    return(res)
  }
}

# Fonction pour limiter la valeur de la prédiction
limited_value = function(value){
  if(is.na(value)){
    return(NA)
  }
  else if(value > 5){
    return(5)
  }
  else if(value < 1){
    return(1)
  }
  else{
    return(value)
  }
}

knn_user_predicteur = function(weights, ratings, stat.Users, userID, predicteur, K_Neighbors){
  # INPUT   weights     : vecteur contenant le degré de similarité par plus proches voisins
  #         ratings     : vecteur contenant les notes des plus proches voisins
  #         stat.Users  : statistiques des utilsateurs
  #         userID      : identifiant de l'utilisateur
  #         predicteur  : la fonction de prédiction
  #         K_Neighbors : les identifiants des plus proches voisins
  # OUTPUT              : la note prédite
  
  # Calcul des prédicteurs
  meanOfUser = as.numeric(stat.Users$mean[stat.Users$userID == userID])
  meanOfNeighbors = sapply(K_Neighbors,get_meanRatings, stat.Users)
  
  # Notation  : &a : pour les prédicteurs pondérés, la valeur est majorée/minorée et le dénominateur est la somme des valeurs absolues (a pour absolute)
  
  switch(predicteur, 
         'mean' = mean(ratings, na.rm = TRUE),
         'weighted' = limited_value(sum(ratings * weights, na.rm = TRUE)/ sum(weights, na.rm = TRUE)),
         'weighted-centered' = limited_value(meanOfUser + sum((ratings-meanOfNeighbors) * weights, na.rm = TRUE)/ sum(weights, na.rm = TRUE)),
         'weighted&a' = limited_value(sum(ratings * weights, na.rm = TRUE)/ sum(abs(weights), na.rm = TRUE)),
         'weighted-centered&a' = limited_value(meanOfUser + sum((ratings-meanOfNeighbors) * weights, na.rm = TRUE)/ sum(abs(weights), na.rm = TRUE))
  )
}