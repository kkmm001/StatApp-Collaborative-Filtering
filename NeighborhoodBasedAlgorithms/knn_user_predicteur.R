knn_user_predicteur = function(weight, ratings, stat.Users, userID, predicteur, Q_Neighbors){
  # INPUT   weight                : vecteur contenant le degré de similarité par plus proches voisins
  #         ratings               : vecteur contenant les notes des plus proches voisins
  #         stat.Users            : statistiques des utilsateurs
  #         userID                : identifiant de l'utilisateur
  #         predicteur            : la fonction de prédiction
  #         Q_Neighbors           : les identifiants et les similarités des plus proches voisins
  # OUTPUT                        : la note prédite
  
  # Fonction pour obtenir la moyenne des notes d'un individu
  get_meanRatings = function(userID){
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
  
  # Calcul des prédicteurs

  if(predicteur != "mean"){
    meanOfUser = as.numeric(stat.Users$mean[stat.Users$userID == userID])
    meanOfNeighbors = sapply(Q_Neighbors,get_meanRatings)
  }
  
  # Notation  : &b  : pour les prédicteurs pondérés, la valeur est majorée par 5 et minorée par 1 (b pour bounds)
  #           : &ab : pour les prédicteurs pondérés, la valeur est majorée/minorée et le dénominateur est la somme des valeurs absolues (a pour absolute)
    
  switch(predicteur, 
         'mean' = mean(ratings, na.rm = TRUE),
         'weighted&b' = limited_value(sum(ratings * weight, na.rm = TRUE)/ sum(weight, na.rm = TRUE)),
         'weighted-centered&b' = limited_value(meanOfUser + sum((ratings-meanOfNeighbors) * weight, na.rm = TRUE)/ sum(weight, na.rm = TRUE)),
         'weighted&ab' = limited_value(sum(ratings * weight, na.rm = TRUE)/ sum(abs(weight), na.rm = TRUE)),
         'weighted-centered&ab' = limited_value(meanOfUser + sum((ratings-meanOfNeighbors) * weight, na.rm = TRUE)/ sum(abs(weight), na.rm = TRUE))
         )
}