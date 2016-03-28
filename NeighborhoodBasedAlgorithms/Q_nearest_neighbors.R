Q_nearest_neighbors = function(userID, movieID, Q, list.dejaVu, vect.Users, similarity){
  #INPUT  userID              : l'identifiant de l'utilisateur
  #       movieID             : l'identifiant du film
  #       list.dejaVu         : la liste des films notés par utilisateur
  #       similarity          : la similarité utilisée pour calculer la proximité
  #       movieID             : ID du film dont on veut prédire la note
  #       Q                   : le nombre de voisins
  #OUTPUT neighbors           : les Q plus proches voisins pour le film movieID pour userID

  userIND = which(vect.Users == userID)
  if (similarity == "pearson"){
    betterIsHigh = TRUE
  } else if(similarity %in% c("nrmse", "nmae")){
    betterIsHigh = FALSE
  }
  vect.similarity = vect.Users[order(get(paste0("mat.sim_", similarity))[userIND,], decreasing = betterIsHigh)]
  
  nb.Users = length(vect.similarity)
  
  neighbors = vector()
  userIND2 = 1
  nb.Neighbors = 0
  
  while((nb.Neighbors<Q)&(userIND2<nb.Users)){
    userID2 = vect.similarity[userIND2]
    
    if(movieID %in% list.dejaVu[[userID2]]){
      nb.Neighbors = nb.Neighbors+1
      neighbors[nb.Neighbors] = userID2
    }
    userIND2 = userIND2+1
  }
  return(neighbors)
}
