Q_nearest_neighbors = function(list.dejaVu, vect.similarity, movieID, Q){
  #INPUT  list.dejaVu         : la liste des films notés par utilisateur
  #       vect.similarity     : vecteur des similarités de l'individu dans l'ordre croissant
  #       movieID             : ID du film dont on veut prédire la note
  #       Q                   : le nombre de voisins
  #OUTPUT neighbors           : les Q plus proches voisins pour le film movieID pour la personne concernée

  nb.Users = length(vect.similarity)
  
  neighbors = vector()
  userIND = 1
  nb.Neighbors = 0
  
  while((nb.Neighbors<Q)&(userIND<nb.Users)){
    userID = vect.similarity[userIND]
    
    if(movieID %in% list.dejaVu[[userID]]){
      nb.Neighbors = nb.Neighbors+1
      neighbors[nb.Neighbors] = userID
    }
    userIND = userIND+1
  }
  return(neighbors)
}
