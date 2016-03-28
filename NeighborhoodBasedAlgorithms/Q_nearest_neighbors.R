Q_nearest_neighbors = function(userID, movieID, Q, list.dejaVu, vect.Users, similarity){
  #INPUT  userID              : l'identifiant de l'utilisateur
  #       movieID             : l'identifiant du film
  #       list.dejaVu         : la liste des films notés par utilisateur
  #       similarity          : la similarité utilisée pour calculer la proximité
  #       movieID             : ID du film dont on veut prédire la note
  #       Q                   : le nombre de voisins
  #OUTPUT neighbors           : les Q plus proches voisins pour le film movieID pour userID
  
  # Plus spécifiquement, cette fonction retourne le vecteur de taille Q contenant les valeurs 
  # de vect.Users (ie les identifiants des utilisateurs), à partir des calculs de proximité 
  # (au sens de similarity), de userID pour le film movieID. 
  # Rq : un itulisateur peut se prétendre un plus proche voisin d'un utilisateur X pour le film Y
  # a condition qu'il ait noté le film Y ! D'où l'utilité de list.dejaVu 

  # Similarité et ordre
  if (similarity == "pearson"){
    betterIsHigh = TRUE
  } else if(similarity %in% c("nrmse", "nmae")){
    betterIsHigh = FALSE
  }
  
  # La position de userID dans la matrice de similarité
  userIND = which(vect.Users == userID)
  
  # Le vecteur contenant les plus proches voisins de userID au sens général
  vect.similarity = vect.Users[order(get(paste0("mat.sim_", similarity))[userIND,], decreasing = betterIsHigh)]
  nb.Users = length(vect.similarity)
  
  # Création du vecteur des plus proches voisins pour le film movieID
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
