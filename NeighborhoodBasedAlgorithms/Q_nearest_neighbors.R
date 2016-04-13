Q_nearest_neighbors = function(userID, movieID, Qmax, list.dejaVu, vect.Users, mat.sim){
  #INPUT  userID              : l'identifiant de l'utilisateur
  #       movieID             : ID du film dont on veut prédire la note  
  #       Qmax                   : le nombre de voisins
  #       list.dejaVu         : la liste des films notés par utilisateur
  #       vect.Users          : l'ensemble des utilisateurs
  #       mat.sim             : la matrice de similarité
  #OUTPUT mat.neighbors       : les (au plus) Qmax plus proches voisins pour le film movieID pour userID et leur degré de similarité
  
  # Plus spécifiquement, cette fonction retourne le vecteur de taille Qmax contenant les valeurs 
  # de vect.Users (ie les identifiants des utilisateurs), à partir des calculs de proximité 
  # (au sens de la matrice de similarité), de userID pour le film movieID. 
  # Rq : un utilisateur peut se prétendre un plus proche voisin d'un utilisateur X pour le film Y
  # a condition qu'il ait noté le film Y ! D'où l'utilité de list.dejaVu 

  # La position de userID dans la matrice de similarité
  userIND = which(vect.Users == userID)
  
  # Le vecteur contenant les plus proches voisins de userID au sens général
  vect.Neighbors = order(mat.sim[userIND,], decreasing = TRUE)
  vect.Similarity = mat.sim[userIND,vect.Neighbors]
  nb.OtherUsers = length(vect.Similarity)
  
  # Création de la matrice des plus proches voisins pour le film movieID (userID et coefficient de similarité)
  mat.neighbors = matrix(NA, nrow = Qmax, ncol = 2)
  userIND2 = 1 #indice dans vect.similarity
  nb.Neighbors = 0 #indice dans mat.neighbors
  
  while((nb.Neighbors < Qmax) & (userIND2 <= nb.OtherUsers)){
    if(movieID %in% list.dejaVu[[vect.Neighbors[userIND2]]]){
      nb.Neighbors = nb.Neighbors+1
      mat.neighbors[nb.Neighbors,1] = vect.Neighbors[userIND2]
      mat.neighbors[nb.Neighbors,2] = vect.Similarity[userIND2]
    }
    userIND2 = userIND2+1
    mat.neighbors
  }
  return(mat.neighbors)
}
