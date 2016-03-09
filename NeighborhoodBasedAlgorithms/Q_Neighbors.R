Q_neighbors = function(data.Ratings,vect.similarity,movieID,Q){
  #INPUT  Q                   : le nombre de voisins
  #       movieID             : ID du film dont on veut prédire la note
  #       vect.similarity     : matrice des ID et indices de similarité
  #       data.Ratings        : la base des notes
  
  Nu = length(vect.similarity)
  neighbors = vector()
  i = 1
  q = 0
  while((q<Q)&(i<Nu)){
    if(isRated(vect.similarity[i],movieID,data.Ratings)){
      q = q+1
      neighbors[q] = vect.similarity[i]
    }
    i = i+1
  }
  return(neighbors)
}
