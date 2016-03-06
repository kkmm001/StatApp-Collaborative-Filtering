Q_neighbors = function(data.Ratings,vect.similarity,Q){
  #INPUT  Q                   : le nombre de voisins
  #       vect.similarity     : matrice des ID et indices de similarité
  #       data.Ratings        : la base des notes
  
  Nu = dim(vect.similarity)[1]
  neighbors = vector()
  i = 1
  q = 0
  while((q<Q)&(i<Nu)){
    if(israted(vect.similarity[i],movieID,data.Ratings)){
      q = q+1
      neighbors[q] = vect.similarity[1,i]
    }
    i = i+1
  }
  return(neighbors)
}
