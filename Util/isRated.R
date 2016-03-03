isRated = function(userID, movieID, data.Ratings){
  #INPUT  userID        : l'ID de l'utilisateur
  #       movieID       : l'ID du film
  #       data.Ratings  : la base des notes
  #OUTPUT               : renvoie TRUE si userID a noté le film movieID, sinon FALSE
  
  res = movieID %in% data.Ratings[data.Ratings$user == userID,"movieID"]
  return(res)
}