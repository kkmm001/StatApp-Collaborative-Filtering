modify_data.Ratings = function(data.Ratings, userID, taux, list.dejaVu){
  # INPUT data.Ratings  : la base des notes$
  #       userID        : l'identifiant de l'utilisateur dont on va changer les notes
  #       taux          : le taux de note de l'utilisateur qui va être modifiée (entre 0 et 100)
  #       list.dejaVu   : la liste des films notés par utilisateur
  # OUTPUT              : la nouvelle base des notes
  
  # Les films notés par l'utilisateur
  vect.MoviesRated = list.dejaVu[[userID]]
  
  # Le nombre de films notés par l'utilisateur
  nb.MoviesRated = length(vect.MoviesRated)
  
  # Le nombre de notes qui vont être modifiées
  nb.ChangedRatings = floor(nb.MoviesRated * taux/100)
  
  # Les films qui vont voir leur note modifiée
  vect.MoviesChangedRated = sample(vect.MoviesRated, nb.ChangedRatings)
  
  # Changement des notes
  for(rating in 1:dim(data.Ratings)[1]){
    movie = data.Ratings$movieID[rating]
    user = data.Ratings$userID[rating]
    if(movie %in% vect.MoviesChangedRated & user == userID){
      realRating = data.Ratings$rating[rating]
      if(realRating == 1){
        modifiedRating = 2
      }
      else if(realRating == 5){
        modifiedRating = 4
      }
      else{
        delta = rbinom(1,1,prob = 0.5)
        modifiedRating = realRating + (2*delta-1) #note finale = note initiale +/- 1
      }
      data.Ratings$rating[rating] = modifiedRating
    }
  }
  
  return(data.Ratings)
  
  
  
  
  
  
  
}