proxi_UsersComplet = function(userID, data.Ratings){
  
  # proxi_Users calcule la prximité (au sens de Pearson) avec tous les individus de la base
  #TODO(vérifier que la personne existe)
  
  vect.Users = unique(data.Ratings$userID) #Penser à supprimer l'individu en question 
  vect.cor = matrix(NA, nrow = 1 , ncol = length(vect.Users))
  
  for(user in vect.Users){
      vect.cor[user] = proxi_Users(userID,user,data.Ratings)
  }
  
  return(vect.cor)
}
