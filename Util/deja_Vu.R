deja_Vu = function(data.Ratings){
  #INPUT data.Ratings : la base des notes 
  #OUTPUT list.DejaVu  : la liste comprenant pour chaque utilisateur les films qu'il a notés
  
  vect.Users = sort(unique(data.Ratings$userID))
  nb.Users = length(vect.Users)
  
  list.DejaVu = matrix(NA, nrow = nb.Users, ncol = 1)

  for (userIND in 1:nb.Users){
    userID = vect.Users[userIND] #Précaution au cas où des userID sont supprimés
    list.DejaVu[userID] = list(sort(data.Ratings$movieID[data.Ratings$userID == userID]))
  }              
  
  return(list.DejaVu)
  
}
