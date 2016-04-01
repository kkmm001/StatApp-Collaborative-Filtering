deja_Vu = function(data.Ratings){
  #INPUT  data.Ratings : la base des notes 
  #OUTPUT list.DejaVu  : la liste comprenant pour chaque utilisateur les films qu'il a notés
  
  vect.Users = sort(unique(data.Ratings$userID))
  list.DejaVu = lapply(vect.Users, function(x) sort(data.Ratings$movieID[data.Ratings$userID == x]))
  
  return(list.DejaVu)
  
}
