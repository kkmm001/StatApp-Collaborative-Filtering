nb_MoviesInCommon = function(data.Ratings){
  #INPUT  data.Ratings  : la base des notes
  #OUTPUT mat.com       : la matrice contenant le nombre de films notés en commun pour chaque couple d'utilisateurs

  vect.Users = sort(unique(data.Ratings$userID))
  nb.Users = length(vect.Users) # nombre d’individus différents dans la data frame data.Ratings
  
  mat.com = matrix(NA, nrow = nb.Users, ncol = nb.Users) 
  
  for (userIND1 in 1:(nb.Users-1)){
    
    userID1 = vect.Users[userIND1]
    mat.MoviesOfuserID1 = data.Ratings[data.Ratings$userID == userID1, c("movieID", "rating")]
    mat.MoviesOfuserID1 = mat.MoviesOfuserID1[sort.list(mat.MoviesOfuserID1[,1]),]
    
    for(userIND2 in (userID1+1):nb.Users){
      
      userID2 = vect.Users[userIND2]
      mat.MoviesOfuserID2 = data.Ratings[data.Ratings$userID == userID2, c("movieID", "rating")]
      mat.MoviesOfuserID2 = mat.MoviesOfuserID2[sort.list(mat.MoviesOfuserID2[,1]),]
      
      inCommon = length(intersect(mat.MoviesOfuserID1$movieID, mat.MoviesOfuserID2$movieID))
      
      mat.com[userIND1, userIND2] = inCommon
      mat.com[userIND2, userIND1] = inCommon 
    }
  }
  
  return(mat.com)
}