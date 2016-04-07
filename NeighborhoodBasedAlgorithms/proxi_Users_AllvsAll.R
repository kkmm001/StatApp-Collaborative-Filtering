proxi_Users_AllvsAll = function(data.Ratings, similarity){
  #INPUT    data.Ratings  : la base des notes
  #         similarity    : la métrique utilisée pour calculer la similarité ("pearson", "nrmse", "nmae", "RFP")
  #OUTPUT   mat.sim       : matrice contenant le degré de similarité entre les utilisateurs
  
  # Dimension du problème
  vect.Users = sort(unique(data.Ratings$userID))
  nb.Users = length(vect.Users) # nombre d’individus différents dans la data frame data.Ratings
  
  # Création de la matrice des similarités
  mat.sim = matrix(NA, nrow = nb.Users, ncol = nb.Users) 
  
  cat(nb.Users)
  
  for (userIND1 in 1:(nb.Users-1)){
    
    cat(sprintf("|%0.f", userIND1))
    
    userID1 = vect.Users[userIND1]
    mat.MoviesOfuserID1 = data.Ratings[data.Ratings$userID == userID1, c("movieID", "rating")]
    mat.MoviesOfuserID1 = mat.MoviesOfuserID1[sort.list(mat.MoviesOfuserID1[,1]),]
    
    for(userIND2 in (userID1+1):nb.Users){
      
      userID2 = vect.Users[userIND2]
      sim = proxi_Users(mat.MoviesOfuserID1, userID2, data.Ratings, similarity)
      
      mat.sim[userIND1,userIND2] = sim
      mat.sim[userIND2,userIND1] = sim
    }
  }
  
  return(mat.sim)
}
