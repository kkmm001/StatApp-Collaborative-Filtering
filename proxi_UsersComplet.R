proxi_UsersComplet = function(userID, data.Ratings){
  
  # proxi_Users calcule la proximite (au sens de Pearson) avec tous les individus de la base
  
  isPresent = !(is.null(intersect(userID,data.Ratings$userID)))
  
  if (isPresent){
  
    mat.MoviesOfuserID = data.Ratings[data.Ratings$userID == userID, c("movieID", "rating")]
    mat.MoviesOfuserID = mat.MoviesOfuserID[sort.list(mat.MoviesOfuserID[,1]),]
    
    vect.Users = unique(data.Ratings$userID) 
    vect.Users = vect.Users[vect.Users != userID] #suppression de l'individu en question
    
    vect.cor = matrix(NA, nrow = length(vect.Users) , ncol = 2)
    
    for(ind.User in 1:length(vect.Users)){
        user = vect.Users[ind.User]
        vect.cor[ind.User,1] = user
        vect.cor[ind.User,2] = proxi_Users(mat.MoviesOfuserID,user,data.Ratings)
    }
    
    return(vect.cor)
  }
  else{
    warning("L'individu ", userID1, "n'est pas dans la base")
  }
}
