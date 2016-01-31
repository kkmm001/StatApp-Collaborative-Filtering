proxi_UsersComplet = function(userID, data.Ratings){
  
  # proxi_Users calcule la proximite (au sens de Pearson) avec tous les individus de la base
  
  isPresent = !(is.null(intersect(userID,data.Ratings$userID)))
  
  if (isPresent){
  
    mat.MoviesOfuserID = data.Ratings[data.Ratings$userID == userID, c("movieID", "rating")]
    mat.MoviesOfuserID = mat.MoviesOfuserID[sort.list(mat.MoviesOfuserID[,1]),]
    
    vect.Users = unique(data.Ratings$userID) 
    vect.Users = vect.Users[vect.Users != userID] #suppression de l'individu en question
    
    mat.cor = matrix(NA, nrow = length(vect.Users) , ncol = 2)
    
    for(ind.User in 1:length(vect.Users)){
        user = vect.Users[ind.User]
        mat.cor[ind.User,1] = user
        mat.cor[ind.User,2] = proxi_Users(mat.MoviesOfuserID,user,data.Ratings)
    }
    
    return(mat.cor)
  }
  else{
    warning("L'individu ", userID1, "n'est pas dans la base")
  }
}

  # On teste pour un individu quelconque

mat.cor = proxi_UsersComplet(1,TestU1)
colnames(mat.cor) = c("ID","corcoef")
mat.cor = as.data.frame(mat.cor)
mat.cor[apply(mat.cor,1,function(mat.cor) !any(is.na(mat.cor))),] # On supprime les NA
