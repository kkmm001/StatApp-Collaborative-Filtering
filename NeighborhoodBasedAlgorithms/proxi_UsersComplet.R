proxi_UsersComplet = function(userID, data.Ratings, similarity){
  #INPUT  userID              : l'identifiant du premier utilisateur
  #       data.Ratings        : la base des notes
  #       similarity          : la métrique utilisée pour calculer la similarité ("pearson", "nrmse", "nmae")
  #OUTPUT                     : le degré de similarité en fonction des notes entre l'utilisateur et les autres de la base des notes
  
  isPresent = !(is.null(intersect(userID,data.Ratings$userID)))
  
  if (isPresent){
  
    mat.MoviesOfuserID = data.Ratings[data.Ratings$userID == userID, c("movieID", "rating")]
    mat.MoviesOfuserID = mat.MoviesOfuserID[sort.list(mat.MoviesOfuserID[,1]),]
    
    vect.Users = unique(data.Ratings$userID) 
    vect.Users = vect.Users[vect.Users != userID] #suppression de l'individu en question
    
    mat.cor = matrix(NA, nrow = length(vect.Users) , ncol = 2)
    for(userIND in 1:length(vect.Users)){
        user = vect.Users[userIND]
        mat.cor[userIND,1] = user
        mat.cor[ind.User,2] = proxi_Users(mat.MoviesOfuserID,user,data.Ratings, similarity = similarity)
    }
    
    return(mat.cor)
  }
  
  else{
    warning("L'individu ", userID, "n'est pas dans la base")
  }
  
}

  # On teste pour un individu quelconque
#======== PEARSON ====================
#mat.cor_pearson = proxi_UsersComplet_Pearson(1,TestU1)
#colnames(mat.cor_pearson) = c("ID","corcoef")
#mat.cor_pearson = as.data.frame(mat.cor_pearson)
#mat.cor_pearson = mat.cor_pearson[apply(mat.cor_pearson,1,function(mat.cor_pearson) !any(is.na(mat.cor_pearson))),] # On supprime les NA

#======== EUCLIDE ====================
#mat.dist_euclid = proxi_UsersComplet_euclid(1,TestU1)
#colnames(mat.dist_euclid) = c("ID","euclidist")
#mat.dist_euclid = as.data.frame(mat.dist_euclid)
#mat.dist_euclid = mat.dist_euclid[apply(mat.dist_euclid,1,function(mat.dist_euclid) !any(is.na(mat.dist_euclid))),] # On supprime les NA
