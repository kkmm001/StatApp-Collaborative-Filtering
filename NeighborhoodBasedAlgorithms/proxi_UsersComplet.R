#====================================== 1. Coefficient de corrélation de Pearson ===============================

proxi_UsersComplet_Pearson = function(userID, data.Ratings){
  
  source("./NeighborhoodBasedAlgorithms/proxi_Users.R")
  
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
        mat.cor[ind.User,2] = proxi_Users_Pearson(mat.MoviesOfuserID,user,data.Ratings)
    }
    
    return(mat.cor)
  }
  else{
    warning("L'individu ", userID1, "n'est pas dans la base")
  }
}

#====================================== 2. Distance Euclidienne ===============================

proxi_UsersComplet_euclid = function(userID, data.Ratings){
  
  source("./NeighborhoodBasedAlgorithms/proxi_Users.R")
  
  isPresent = !(is.null(intersect(userID,data.Ratings$userID)))
  
  if (isPresent){
    
    mat.MoviesOfuserID = data.Ratings[data.Ratings$userID == userID, c("movieID", "rating")]
    mat.MoviesOfuserID = mat.MoviesOfuserID[sort.list(mat.MoviesOfuserID[,1]),]
    
    vect.Users = unique(data.Ratings$userID) 
    vect.Users = vect.Users[vect.Users != userID] #suppression de l'individu en question
    
    mat.dist = matrix(NA, nrow = length(vect.Users) , ncol = 2)
    
    for(ind.User in 1:length(vect.Users)){
      user = vect.Users[ind.User]
      mat.dist[ind.User,1] = user
      mat.dist[ind.User,2] = proxi_Users_euclid(mat.MoviesOfuserID,user,data.Ratings)
    }
    
    return(mat.dist)
  }
  else{
    warning("L'individu ", userID1, "n'est pas dans la base")
  }
}


  # On teste pour un individu quelconque
#======== PEARSON ====================
mat.cor_pearson = proxi_UsersComplet_Pearson(1,TestU1)
colnames(mat.cor_pearson) = c("ID","corcoef")
mat.cor_pearson = as.data.frame(mat.cor_pearson)
mat.cor_pearson = mat.cor_pearson[apply(mat.cor_pearson,1,function(mat.cor_pearson) !any(is.na(mat.cor_pearson))),] # On supprime les NA

#======== EUCLIDE ====================
mat.dist_euclid = proxi_UsersComplet_euclid(1,TestU1)
colnames(mat.dist_euclid) = c("ID","euclidist")
mat.dist_euclid = as.data.frame(mat.dist_euclid)
mat.dist_euclid = mat.dist_euclid[apply(mat.dist_euclid,1,function(mat.dist_euclid) !any(is.na(mat.dist_euclid))),] # On supprime les NA


