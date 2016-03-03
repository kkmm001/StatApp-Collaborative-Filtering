#====================================== 1. Coefficient de corrélation de Pearson ===============================

proxi_Users_Pearson = function(mat.MoviesOfuserID1, userID2, data.Ratings){
  
    mat.MoviesOfuserID2 = data.Ratings[data.Ratings$userID == userID2, c("movieID", "rating")]
    mat.MoviesOfuserID2 = mat.MoviesOfuserID2[sort.list(mat.MoviesOfuserID2[,1]),]
    
    vect.MoviesInCommon = intersect(mat.MoviesOfuserID1$movieID, mat.MoviesOfuserID2$movieID)
    
    mat.MoviesinCommon = matrix(NA,ncol = length(vect.MoviesInCommon), nrow = 2)
    mat.MoviesinCommon[1,] = mat.MoviesOfuserID1$rating[mat.MoviesOfuserID1$movieID %in% vect.MoviesInCommon]
    mat.MoviesinCommon[2,] = mat.MoviesOfuserID2$rating[mat.MoviesOfuserID2$movieID %in% vect.MoviesInCommon]
    
    res = cor(mat.MoviesinCommon[1,],mat.MoviesinCommon[2,], method="pearson")
    
    return(res)
}

#====================================== 2. Distance euclidienne ===============================

proxi_Users_Euclid = function(mat.MoviesOfuserID1, userID2, data.Ratings){
  # proxi_Users_euclid calcule la proximite (au sens de la distance euclidienne) de deux individus
  
  mat.MoviesOfuserID2 = data.Ratings[data.Ratings$userID == userID2, c("movieID", "rating")]
  mat.MoviesOfuserID2 = mat.MoviesOfuserID2[sort.list(mat.MoviesOfuserID2[,1]),]
  
  vect.MoviesInCommon = intersect(mat.MoviesOfuserID1$movieID, mat.MoviesOfuserID2$movieID)
  
  mat.MoviesinCommon = matrix(NA,ncol = length(vect.MoviesInCommon), nrow = 2)
  mat.MoviesinCommon[1,] = mat.MoviesOfuserID1$rating[mat.MoviesOfuserID1$movieID %in% vect.MoviesInCommon]
  mat.MoviesinCommon[2,] = mat.MoviesOfuserID2$rating[mat.MoviesOfuserID2$movieID %in% vect.MoviesInCommon]
  
  res = dist(mat.MoviesinCommon, method="euclidean")
  
  return(res)
}




#TODO : retirer les individus dont la variance des notes est nulle => pour cela, cette methode ne fonctionne pas
# Il faudra utiliser leur moyenne (note unique)
