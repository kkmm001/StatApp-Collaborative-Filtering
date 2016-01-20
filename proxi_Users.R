proxi_Users = function(userID1, userID2, data.Ratings){
  # proxi_Users calcule la prximité (au sens de Pearson) de deux individus
  
  #TODO(vérifier que la personne existe)
  
  mat.MoviesOfuserID1 = data.Ratings[data.Ratings$userID == userID1, c("movieID", "rating")]
  mat.MoviesOfuserID1 = mat.MoviesOfuserID1[sort.list(mat.MoviesOfuserID1[,1]),]
  
  mat.MoviesOfuserID2 = data.Ratings[data.Ratings$userID == userID2, c("movieID", "rating")]
  mat.MoviesOfuserID2 = mat.MoviesOfuserID2[sort.list(mat.MoviesOfuserID2[,1]),]
  
  vect.MoviesInCommon = intersect(mat.MoviesOfuserID1$movieID, mat.MoviesOfuserID2$movieID)
  
  mat.MoviesinCommon = matrix(NA,ncol = length(vect.MoviesInCommon), nrow = 2)
  mat.MoviesinCommon[1,] = mat.MoviesOfuserID1$rating[mat.MoviesOfuserID1$movieID %in% vect.MoviesInCommon]
  mat.MoviesinCommon[2,] = mat.MoviesOfuserID2$rating[mat.MoviesOfuserID2$movieID %in% vect.MoviesInCommon]
  
  res = cor(mat.MoviesinCommon[1,],mat.MoviesinCommon[2,], method="pearson")
  
  return(res)
}
