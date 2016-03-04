proxi_Users_Pearson = function(mat.MoviesOfuserID1, userID2, data.Ratings,similarity="P"){
 
    mat.MoviesOfuserID2 = data.Ratings[data.Ratings$userID == userID2, c("movieID", "rating")]
    mat.MoviesOfuserID2 = mat.MoviesOfuserID2[sort.list(mat.MoviesOfuserID2[,1]),]
  
    vect.MoviesInCommon = intersect(mat.MoviesOfuserID1$movieID, mat.MoviesOfuserID2$movieID)
  
    mat.MoviesinCommon = matrix(NA,ncol = length(vect.MoviesInCommon), nrow = 2)
    mat.MoviesinCommon[1,] = mat.MoviesOfuserID1$rating[mat.MoviesOfuserID1$movieID %in% vect.MoviesInCommon]
    mat.MoviesinCommon[2,] = mat.MoviesOfuserID2$rating[mat.MoviesOfuserID2$movieID %in% vect.MoviesInCommon]
  
  if(similarity=="P"){
    res = cor(mat.MoviesinCommon[1,],mat.MoviesinCommon[2,], method="pearson")}
 }else{
   if(similarity=="DE"){
      res = dist(mat.MoviesinCommon, method="euclidean")}
   }else{
    break
   }
 }

  return(res)
}



# TODO : retirer les individus dont la variance des notes est nulle => pour cela, cette methode ne fonctionne pas
# Il faudra utiliser leur moyenne (note unique)
