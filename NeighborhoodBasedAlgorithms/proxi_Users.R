proxi_Users = function(mat.MoviesOfuserID1, userID2, data.Ratings,similarity){
  #INPUT  mat.MoviesOfUserID1 : l'ensemble des films et leur note vus par le premier utilisateur ordonné par l'ID des films
  #       userID2             : l'identifiant du second utilisateur
  #       data.Ratings        : la base des notes
  #       similarity          : la métrique utilisée pour calculer la similarité ("pearson", "nrmse", "nmae")
  #OUTPUT                     : le degré de similarité en fonction des notes entre les deux utilisateurs
  
  mat.MoviesOfuserID2 = data.Ratings[data.Ratings$userID == userID2, c("movieID", "rating")]
  mat.MoviesOfuserID2 = mat.MoviesOfuserID2[sort.list(mat.MoviesOfuserID2[,1]),]
  
  vect.MoviesInCommon = intersect(mat.MoviesOfuserID1$movieID, mat.MoviesOfuserID2$movieID)
  
  print(vect.MoviesInCommon)
  print(mat.MoviesOfuserID1$rating[mat.MoviesOfuserID1$movieID %in% vect.MoviesInCommon])
  print(mat.MoviesOfuserID2$rating[mat.MoviesOfuserID2$movieID %in% vect.MoviesInCommon])
  
  mat.MoviesinCommon = matrix(NA,ncol = length(vect.MoviesInCommon), nrow = 2)
  mat.MoviesinCommon[1,] = mat.MoviesOfuserID1$rating[mat.MoviesOfuserID1$movieID %in% vect.MoviesInCommon]
  mat.MoviesinCommon[2,] = mat.MoviesOfuserID2$rating[mat.MoviesOfuserID2$movieID %in% vect.MoviesInCommon]
  
  switch(similarity,
         'pearson' = cor(mat.MoviesinCommon[1,],mat.MoviesinCommon[2,], method="pearson"),
         'nrmse'   = as.numeric(dist(mat.MoviesinCommon, method="euclidean")/length(vect.MoviesInCommon)),
         'nmae'    = as.numeric(dist(mat.MoviesinCommon, method="manhattan")/length(vect.MoviesInCommon))
  )
}


#TODO : retirer les individus dont la variance des notes est nulle => pour cela, cette methode ne fonctionne pas
# Il faudra utiliser leur moyenne (note unique)
