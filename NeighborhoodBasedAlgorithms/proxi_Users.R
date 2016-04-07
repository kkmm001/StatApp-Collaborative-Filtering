proxi_Users = function(mat.MoviesOfuserID1, userID2, data.Ratings, similarity){
  #INPUT  mat.MoviesOfUserID1 : l'ensemble des (films,notes) vus par le premier utilisateur ordonné par l'ID des films
  #       userID2             : l'identifiant du second utilisateur
  #       data.Ratings        : la base des notes
  #       similarity          : la métrique utilisée pour calculer la similarité ("pearson", "nrmse", "nmae", "RFP")
  #OUTPUT                     : le degré de similarité en fonction des notes entre les deux utilisateurs
  
  # Matrice contenant les couples (film, note) de l'utilisateur userID2 trié par identifiant de film
  mat.MoviesOfuserID2 = data.Ratings[data.Ratings$userID == userID2, c("movieID", "rating")]
  mat.MoviesOfuserID2 = mat.MoviesOfuserID2[sort.list(mat.MoviesOfuserID2[,1]),]
  
  # Vecteur contenant les films vus par userID1 et userID2
  vect.MoviesInCommon = intersect(mat.MoviesOfuserID1$movieID, mat.MoviesOfuserID2$movieID)
  
  # Matrice contenant les notes des films vus en commun par userID1 et userID2
  mat.MoviesinCommon = matrix(NA,ncol = length(vect.MoviesInCommon), nrow = 2)
  mat.MoviesinCommon[1,] = mat.MoviesOfuserID1$rating[mat.MoviesOfuserID1$movieID %in% vect.MoviesInCommon]
  mat.MoviesinCommon[2,] = mat.MoviesOfuserID2$rating[mat.MoviesOfuserID2$movieID %in% vect.MoviesInCommon]
  
  # Calcul des degrés de similarité
  # TODO ATTENTION AUX INFINITE DANS NMAE ET NRMSE
  switch(similarity,
         'pearson' = cor(mat.MoviesinCommon[1,],mat.MoviesinCommon[2,], method="pearson"),
         'nrmse'   = as.numeric(length(vect.MoviesInCommon)/dist(mat.MoviesinCommon, method="euclidean")),
         'nmae'    = as.numeric(length(vect.MoviesInCommon)/dist(mat.MoviesinCommon, method="manhattan")),
         'RFP'     = cor(mat.MoviesinCommon[1,],mat.MoviesinCommon[2,], method="pearson") * log(length(vect.MoviesInCommon))
  )
  
}