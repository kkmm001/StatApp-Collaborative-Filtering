modify_mat.sim = function(mat.sim, data.Ratings, userID1, similarity, mat.InCommon, nbMin.InCommon){
  # INPUT mat.sim       : la matrice des similarités d'origine
  #       data.Ratings  : la base des notes$
  #       userID        : l'identifiant de l'utilisateur dont on va changer les notes
  #       proportion    : la proportion des notes de l'utilisateur qui va être modifiée
  #       list.dejaVu   : la liste des films notés par utilisateur
  # OUTPUT              : la nouvelle matrice des similarités
  
  # Ensemble des utilisateurs
  vect.Users = sort(unique(data.Ratings$userID))
  nb.Users = length(vect.Users)
  
  # Indice de userID dans la matrice 
  userIND1 = which(vect.Users == userID1)
  
  # Matrice contenant les couples (film, note) de l'utilisateur userID trié par identifiant de film
  mat.MoviesOfuserID1 = data.Ratings[data.Ratings$userID == userID1, c("movieID", "rating")]
  mat.MoviesOfuserID1 = mat.MoviesOfuserID1[sort.list(mat.MoviesOfuserID1[,1]),]
  
  # Calcul des nouvelles similarités de userID et de tous les autres utilisateurs
  for (userIND2 in 1:(nb.Users-1)){
    userID2 = vect.Users[userIND2]

    sim = proxi_Users(mat.MoviesOfuserID1, userID2, data.Ratings, similarity)
      
    mat.sim[userIND1,userIND2] = sim
    mat.sim[userIND2,userIND1] = sim
  }
  
  # filtrage par nbMin.InCommon
  mat.sim = filtrer_similarite(mat.sim, mat.InCommon, nbMin.InCommon)
  
  return(mat.sim)
  
}