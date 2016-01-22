# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =#
#       Fichier : recap_Users.R                                                                    #
#       Description : Fonction récap des users sur les bases                                       #
# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =#

stat_Users=function(data.Ratings){ 
  
  vect.Users = unique(data.Ratings$userID)
  nb.Users = length(vect.Users)
  
  stat.RatingPerUser = as.data.frame(matrix(0, nrow = nb.Users, ncol = 7))
  colnames(stat.RatingPerUser) = c("userID", "nb.Ratings","mean", "sd", "max", "min", "med")
  # matrice comprenant l'ID de l'utilisateur 
  #                    le nombre de films notes par utilisateur
  #                    la moyenne des notes
  #                    l'ecart-type des notes
  #                    la note maximale 
  #                    la note minimale
  #                    la mede des notes
  
  for (user_fakeID in 1:nb.Users){ 
    user = vect.Users[user_fakeID]
    x=data.Ratings$rating[data.Ratings$userID == user]
    stat.RatingPerUser[user_fakeID,] = c(user,length(x),round(mean(x),2),round(sd(x),2),max(x),min(x),median(x))
  }
  
  return(stat.RatingPerUser)
  
}
